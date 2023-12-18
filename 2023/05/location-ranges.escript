#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
  [SeedsLine | MappingLines] = read_lines(Filename),
  Seeds = read_seeds(SeedsLine),
  SeedRanges = seeds_to_ranges(Seeds),
  find_mappings(SeedRanges, MappingLines).

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, <<"\n">>, [global, trim_all]).

read_seeds(<<"seeds: ", SeedsRest/binary>>) ->
  read_integer_list(SeedsRest).

seeds_to_ranges(Seeds) ->
  lists:sort(
    fun ({A, _}, {B, _}) -> A < B end,
    lists:reverse(seeds_to_ranges(Seeds, []))
  ).
seeds_to_ranges([], Ranges) ->
  Ranges;
seeds_to_ranges([Start, Length | Seeds], Ranges) ->
  seeds_to_ranges(Seeds, [{Start, Start + Length} | Ranges]).

find_mappings(SeedRanges, MappingLines) ->
  io:format("SeedRanges: ~p~n", [SeedRanges]),
  Mappings = parse_mappings(MappingLines),
  [{RangeStart, _} | MappedSeedRangesRest] = seeds_lookup(seed, location, SeedRanges, Mappings),
  Min = lists:foldl(
    fun({Start, _}, Min) -> case Start < Min of true -> Start; false -> Min end end,
    RangeStart,
    MappedSeedRangesRest
  ),
  io:format("Min mapping: ~p~n", [Min]).

parse_mappings(MappingLines) ->
  lists:reverse(parse_mappings(MappingLines, [])).
parse_mappings([], Acc) ->
  Acc;
parse_mappings([MappingLine | MappingLines], Acc) ->
  case binary:split(MappingLine, <<" ">>) of
    [ToMapping, <<"map:">>] ->
      Header = parse_mapping_header(ToMapping),
      parse_mappings(MappingLines, [Header | Acc]);
    _ ->
      Range = parse_mapping(MappingLine),
      parse_mappings(MappingLines, [Range | Acc])
  end.

parse_mapping_header(Header) ->
  [FromBin, ToBin] = binary:split(Header, <<"-to-">>),
  From = binary_to_atom(FromBin),
  To = binary_to_atom(ToBin),
  {From, To}.

parse_mapping(MappingLine) ->
  [RangeStart, SourceRangeStart, RangeLength] = read_integer_list(MappingLine),
  {SourceRangeStart, SourceRangeStart + RangeLength, RangeStart}.

% Reached final mapping
seeds_lookup(FinalType, FinalType, SeedRanges, _) ->
  SeedRanges;
% Run out of mappings - doesn't map to anything
seeds_lookup(_, _, _, []) ->
  [];
% No more seed ranges
seeds_lookup(_, _, [], _) ->
  [];

seeds_lookup(Type, FinalType, [SeedRange | SeedRanges], [{Type, To} | OtherMappings] = Mappings) ->
  MappedSeeds = seeds_range_lookup([SeedRange], OtherMappings),
  OtherMappedSeeds = seeds_lookup(Type, To, SeedRanges, Mappings),
  seeds_lookup(To, FinalType, MappedSeeds ++ OtherMappedSeeds, Mappings);

% Next element
seeds_lookup(Type, FinalType, SeedRanges, [_ | Mappings]) ->
  seeds_lookup(Type, FinalType, SeedRanges, Mappings).

seeds_range_lookup(SeedRanges, Mappings) ->
  seeds_range_lookup(SeedRanges, Mappings, Mappings).

% Reached end of mappings - maps to itself
seeds_range_lookup(SeedRanges, [], _) ->
  SeedRanges;
% Reach end of ranges list into new category - maps to itself
seeds_range_lookup(SeedRanges, [{_From, _To} | _], _) ->
  SeedRanges;
% No more ranges
seeds_range_lookup([], _, _) ->
  [];
% Range completely before or completely after mapping
seeds_range_lookup([{Start, End} | _] = SeedRanges, [{MapStart, MapEnd, _} | Mappings], OgMappings)
  when End < MapStart; Start > MapEnd ->
    seeds_range_lookup(SeedRanges, Mappings, OgMappings);
% Range starts before mapping - make two ranges and try again
seeds_range_lookup([{Start, End} | SeedRanges], [{MapStart, _, _} | _] = Mappings, OgMappings)
  when Start < MapStart ->
    seeds_range_lookup([{Start, MapStart - 1}, {MapStart, End} | SeedRanges], Mappings, OgMappings);
% Range ends after mapping - make two ranges and try again
seeds_range_lookup([{Start, End} | SeedRanges], [{_, MapEnd, _} | _] = Mappings, OgMappings)
  when End > MapEnd ->
    seeds_range_lookup([{Start, MapEnd}, {MapEnd + 1, End} | SeedRanges], Mappings, OgMappings);
% Range is entirely contained in mapping
seeds_range_lookup([{Start, End} | SeedRanges], [{MapStart, _, MapNewStart} | _], OgMappings) ->
  [{MapNewStart - MapStart + Start, MapNewStart - MapStart + End}] ++
  seeds_range_lookup(SeedRanges, OgMappings, OgMappings).

read_integer_list(B) ->
  lists:reverse(read_integer_list(B, [])).
read_integer_list(<<" ", Rest/binary>>, Acc) ->
  read_integer_list(Rest, Acc);
read_integer_list(<<>>, Acc) ->
  Acc;
read_integer_list(B, Acc) ->
  {N, Rest} = read_integer(B),
  read_integer_list(Rest, [N | Acc]).

read_integer(B) ->
  read_integer(B, 0).
read_integer(<<Char:8, Rest/binary>>, Acc) when Char >= $0, Char =< $9 ->
  Tens = Char - $0,
  read_integer(Rest, Acc * 10 + Tens);
read_integer(B, Acc) ->
  {Acc, B}.
