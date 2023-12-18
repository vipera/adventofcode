#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
  [SeedsLine | MappingLines] = read_lines(Filename),
  Seeds = read_seeds(SeedsLine),
  find_mappings(Seeds, MappingLines).

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, <<"\n">>, [global, trim_all]).

read_seeds(<<"seeds: ", SeedsRest/binary>>) ->
  read_integer_list(SeedsRest).

find_mappings(Seeds, MappingLines) ->
 io:format("Seeds: ~p~n", [Seeds]),
  Mappings = parse_mappings(MappingLines),
  MappedSeeds = lists:map(
    fun (Seed) -> seed_lookup(seed, location, Seed, Mappings) end,
    Seeds
  ),
  Result = lists:zip(Seeds, MappedSeeds, trim),
  io:format("Mappings~n"),
  lists:foreach(fun({Seed, Mapping}) -> io:format("~p - ~p~n", [Seed, Mapping]) end, Result),
  Min = lists:min(MappedSeeds),
  io:format("Minimal mapping: ~p~n", [Min]).

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
  {RangeStart, SourceRangeStart, RangeLength}.

% Reached final mapping
seed_lookup(FinalType, FinalType, Seed, _) ->
  Seed;
% Run out of mappings - doesn't map to anything
seed_lookup(_, _, _, []) ->
  nil;
% Mappings header
seed_lookup(Type, FinalType, Seed, [{Type, To} | MappingLines]) ->
  MappedSeed = seed_range_lookup(Seed, MappingLines),
  seed_lookup(To, FinalType, MappedSeed, MappingLines);
% Next element
seed_lookup(Type, FinalType, Seed, [_ | MappingLines]) ->
  seed_lookup(Type, FinalType, Seed, MappingLines).

% Reached end of mappings - maps to itself
seed_range_lookup(Seed, []) ->
  Seed;
% Reach end of ranges list into new category - maps to itself
seed_range_lookup(Seed, [{_From, _To} | _]) ->
  Seed;
% If in range, map to new range
seed_range_lookup(Seed, [{RangeStart, SourceRangeStart, RangeLength} | _])
  when Seed >= SourceRangeStart, Seed < SourceRangeStart + RangeLength ->
    RangeStart - SourceRangeStart + Seed;
% Next element
seed_range_lookup(Seed, [_ | MappingLines]) ->
  seed_range_lookup(Seed, MappingLines).

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
