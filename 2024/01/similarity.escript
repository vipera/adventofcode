#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
  LocationIds = read_location_ids(Filename),
  Similarity = similarity(LocationIds),
  io:format("Similarity: ~p~n", [Similarity]).

read_location_ids(Filename) ->
  lists:map(fun lists:sort/1, read_number_columns(Filename)).

similarity([ Ids1, Ids2 | _ ]) ->
  Appearances = appearances(Ids2),
  similarity_score(Ids1, Appearances).

appearances(L) -> appearances(L, []).
appearances([], Appearances) ->
  Appearances;
appearances([ Id | Rest ], Appearances) ->
  NewAppearances = case lists:keyfind(Id, 1, Appearances) of
    false -> [{Id, 1} | Appearances];
    {_, Count} -> lists:keyreplace(Id, 1, Appearances, {Id, Count + 1})
  end,
  appearances(Rest, NewAppearances).

similarity_score(Ids, Appearances) -> similarity_score(Ids, Appearances, 0).
similarity_score([], _, Score) -> Score;
similarity_score([ Id | Rest ], Appearances, Score) ->
  Similarity = case lists:keyfind(Id, 1, Appearances) of
    false -> 0;
    {_, Count} -> Count * Id
  end,
  similarity_score(Rest, Appearances, Score + Similarity).

read_number_columns(Filename) ->
  Lines = read_lines(Filename),
  transpose(lists:map(fun read_integer_list/1, Lines)).

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, <<"\n">>, [global, trim_all]).

transpose([ [] | _ ]) -> [];
transpose(L) ->
  [lists:map(fun hd/1, L) | transpose(lists:map(fun tl/1, L))].

read_integer_list(Str) ->
  lists:reverse(read_integer_list(Str, [])).
read_integer_list(<<>>, Acc) ->
  Acc;
read_integer_list(<<"   ", Rest/binary>>, Acc) ->
  read_integer_list(Rest, Acc);
read_integer_list(Str, Acc) ->
  {N, Rest} = read_integer(Str),
  read_integer_list(Rest, [N | Acc]).

read_integer(B) ->
  read_integer(B, 0).
read_integer(<<Char:8, Rest/binary>>, Acc) when Char >= $0, Char =< $9 ->
  Tens = Char - $0,
  read_integer(Rest, Acc * 10 + Tens);
read_integer(B, Acc) ->
  {Acc, B}.
