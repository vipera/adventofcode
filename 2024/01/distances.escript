#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
  LocationIds = read_location_ids(Filename),
  TotalDistance = total_distance(LocationIds),
  io:format("Total distance: ~p~n", [TotalDistance]).

read_location_ids(Filename) ->
  lists:map(fun lists:sort/1, read_number_columns(Filename)).

total_distance([ Ids1, Ids2 | _ ]) ->
  list_distance(Ids1, Ids2).

list_distance(L1, L2) ->
  list_distance(L1, L2, 0).
list_distance([], _, Dist) ->
  Dist;
list_distance([Id1 | R1], [Id2 | R2], Dist) ->
  list_distance(R1, R2, Dist + abs(Id1 - Id2)).

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
