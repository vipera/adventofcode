#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ../

-import(common, [read_lines/1, read_integer_list/1, transpose/1]).
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
  Lines = common:read_lines(Filename),
  common:transpose(lists:map(fun common:read_integer_list/1, Lines)).
