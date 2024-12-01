#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pz ../

-import(common, [read_lines/1, read_integer_list/1, transpose/1]).
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
  Lines = common:read_lines(Filename),
  common:transpose(lists:map(fun common:read_integer_list/1, Lines)).
