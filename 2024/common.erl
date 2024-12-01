-module(common).
-export([
    read_lines/1,
    transpose/1,
    read_integer_list/1,
    read_integer/1
]).

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
