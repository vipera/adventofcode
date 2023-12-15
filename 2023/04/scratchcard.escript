#!/usr/bin/env escript

-mode(compile).

main([Filename]) ->
  CardLines = read_lines(Filename),
  CardInfos = lists:map(fun read_card_info/1, CardLines),
  Score = score_cards(CardInfos),
  WonScratchcards = won_scratchcards(CardInfos),
  io:format("Points: ~p~nScratchcards: ~p~n", [Score, WonScratchcards]).

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, <<"\n">>, [global, trim_all]).

read_card_info(<<"Card ", Rest/binary>>) ->
  {CardId, CardIdRest} = read_integer(Rest),
  CardSepRest = discard_until_after(CardIdRest, $:),
  [Winning, Picked] = lists:map(fun read_integer_list/1,
                                binary:split(CardSepRest, <<"|">>)),
  {CardId, Winning, Picked}.

discard_until_after(<<>>, _) ->
  <<>>;
discard_until_after(<<UntilChar, Rest/binary>>, UntilChar) ->
  Rest;
discard_until_after(<<_:8, Rest/binary>>, UntilChar) ->
  discard_until_after(Rest, UntilChar).

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

score_cards(CardInfos) ->
  lists:foldl(fun(CardInfo, Sum) ->
                {_, Score} = score_card(CardInfo),
                Sum + Score
              end, 0, CardInfos).

score_card(CardInfo) ->
  score_card(CardInfo, 0, 0).
score_card({_, _, []}, WinCount, Score) ->
  {WinCount, Score};
score_card({_CardId, Winning, [Picked | PickedRest]}, WinCount, Score) ->
  {NewWinCount, NewScore} = case lists:member(Picked, Winning) of
    true -> {WinCount + 1, case Score of
        0 -> 1;
        NonZero -> NonZero * 2
      end};
    false -> {WinCount, Score}
  end,
  score_card({_CardId, Winning, PickedRest}, NewWinCount, NewScore).

won_scratchcards(CardInfos) ->
  won_scratchcards(CardInfos, 0, length(CardInfos)).
won_scratchcards([], Count, _) ->
  Count;
won_scratchcards(_, Count, 0) ->
  Count;
won_scratchcards([CardInfo | CardInfos], Count, Remaining) ->
  {WinCount, _} = score_card(CardInfo),
  won_scratchcards(CardInfos, 0, WinCount) +
    won_scratchcards(CardInfos, Count + 1, Remaining - 1).
