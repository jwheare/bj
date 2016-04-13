-module(bj_cards).

-include_lib("eunit/include/eunit.hrl").

-export([deck/0, blackjack/1, hand_values/1]).

deck() ->
    (lists:seq($🂡, $🂮) -- [$🂬]) ++
    (lists:seq($🂱, $🂾) -- [$🂼]) ++
    (lists:seq($🃁, $🃎) -- [$🃌]) ++
    (lists:seq($🃑, $🃞) -- [$🃜]).

hand_values_test_() ->
    Tests = [
        {"🂢🂲", [4], false},
        {"🂢🂲🃂🃒", [8], false},
        {"🂢🂲🃇🃘", [19], false},
        {"🂮🂵🃆", [21], false},
        {"🂫🂽🃃", [23], false},
        {"🃙🂱🃃🃞", [23, 33], false},
        {"🂫🂸🃉", [27], false},
        {"🂡🂮", [11, 21], true},
        {"🂡🃊", [11, 21], true},
        {"🃁🃅", [6, 16], false},
        {"🃑🂵🃇", [13, 23], false},
        {"🃁🃑", [2, 12], false},
        {"🃚🃍🂱", [21, 31], false},
        {"🂡🂱🃁🃑", [4, 14], false},
        {"🃁🃑🂮🃋", [22, 32], false},
        {"🂡🂱🃁🃑🂮🃋", [24, 34], false},
        {"🂺🃍🃑", [21, 31], false}
    ],
    lists:foldl(fun ({Hand, Expected, Blackjack}, Acc) ->
        Actual = hand_values(Hand),
        ActualBlackjack = blackjack(Hand),
        [
            {Hand, ?_assertMatch(Expected, Actual)},
            {Hand ++ " blackjack?", ?_assertMatch(Blackjack, ActualBlackjack)}
            | Acc
        ]
    end, [], Tests).
    
blackjack(Hand) ->
    lists:max(hand_values(Hand)) =:= 21 andalso length(Hand) =:= 2.

hand_values(Hand) when is_list(Hand) ->
    case lists:foldl(fun (Card, {LowAcc, HighAcc}) ->
        case card_value(Card) of
            {Low, High} when LowAcc =:= HighAcc ->
                % No aces so far
                {Low + LowAcc, High + HighAcc};
            {Low, _High} ->
                % Had an ace, only increment with the low value, otherwise it's a bust
                {Low + LowAcc, Low + HighAcc};
            Val ->
                {Val + LowAcc, Val + HighAcc}
        end
    end, {0, 0}, Hand) of
        {L, H} when L =:= H ->
            [L];
        {L, H} ->
            [L, H]
    end.

card_value_test_() ->
    Tests = [
        {$🂢, 2},
        {$🃒, 2},
        {$🃘, 8},
        {$🂮, 10},
        {$🂮, 10},
        {$🂫, 10},
        {$🂽, 10},
        {$🃞, 10},
        {$🃊, 10},
        {$🃑, {1, 11}}
    ],
    lists:foldl(fun ({Hand, Expected}, Acc) ->
        Actual = card_value(Hand),
        [{[Hand], ?_assertMatch(Expected, Actual)} | Acc]
    end, [], Tests).

card_value(Card) when is_integer(Card) ->
    lists:foldl(fun
        (Start, 0) when Card > (Start + 1), Card < (Start + 11) ->
            Card - Start;
        (Start, 0) when Card > (Start + 10), Card < (Start + 15) ->
            10;
        (Start, 0) when Card =:= Start + 1 ->
            {1, 11};
        (_Start, Acc) ->
            Acc
    end, 0, [16#1F0A0, 16#1F0B0, 16#1F0C0, 16#1F0D0]).
