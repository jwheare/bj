%%%-------------------------------------------------------------------
%% @doc blackjack game
%% @end
%%%-------------------------------------------------------------------

-module(bj_game).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

%% public api
-export([
    hit/1
    , stand/1
    , resume/1
]).

%% supervisor api
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1, terminate/2, code_change/3,
    handle_call/3, handle_cast/2, handle_info/2
]).

%%====================================================================
%% State record
%%====================================================================

-record(state, {
    deck,
    dealer,
    player
}).

%%====================================================================
%% API functions
%%====================================================================

hit(Pid) ->
    gen_server:call(Pid, hit).

stand(Pid) ->
    gen_server:call(Pid, stand).

resume(Pid) ->
    gen_server:call(Pid, resume).

%%====================================================================
%% Supervisor api
%%====================================================================

start_link() ->
    [A, B, C, D | Deck] = new_deck(),
    State = #state{
        deck=Deck,
        dealer=[D, B],
        player=[C, A]
    },
    do_render(State),
    gen_server:start_link(?MODULE, [State], []).

%%====================================================================
%% private api
%%====================================================================

new_deck() ->
    random:seed(os:timestamp()),
    [X || {_,X} <- lists:sort([{random:uniform(), N} || N <- bj_cards:deck()])].

do_render(State) ->
    do_render(State, false).
do_render(#state{dealer=Dealer, player=Player}, EndGame) ->
    DealerHand = case EndGame of
        true ->
            lists:reverse(Dealer);
        false ->
             [$🂠 | tl(lists:reverse(Dealer))]
    end,
    io:format("Dealer: ~ts~n", [DealerHand]),
    io:format("Player: ~ts~n", [lists:reverse(Player)]),
    case EndGame of
        true ->
            io:format("Your hand: ~B~n", [best_hand_value(Player)]),
            io:format("Dealer's hand: ~B~n", [best_hand_value(Dealer)]);
        false ->
            io:format("Your best hand value: ~B~n", [best_hand_value(Player)])
    end,
    ok.

check_winner_test_() ->
    Tests = [
        {"dealer_win", {false, false, 18, 17}, dealer},
        {"player_win", {false, false, 18, 19}, player},
        {"draw", {false, false, 18, 18}, draw},
        {"player_win_bust", {false, false, 25, 15}, player},
        {"dealer_win_bust", {false, false, 17, 22}, dealer},
        {"dealer_win_both_bust", {false, false, 22, 22}, dealer},
        {"player_blackjack", {false, true, 21, 21}, {player, blackjack}},
        {"dealer_blackjack", {true, false, 21, 21}, {dealer, blackjack}},
        {"draw_blackjack", {true, true, 21, 21}, {draw, blackjack}}
    ],
    lists:foldl(fun ({Name, {DB, PB, D, P}, Expected}, Acc) ->
        Actual = check_winner(DB, PB, D, P),
        [{Name, ?_assertMatch(Expected, Actual)} | Acc]
    end, [], Tests).

check_winner(true, true, _, _) ->
    {draw, blackjack};
check_winner(true, _, _, _) ->
    {dealer, blackjack};
check_winner(_, true, _, _) ->
    {player, blackjack};
check_winner(_, _, _D, P) when P > 21 ->
    dealer;
check_winner(_, _, D, _P) when D > 21 ->
    player;
check_winner(_, _, D, P) when D > P ->
    dealer;
check_winner(_, _, D, P) when D =:= P ->
    draw;
check_winner(_, _, _, _) ->
    player.

get_winner(State) ->
    check_winner(
        bj_cards:blackjack(State#state.dealer),
        bj_cards:blackjack(State#state.player),
        best_hand_value(State#state.dealer),
        best_hand_value(State#state.player)
    ).

min_hand_value(Hand) ->
    lists:min(bj_cards:hand_values(Hand)).

best_hand_value(Hand) ->
    case bj_cards:hand_values(Hand) of
        [Min, Max] when Max > 21 ->
            Min;
        [_, Max] ->
            Max;
        [Val] ->
            Val
    end.

is_bust(Hand) ->
    min_hand_value(Hand) > 21.

is_dealer_over(#state{dealer=Hand}) ->
    best_hand_value(Hand) >= 17.

deal_out(State = #state{}) ->
    case is_dealer_over(State) of
        true ->
            do_render(State, true),
            case is_bust(State#state.dealer) of
                true ->
                    {bust, State};
                false ->
                    {safe, State}
            end;
        false ->
            {NewHand, NewDeck} = deal(State#state.deck, State#state.dealer),
            NewState = State#state{dealer=NewHand, deck=NewDeck},
            deal_out(NewState)
    end.

deal([Card | Deck], Hand) ->
    {[Card | Hand], Deck}.
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([State]) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change({down, _UpVsn}, _UpState, _Extra) ->
    {ok, #state{}};

code_change(_DownVsn, _DownState, _Extra) ->
    {ok, #state{}}.

%%====================================================================
%% call
%%====================================================================

handle_call(hit, _From, State = #state{}) ->
    {NewHand, NewDeck} = deal(State#state.deck, State#state.player),
    NewState = State#state{player=NewHand, deck=NewDeck},
    case is_bust(NewHand) of
        true ->
            {_Result, FinalState} = deal_out(NewState),
            io:format("bust 😵~n"),
            {stop, normal, ok, FinalState};
        false ->
            do_render(NewState),
            {reply, ok, NewState}
    end;

handle_call(stand, _From, State = #state{}) ->
    case deal_out(State) of
        {bust, FinalState} ->
            case bj_cards:blackjack(State#state.player) of
                true ->
                    io:format("you win! 👍~n");
                false ->
                    io:format("blackjack! you win 🎉~n")
            end;
        {safe, FinalState} ->
            case get_winner(FinalState) of
                {draw, blackjack} ->
                    io:format("blackjack draw 🙀~n", []);
                draw ->
                    io:format("draw 😼~n", []);
                {player, blackjack} ->
                    io:format("blackjack! you win 🎉~n");
                player ->
                    io:format("you win 👍~n");
                {Winner, blackjack} ->
                    io:format("blackjack! ~s wins 😩~n", [Winner]);
                Winner ->
                    io:format("~s wins 😭~n", [Winner])
            end
    end,
    {stop, normal, ok, FinalState};

handle_call(resume, _From, State) ->
    do_render(State),
    {reply, ok, State};

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%%====================================================================
%% cast
%%====================================================================

handle_cast(_Cast, State = #state{}) ->
    State.

%%====================================================================
%% info
%%====================================================================

handle_info(_Info, State) ->
    {noreply, State}.
