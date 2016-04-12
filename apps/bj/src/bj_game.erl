%%%-------------------------------------------------------------------
%% @doc blackjack game
%% @end
%%%-------------------------------------------------------------------

-module(bj_game).

-behaviour(gen_server).

%% public api
-export([
    hit/1
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
    stage, % player, dealer or done
    deck,
    dealer,
    player
}).

%%====================================================================
%% API functions
%%====================================================================

hit(Pid) ->
    gen_server:call(Pid, hit).

%%====================================================================
%% Supervisor api
%%====================================================================

start_link() ->
    State = #state{
        stage=dealer,
        deck=new_deck(),
        dealer=[],
        player=[]
    },
    gen_server:start_link(?MODULE, [State], []).

%%====================================================================
%% private api
%%====================================================================

new_deck() ->
    lists:seq(52).

render(#state{dealer=Dealer, player=Player}) ->
    io:format("Dealer: ~p~n", [Dealer]),
    io:format("Player: ~p~n", [Player]),
    io:format("enter one of: quit stand hit~n").

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

handle_call(_Call, _From, State) ->
    {reply, ok, State}.

%%====================================================================
%% cast
%%====================================================================

handle_cast(hit, State = #state{deck=[Card | NewDeck]}) ->
    NewState = State#state{player=[Card | State#state.player], deck=NewDeck},
    % TODO check win state, dealer plays, update stage
    render(NewState),
    {noreply, NewState}.

%%====================================================================
%% info
%%====================================================================

handle_info(_Info, State) ->
    {noreply, State}.
