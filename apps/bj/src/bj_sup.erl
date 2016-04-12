%%%-------------------------------------------------------------------
%% @doc blackjack top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bj_sup).

-behaviour(supervisor).

%% Public API
-export([children/0]).

%% Application API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Public api
%%====================================================================

children() ->
    supervisor:which_children(?SERVER).

%%====================================================================
%% Application api
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpecs = [
        {
            Child, % Supervision id
            {Child, start_link, []}, % Start: Module, function, args
            permanent, % restart mode (permanent | transient | temporary)
            5000, % shutdown timeout (timeout | brutal_kill)
            Type, % child type (worker | supervisor)
            [Child] % modules [for code reloading during a release upgrade]
        } || {Child, Type} <- [
            {bj_game_sup, supervisor}
        ]
    ],
    {ok, {{
        one_for_one, % restart strategy
        % one_for_one [processes are isolated]
        % one_for_all [if one process crashes, they're all restarted]
        % rest_for_one [if one process crashes, every *subsequent* process is restarted]
        % simple_one_for_one [like one_for_one, but allows dynamically started pools of processes of one type]
        1, 5 % restart frequency
        % [if more than 1 restarts occcur within 5 seconds, the supervisor gives up]
    }, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
