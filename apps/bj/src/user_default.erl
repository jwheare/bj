-module(user_default).
-compile(export_all).

start() ->
    {ok, G} = bj_game_sup:start_game(),
    put(game, G),
    ok.

hit() ->
    G = get(game),
    bj_game:hit(G),
    ok.

stand() ->
    G = get(game),
    bj_game:stand(G),
    ok.

resume(N) ->
    G = element(2, lists:nth(N, supervisor:which_children(bj_game_sup))),
    put(game, G),
    bj_game:resume(G),
    ok.

games() ->
    length(supervisor:which_children(bj_game_sup)).
