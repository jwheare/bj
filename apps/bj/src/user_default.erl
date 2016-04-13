-module(user_default).
-compile(export_all).

start() ->
    {ok, G} = bj_game_sup:start_game(),
    put(game, G),
    ok.

hit() ->
    G = get(game),
    bj_game:hit(G).

stand() ->
    G = get(game),
    bj_game:stand(G).
