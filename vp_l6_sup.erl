-module(vp_l6_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, vp_l6_sup}, vp_l6_sup, []).

init(_) ->
    {ok, {{one_for_one, 5, 10},
          [{vp_l6, {vp_l6, start_link, []}, permanent, 5000, worker, [vp_l6]}]}}.
