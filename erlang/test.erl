-module(test).
-export([base/0]).

base() ->
    A = 1,
    (fun() -> A = 1 end)().
