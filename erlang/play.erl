-module(play).
-compile(export_all).

hello(X) ->
    if
      X > 0 -> a
    end.

intersect() ->
	L1 = [5,3,4,2],
	L2 = [4,6,1,3,5,9,2],
	[X || X <- L1, Y <- L2, X =:= Y].
