-module(init).
-export([test/0]).

test() -> recurse(10).

recurse(X) when X > 0 -> recurse(X-1);
recurse(X) -> X.
