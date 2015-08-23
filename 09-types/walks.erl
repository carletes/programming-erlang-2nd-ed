-module(walks).
-export([plan_route/2]).

-spec plan_route(From::point(), To::point()) -> route().
-type point() :: {integer(), integer()}.
-type route() :: [{go, direction(), integer()}].
-type direction() :: north | south | east | west.
