-module(geometry).
-export([area/1, perimeter/1]).
-export([test/0]).

area({rectangle, Width, Height}) ->
    Width * Height;
area({square, Side}) ->
    Side * Side;
area({circle, Radius}) ->
    3.141592 * Radius * Radius;
area({triangle, Base, Height}) ->
    (Base * Height) / 2.

perimeter({rectangle, Width, Height}) ->
    2 * (Width + Height);
perimeter({square, Side}) ->
    4 * Side;
perimeter({circle, Radius}) ->
    2 * 3.141592 * Radius;
perimeter({triangle, Base, Height}) ->
    Base + Height + math:sqrt(Base * Base + Height * Height).

test() ->
    Rectangle = {rectangle, 3, 4},
    Square = {square, 12},
    Circle = {circle, 10},
    Triangle = {triangle, 3, 4},
    12 = area(Rectangle),
    144 = area(Square),
    6.0 = area(Triangle),
    314.1592 = area(Circle),
    14 = perimeter(Rectangle),
    48 = perimeter(Square),
    62.83184 = perimeter(Circle),
    12.0 = perimeter(Triangle),
    tests_worked.
