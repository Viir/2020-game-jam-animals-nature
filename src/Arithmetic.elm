module Arithmetic exposing (squareRoot)

import Bitwise exposing (shiftRightBy)


{-| Returns the greatest integer less than or equal to the square root of n.
This function only supports inputs up to (2 ^ 32 - 2), to reflect limitations with popular methods of executing Elm code, as pointed out by dmy at <https://discourse.elm-lang.org/t/how-to-compute-square-roots-without-dependency-on-float-arithmetics/4324/7?u=viir>

Credits to:

  - dmy for his solution at <https://discourse.elm-lang.org/t/how-to-compute-square-roots-without-dependency-on-float-arithmetics/4324/3?u=viir>
  - W Gordon Goodsman for his solution at <https://discourse.elm-lang.org/t/how-to-compute-square-roots-without-dependency-on-float-arithmetics/4324/6?u=viir>

-}
squareRoot : Int -> Maybe Int
squareRoot n =
    if n < 0 || 2 ^ 32 - 2 < n then
        Nothing

    else
        Just (squareRootHelper (squareRootRdcrHelper n 0x40000000) n 0)


squareRootRdcrHelper : Int -> Int -> Int
squareRootRdcrHelper n v =
    if v > n then
        squareRootRdcrHelper n (shiftRightBy 2 v)

    else
        v


squareRootHelper : Int -> Int -> Int -> Int
squareRootHelper plc rmndr root =
    if plc <= 0 then
        root

    else if rmndr - root - plc < 0 then
        squareRootHelper (shiftRightBy 2 plc) rmndr (shiftRightBy 1 root)

    else
        squareRootHelper (shiftRightBy 2 plc) (rmndr - root - plc) (shiftRightBy 1 root + plc)
