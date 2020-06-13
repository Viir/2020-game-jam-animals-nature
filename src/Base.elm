module Base exposing
    ( Float2
    , tuple2MapAll
    , tuple2Swap
    )

import Tuple


type alias Float2 =
    ( Float, Float )


tuple2Swap : ( a, b ) -> ( b, a )
tuple2Swap ( a, b ) =
    ( b, a )


tuple2MapAll : (a -> b) -> ( a, a ) -> ( b, b )
tuple2MapAll map =
    Tuple.mapFirst map >> Tuple.mapSecond map
