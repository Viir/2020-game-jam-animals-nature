module Main exposing (Event(..), State, main)

import Browser
import Html exposing (..)
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias State =
    { time : Time.Posix
    , playerLocation : GameWorldLocation
    }


type Event
    = ArrivedAtTime Time.Posix


type alias GameWorldLocation =
    { x : Int, y : Int }


init : () -> ( State, Cmd Event )
init _ =
    ( { time = Time.millisToPosix 0
      , playerLocation = { x = 130, y = 100 }
      }
    , Cmd.none
    )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        ArrivedAtTime time ->
            ( { stateBefore | time = time }, Cmd.none )


subscriptions : State -> Sub Event
subscriptions model =
    Time.every 1000 ArrivedAtTime


view : State -> Browser.Document Event
view state =
    { body =
        [ Html.node "style" [] [ Html.text css ]
        , Svg.svg
            [ SA.viewBox "0 0 1000 700"
            , HA.style "height" "99vh"
            , HA.style "width" "100vw"
            ]
            [ [ viewPlayer ] |> translateSvg state.playerLocation ]
        ]
    , title = "Game????"
    }


translateSvg : GameWorldLocation -> List (Svg.Svg e) -> Svg.Svg e
translateSvg { x, y } =
    Svg.g [ HA.style "transform" ("translate(" ++ (x |> String.fromInt) ++ "px, " ++ (y |> String.fromInt) ++ "px)") ]


viewPlayer : Svg.Svg e
viewPlayer =
    Svg.rect
        [ SA.x "-20"
        , SA.y "-5"
        , SA.width "40"
        , SA.height "10"
        , HA.style "fill" "firebrick"
        ]
        []


css : String
css =
    """
body
{
    background: #111;
    margin: 0;
}
"""
