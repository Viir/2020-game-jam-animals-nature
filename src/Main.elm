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
    { time : Time.Posix }


type Event
    = ArrivedAtTime Time.Posix


init : () -> ( State, Cmd Event )
init _ =
    ( State (Time.millisToPosix 0), Cmd.none )


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
            []
        ]
    , title = "Game????"
    }


css : String
css =
    """
body
{
    background: #111;
    margin: 0;
}
"""
