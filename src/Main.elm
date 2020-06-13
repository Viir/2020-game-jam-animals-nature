module Main exposing (Event(..), State, main)

import Browser
import Html
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import Time


productVersionId : String
productVersionId =
    "2020-06-13"


main : Program () State Event
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
        [ Html.node "style" [] [ Html.text globalStyleInDedicatedElement ]
        , Svg.svg
            [ SA.viewBox "0 0 1000 700"
            , HA.style "height" "99vh"
            , HA.style "width" "100vw"
            ]
            [ [ viewPlayer ] |> translateSvg state.playerLocation
            ]
        , versionInfoHtml
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


rootBackgroundColor : String
rootBackgroundColor =
    "#111"


rootStyle : List ( String, String )
rootStyle =
    [ ( "background-color", rootBackgroundColor )
    , ( "color", "whitesmoke" )
    , ( "font-size", "18px" )
    , ( "font-family", "'Segoe UI', Tahoma, Geneva, Verdana, sans-serif" )
    , ( "margin", "0px" )
    ]


globalStyleInDedicatedElement : String
globalStyleInDedicatedElement =
    let
        rootStyleText =
            rootStyle
                |> List.map (\( property, value ) -> property ++ ": " ++ value ++ ";")
                |> String.join "\n"
    in
    """
body {
"""
        ++ rootStyleText
        ++ """
}"""


versionInfoHtml : Html.Html a
versionInfoHtml =
    [ ("version " ++ productVersionId) |> Html.text ]
        |> Html.div [ HA.style "color" "rgba(233,233,233,0.2)", HA.style "font-size" "70%", HA.style "margin" "4px" ]
        |> List.singleton
        |> Html.div [ HA.style "position" "fixed", HA.style "bottom" "0px", HA.style "pointer-events" "none" ]
