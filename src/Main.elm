module Main exposing (Event(..), State, main)

import Base exposing (Float2, tuple2MapAll)
import Browser
import Browser.Dom
import Browser.Events
import Console
import Html
import Html.Attributes as HA
import Point2d
import Svg
import Svg.Attributes as SA
import Task
import Time
import Visuals


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
    , windowSize : { width : Int, height : Int }
    , playerLocation : GameWorldLocation
    , playerInputDestination : Maybe GameWorldLocation
    }


type Event
    = ArrivedAtTime Time.Posix
    | ResizedWindow { width : Int, height : Int }
    | UserInputPointInGameWorldViewport GameWorldLocation
    | UserInputMoveMouse GameWorldLocation
      -- TODO: Use an adapted decoder instead of FailedToDecodeEvent
    | FailedToDecodeEvent


type alias GameWorldLocation =
    { x : Int, y : Int }


type alias DisplayConfiguration =
    { appViewScale : Float
    , offset : Float2
    }


type alias HtmlStyle a =
    List (Html.Attribute a)


init : () -> ( State, Cmd Event )
init _ =
    ( { time = Time.millisToPosix 0
      , windowSize = { width = 400, height = 300 }
      , playerLocation = { x = 130, y = 100 }
      , playerInputDestination = Nothing
      }
    , Task.perform
        (\viewport ->
            ResizedWindow { width = viewport.viewport.width |> floor, height = viewport.viewport.height |> floor }
        )
        Browser.Dom.getViewport
    )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        ArrivedAtTime time ->
            ( { stateBefore | time = time }, Cmd.none )

        ResizedWindow windowSize ->
            ( { stateBefore | windowSize = windowSize }, Cmd.none )

        UserInputPointInGameWorldViewport location ->
            ( { stateBefore | playerInputDestination = Just location }, Cmd.none )

        UserInputMoveMouse _ ->
            ( stateBefore, Cmd.none )

        FailedToDecodeEvent ->
            ( stateBefore, Cmd.none )


subscriptions : State -> Sub Event
subscriptions model =
    [ Time.every 1000 ArrivedAtTime
    , Browser.Events.onResize (\width height -> ResizedWindow { width = width, height = height })
    ]
        |> Sub.batch


gameDisplayWidth : Float
gameDisplayWidth =
    1000


gameDisplayHeight : Float
gameDisplayHeight =
    700


view : State -> Browser.Document Event
view state =
    let
        availableSize =
            ( state.windowSize.width |> toFloat, state.windowSize.height |> toFloat )

        ( displayConfig, svgContainer ) =
            viewScaleAndContainerFromAvailableSize availableSize

        scenarioMouseEventOffsetTransform =
            identity

        inputElementAttributes =
            [ Visuals.svgRectAttributesSizeAll
            , [ SA.fill "transparent" ]
            , Console.attributesForMouseAndTouchEventsWithLocationMapped scenarioMouseEventOffsetTransform
            ]
                |> List.concat

        eventFromMouseEvent : Console.MouseEvent -> Event
        eventFromMouseEvent mouseEvent =
            let
                location =
                    { x = mouseEvent.location |> Point2d.xCoordinate |> floor
                    , y = mouseEvent.location |> Point2d.yCoordinate |> floor
                    }
            in
            case mouseEvent.eventType of
                Console.MouseDown ->
                    UserInputPointInGameWorldViewport location

                _ ->
                    UserInputMoveMouse location

        inputElement : Html.Html Event
        inputElement =
            Svg.rect inputElementAttributes []
                |> Html.map (Maybe.map eventFromMouseEvent >> Maybe.withDefault FailedToDecodeEvent)
                -- Fix for Firefox: It appeared that firefox applied the scaling of parent elements to compute the mouse event offset (in contrast to chrome and edge). To accomplish symmetry between firefox and chrome, we apply a transform here to revert scaling transforms between the svg root and here.
                |> List.singleton
                |> Visuals.svgGroupTransformedScaleUniform (1 / displayConfig.appViewScale)

        destinationIndication =
            case state.playerInputDestination of
                Nothing ->
                    Html.text ""

                Just playerInputDestination ->
                    [ destinationIndicationSvg ] |> translateSvg playerInputDestination
    in
    { body =
        [ Html.node "style" [] [ Html.text globalStyleInDedicatedElement ]
        , svgContainer
            [ destinationIndication
            , [ viewPlayer ] |> translateSvg state.playerLocation
            , inputElement
            ]
        , versionInfoHtml
        ]
    , title = "Game????"
    }


viewScaleAndContainerFromAvailableSize : Float2 -> ( DisplayConfiguration, List (Html.Html msg) -> Html.Html msg )
viewScaleAndContainerFromAvailableSize ( displayWidth, displayHeight ) =
    let
        appViewScale =
            min (displayWidth / gameDisplayWidth) (displayHeight / gameDisplayHeight)

        ( widthString, heightString ) =
            ( displayWidth, displayHeight ) |> tuple2MapAll String.fromFloat

        container =
            Svg.svg
                ([ SA.width widthString
                 , SA.height heightString
                 , SA.viewBox ("0 0 " ++ widthString ++ " " ++ heightString)
                 ]
                    ++ viewportStyle
                )
                >> List.singleton
                >> Html.div appViewContainerStyle

        offsetX =
            (displayWidth - gameDisplayWidth * appViewScale) / 2
    in
    ( { appViewScale = appViewScale, offset = ( offsetX, 0 ) }, container )


viewportStyle : HtmlStyle a
viewportStyle =
    [ ( "cursor", "default" )
    , ( "user-select", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ie-user-select", "none" )
    ]
        |> Visuals.htmlStyleFromList


appViewContainerStyle : HtmlStyle a
appViewContainerStyle =
    [ ( "padding", "0" )
    , ( "margin", "0" )
    , ( "height", "100vh" )
    , ( "width", "100vw" )
    , ( "overflow", "hidden" )
    ]
        |> Visuals.htmlStyleFromList


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


destinationIndicationSvg : Svg.Svg e
destinationIndicationSvg =
    Svg.circle
        [ SA.r "10"
        , HA.style "fill" "orange"
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
