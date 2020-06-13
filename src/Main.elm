module Main exposing (Event(..), State, main)

import Arithmetic
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
    , playerLocation : GameWorldVector
    , playerVelocity : GameWorldVector
    , playerInputDestination : Maybe GameWorldVector
    }


type Event
    = ArrivedAtTime Time.Posix
    | ResizedWindow { width : Int, height : Int }
    | UserInputPointInGameWorldViewport GameWorldVector
    | UserInputMoveMouse GameWorldVector


type alias GameWorldVector =
    { x : Int, y : Int }


type alias DisplayConfiguration =
    { appViewScale : Float
    , offset : Float2
    }


type alias HtmlStyle a =
    List (Html.Attribute a)


screenToWorldScale : Int
screenToWorldScale =
    100


init : () -> ( State, Cmd Event )
init _ =
    ( { time = Time.millisToPosix 0
      , windowSize = { width = 400, height = 300 }
      , playerLocation = { x = 130, y = 100 } |> scaleVector screenToWorldScale
      , playerVelocity = { x = 0, y = 0 }
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
            let
                state =
                    { stateBefore | time = time }
                        |> updateGameWorldForPassingTime
                            (((time |> Time.posixToMillis) - (stateBefore.time |> Time.posixToMillis)) |> min 1000)
            in
            ( state, Cmd.none )

        ResizedWindow windowSize ->
            ( { stateBefore | windowSize = windowSize }, Cmd.none )

        UserInputPointInGameWorldViewport location ->
            ( { stateBefore | playerInputDestination = Just location }, Cmd.none )

        UserInputMoveMouse _ ->
            ( stateBefore, Cmd.none )


updateGameWorldForPassingTime : Int -> State -> State
updateGameWorldForPassingTime milliseconds stateBefore =
    let
        distanceFromPlayerToDestination =
            case stateBefore.playerInputDestination of
                Nothing ->
                    { x = 0, y = 0 }

                Just playerInputDestination ->
                    subtractVector playerInputDestination stateBefore.playerLocation |> divideVector screenToWorldScale

        distanceLength =
            distanceFromPlayerToDestination |> lengthFromVector

        distanceScaled =
            distanceFromPlayerToDestination |> scaleVectorToLength (distanceLength |> min 100)

        acceleration =
            distanceScaled |> scaleVector 10

        dragBase =
            1000

        dampFactorMilli =
            List.range 0 milliseconds
                |> List.foldl (\_ intermediate -> (intermediate * 997) // 1000) dragBase

        playerVelocity =
            acceleration
                |> scaleVector (milliseconds * screenToWorldScale // 500)
                |> addVector stateBefore.playerVelocity
                |> scaleVector dampFactorMilli
                |> divideVector dragBase

        playerLocation =
            playerVelocity
                |> scaleVector milliseconds
                |> divideVector 1000
                |> addVector stateBefore.playerLocation
    in
    { stateBefore
        | playerVelocity = playerVelocity
        , playerLocation = playerLocation
    }


subscriptions : State -> Sub Event
subscriptions model =
    [ Browser.Events.onAnimationFrame ArrivedAtTime
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
                        |> scaleVector screenToWorldScale
            in
            case mouseEvent.eventType of
                Console.MouseDown ->
                    UserInputPointInGameWorldViewport location

                _ ->
                    UserInputMoveMouse location

        inputElement : Html.Html Event
        inputElement =
            Svg.rect inputElementAttributes []
                |> Html.map eventFromMouseEvent
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
    , title = "Dragonfly Game"
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


translateSvg : GameWorldVector -> List (Svg.Svg e) -> Svg.Svg e
translateSvg { x, y } =
    let
        ( svgX, svgY ) =
            ( x, y ) |> tuple2MapAll (toFloat >> (*) (1 / (screenToWorldScale |> toFloat)) >> String.fromFloat)
    in
    Svg.g [ HA.style "transform" ("translate(" ++ svgX ++ "px, " ++ svgY ++ "px)") ]


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
        [ SA.r "20"
        , HA.style "stroke" "orange"
        , HA.style "stroke-width" "3px"
        , HA.style "opacity" "0.5"
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


scaleVector : Int -> GameWorldVector -> GameWorldVector
scaleVector scale vector =
    { x = vector.x * scale, y = vector.y * scale }


divideVector : Int -> GameWorldVector -> GameWorldVector
divideVector divisor vector =
    { x = vector.x // divisor, y = vector.y // divisor }


addVector : GameWorldVector -> GameWorldVector -> GameWorldVector
addVector addend0 addend1 =
    { x = addend0.x + addend1.x, y = addend0.y + addend1.y }


subtractVector : GameWorldVector -> GameWorldVector -> GameWorldVector
subtractVector minuend subtrahend =
    { x = minuend.x - subtrahend.x, y = minuend.y - subtrahend.y }


lengthFromVector : GameWorldVector -> Int
lengthFromVector vector =
    (vector.x * vector.x + vector.y * vector.y) |> Arithmetic.squareRoot |> Maybe.withDefault 0


scaleVectorToLength : Int -> GameWorldVector -> GameWorldVector
scaleVectorToLength length vector =
    let
        lengthBefore =
            lengthFromVector vector
    in
    vector
        |> scaleVector length
        |> divideVector lengthBefore
