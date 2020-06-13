module Console exposing
    ( MouseButtonType(..)
    , MouseEvent
    , MouseEventType(..)
    , attributesForMouseAndTouchEventsWithLocationMapped
    )

import Base exposing (tuple2MapAll)
import Dict
import Html
import Html.Events
import Html.Events.Extra.Touch as Touch
import Json.Decode
import Point2d exposing (Point2d)
import Vector2d exposing (Vector2d)


type alias MouseEventRaw =
    { eventType : String
    , clientX : Float
    , clientY : Float
    , button : Int
    , wheelDeltaX : Maybe Float
    , wheelDeltaY : Maybe Float
    }


type alias MouseEvent =
    { eventType : MouseEventType
    , location : Point2d
    , button : MouseButtonType
    , wheelDelta : Vector2d
    }


type MouseButtonType
    = Left
    | Right
    | Middle


type MouseEventType
    = MouseDown
    | MouseUp
    | MouseMove
    | MouseWheel
    | Contextmenu
    | Other


attributesForMouseAndTouchEventsWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute MouseEvent)
attributesForMouseAndTouchEventsWithLocationMapped locationMap =
    setMouseEventAttributeWithLocationMapped locationMap
        ++ touchEventsMappedToMouseEventsWithLocationMapped locationMap


setMouseEventAttributeWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute MouseEvent)
setMouseEventAttributeWithLocationMapped locationMap =
    mouseEventTypeParseDict
        |> Dict.keys
        |> List.map
            (\event ->
                Html.Events.on event
                    (mouseEventDecoder
                        |> Json.Decode.andThen
                            (mouseEventParsedWithLocationFromClient
                                >> Maybe.map (mouseEventWithLocationMapped locationMap >> Json.Decode.succeed)
                                >> Maybe.withDefault (Json.Decode.fail "Event does not match")
                            )
                    )
            )


touchEventsMappedToMouseEvents : List ( (Touch.Event -> msg) -> Html.Attribute msg, MouseEventType )
touchEventsMappedToMouseEvents =
    [ ( Touch.onStart, MouseDown )
    , ( Touch.onMove, MouseMove )
    , ( Touch.onEnd, MouseUp )
    ]


touchEventsMappedToMouseEventsWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute MouseEvent)
touchEventsMappedToMouseEventsWithLocationMapped locationMap =
    touchEventsMappedToMouseEvents
        |> List.map
            (\( attributeConstructor, mouseEventType ) ->
                let
                    eventFromCoordinates touchEvent =
                        let
                            clientPos =
                                touchEvent.changedTouches
                                    |> List.head
                                    |> Maybe.map .clientPos
                                    |> Maybe.withDefault ( 0, 0 )
                        in
                        { button = Left
                        , eventType = mouseEventType
                        , wheelDelta = Vector2d.zero
                        , location = clientPos |> Point2d.fromCoordinates |> locationMap
                        }
                in
                attributeConstructor eventFromCoordinates
            )


mouseEventTypeParseDict : Dict.Dict String MouseEventType
mouseEventTypeParseDict =
    [ ( "mousedown", MouseDown ), ( "mouseup", MouseUp ), ( "mousemove", MouseMove ), ( "contextmenu", Contextmenu ), ( "mousewheel", MouseWheel ) ] |> Dict.fromList


mouseEventButtonParseDict : Dict.Dict Int MouseButtonType
mouseEventButtonParseDict =
    [ ( 0, Left ), ( 2, Right ), ( 1, Middle ) ] |> Dict.fromList


mouseEventParsedWithLocationFromClient : MouseEventRaw -> Maybe MouseEvent
mouseEventParsedWithLocationFromClient mouseEventRaw =
    case ( mouseEventTypeParseDict |> Dict.get mouseEventRaw.eventType, mouseEventButtonParseDict |> Dict.get mouseEventRaw.button ) of
        ( Just eventType, Just buttonType ) ->
            Just
                { eventType = eventType
                , location = ( mouseEventRaw.clientX, mouseEventRaw.clientY ) |> Point2d.fromCoordinates
                , button = buttonType
                , wheelDelta = ( mouseEventRaw.wheelDeltaX, mouseEventRaw.wheelDeltaY ) |> tuple2MapAll (Maybe.withDefault 0) |> Vector2d.fromComponents
                }

        _ ->
            Nothing


mouseEventWithLocationMapped : (Point2d -> Point2d) -> MouseEvent -> MouseEvent
mouseEventWithLocationMapped locationMap event =
    { event | location = event.location |> locationMap }


mouseEventDecoder : Json.Decode.Decoder MouseEventRaw
mouseEventDecoder =
    Json.Decode.map6 MouseEventRaw
        (Json.Decode.field "type" Json.Decode.string)
        (Json.Decode.field "clientX" Json.Decode.float)
        (Json.Decode.field "clientY" Json.Decode.float)
        (Json.Decode.field "button" Json.Decode.int)
        (Json.Decode.field "wheelDeltaX" Json.Decode.float |> Json.Decode.maybe)
        (Json.Decode.field "wheelDeltaY" Json.Decode.float |> Json.Decode.maybe)
