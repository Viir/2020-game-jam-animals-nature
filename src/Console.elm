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
import Json.Decode as Decode
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


attributesForMouseAndTouchEventsWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute (Maybe MouseEvent))
attributesForMouseAndTouchEventsWithLocationMapped locationMap =
    setMouseEventAttributeWithLocationMapped locationMap
        ++ touchEventsMappedToMouseEventsWithLocationMapped locationMap


setMouseEventAttributeWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute (Maybe MouseEvent))
setMouseEventAttributeWithLocationMapped locationMap =
    mouseEventTypeParseDict
        |> Dict.keys
        |> List.map
            (\event ->
                Html.Events.on event
                    (mouseEventDecoder
                        |> Decode.map (mouseEventParsedWithLocationFromClient >> Maybe.map (mouseEventWithLocationMapped locationMap))
                    )
            )


touchEventsMappedToMouseEvents : List ( (Touch.Event -> msg) -> Html.Attribute msg, MouseEventType )
touchEventsMappedToMouseEvents =
    [ ( Touch.onStart, MouseDown )
    , ( Touch.onMove, MouseMove )
    , ( Touch.onEnd, MouseUp )
    ]


touchEventsMappedToMouseEventsWithLocationMapped : (Point2d -> Point2d) -> List (Html.Attribute (Maybe MouseEvent))
touchEventsMappedToMouseEventsWithLocationMapped locationMap =
    touchEventsMappedToMouseEvents
        |> List.map
            (\( attributeConstructor, mouseEventType ) ->
                let
                    eventFromCoordinates touchEvent =
                        touchEvent.changedTouches
                            |> List.head
                            |> Maybe.map
                                (\singleTouch ->
                                    { button = Left
                                    , eventType = mouseEventType
                                    , wheelDelta = Vector2d.zero
                                    , location = singleTouch.clientPos |> Point2d.fromCoordinates |> locationMap
                                    }
                                )
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


mouseEventDecoder : Decode.Decoder MouseEventRaw
mouseEventDecoder =
    Decode.map6 MouseEventRaw
        (Decode.at [ "type" ] Decode.string)
        (Decode.at [ "clientX" ] Decode.float)
        (Decode.at [ "clientY" ] Decode.float)
        (Decode.at [ "button" ] Decode.int)
        (Decode.at [ "wheelDeltaX" ] Decode.float |> Decode.maybe)
        (Decode.at [ "wheelDeltaY" ] Decode.float |> Decode.maybe)
