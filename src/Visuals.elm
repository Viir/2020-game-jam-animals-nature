module Visuals exposing (..)

import Base exposing (Float2)
import Html
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as SA


type alias HtmlStyle a =
    List (Html.Attribute a)


svgGroupWithTranslationAndElements : Float2 -> List (Svg a) -> Svg a
svgGroupWithTranslationAndElements translation elements =
    svgGroupWithListTransformStringAndElements [ svgTransformTranslate translation ] elements


svgGroupWithListTransformStringAndElements : List String -> List (Svg a) -> Svg a
svgGroupWithListTransformStringAndElements listTransform elements =
    Svg.g [ SA.transform (listTransform |> String.join " ") ] elements


svgGroupTransformedScaleUniform : Float -> List (Svg a) -> Svg a
svgGroupTransformedScaleUniform scale groupElements =
    svgGroupWithListTransformStringAndElements [ svgTransformScaleUniform scale ] groupElements


svgTransformTranslate : Float2 -> String
svgTransformTranslate ( offsetX, offsetY ) =
    "translate(" ++ String.fromFloat offsetX ++ "," ++ String.fromFloat offsetY ++ ")"


cssTransformTranslate : Float2 -> String
cssTransformTranslate ( offsetX, offsetY ) =
    "translate(" ++ String.fromFloat offsetX ++ "px," ++ String.fromFloat offsetY ++ "px)"


svgTransformScaleUniform : Float -> String
svgTransformScaleUniform scale =
    "scale(" ++ (scale |> String.fromFloat) ++ ")"


svgTransformRotate : Float -> String
svgTransformRotate rotation =
    "rotate(" ++ (rotation * 360 |> String.fromFloat) ++ ")"


svgRectAttributesSizeAll : List (Html.Attribute a)
svgRectAttributesSizeAll =
    [ SA.width "9999", SA.height "9999" ]


htmlStyleFromList : List ( String, String ) -> HtmlStyle a
htmlStyleFromList =
    List.map (\( attribute, value ) -> Html.Attributes.style attribute value)
