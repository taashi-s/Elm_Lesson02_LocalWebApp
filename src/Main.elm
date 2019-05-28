module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { left_term : Maybe Float
    , right_term : Maybe Float
    , answer : Maybe Float
    }


init =
    Model Nothing Nothing Nothing



-- Msg


type Msg
    = SetLeftTerm (Maybe Float)
    | SetRightTerm (Maybe Float)
    | Addition
    | Subtraction
    | Multiplication
    | Division



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetLeftTerm f ->
            { model | left_term = f }

        SetRightTerm f ->
            { model | right_term = f }

        Addition ->
            { model | answer = Maybe.map2 (+) model.left_term model.right_term }

        Subtraction ->
            { model | answer = Maybe.map2 (-) model.left_term model.right_term }

        Multiplication ->
            { model | answer = Maybe.map2 (*) model.left_term model.right_term }

        Division ->
            { model
                | answer =
                    Maybe.andThen
                        (\f ->
                            if isInfinite f then
                                Nothing

                            else
                                Just f
                        )
                    <|
                        Maybe.map2 (/) model.left_term model.right_term
            }



-- View
{-
   view : Model -> Html Msg
   view model =
       div []
           [ div []
               [ text "第１項"
               , text "\u{3000}\u{3000}\u{3000}\u{3000}"
               , text "第２項"
               ]
           , div []
               [ input
                   [ style "width" "55px"
                   , onInput (\strf -> SetLeftTerm <| String.toFloat strf)
                   ]
                   []
               , text "\u{3000}？\u{3000}"
               , input
                   [ style "width" "55px"
                   , onInput (\strf -> SetRightTerm <| String.toFloat strf)
                   ]
                   []
               ]
           , br [] []
           , div []
               [ text "？ : "
               , button [ onClick Addition ] [ text " + " ]
               , text "\u{3000}"
               , button [ onClick Subtraction ] [ text " - " ]
               , text "\u{3000}"
               , button [ onClick Multiplication ] [ text " - " ]
               , text "\u{3000}"
               , button [ onClick Division ] [ text " - " ]
               ]
           , br [] []
           , div []
               (case model.answer of
                   Just ans ->
                       [ text <| "計算結果 : " ++ String.fromFloat ans ]

                   Nothing ->
                       []
               )
           ]
-}


view : Model -> Html Msg
view model =
    div []
        [ view_part_1 model
        , br [] []
        , view_part_2 model
        , br [] []
        , view_part_3 model
        ]


view_part_1 model =
    div []
        [ div [ style "float" "left" ]
            [ text "第１項"
            , div [] []
            , input
                [ style "width" "55px"
                , onInput (\strf -> SetLeftTerm <| String.toFloat strf)
                ]
                []
            ]
        , div [ style "float" "left" ]
            [ br [] []
            , div
                [ style "margin-left" "10px"
                , style "margin-right" "10px"
                ]
                [ text "？" ]
            ]
        , div []
            [ text "第２項"
            , div [] []
            , input
                [ style "width" "55px"
                , onInput (SetRightTerm << String.toFloat)
                ]
                []
            ]
        ]


view_part_2 model =
    div []
        [ text "？ : "
        , button [ onClick Addition, style "margin-right" "10px" ] [ text " + " ]
        , button [ onClick Subtraction ] [ text " - " ]
        ]


view_part_3 model =
    div []
        (case model.answer of
            Just ans ->
                [ text <| "計算結果 : " ++ String.fromFloat ans ]

            Nothing ->
                []
        )
