module Main exposing (main)

import Element exposing (el, text)
import Element.Attributes exposing (center, verticalCenter)
import Html exposing (Html)
import Style exposing (style)


-- MODEL


type alias Model =
    { message : String
    }



-- MSG


type Msg
    = NoOp



-- INIT


init : ( Model, Cmd Msg )
init =
    { message = "Hello world! 👋" } ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- VIEW


type alias ViewResult variationType messageType =
    Element.Element AppStyle variationType messageType


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        layout model


layout : Model -> ViewResult v Msg
layout model =
    el NoStyle [ center, verticalCenter ] (text model.message)



-- STYLE


type AppStyle
    = NoStyle


stylesheet : Style.StyleSheet AppStyle v
stylesheet =
    Style.styleSheet
        [ style NoStyle []
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
