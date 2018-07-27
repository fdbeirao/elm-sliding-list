module Main exposing (main)

import Element exposing (button, column, el, row, text)
import Element.Attributes exposing (center, padding, spacing, verticalCenter)
import Element.Events exposing (onClick)
import Html exposing (Html)
import SlidingList exposing (SlidingList)
import Style exposing (style)


-- MODEL


type alias Model =
    { theList : SlidingList String
    , desiredListSize : Int
    }



-- MSG


type Msg
    = InsertIntoList String



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        initListSize =
            5
    in
    { theList = SlidingList.new initListSize
    , desiredListSize = initListSize
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InsertIntoList item ->
            { model | theList = model.theList |> SlidingList.insert item } ! []



-- VIEW


type alias ViewResult variationType messageType =
    Element.Element AppStyle variationType messageType


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        layout model


layout : Model -> ViewResult v Msg
layout model =
    el
        NoStyle
        [ center, verticalCenter ]
        (listView model)


listView : Model -> ViewResult v Msg
listView model =
    column
        NoStyle
        [ padding 10 ]
        [ listItemsView model.theList
        , listInfoView model.theList
        , addItemsView
        ]


listItemsView : SlidingList String -> ViewResult v Msg
listItemsView theList =
    column NoStyle
        [ padding 10
        , spacing 10
        , center
        ]
        [ text "Elements currently on the list:"
        , row
            NoStyle
            [ padding 10, spacing 10 ]
            [ "[ " ++ (theList |> SlidingList.items |> String.join ", ") ++ " ]" |> text ]
        ]


listInfoView : SlidingList a -> ViewResult v Msg
listInfoView theList =
    el
        NoStyle
        [ padding 10 ]
        ("Maximum items on the list: " ++ (theList |> SlidingList.maximumSize |> toString) |> text)


addItemsView : ViewResult v Msg
addItemsView =
    row
        NoStyle
        [ spacing 10 ]
        ([ "A", "B", "C", "D", "E" ]
            |> List.map
                (\i ->
                    button
                        NoStyle
                        [ padding 10
                        , onClick <| InsertIntoList i
                        ]
                        (text i)
                )
        )



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
