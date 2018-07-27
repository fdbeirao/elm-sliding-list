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
    | IncreaseListSize
    | DecreaseListSize



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
            ( { model | theList = model.theList |> SlidingList.insert item }
            , Cmd.none
            )

        IncreaseListSize ->
            let
                newDesiredListSize =
                    model.desiredListSize + 1
            in
            ( { model
                | desiredListSize = newDesiredListSize
                , theList = model.theList |> SlidingList.resize newDesiredListSize
              }
            , Cmd.none
            )

        DecreaseListSize ->
            let
                newDesiredListSize =
                    model.desiredListSize - 1
            in
            ( { model
                | desiredListSize = newDesiredListSize
                , theList = model.theList |> SlidingList.resize newDesiredListSize
              }
            , Cmd.none
            )



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
        [ padding 10
        , spacing 5
        ]
        [ listItemsView model.theList
        , listInfoView model
        , addItemsView
        ]


listItemsView : SlidingList String -> ViewResult v Msg
listItemsView theList =
    row
        NoStyle
        [ padding 10
        , spacing 10
        , center
        ]
        [ "Elements currently on the list:" |> text
        , "[ " ++ (theList |> SlidingList.items |> String.join ", ") ++ " ]" |> text
        ]


listInfoView : Model -> ViewResult v Msg
listInfoView model =
    row
        NoStyle
        [ spacing 10 ]
        [ button
            NoStyle
            [ padding 10
            , onClick DecreaseListSize
            ]
            (text "-")
        , column
            NoStyle
            [ spacing 5 ]
            [ "Desired maximum items on the list: " ++ (model.desiredListSize |> toString) |> text
            , "Actual maximum items on the list: " ++ (model.theList |> SlidingList.maximumSize |> toString) |> text
            ]
        , button
            NoStyle
            [ padding 10
            , onClick IncreaseListSize
            ]
            (text "+")
        ]


addItemsView : ViewResult v Msg
addItemsView =
    column
        NoStyle
        [ spacing 5 ]
        [ text "Insert elements on the list:"
        , row
            NoStyle
            [ spacing 10
            , center
            ]
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
        ]



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
