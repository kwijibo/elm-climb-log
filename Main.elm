-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html


port module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.ListGroup as ListGroup
import Date
import Date.Distance as Distance
import Html exposing (..)
import Html.Attributes exposing (property, style)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (string)
import Task exposing (..)
import Time exposing (..)


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault model savedModel ! []



-- MODEL


type alias Model =
    { now : Time
    , grade : String
    , ascents : List Ascent
    , gradeList : List String
    }


type alias Ascent =
    { grade : String
    , dateTime : Time
    }


model : Model
model =
    Model 0
        "4"
        []
        [ "4"
        , "4+"
        , "5"
        , "5+"
        , "6a"
        , "6a+"
        , "6b"
        , "6b+"
        , "6c"
        , "6c+"
        , "7a"
        , "7a+"
        , "7b"
        , "7b"
        , "7b+"
        , "7c"
        , "7c+"
        , "8a"
        , "8a+"
        , "8b"
        , "8b"
        , "8b+"
        , "8c"
        , "8c+"
        ]



-- UPDATE


type Msg
    = Grade String
    | Add
    | Del Ascent
    | CurrentTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Grade grade ->
            ( { model | grade = grade }, Cmd.none )

        Add ->
            ( model, Time.now |> Task.perform CurrentTime )

        CurrentTime time ->
            ( { model | ascents = Ascent model.grade time :: model.ascents, now = time }, Cmd.none )

        Del ascent ->
            ( { model | ascents = List.filter (notAscent ascent) model.ascents }, Cmd.none )


notAscent : Ascent -> Ascent -> Bool
notAscent a b =
    a /= b


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row [] [ Grid.col [] [ h1 [] [ text "Indoor Climbing Log" ] ] ]
        , Grid.row []
            [ Grid.col []
                [ Grid.row []
                    [ Grid.col []
                        [ select [ onInput Grade ] (List.map (\x -> option [] [ text x ]) model.gradeList)
                        , spacer
                        , Button.button [ Button.primary, Button.onClick Add ] [ text "Add" ]
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ p [] [] ] ]
                , Grid.row []
                    [ Grid.col []
                        [ ListGroup.ul
                            (List.map
                                (\ascent ->
                                    ListGroup.li []
                                        [ span [ style [ ( "width", "3em" ) ] ] [ text ascent.grade ]
                                        , spacer
                                        , small [ style [ ( "width", "9em" ) ] ] [ text (Distance.inWords (Date.fromTime model.now) (Date.fromTime ascent.dateTime)) ]
                                        , spacer
                                        , Button.button [ Button.onClick (Del ascent) ] [ text "x" ]
                                        ]
                                )
                                model.ascents
                            )
                        ]
                    ]
                ]
            ]
        ]


spacer : Html msg
spacer =
    span [ property "innerHTML" (string "&nbsp;&nbsp;") ] []
