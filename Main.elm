-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html


port module Main exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Date
import Date.Distance as Distance
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task exposing (..)
import Time exposing (..)


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

        Del ascent ->
            ( { model | ascents = List.filter (notAscent ascent) model.ascents }, Cmd.none )

        CurrentTime time ->
            ( { model | ascents = Ascent model.grade time :: model.ascents, now = time }, Cmd.none )


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
                [ select [ onInput Grade ] (List.map (\x -> option [] [ text x ]) model.gradeList)
                , input [ type_ "button", value "Add", onClick Add ] []
                , ul []
                    (List.map
                        (\x ->
                            li []
                                [ text (x.grade ++ " " ++ Distance.inWords (Date.fromTime model.now) (Date.fromTime x.dateTime))
                                , input [ type_ "button", value "X", onClick (Del x) ] []
                                ]
                        )
                        model.ascents
                    )
                ]
            ]
        ]


viewTime : Time -> String
viewTime time =
    time
        |> Date.fromTime
        |> toString
