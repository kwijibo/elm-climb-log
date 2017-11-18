-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html


port module Main exposing (main)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Dict
import Html exposing (..)
import Html.Attributes exposing (property, style, title)
import Set
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


type alias Grade =
    { name : String, color : String }


type alias Model =
    { now : Time
    , ascents : List Ascent
    }


type alias Ascent =
    { grade : Grade
    , dateTime : Time
    }


model : Model
model =
    Model 0
        []


gradeList : List Grade
gradeList =
    [ Grade "4" "SpringGreen"
    , Grade "4+" "Turquoise"
    , Grade "5" "Aqua"
    , Grade "5+" "SteelBlue"
    , Grade "6a" "Olive"
    , Grade "6a+" "Wheat"
    , Grade "6b" "Lime"
    , Grade "6b+" "Violet"
    , Grade "6c" "Orange"
    , Grade "6c+" "Red"
    , Grade "7a" "Maroon"
    , Grade "7a+" "Fuchsia"
    , Grade "7b" "Teal"
    , Grade "7b+" "Tomato"
    , Grade "7c" "Gray"
    , Grade "7c+" "Thistle"
    , Grade "8a" "Tan"
    ]



-- UPDATE


type Msg
    = Add Grade
    | TimeStampAscent Grade Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add grade ->
            ( model, Time.now |> Task.perform (TimeStampAscent grade) )

        TimeStampAscent grade time ->
            ( { model | ascents = Ascent grade time :: model.ascents, now = time }, Cmd.none )


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
    let
        counts =
            countAscentsByGrade model.ascents

        maxNum =
            maxValue counts
    in
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row [] [ Grid.col [] [ h1 [] [ text "Sport Climbing Pyramid" ] ] ]
        , table []
            (gradeList
                |> List.take
                    (1
                        + countDistinct (List.map (.grade >> .name) model.ascents)
                    )
                |> List.reverse
                |> List.map
                    (\grade ->
                        tr []
                            [ td [] [ Button.button [ Button.onClick (Add grade), Button.attrs [ style [ ( "width", "3.25em" ) ] ] ] [ text grade.name ] ]
                            , td []
                                [ makeBar
                                    (gradeWidth grade.name counts maxNum)
                                    grade.color
                                    (Dict.get grade.name counts |> Maybe.withDefault 0)
                                ]
                            ]
                    )
            )
        ]


makeBar : String -> String -> Int -> Html msg
makeBar width color numAscents =
    div [ style [ ( "width", "200px" ) ] ]
        [ div
            [ style
                [ ( "opacity", "0.8" )
                , ( "border-radius", "2px" )
                , ( "width", width )
                , ( "color", "White" )
                , ( "background-color", color )
                , ( "height", "2.5em" )
                , ( "margin-top", "1px" )
                , ( "margin-bottom", "1px" )
                , ( "margin-left", "auto" )
                , ( "margin-right", "auto" )
                , ( "text-align", "center" )
                , ( "padding", "0.5em" )
                ]
            , title (toString numAscents ++ " ascents")
            ]
            [ text (toString numAscents) ]
        ]


gradeWidth : comparable -> Dict.Dict comparable Int -> Float -> String
gradeWidth key dict maxNum =
    (Dict.get key dict |> Maybe.withDefault 0 |> toFloat) / maxNum |> (*) 100 |> toString |> (\p -> p ++ "%")


countAscentsByGrade : List Ascent -> Dict.Dict String Int
countAscentsByGrade ascents =
    List.foldl (\ascent counts -> Dict.update ascent.grade.name incWithZero counts) Dict.empty ascents


incWithZero : Maybe number -> Maybe number
incWithZero =
    \n -> Maybe.withDefault 0 n + 1 |> Maybe.Just


countDistinct : List comparable -> Int
countDistinct xs =
    xs |> Set.fromList |> Set.size


maxValue : Dict.Dict String Int -> Float
maxValue counts =
    counts
        |> Dict.values
        |> List.foldl max 0
        |> toFloat
