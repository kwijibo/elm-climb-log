-- Read all about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/forms.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid



main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { grade : String
  , ascents : List Ascent
  , gradeList: List String
  }

type alias Ascent = 
  {
      grade: String
    , dateTime: String
  }

model : Model
model =
  Model "" [] ["4", "4+"
  , "5", "5+"
  , "6a", "6a+", "6b", "6b+", "6c", "6c+"
  , "7a", "7a+", "7b", "7b", "7b+", "7c", "7c+"
  , "8a", "8a+", "8b", "8b", "8b+", "8c", "8c+"
  ]



-- UPDATE


type Msg
    = Grade String
    | Add


update : Msg -> Model -> Model
update msg model =
  case msg of
    Grade grade ->
      { model | grade = grade }
    Add ->
      { model | ascents = (Ascent model.grade "today")::model.ascents}


-- VIEW


view : Model -> Html Msg
view model =
 Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ 
                  select [ onInput Grade  ] (List.map (\x -> option [] [text x]) model.gradeList)
                , input [ type_ "button", value "Add", onClick Add  ] []
                , ul [] (List.map (\x -> li [] [text (x.grade ++ " " ++ x.dateTime)] ) model.ascents)
                ]
            ]

        ]   


