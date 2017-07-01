import Html exposing (Html, div, text, textarea, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Markdown exposing (toHtml)

main =
  Html.beginnerProgram {model = model, view = view, update = update}

--Model

type Tabs = InputTab | PreviewTab

type alias Model = { tab : Tabs, input : String, parsed : Html Msg}

model : Model
model = { tab = InputTab, input = "", parsed = text "" }


-- Update

type Msg = TabSwitchInput | TabSwitchPreview | UpdateInput String

update : Msg -> Model -> Model
update msg model = 
  case msg of 
    TabSwitchInput ->
     {model | tab = InputTab}
    TabSwitchPreview ->
     {model | tab = PreviewTab, parsed = Markdown.toHtml [] model.input }
    UpdateInput newText ->
     {model | input = newText }


-- View
view : Model -> Html Msg
view model = 
  div [] [
    div [] [
      a [ onClick TabSwitchInput ] [ text "Input" ],
      a [ onClick TabSwitchPreview ] [ text "Preview" ]
    ],
    div [ tabStyle model InputTab ] [
      textarea [ onInput UpdateInput ] []
    ],
    div [ tabStyle model PreviewTab ] [
      model.parsed
    ]
  ]

tabStyle : Model -> Tabs -> Html.Attribute Msg
tabStyle model tab = 
  let
    attrText = if model.tab == tab then "block" else "none"
  in
    style [ ( "display", attrText ) ]
  
