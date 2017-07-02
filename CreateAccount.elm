module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- Model


type alias Model =
    { username : String, password : String, passwordAgain : String, showValidation : Bool, errors : List ValidationFailures }


model : Model
model =
    { username = "", password = "", passwordAgain = "", showValidation = False, errors = [] }



-- Update


type Msg
    = ChangeName String
    | ChangePassword String
    | ChangePasswordAgain String
    | ShowValidation


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeName newName ->
            { model | username = newName }

        ChangePassword newPassword ->
            { model | password = newPassword }

        ChangePasswordAgain newPasswordAgain ->
            { model | passwordAgain = newPasswordAgain }

        ShowValidation ->
            { model | showValidation = True, errors = validateForm model }



-- View


view : Model -> Html Msg
view model =
    div []
        [ div [] [ input [ type_ "text", placeholder "User Name", onInput ChangeName ] [] ]
        , div [] [ input [ type_ "password", placeholder "Password", onInput ChangePassword ] [] ]
        , div [] [ input [ type_ "password", placeholder "Re-enter Password", onInput ChangePasswordAgain ] [] ]
        , if model.showValidation then
            viewValidation model
          else
            div [] []
        , button [ onClick ShowValidation ] [ text "Validate" ]
        ]


viewValidation : Model -> Html Msg
viewValidation model =
    model.errors
        |> List.map validationMessage
        |> List.map viewError
        |> div []


viewError : ( String, String ) -> Html Msg
viewError error =
    let
        ( color, message ) =
            error
    in
        div [ style [ ( "color", color ) ] ] [ text message ]


type ValidationFailures
    = NoUpperCase
    | NoLowerCase
    | NoDigits
    | MinLength
    | NotMatching


validateForm : Model -> List ValidationFailures
validateForm model =
    [] ++ validateLength model ++ validateMatching model ++ validationUppercase model ++ validationLowercase model ++ validationDigits model


validateLength model =
    if String.length model.password < 8 then
        [ MinLength ]
    else
        []


validateMatching model =
    if not (model.password == model.passwordAgain) then
        [ NotMatching ]
    else
        []


validationUppercase model =
    if not (String.any isUpper model.password) then
        [ NoUpperCase ]
    else
        []


validationLowercase model =
    if not (String.any isLower model.password) then
        [ NoLowerCase ]
    else
        []


validationDigits model =
    if not (String.any isDigit model.password) then
        [ NoDigits ]
    else
        []


validationMessage : ValidationFailures -> ( String, String )
validationMessage error =
    case error of
        MinLength ->
            ( "red", "Minimum 8 characters" )

        NotMatching ->
            ( "red", "Passwords do not match" )

        NoUpperCase ->
            ( "red", "Must contain at least one uppercase" )

        NoLowerCase ->
            ( "red", "Must contain at least one lowercase" )

        NoDigits ->
            ( "red", "Must contain at least one digit" )
