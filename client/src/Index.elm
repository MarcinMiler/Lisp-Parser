module Index exposing(..)

import Browser
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Http

import Element as Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element exposing (Element)

main : Program () Model Msg
main = Browser.element { init = init , update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


type alias Model =
  { history : List History
  , content : String
  }
type alias History = 
  { expression : String
  , result : String 
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({ history = [], content = "" }, Cmd.none)

type Msg
  = ChangeContent String
  | EnterWasPressed
  | GotResult (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeContent newContent ->
      ({ model | content = newContent }, Cmd.none)

    EnterWasPressed ->
      (model, getParsedExpression model.content)

    GotResult result ->
      case result of
        Ok expression ->
          ({ model | content = "", history = List.append [{ expression = model.content, result = expression}] model.history }, Cmd.none)
        
        Err err -> (model, Cmd.none)

type alias Attributes = List (Element.Attribute Msg)

onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
common : Attributes
common = [ Font.size 20, Element.paddingXY 5 0 ]

promptView : Element Msg
promptView = Element.el common <| Element.text "λ >"

inputView : Model -> Element Msg
inputView model =
  Input.text [ Background.color <| Element.rgb255 0 0 0
             , Border.width 0
             , Font.size 20
             , Element.padding 0
             , Element.spacing 0
             , onEnter EnterWasPressed
             , Input.focusedOnLoad ] 
             { placeholder = Nothing
             , onChange = ChangeContent
             , label = Input.labelAbove [] Element.none
             , text = model.content }

historyView : History -> Element Msg
historyView history =
  Element.column [ Element.height Element.fill, Element.width Element.fill, Font.color <| Element.rgb255 255 255 255, Font.size 20, Element.spacingXY 0 0 ] [
    Element.row [] [
      promptView,
      Element.text history.expression
    ],
    Element.row [ Element.paddingXY 5 5] [
      Element.text history.result
    ]
  ]

inputWithPromptLayout : Model -> Element Msg
inputWithPromptLayout model =
    Element.column [ Element.height Element.fill, Element.width Element.fill, Font.color <| Element.rgb255 255 255 255, Element.centerX, Element.centerY ] [
        Element.row [ Element.width Element.fill ] [
          promptView,
          inputView model
      ]
    ]

view : Model -> Html Msg
view model =
    Element.layoutWith { options = [Element.focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing }, Element.noHover] } [] <|
        Element.column [ Element.width Element.fill, Element.height Element.fill, Background.color <| Element.rgb255 0 0 0, Element.paddingXY 0 5 ] [
            Element.column [ Element.width Element.fill ]
              (List.reverse <| List.map historyView model.history),
            Element.column [] [ inputWithPromptLayout model]
        ]

type alias Expression = 
  { expression : String }

getParsedExpression : String -> Cmd Msg
getParsedExpression expression =
  Http.post
    { url = "http://127.0.0.1:4000/parse"
    , body = Http.jsonBody (newParseExpression expression)
    , expect = Http.expectJson GotResult resultDecoder
    }

newParseExpression : String -> Encode.Value
newParseExpression expression =
    Encode.object
        [ ( "expression", Encode.string expression )]


resultDecoder : Decode.Decoder String
resultDecoder =
  Decode.field "expression" Decode.string