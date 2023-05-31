module Main exposing (main)

import Browser
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { tree : Node
    , selected : Node
    }


init _ =
    ( { tree = Text "Okay", selected = None }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( ui, code ) =
            parse model.tree 0
    in
    El.layout
        [ El.width El.fill
        ]
    <|
        El.row
            [ El.width El.fill
            , El.padding 10
            , El.spacing 5
            , El.height El.fill
            ]
            [ El.column ([ El.width El.fill, El.height El.fill, El.spacing 10 ] ++ border)
                [ El.el [ El.centerX, El.padding 5 ] <| El.text "Code"
                , El.el
                    [ Background.color <| El.rgb255 0 0 0
                    , Font.color <| El.rgb255 255 255 255
                    , El.padding 10
                    , Font.family [ Font.monospace ]
                    , El.centerX
                    , Border.rounded 5
                    ]
                  <|
                    El.text code
                , Input.button [ El.centerX]
                    { label =
                        El.el
                            ([ El.width El.fill, El.padding 5 ] ++ border)
                        <|
                            El.text "Add Text"
                    , onPress = Just AddText
                    }
                , Input.button [ El.centerX]
                    { label =
                        El.el
                            ([ El.width El.fill, El.padding 5 ] ++ border)
                        <|
                            El.text "Add Element"
                    , onPress = Just AddElement
                    }
                ]
            , El.column
                ([ El.width El.fill
                 , El.height El.fill
                 ]
                    ++ border
                )
                [ El.el [ El.centerX, El.padding 5 ] <| El.text "View"
                , ui
                ]
            ]


type Msg
    = NoOp
    | AddText
    | AddElement


update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddText ->
            ( { model | tree = Text "Some text" }, Cmd.none )

        AddElement ->
            ( { model | tree = El model.tree }, Cmd.none )


border =
    [ Border.width 1, Border.color <| El.rgb255 0 0 0, Border.rounded 5 ]


parse : Node -> Int -> ( El.Element msg, String )
parse tree indent =
    let
        pipe =
            if indent == 0 then
                ""

            else
                "<| "
    in
    case tree of
        Text value ->
            ( El.text value
            , pipe
                ++ "El.text \""
                ++ value
                ++ "\""
            )

        El node ->
            let
                ( child, code ) =
                    parse node (indent + 1)
            in
            ( El.el
                [ El.explain Debug.todo ]
                child
            , pipe ++ "El.el\n\t[]\n\t" ++ code ++ "\""
            )

        _ ->
            ( El.none, "" )


type Node
    = El Node
    | Text String
    | Row (List Node)
    | Column (List Node)
    | None
