module Main exposing (main)

import Browser
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import String.Interpolate as SI


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
    , selected : Id
    , nextId : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree = Text 0 "Okay", selected = -1, nextId = 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( ui, code ) =
            parse model.tree 0 model.selected
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
            [ El.column
                ([ El.width El.fill
                 , El.height El.fill
                 , El.spacing 10
                 ]
                    ++ border
                )
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
                    code
                , Input.button [ El.centerX ]
                    { label =
                        El.el
                            ([ El.width El.fill, El.padding 5 ] ++ border)
                        <|
                            El.text "Add Text"
                    , onPress = Just AddText
                    }
                , Input.button [ El.centerX ]
                    { label =
                        El.el
                            ([ El.width El.fill, El.padding 5 ] ++ border)
                        <|
                            El.text "Add Element"
                    , onPress = Just AddElement
                    }
                , selectedMenu model
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
    | Selected Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nextNextId =
            model.nextId + 1
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddText ->
            ( { model
                | tree = Text model.nextId "Some text"
                , nextId = nextNextId
              }
            , Cmd.none
            )

        AddElement ->
            ( { model
                | tree = El model.nextId model.tree
                , nextId = nextNextId
              }
            , Cmd.none
            )

        Selected id ->
            ( { model | selected = id }, Cmd.none )


border : List (El.Attribute msg)
border =
    [ Border.width 1
    , Border.color <| El.rgb255 0 0 0
    , Border.rounded 5
    ]


parse : Node -> Int -> Id -> ( El.Element msg, El.Element Msg )
parse tree indent selected =
    let
        tabs x =
            List.repeat x "  " |> String.join ""

        pipe =
            if indent == 0 then
                ""

            else
                tabs (indent - 1) ++ "<|\n"

        selectedStyle id =
            if id == selected then
                [ Font.color <| El.rgb255 0 0 255 ]

            else
                [ Font.color <| El.rgb255 255 255 255 ]
    in
    case tree of
        Text id value ->
            ( El.text value
            , El.el ((Events.onClick <| Selected id) :: selectedStyle id) <|
                El.text <|
                    SI.interpolate
                        "{1}{0}El.text \"{2}\""
                        [ tabs indent, pipe, value ]
            )

        El id node ->
            let
                ( child, code ) =
                    parse node (indent + 1) selected
            in
            ( El.el
                [ El.explain Debug.todo ]
                child
            , El.column [] <|
                [ El.el ((Events.onClick <| Selected id) :: selectedStyle id) <|
                    El.text <|
                        SI.interpolate
                            "{1}{0}El.el\n{2}[]{0}"
                            [ tabs indent, pipe, tabs (indent + 1) ]
                , code
                ]
            )

        _ ->
            ( El.none, El.none )


type Node
    = El Id Node
    | Text Id String
    | Row Id (List Node)
    | Column Id (List Node)
    | None


type alias Id =
    Int


getId : Node -> Id
getId node =
    case node of
        El id _ ->
            id

        Text id _ ->
            id

        Row id _ ->
            id

        Column id _ ->
            id

        None ->
            -1


selectedMenu model =
    let
        node =
            findNodeById model.tree model.selected
    in
    case node of
        El _ child ->
            El.text "Element selected"

        Text _ value ->
            El.text <| "Text with value " ++ value ++ " selected"

        _ ->
            El.text "None selected"


findNodeById : Node -> Id -> Node
findNodeById tree id =
    if getId tree == id then
        tree

    else
        case tree of
            El _ child ->
                findNodeById child id

            _ ->
                None
