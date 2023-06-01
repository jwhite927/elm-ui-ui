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
    | StyledNode Id (List Style)
    | GotText String


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
                | tree = El [ Explain ] model.nextId model.tree
                , nextId = nextNextId
              }
            , Cmd.none
            )

        Selected id ->
            ( { model | selected = id }, Cmd.none )

        StyledNode id style ->
            ( { model | tree = withNode id (El style id None) model.tree }, Cmd.none )

        GotText i ->
            ( { model | tree = withNode model.selected (Text model.selected i) model.tree }, Cmd.none )


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

        El style id node ->
            let
                ( child, code ) =
                    parse node (indent + 1) selected
            in
            ( El.el
                (List.map toAttr style)
                child
            , El.column [] <|
                [ El.el ((Events.onClick <| Selected id) :: selectedStyle id) <|
                    El.text <|
                        SI.interpolate
                            "{1}{0}El.el\n{2}[{3}]{0}"
                            [ tabs indent, pipe, tabs (indent + 1), List.map toText style |> String.join ", " ]
                , code
                ]
            )

        _ ->
            ( El.none, El.none )


type Node
    = El (List Style) Id Node
    | Text Id String
      -- | Row Id (List Node)
      -- | Column Id (List Node)
    | None


type alias Id =
    Int


getId : Node -> Id
getId node =
    case node of
        El _ id _ ->
            id

        Text id _ ->
            id

        -- Row id _ ->
        --     id
        -- Column id _ ->
        --     id
        None ->
            -1


selectedMenu model =
    let
        node =
            findNodeById model.tree model.selected
    in
    case node of
        El style _ child ->
            El.column [ El.spacing 5, El.padding 10 ]
                [ El.text "Add Style"
                , El.wrappedRow [ El.height <| El.minimum 40 <| El.shrink ]
                    (allStyles |> List.filter (\this -> not <| List.member this style) |> List.map (toAddButton model.selected style))
                , El.text "Remove Style"
                , El.wrappedRow [ El.height <| El.minimum 40 <| El.shrink ]
                    (allStyles |> List.filter (\this -> List.member this style) |> List.map (toRemoveButton model.selected style))
                ]

        Text _ value ->
            El.column [] [ Input.text [] { onChange = GotText, text = value, placeholder = Nothing, label = Input.labelAbove [] <| El.text "Text Node" } ]

        _ ->
            El.text "None selected"


toAddButton id styles style =
    Input.button button { label = El.text <| toText style, onPress = StyledNode id (style :: styles) |> Just }


toRemoveButton id styles style =
    Input.button button { label = El.text <| toText style, onPress = StyledNode id (List.filter ((/=) style) styles) |> Just }


button =
    [ Border.rounded 10
    , Background.color <| El.rgb255 81 208 143
    , El.padding 5
    , Font.color <| El.rgb255 255 255 255
    ]


findNodeById : Node -> Id -> Node
findNodeById tree id =
    if getId tree == id then
        tree

    else
        case tree of
            El _ _ child ->
                findNodeById child id

            _ ->
                None


withNode : Id -> Node -> Node -> Node
withNode id node tree =
    case tree of
        El style nodeId child ->
            if nodeId == id then
                case node of
                    El insertStyle _ _ ->
                        El insertStyle id child

                    _ ->
                        node

            else
                El style nodeId <| withNode id node child

        Text nodeId value ->
            if nodeId == id then
                node

            else
                Text nodeId value

        _ ->
            None


type Style
    = Explain
    | WidthFill


allStyles =
    [ Explain
    , WidthFill
    ]


toAttr style =
    case style of
        Explain ->
            El.explain Debug.todo
        WidthFill ->
            El.width El.fill


toText style =
    case style of
        Explain ->
            "El.explain Debug.todo"
        WidthFill ->
            "El.width El.fill"
