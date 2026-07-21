-- compile with `elm make src/Main.elm --output=elm.js`


port module Main exposing (main)

import Browser
import Browser.Dom
import Html exposing (Html, button, div, form, img, input, li, span, text, ul)
import Html.Attributes exposing (attribute, class, id, placeholder, src, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Json.Decode as D
import Json.Encode as E
import Random
import Task


port toJs : String -> Cmd msg


port fromJs : (String -> msg) -> Sub msg


type Msg
    = PressedGetTree
    | GotNewTreeId Int
    | GotJsMessage String
    | ClickedTree Int
    | ClickedTrunkModalBg
    | SubmittedMessage Int String
    | ChangedMessageInput String
    | SignalPressed
    | GotSignalIndex Int
    | NoOp


type alias Tree =
    { id : Int, messages : List String }


type alias Model =
    { trees : List Tree, selectedTreeId : Maybe Int, messageInput : String }


type JsMsg
    = ReceivedTrees (List Tree)
    | ReceivedSignal Int


type PortMsg
    = AddTree Int
    | AddMessage Int String
    | SignalMessage Int


init : Model
init =
    { trees = [], selectedTreeId = Nothing, messageInput = "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedGetTree ->
            ( model
            , Random.generate GotNewTreeId (Random.int 1 10000)
            )

        GotNewTreeId n ->
            ( model
            , toJs (encodePortMsg (AddTree n))
            )

        GotJsMessage json ->
            case D.decodeString msgDecoder json of
                Ok (ReceivedTrees trees) ->
                    ( { model | trees = trees }, scrollToBottom )

                Ok (ReceivedSignal _) ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ClickedTree treeId ->
            ( { model | selectedTreeId = Just treeId }, scrollToBottom )

        ClickedTrunkModalBg ->
            ( { model | selectedTreeId = Nothing, messageInput = "" }, Cmd.none )

        SubmittedMessage treeId messageText ->
            ( { model | messageInput = "" }, toJs (encodePortMsg (AddMessage treeId messageText)) )

        ChangedMessageInput value ->
            ( { model | messageInput = value }, Cmd.none )

        SignalPressed ->
            let
                treesWithMessages =
                    List.filter (\t -> not (List.isEmpty t.messages)) model.trees
            in
            case treesWithMessages of
                [] ->
                    ( model, Cmd.none )

                _ ->
                    ( model
                    , Random.generate GotSignalIndex (Random.int 0 (List.length treesWithMessages - 1))
                    )

        GotSignalIndex index ->
            let
                treesWithMessages =
                    List.filter (\t -> not (List.isEmpty t.messages)) model.trees

                maybeTree =
                    List.drop index treesWithMessages |> List.head
            in
            case maybeTree of
                Just tree ->
                    ( model, toJs (encodePortMsg (SignalMessage tree.id)) )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


scrollToBottom : Cmd Msg
scrollToBottom =
    Browser.Dom.getViewportOf "message-list"
        |> Task.andThen (\info -> Browser.Dom.setViewportOf "message-list" 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


view : Model -> Html Msg
view model =
    div [ class "p-8 w-full h-screen bg-background overflow-hidden" ]
        [ span
            [ id "sprite"
            , class "fixed top-8 right-8 text-3xl z-1 cursor-pointer"
            , onClick SignalPressed
            ]
            [ img
                [ class "w-32"
                , src "assets/cat.png"
                ]
                []
            ]
        , Keyed.node "div"
            []
            (List.map
                (\tree ->
                    ( String.fromInt tree.id
                    , img
                        [ id (String.fromInt tree.id)
                        , src ("assets/" ++ getTree tree.id)
                        , class "h-32 absolute tree-loading"
                        , attribute "can-move" ""
                        , onDoubleClick (ClickedTree tree.id)
                        ]
                        []
                    )
                )
                model.trees
            )
        , div [ class "h-full flex flex-col justify-end items-end" ]
            [ div [ class "w-[100%] flex flex-row justify-center" ]
                [ button
                    [ onClick PressedGetTree
                    , class "px-24 py-4 bg-button active:bg-button-pressed border-8 border-button-border rounded-3xl text-2xl text-text-white z-40"
                    ]
                    [ text "Get a tree" ]
                ]
            ]
        , case model.selectedTreeId |> Maybe.andThen (\sid -> List.filter (\t -> t.id == sid) model.trees |> List.head) of
            Just tree ->
                div []
                    [ div
                        [ class "absolute z-4 bottom-0 right-0 h-screen w-screen bg-black opacity-80"
                        , onClick ClickedTrunkModalBg
                        ]
                        []
                    , div
                        [ class "absolute z-8 bottom-0 right-0 h-screen w-96 bg-tree-brown modal flex flex-col" ]
                        [ ul [ id "message-list", class "flex-1 overflow-auto" ] (List.map (\message -> li [ class "text-text-white yuyu-regular text-4xl p-2" ] [ text message ]) tree.messages)
                        , form [ class "flex flex-row", onSubmit (SubmittedMessage tree.id model.messageInput) ]
                            [ input
                                [ class "p-2 m-2 flex-1 min-w-0"
                                , onInput ChangedMessageInput
                                , value model.messageInput
                                , placeholder "Carve a message on that tree..."
                                ]
                                []
                            , button
                                [ class "m-2 shrink-0" ]
                                [ img
                                    [ src "assets/tree-icon.svg"
                                    , class "w-8 h-8"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    ]

            Nothing ->
                text ""
        ]


getTree : Int -> String
getTree id =
    let
        options =
            [ 0, 0, 0, 1, 1, 2, 2, 3, 3, 4 ]

        t =
            List.drop (modBy 10 id) options |> List.head |> Maybe.withDefault 0
    in
    "tree-" ++ String.fromInt t ++ ".png"


encodePortMsg : PortMsg -> String
encodePortMsg msg =
    case msg of
        AddTree treeId ->
            E.encode 0
                (E.object
                    [ ( "id", E.string "addTree" )
                    , ( "direction", E.string "toJs" )
                    , ( "data", E.object [ ( "treeId", E.int treeId ) ] )
                    ]
                )

        AddMessage treeId messageText ->
            E.encode 0
                (E.object
                    [ ( "id", E.string "addMessage" )
                    , ( "direction", E.string "toJs" )
                    , ( "data", E.object [ ( "treeId", E.int treeId ), ( "message", E.string messageText ) ] )
                    ]
                )

        SignalMessage treeId ->
            E.encode 0
                (E.object
                    [ ( "id", E.string "signalMessage" )
                    , ( "direction", E.string "toJs" )
                    , ( "treeId", E.int treeId )
                    ]
                )


msgDecoder : D.Decoder JsMsg
msgDecoder =
    D.field "id" D.string
        |> D.andThen
            (\msgId ->
                case msgId of
                    "fetchTrees" ->
                        D.field "data"
                            (D.list
                                (D.map2 Tree
                                    (D.field "id" D.int)
                                    (D.oneOf
                                        [ D.field "messages" (D.list D.string)
                                        , D.succeed []
                                        ]
                                    )
                                )
                            )
                            |> D.map ReceivedTrees

                    "signalTree" ->
                        D.field "data" D.int
                            |> D.map ReceivedSignal

                    _ ->
                        D.fail ("Unknown message id: " ++ msgId)
            )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> fromJs GotJsMessage
        }
