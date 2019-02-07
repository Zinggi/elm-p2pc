port module Main exposing (main)

import Browser
import Game exposing (Game)
import Html exposing (..)
import Html.Attributes exposing (class, cols, placeholder, readonly, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Move exposing (Move)
import Notation
import Piece exposing (Piece)
import PieceColor
import PieceType
import Position exposing (Position)
import Square exposing (Square)
import SquareFile as File
import SquareRank as Rank


type alias Model =
    { myId : Maybe String
    , error : Maybe String
    , otherId : String
    , game : Game
    , isBlack : Bool
    , selectedSquare : Maybe Square
    , candidateMoves : List Move
    , gameState : GameState
    }


initModel =
    { game = Game.empty
    , error = Nothing
    , selectedSquare = Nothing
    , candidateMoves = []
    , isBlack = False
    , myId = Nothing
    , otherId = ""
    , gameState = Init
    }


init flags =
    ( initModel, output (JE.int 57) )


type GameState
    = Init
    | MyTurn
    | OtherTurn


type Msg
    = JsPort Value
    | SetOtherId String
    | Connect
    | SquarePressed Square
      -- | DoMove Move
    | GotId String
    | Ready
    | Ready2
    | GotMove String
    | Rematch
    | GotRematch


update msg model =
    case msg of
        JsPort val ->
            case JD.decodeValue portDecoder val of
                Ok msg_ ->
                    update msg_ model

                Err err ->
                    ( { model | error = Just (JD.errorToString err) }, Cmd.none )

        SetOtherId s ->
            ( { model | otherId = s }, Cmd.none )

        Connect ->
            ( model, output (JE.object [ ( "type", JE.string "Connect" ), ( "otherId", JE.string model.otherId ) ]) )

        SquarePressed sq ->
            squarePressed sq model

        -- DoMove move ->
        --     doMove move model
        GotId id ->
            ( { model | myId = Just id }, Cmd.none )

        Ready ->
            ( { model | gameState = MyTurn }, send [ ( "type", JE.string "Ready2" ) ] )

        Ready2 ->
            ( { model | gameState = OtherTurn, isBlack = True }, Cmd.none )

        GotMove m ->
            case Notation.fromSan m (Game.position model.game) of
                Just move ->
                    ( { model | game = Game.addMove move model.game, gameState = MyTurn, selectedSquare = Nothing, candidateMoves = [] }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Rematch ->
            ( rematch model, send [ ( "type", JE.string "Rematch" ) ] )

        GotRematch ->
            ( rematch model, Cmd.none )


rematch model =
    { model
        | game = Game.empty

        -- , error = Nothing
        , selectedSquare = Nothing
        , candidateMoves = []
        , isBlack = not model.isBlack

        -- , myId = Nothing
        -- , otherId = ""
        , gameState =
            if model.isBlack then
                MyTurn

            else
                OtherTurn
    }


send msg =
    output (JE.object [ ( "type", JE.string "Send" ), ( "data", JE.object msg ) ])


doMove : Move -> Model -> ( Model, Cmd Msg )
doMove move model =
    let
        ( newGame, newState, cmd ) =
            case model.gameState of
                MyTurn ->
                    ( Game.addMove move model.game
                    , OtherTurn
                    , send [ ( "type", JE.string "GotMove" ), ( "move", JE.string (Notation.toSan move (Game.position model.game)) ) ]
                    )

                _ ->
                    ( model.game, model.gameState, Cmd.none )
    in
    ( { model
        | game = newGame
        , selectedSquare = Nothing
        , candidateMoves = []
        , gameState = newState
      }
    , cmd
    )


squarePressed : Square -> Model -> ( Model, Cmd Msg )
squarePressed s model =
    let
        moves =
            List.filter (\m -> Move.to m == s) model.candidateMoves
    in
    case List.head moves of
        Just m ->
            doMove m model

        Nothing ->
            let
                newMoves =
                    Game.position model.game |> Position.movesFrom s
            in
            ( { model
                | candidateMoves = newMoves
                , selectedSquare =
                    if List.length newMoves == 0 then
                        Nothing

                    else
                        Just s
              }
            , Cmd.none
            )


portDecoder : Decoder Msg
portDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "GotId" ->
                        JD.field "id" JD.string
                            |> JD.map GotId

                    "GotMsg" ->
                        JD.field "data" msgDecoder

                    _ ->
                        JD.fail "Unknown object"
            )


msgDecoder : Decoder Msg
msgDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\t ->
                case t of
                    "Ready" ->
                        JD.succeed Ready

                    "Ready2" ->
                        JD.succeed Ready2

                    "GotMove" ->
                        JD.field "move" JD.string
                            |> JD.map GotMove

                    "Rematch" ->
                        JD.succeed GotRematch

                    _ ->
                        JD.fail "Unknown object"
            )


port output : Value -> Cmd msg


port input : (Value -> msg) -> Sub msg


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "my id", Html.input [ readonly True, value (model.myId |> Maybe.withDefault "") ] [] ]
        , text "other id"
        , case model.error of
            Nothing ->
                text ""

            Just e ->
                div [] [ b [] [ text e ] ]
        , Html.input [ value model.otherId, onInput SetOtherId ] []
        , button [ onClick Connect ] [ text "Connect" ]
        , case model.gameState of
            Init ->
                div [] [ text "Waiting for other player" ]

            MyTurn ->
                div [] [ board model.isBlack (Game.position model.game) 400.0 model.isBlack ]

            OtherTurn ->
                div []
                    [ board model.isBlack (Game.position model.game) 400.0 model.isBlack
                    , text "Wait for other player to make a move"
                    ]

        -- , div [] [ text (Debug.toString model) ]
        ]


px : Float -> String
px f =
    String.fromFloat f ++ "px"


url : String -> String
url u =
    "url(" ++ u ++ ")"


board : Bool -> Position -> Float -> Bool -> Html Msg
board isBlack position size isRotated =
    div []
        [ Html.div
            [ style "width" (px size)
            , style "height" (px size)
            , style "position" "relative"
            , style "marginLeft" "auto"
            , style "marginRight" "auto"
            ]
            (List.map
                (\s ->
                    square
                        (squareToCoordinates s isRotated)
                        (Position.pieceOn s position)
                        (size / 8)
                        (SquarePressed s)
                )
                Square.all
            )
        , if Position.isCheckmate position then
            let
                txt =
                    if Position.sideToMove position == PieceColor.black && isBlack then
                        "You lose!"

                    else
                        "You win!"
            in
            div []
                [ b [] [ text ("Checkmate! " ++ txt) ]
                , button [ onClick Rematch ] [ text "Rematch" ]
                ]

          else
            text ""
        ]


square : ( Int, Int ) -> Maybe Piece -> Float -> Msg -> Html Msg
square ( col, row ) piece sqSize msg =
    Html.div
        [ onClick msg
        , style "backgroundColor"
            (if modBy 2 (col + row) == 0 then
                "rgb(200, 200, 200)"

             else
                "rgb(140, 140, 140)"
            )
        , style "position" "absolute"
        , style "top" (px (toFloat row * sqSize))
        , style "left" (px (toFloat col * sqSize))
        , style "width" (px sqSize)
        , style "height" (px sqSize)
        ]
        [ case piece of
            Nothing ->
                text ""

            Just piece_ ->
                div
                    [ style "position" "absolute"
                    , style "width" (px sqSize)
                    , style "height" (px sqSize)
                    , style "backgroundImage" (url (pieceImgUrl piece_))
                    , style "backgroundSize" (px sqSize ++ " " ++ px sqSize)
                    ]
                    []
        ]


squareToCoordinates : Square -> Bool -> ( Int, Int )
squareToCoordinates square_ isRotated =
    ( if isRotated then
        7 - (square_ |> Square.file |> File.toIndex)

      else
        square_ |> Square.file |> File.toIndex
    , if isRotated then
        square_ |> Square.rank |> Rank.toIndex

      else
        7 - (square_ |> Square.rank |> Rank.toIndex)
    )


pieceImgUrl : Piece -> String
pieceImgUrl piece =
    "http://res.cloudinary.com/ds1kquy7j/image/upload/"
        ++ (piece |> Piece.color |> PieceColor.toString)
        ++ (piece |> Piece.kind |> PieceType.toString |> String.toLower)
        ++ ".png"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> input JsPort
        }
