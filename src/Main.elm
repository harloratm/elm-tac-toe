module Main exposing (view)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (svg, use)
import Svg.Attributes exposing (viewBox, xlinkHref)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Cell
    = X
    | O
    | Empty


type GameResult
    = XWin
    | OWin
    | Draw
    | Ongoing


type alias Model =
    { board : List Cell, current : Cell, result : GameResult }


cellByIndex : List Cell -> Int -> Maybe Cell
cellByIndex b i =
    Array.get i (Array.fromList b)


init : Model
init =
    { board = List.repeat 9 Empty
    , current = X
    , result = Ongoing
    }



-- UPDATE


type Msg
    = Turn Int
    | NewGame
    | Noop


checkTriplet : List Cell -> ( Int, Int, Int ) -> Bool
checkTriplet board ( i1, i2, i3 ) =
    let
        c1 =
            cellByIndex board i1

        c2 =
            cellByIndex board i2

        c3 =
            cellByIndex board i3
    in
    c1 == c2 && c2 == c3 && c1 /= Just Empty && c2 /= Just Empty && c3 /= Just Empty


checkBoard : Model -> GameResult
checkBoard model =
    let
        res =
            [ checkTriplet model.board ( 0, 1, 2 )
            , checkTriplet model.board ( 3, 4, 5 )
            , checkTriplet model.board ( 6, 7, 8 )
            , checkTriplet model.board ( 0, 3, 6 )
            , checkTriplet model.board ( 1, 4, 7 )
            , checkTriplet model.board ( 2, 5, 8 )
            , checkTriplet model.board ( 0, 4, 8 )
            , checkTriplet model.board ( 2, 4, 6 )
            ]
    in
    if List.any (\x -> x == True) res then
        if model.current == X then
            OWin

        else
            XWin

    else if List.all (\x -> x /= Empty) model.board then
        Draw

    else
        Ongoing


updateBoard : Model -> Int -> Model
updateBoard model n =
    if cellByIndex model.board n == Just Empty then
        { model
            | board =
                List.indexedMap
                    (\i prev ->
                        if n == i then
                            model.current

                        else
                            prev
                    )
                    model.board
            , current =
                if model.current == X then
                    O

                else
                    X
        }

    else
        model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        NewGame ->
            init

        Turn n ->
            let
                updated =
                    updateBoard model n
            in
            { model
                | board = updated.board
                , current = updated.current
                , result = checkBoard updated
            }



-- VIEW


renderIcon : String -> String -> Html msg
renderIcon symbol vb =
    i [ class "icon" ] [ svg [ viewBox vb ] [ use [ xlinkHref symbol ] [] ] ]


renderIconVoid : Html msg
renderIconVoid =
    i [] [ text " " ]


renderTurnIcon : List Cell -> Int -> Html msg
renderTurnIcon b idx =
    case cellByIndex b idx of
        Nothing ->
            renderIconVoid

        Just t ->
            case t of
                X ->
                    renderIcon "#X" "0 0 24 24"

                O ->
                    renderIcon "#O" "0 0 24 24"

                Empty ->
                    renderIconVoid


renderCell : Model -> Int -> Html Msg
renderCell model i =
    button
        [ class "cell"
        , onClick
            (if model.result == Ongoing then
                Turn i

             else
                Noop
            )
        ]
        [ renderTurnIcon model.board i ]


renderBoard : Model -> Html Msg
renderBoard model =
    section [ class "board" ]
        (List.map
            (renderCell model)
            (List.range 0 8)
        )


renderGameOver : String -> Html Msg
renderGameOver m =
    div [ class "game-result" ]
        [ p [] [ text m ]
        , button [ class "newgame", onClick NewGame ] [ text "New game" ]
        ]


renderGameResult : Model -> Html Msg
renderGameResult model =
    case model.result of
        XWin ->
            renderGameOver "Player X wins the game!"

        OWin ->
            renderGameOver "Player O wins the game!"

        Draw ->
            renderGameOver "It's a draw."

        Ongoing ->
            Html.text ""


view model =
    section [] [ renderGameResult model, renderBoard model ]
