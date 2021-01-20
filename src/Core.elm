module Core exposing
    ( Document
    , Error(..)
    , ValueOrError
    , emptyDocument
    , eval
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict exposing (Dict)
import Maybe as M
import Parser exposing ((|.), (|=), Parser, andThen, backtrackable, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set exposing (Set)
import Types exposing (..)


type AST
    = Plus AST AST
    | Minus AST AST
    | IntLiteral Int
    | StringLiteral String
    | Reference String


type Error
    = ParsingError (List Parser.DeadEnd)
    | UndefinedNameError Name
    | UndefinedIdError Id
    | EvaluationError String
    | NameNotFoundForId Id
    | CyclicReferenceError (List Name)


type alias ValueOrError =
    Result Error Value


type alias Document =
    { namesIdx : Dict Name Id
    , sources : Dict Id String
    }


emptyDocument : Document
emptyDocument =
    { namesIdx = Dict.empty
    , sources = Dict.empty
    }


type alias Memo =
    Dict Name ValueOrError


eval : Name -> Memo -> Document -> ( ValueOrError, Memo )
eval name memo doc =
    let
        evalBinaryOp : Memo -> AST -> AST -> (ValueOrError -> ValueOrError -> ValueOrError) -> ( ValueOrError, Memo )
        evalBinaryOp memo_ x y f =
            let
                ( xVal, xMemo ) =
                    evalAst memo_ x

                ( yVal, yMemo ) =
                    evalAst xMemo y

                v =
                    f xVal yVal
            in
            ( v, yMemo )

        evalAst : Memo -> AST -> ( ValueOrError, Memo )
        evalAst memo_ ast =
            case ast of
                IntLiteral i ->
                    ( Ok <| IntValue i, memo_ )

                StringLiteral s ->
                    ( Ok <| StringValue s, memo_ )

                Plus x y ->
                    evalBinaryOp memo_
                        x
                        y
                        (\xVal yVal ->
                            case ( xVal, yVal ) of
                                ( Ok (IntValue i), Ok (IntValue j) ) ->
                                    Ok <| IntValue (i + j)

                                _ ->
                                    Err <| EvaluationError "(+) works only on IntValue"
                        )

                Minus x y ->
                    evalBinaryOp memo_
                        x
                        y
                        (\xVal yVal ->
                            case ( xVal, yVal ) of
                                ( Ok (IntValue i), Ok (IntValue j) ) ->
                                    Ok <| IntValue (i - j)

                                _ ->
                                    Err <| EvaluationError "(-) works only on IntValue"
                        )

                Reference refName ->
                    eval refName memo_ doc

        memoize ( v, memo_ ) =
            ( v, Dict.insert name v memo_ )

        getSource id =
            Dict.get id doc.sources |> R.fromMaybe (UndefinedIdError id)
    in
    case Dict.get name memo of
        Just v ->
            ( v, memo )

        Nothing ->
            Dict.get name doc.namesIdx
                |> R.fromMaybe (UndefinedNameError name)
                |> R.andThen getSource
                |> R.andThen parse
                |> R.map (evalAst memo)
                |> (\ast_ ->
                        case ast_ of
                            Err e ->
                                ( Err e, memo )

                            Ok v ->
                                v
                   )
                |> memoize


parse : String -> Result Error AST
parse s =
    Parser.run root s |> R.mapError ParsingError


root : Parser AST
root =
    let
        var =
            variable
                { start = \c -> c /= '='
                , inner = \c -> True
                , reserved = Set.empty
                }
    in
    oneOf
        [ succeed identity
            |. symbol "="
            |. spaces
            |= expression
            |. end
        , map StringLiteral var
        ]


reference : Parser AST
reference =
    succeed Reference
        |= variable
            { start = Char.isAlpha
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }


term : Parser AST
term =
    succeed identity
        |= oneOf
            [ map IntLiteral int
            , succeed StringLiteral
                |. symbol "\""
                |= variable { start = \c -> True, inner = \c -> c /= '"', reserved = Set.empty }
                |. symbol "\""
            , reference
            ]
        |. spaces


expression : Parser AST
expression =
    term |> andThen (expressionHelp [])


expressionHelp : List ( Operator, AST ) -> AST -> Parser AST
expressionHelp reversedOps expr =
    oneOf
        [ succeed Tuple.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> andThen (\( op, newExpr ) -> expressionHelp (( op, expr ) :: reversedOps) newExpr)
        , lazy (\_ -> succeed <| finalize reversedOps expr)
        ]


type Operator
    = PlusOp
    | MinusOp


operator : Parser Operator
operator =
    oneOf
        [ map (\_ -> PlusOp) (symbol "+")
        , map (\_ -> MinusOp) (symbol "-")
        ]


finalize : List ( Operator, AST ) -> AST -> AST
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( PlusOp, expr ) :: previousOps ->
            Plus (finalize previousOps expr) finalExpr

        ( MinusOp, expr ) :: previousOps ->
            Minus (finalize previousOps expr) finalExpr
