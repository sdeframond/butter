module Document exposing
    ( Document
    , Error(..)
    , Name
    , Value(..)
    , ValueOrError
    , empty
    , eval
    , evalAll
    , fromList
    , insert
    , source
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict as D exposing (Dict)
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
    | TypeError String
    | NameNotFoundForId Id
    | CyclicReferenceError (List Name)


type alias Id =
    Int


type alias Name =
    String


type Value
    = IntValue Int
    | StringValue String


type alias ValueOrError =
    Result Error Value


type Document
    = Document
        { namesIdx : Dict Name Id
        , sources : Dict Id String
        , serial : Serial
        }


type Serial
    = Serial Id


next : Serial -> ( Id, Serial )
next (Serial id) =
    ( id, Serial <| id + 1 )


empty : Document
empty =
    Document
        { namesIdx = D.empty
        , sources = D.empty
        , serial = Serial 0
        }


insert : String -> String -> Document -> Document
insert name value doc =
    let
        (Document { sources, namesIdx, serial }) =
            doc
    in
    case value of
        "" ->
            remove name doc

        _ ->
            let
                ( id, newSerial ) =
                    case D.get name namesIdx of
                        Nothing ->
                            next serial

                        Just id_ ->
                            ( id_, serial )
            in
            Document
                { namesIdx = D.insert name id namesIdx
                , sources = D.insert id value sources
                , serial = newSerial
                }


remove : String -> Document -> Document
remove name doc =
    let
        (Document dict) =
            doc
    in
    case D.get name dict.namesIdx of
        Nothing ->
            doc

        Just id ->
            Document
                { dict
                    | sources = D.remove id dict.sources
                    , namesIdx = D.remove name dict.namesIdx
                }


fromList : List ( String, String ) -> Document
fromList pairs =
    List.foldl (\( a, b ) -> insert a b) empty pairs


source : Name -> Document -> Maybe String
source name (Document { sources, namesIdx }) =
    D.get name namesIdx
        |> M.andThen (\id -> D.get id sources)


type alias Memo =
    Dict Name ValueOrError


names : Document -> List Name
names (Document { namesIdx }) =
    D.keys namesIdx


evalAll : Document -> Dict Name ValueOrError
evalAll doc =
    List.foldl
        (\name memo -> Tuple.second <| eval name memo doc)
        D.empty
        (names doc)


eval : Name -> Memo -> Document -> ( ValueOrError, Memo )
eval name memo doc =
    evalHelp [] name memo doc


evalHelp : List Name -> Name -> Memo -> Document -> ( ValueOrError, Memo )
evalHelp ancestors name memo_ doc =
    let
        intBinaryOperator f memo errMsg x y =
            let
                ( xRes, xMemo ) =
                    evalAst memo x

                ( yRes, yMemo ) =
                    evalAst xMemo y

                applyOp xVal yVal =
                    case ( xVal, yVal ) of
                        ( IntValue i, IntValue j ) ->
                            Ok <| IntValue (f i j)

                        _ ->
                            Err <| TypeError errMsg

                res =
                    R.andThen (\xx -> R.andThen (\yy -> applyOp xx yy) yRes) xRes
            in
            ( res, yMemo )

        evalAst : Memo -> AST -> ( ValueOrError, Memo )
        evalAst memo ast =
            case ast of
                IntLiteral i ->
                    ( Ok <| IntValue i, memo )

                StringLiteral s ->
                    ( Ok <| StringValue s, memo )

                Plus x y ->
                    intBinaryOperator (+) memo "(+) works only on IntValue" x y

                Minus x y ->
                    intBinaryOperator (-) memo "(-) works only on IntValue" x y

                Reference refName ->
                    evalHelp (name :: ancestors) refName memo doc

        (Document { namesIdx, sources }) =
            doc

        memoize ( v, m ) =
            ( v, D.insert name v m )

        getSource id =
            D.get id sources |> R.fromMaybe (UndefinedIdError id)
    in
    if List.member name ancestors then
        ( Err <| CyclicReferenceError ancestors, memo_ )

    else
        case D.get name memo_ of
            Just v ->
                ( v, memo_ )

            Nothing ->
                D.get name namesIdx
                    |> R.fromMaybe (UndefinedNameError name)
                    |> R.andThen getSource
                    |> R.andThen parse
                    |> R.map (evalAst memo_)
                    |> (\ast_ ->
                            case ast_ of
                                Err e ->
                                    ( Err e, memo_ )

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


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


term : Parser AST
term =
    succeed identity
        |= oneOf
            [ map IntLiteral myInt
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
