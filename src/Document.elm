module Document exposing
    ( Document
    , Error(..)
    , Name
    , Value(..)
    , ValueOrError
    , empty
    , fromList
    , get
    , insert
    , insertSheet
    , removeSheet
    , sheets
    , singleSheet
    , source
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict as D exposing (Dict)
import List as L
import Maybe as M
import OrderedDict as OD exposing (OrderedDict)
import Parser as P exposing ((|.), (|=), Parser, backtrackable, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set exposing (Set)
import Tuple as T
import Types exposing (..)


type AST
    = Plus AST AST
    | Minus AST AST
    | IntLiteral Int
    | StringLiteral String
    | RelativeReference Name
    | AbsoluteReference Name Name


type Error
    = ParsingError (List P.DeadEnd)
    | UndefinedNameError LocatedName
    | UndefinedIdError Id
    | TypeError String
    | CyclicReferenceError (List LocatedName)


type alias SheetId =
    Int


type alias Name =
    String


type alias LocatedName =
    ( Name, Name )


type Value
    = IntValue Int
    | StringValue String


type alias ValueOrError =
    Result Error Value


type Document
    = Document
        { cells : Dict ( SheetId, Name ) String
        , sheetIds : OrderedDict Name SheetId
        , serial : Serial
        }


type Serial
    = Serial SheetId


next : Serial -> ( Id, Serial )
next (Serial id) =
    ( id, Serial <| id + 1 )


empty : Document
empty =
    let
        ( sheetId, serial ) =
            next (Serial 1)
    in
    Document
        { cells = D.empty
        , sheetIds = OD.empty
        , serial = serial
        }


singleSheet : Name -> Document
singleSheet name =
    empty |> insertSheet name


sheets : Document -> List Name
sheets (Document { sheetIds }) =
    OD.keys sheetIds


insertSheet : Name -> Document -> Document
insertSheet name doc =
    let
        ( _, newDoc ) =
            findOrCreateSheet name doc
    in
    newDoc


findOrCreateSheet : Name -> Document -> ( SheetId, Document )
findOrCreateSheet name (Document d) =
    let
        ( id, newSerial ) =
            case OD.get name d.sheetIds of
                Just id_ ->
                    ( id_, d.serial )

                Nothing ->
                    next d.serial
    in
    ( id
    , Document
        { d
            | serial = newSerial
            , sheetIds = OD.insert name id d.sheetIds
        }
    )


removeSheet : Name -> Document -> Document
removeSheet name (Document d) =
    case OD.get name d.sheetIds of
        Nothing ->
            Document d

        Just id ->
            Document
                { d
                    | sheetIds = OD.remove name d.sheetIds
                    , cells = d.cells |> D.filter (\( id_, _ ) _ -> id /= id_)
                }


insert : Name -> Name -> String -> Document -> Document
insert sheetName cellName value doc =
    let
        ( id, Document d ) =
            findOrCreateSheet sheetName doc
    in
    case value of
        "" ->
            remove sheetName cellName doc

        _ ->
            Document
                { d | cells = D.insert ( id, cellName ) value d.cells }


remove : Name -> Name -> Document -> Document
remove sheetName cellName doc =
    let
        (Document d) =
            doc
    in
    case OD.get sheetName d.sheetIds of
        Nothing ->
            doc

        Just id ->
            Document { d | cells = D.remove ( id, cellName ) d.cells }


fromList : Name -> List ( String, String ) -> Document
fromList sheet pairs =
    List.foldl (\( a, b ) -> insert sheet a b) (singleSheet sheet) pairs


source : Name -> Name -> Document -> Maybe String
source sheetName cellName (Document { cells, sheetIds }) =
    OD.get sheetName sheetIds
        |> M.andThen (\id -> D.get ( id, cellName ) cells)


type alias Memo =
    Dict LocatedName ValueOrError



--names : Document -> List LocatedName
--names (Document { cells }) =
--    D.keys cells
--eval : Document -> Dict LocatedName ValueOrError
--eval doc =
--    List.foldl
--        (\name memo -> T.second <| evalCell name memo doc)
--        D.empty
--        (names doc)


get : Name -> Name -> Document -> ValueOrError
get sheet name doc =
    evalCell ( sheet, name ) D.empty doc |> T.first


evalCell : LocatedName -> Memo -> Document -> ( ValueOrError, Memo )
evalCell name memo doc =
    evalHelp [] name memo doc


evalHelp : List LocatedName -> LocatedName -> Memo -> Document -> ( ValueOrError, Memo )
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

                RelativeReference cellName ->
                    evalHelp (name :: ancestors) ( T.first name, cellName ) memo doc

                AbsoluteReference sheetName cellName ->
                    evalHelp (name :: ancestors) ( sheetName, cellName ) memo doc

        (Document { cells }) =
            doc

        memoize ( v, m ) =
            ( v, D.insert name v m )
    in
    if List.member name ancestors then
        ( Err <| CyclicReferenceError ancestors, memo_ )

    else
        case D.get name memo_ of
            Just v ->
                ( v, memo_ )

            Nothing ->
                source (T.first name) (T.second name) doc
                    |> R.fromMaybe (UndefinedNameError name)
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
    P.run root s |> R.mapError ParsingError


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


nameParser =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


reference : Parser AST
reference =
    succeed identity
        |= nameParser
        |> P.andThen referenceHelp


referenceHelp : String -> Parser AST
referenceHelp name =
    oneOf
        [ succeed (AbsoluteReference name)
            |. symbol "."
            |= nameParser
        , succeed (RelativeReference name)
        ]


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
    term |> P.andThen (expressionHelp [])


expressionHelp : List ( Operator, AST ) -> AST -> Parser AST
expressionHelp reversedOps expr =
    oneOf
        [ succeed T.pair
            |. spaces
            |= operator
            |. spaces
            |= term
            |> P.andThen (\( op, newExpr ) -> expressionHelp (( op, expr ) :: reversedOps) newExpr)
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
