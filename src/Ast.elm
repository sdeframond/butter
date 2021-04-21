module Ast exposing
    ( Ast
    , Context
    , eval
    , filterMapReferences
    , mapSheetReferences
    , parseCell
    , parseInt
    , parseName
    , toString
    )

import List as L
import Parser as P exposing ((|.), (|=), Parser, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set
import String as S
import Tuple as T
import Types exposing (Error(..), Name, Value(..), ValueOrError)



-- MODEL


type Ast sheetRefType
    = RootLiteral Literal
    | Formula (FormulaAst sheetRefType)


type FormulaAst sheetRefType
    = BinOp BinaryOp (FormulaAst sheetRefType) (FormulaAst sheetRefType)
    | FormulaLiteral Literal
    | LocalReference String
    | GlobalReference sheetRefType String


type Literal
    = IntLiteral Int
    | StringLiteral String


type BinaryOp
    = PlusOp
    | MinusOp


type Error
    = Error String (List P.DeadEnd)



-- MAP


mapSheetReferences : (sheetRefType1 -> sheetRefType2) -> Ast sheetRefType1 -> Ast sheetRefType2
mapSheetReferences f ast =
    case ast of
        RootLiteral lit ->
            RootLiteral lit

        Formula formula ->
            Formula (mapSheetReferencesInFormula f formula)


mapSheetReferencesInFormula : (sheetRefType1 -> sheetRefType2) -> FormulaAst sheetRefType1 -> FormulaAst sheetRefType2
mapSheetReferencesInFormula f ast =
    case ast of
        BinOp op x y ->
            BinOp op (mapSheetReferencesInFormula f x) (mapSheetReferencesInFormula f y)

        FormulaLiteral lit ->
            FormulaLiteral lit

        LocalReference cellRef ->
            LocalReference cellRef

        GlobalReference sheetRef cellRef ->
            GlobalReference (f sheetRef) cellRef


filterMapReferences : (sheetRefType1 -> Result x sheetRefType2) -> Ast sheetRefType1 -> Result x (Ast sheetRefType2)
filterMapReferences fn ast =
    case ast of
        RootLiteral lit ->
            Ok (RootLiteral lit)

        Formula formula ->
            filterMapReferencesInFormula fn formula |> Result.map Formula


filterMapReferencesInFormula : (sheetRefType1 -> Result x sheetRefType2) -> FormulaAst sheetRefType1 -> Result x (FormulaAst sheetRefType2)
filterMapReferencesInFormula fn ast =
    case ast of
        BinOp op x y ->
            Result.map2 (BinOp op) (filterMapReferencesInFormula fn x) (filterMapReferencesInFormula fn y)

        FormulaLiteral lit ->
            Ok (FormulaLiteral lit)

        LocalReference cellRef ->
            Ok (LocalReference cellRef)

        GlobalReference sheetRef cellRef ->
            fn sheetRef
                |> Result.map (\newRef -> GlobalReference newRef cellRef)



-- TO STRING


toString : Ast (Maybe String) -> Maybe String
toString ast =
    case ast of
        Formula ast_ ->
            formulaToString ast_ |> Maybe.map ((++) "=")

        RootLiteral (StringLiteral s) ->
            Just s

        RootLiteral (IntLiteral i) ->
            Just (S.fromInt i)


formulaToString : FormulaAst (Maybe String) -> Maybe String
formulaToString ast =
    case ast of
        BinOp op a b ->
            formulaToString a
                |> Maybe.map (\str -> str ++ binaryOpToString op)
                |> Maybe.andThen (\str -> formulaToString b |> Maybe.map ((++) str))

        FormulaLiteral (IntLiteral i) ->
            Just (S.fromInt i)

        FormulaLiteral (StringLiteral s) ->
            Just ("\"" ++ s ++ "\"")

        LocalReference ref ->
            Just ref

        GlobalReference maybeSheetRef cellRef ->
            maybeSheetRef
                |> Maybe.map (\str -> str ++ "." ++ cellRef)


binaryOpToString : BinaryOp -> String
binaryOpToString op =
    case op of
        PlusOp ->
            "+"

        MinusOp ->
            "-"



-- PARSE


parseCell : String -> Result Error (Ast String)
parseCell s =
    P.run root s |> R.mapError (Error s)


parseName : String -> Result Error String
parseName s =
    P.run (name |. end) s |> R.mapError (Error s)


parseInt : String -> Result Error Int
parseInt str =
    P.run (int |. end) str |> R.mapError (Error str)


root : Parser (Ast String)
root =
    let
        var =
            oneOf
                [ variable
                    { start = \c -> c /= '='
                    , inner = always True
                    , reserved = Set.empty
                    }
                , succeed ""
                ]
    in
    oneOf
        [ succeed Formula
            |. symbol "="
            |. spaces
            |= expression
            |. end
        , map (RootLiteral << StringLiteral) var
        ]


name : Parser String
name =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


reference : Parser (FormulaAst String)
reference =
    succeed identity
        |= name
        |> P.andThen referenceHelp


referenceHelp : String -> Parser (FormulaAst String)
referenceHelp str =
    oneOf
        [ succeed (GlobalReference str)
            |. symbol "."
            |= name
        , succeed (LocalReference str)
        ]


int : Parser Int
int =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= P.int
        , P.int
        ]


term : Parser (FormulaAst String)
term =
    succeed identity
        |= oneOf
            [ reference
            , map (FormulaLiteral << IntLiteral) int
            , succeed (FormulaLiteral << StringLiteral)
                |. symbol "\""
                |= variable { start = always True, inner = \c -> c /= '"', reserved = Set.empty }
                |. symbol "\""
            ]
        |. spaces


expression : Parser (FormulaAst String)
expression =
    term |> P.andThen (expressionHelp [])


expressionHelp : List ( BinaryOp, FormulaAst String ) -> FormulaAst String -> Parser (FormulaAst String)
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


operator : Parser BinaryOp
operator =
    oneOf
        [ map (\_ -> PlusOp) (symbol "+")
        , map (\_ -> MinusOp) (symbol "-")
        ]


finalize : List ( BinaryOp, FormulaAst String ) -> FormulaAst String -> FormulaAst String
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( op, expr ) :: previousOps ->
            BinOp op (finalize previousOps expr) finalExpr



-- EVAL


type alias Context sheetRefType =
    { resolveGlobalReference : ( sheetRefType, Name ) -> List ( sheetRefType, Name ) -> ValueOrError
    , resolveLocalReference : Name -> List ( sheetRefType, Name ) -> ValueOrError
    , prefix : sheetRefType
    , ancestors : List ( sheetRefType, Name )
    }


eval : Context Types.SheetId -> Ast Types.SheetId -> ValueOrError
eval context ast =
    case ast of
        RootLiteral lit ->
            Ok (evalLiteral lit)

        Formula formulaAst ->
            evalFormula context formulaAst


evalLiteral : Literal -> Value
evalLiteral lit =
    case lit of
        StringLiteral s ->
            StringValue s

        IntLiteral i ->
            IntValue i


evalFormula : Context Types.SheetId -> FormulaAst Types.SheetId -> ValueOrError
evalFormula context formulaAst =
    let
        intBinaryOperator op errMsg x y =
            let
                xRes =
                    evalFormula context x

                yRes =
                    evalFormula context y

                applyOp xVal yVal =
                    case ( xVal, yVal ) of
                        ( IntValue i, IntValue j ) ->
                            Ok <| IntValue (op i j)

                        _ ->
                            Err <| TypeError errMsg

                andThen2 f a b =
                    R.map2 f a b |> R.andThen identity
            in
            andThen2 applyOp xRes yRes

        checkCycle path ancestors doEval =
            if L.member path ancestors then
                Err (CyclicReferenceError ancestors)

            else
                doEval (path :: ancestors)
    in
    case formulaAst of
        FormulaLiteral lit ->
            Ok (evalLiteral lit)

        BinOp op x y ->
            case op of
                PlusOp ->
                    intBinaryOperator (+) "(+) works only on IntValue" x y

                MinusOp ->
                    intBinaryOperator (-) "(-) works only on IntValue" x y

        LocalReference cellName ->
            checkCycle ( context.prefix, cellName )
                context.ancestors
                (context.resolveLocalReference cellName)

        GlobalReference sheetRef cellName ->
            checkCycle ( sheetRef, cellName )
                context.ancestors
                (context.resolveGlobalReference ( sheetRef, cellName ))
