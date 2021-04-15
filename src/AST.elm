module AST exposing
    ( AST
    , Context
    , checkCycle
    , eval
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


type AST sheetRefType
    = RootLiteral Literal
    | Formula (FormulaAST sheetRefType)


type FormulaAST sheetRefType
    = BinOp BinaryOp (FormulaAST sheetRefType) (FormulaAST sheetRefType)
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


mapSheetReferences : (sheetRefType1 -> sheetRefType2) -> AST sheetRefType1 -> AST sheetRefType2
mapSheetReferences f ast =
    case ast of
        RootLiteral lit ->
            RootLiteral lit

        Formula formula ->
            Formula (mapSheetReferencesInFormula f formula)


mapSheetReferencesInFormula : (sheetRefType1 -> sheetRefType2) -> FormulaAST sheetRefType1 -> FormulaAST sheetRefType2
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


toString : AST (Maybe String) -> Maybe String
toString ast =
    case ast of
        Formula ast_ ->
            formulaToString ast_ |> Maybe.map ((++) "=")

        RootLiteral (StringLiteral s) ->
            Just s

        RootLiteral (IntLiteral i) ->
            Just (S.fromInt i)


formulaToString : FormulaAST (Maybe String) -> Maybe String
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


parseCell : String -> Result Error (AST String)
parseCell s =
    P.run root s |> R.mapError (Error s)


parseName : String -> Result Error String
parseName s =
    P.run (name |. end) s |> R.mapError (Error s)


parseInt : String -> Result Error Int
parseInt str =
    P.run (int |. end) str |> R.mapError (Error str)


root : Parser (AST String)
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


reference : Parser (FormulaAST String)
reference =
    succeed identity
        |= name
        |> P.andThen referenceHelp


referenceHelp : String -> Parser (FormulaAST String)
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


term : Parser (FormulaAST String)
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


expression : Parser (FormulaAST String)
expression =
    term |> P.andThen (expressionHelp [])


expressionHelp : List ( BinaryOp, FormulaAST String ) -> FormulaAST String -> Parser (FormulaAST String)
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


finalize : List ( BinaryOp, FormulaAST String ) -> FormulaAST String -> FormulaAST String
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( op, expr ) :: previousOps ->
            BinOp op (finalize previousOps expr) finalExpr


type alias Context sheetRefType =
    { resolveGlobalReference : ( sheetRefType, Name ) -> ValueOrError
    , resolveLocalReference : Name -> ValueOrError
    }


checkCycle : Types.LocatedName -> List Types.LocatedName -> (() -> ValueOrError) -> ValueOrError
checkCycle path ancestors doEval =
    if L.member path ancestors then
        Err (CyclicReferenceError ancestors)

    else
        doEval ()


eval : Context sheetRefType -> AST sheetRefType -> ValueOrError
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


evalFormula : Context sheetRefType -> FormulaAST sheetRefType -> ValueOrError
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
            context.resolveLocalReference cellName

        GlobalReference sheetName cellName ->
            context.resolveGlobalReference ( sheetName, cellName )
