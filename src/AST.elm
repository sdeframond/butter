module AST exposing
    ( AST(..)
    , BinaryOp(..)
    , Context
    , Error(..)
    , FormulaAST(..)
    , checkCycle
    , eval
    , parseCell
    , parseInt
    , parseName
    , updateReferences
    , toString
    )

import List as L
import Parser as P exposing ((|.), (|=), Parser, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set
import String as S
import Tuple as T
import Types exposing (Error(..), LocatedName, Name, Value(..), ValueOrError)


type AST
    = RootLiteral Value
    | Formula FormulaAST


type FormulaAST
    = BinOp BinaryOp FormulaAST FormulaAST
    | Literal Value
    | RelativeReference String
    | AbsoluteReference String String


type BinaryOp
    = PlusOp
    | MinusOp


type Error
    = Error String (List P.DeadEnd)


updateReferences : (String -> String) -> AST -> AST
updateReferences f ast =
    case ast of
        RootLiteral _ ->
            ast

        Formula formula ->
            Formula (updateReferencesInFormula f formula)


updateReferencesInFormula : (String -> String) -> FormulaAST -> FormulaAST
updateReferencesInFormula f ast =
    case ast of
        BinOp op x y ->
            BinOp op (updateReferencesInFormula f x) (updateReferencesInFormula f y)

        Literal _ ->
            ast

        RelativeReference _ ->
            ast

        AbsoluteReference sheet cell ->
            AbsoluteReference (f sheet) cell


toString : AST -> String
toString ast =
    case ast of
        Formula ast_ ->
            "=" ++ formulaToString ast_

        RootLiteral (StringValue s) ->
            s

        RootLiteral (IntValue i) ->
            S.fromInt i


formulaToString : FormulaAST -> String
formulaToString ast =
    case ast of
        BinOp op a b ->
            formulaToString a ++ binaryOpToString op ++ formulaToString b

        Literal (IntValue i) ->
            S.fromInt i

        Literal (StringValue s) ->
            "\"" ++ s ++ "\""

        RelativeReference ref ->
            ref

        AbsoluteReference sheet ref ->
            sheet ++ "." ++ ref


binaryOpToString : BinaryOp -> String
binaryOpToString op =
    case op of
        PlusOp ->
            "+"

        MinusOp ->
            "-"


parseCell : String -> Result Error AST
parseCell s =
    P.run root s |> R.mapError (Error s)


parseName : String -> Result Error String
parseName s =
    P.run (name |. end) s |> R.mapError (Error s)


parseInt : String -> Result Error Int
parseInt str =
    P.run (int |. end) str |> R.mapError (Error str)


root : Parser AST
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
        , map (RootLiteral << StringValue) var
        ]


name : Parser String
name =
    variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


reference : Parser FormulaAST
reference =
    succeed identity
        |= name
        |> P.andThen referenceHelp


referenceHelp : String -> Parser FormulaAST
referenceHelp str =
    oneOf
        [ succeed (AbsoluteReference str)
            |. symbol "."
            |= name
        , succeed (RelativeReference str)
        ]


int : Parser Int
int =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= P.int
        , P.int
        ]


term : Parser FormulaAST
term =
    succeed identity
        |= oneOf
            [ reference
            , map (Literal << IntValue) int
            , succeed (Literal << StringValue)
                |. symbol "\""
                |= variable { start = always True, inner = \c -> c /= '"', reserved = Set.empty }
                |. symbol "\""
            ]
        |. spaces


expression : Parser FormulaAST
expression =
    term |> P.andThen (expressionHelp [])


expressionHelp : List ( BinaryOp, FormulaAST ) -> FormulaAST -> Parser FormulaAST
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


finalize : List ( BinaryOp, FormulaAST ) -> FormulaAST -> FormulaAST
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( op, expr ) :: previousOps ->
            BinOp op (finalize previousOps expr) finalExpr


type alias Context =
    { resolveAbsolute : LocatedName -> ValueOrError
    , resolveRelative : Name -> ValueOrError
    }


checkCycle : LocatedName -> List LocatedName -> (() -> ValueOrError) -> ValueOrError
checkCycle path ancestors doEval =
    if L.member path ancestors then
        Err (CyclicReferenceError ancestors)

    else
        doEval ()


eval : Context -> AST -> ValueOrError
eval context ast =
    case ast of
        RootLiteral v ->
            Ok v

        Formula formulaAst ->
            evalFormula context formulaAst


evalFormula : Context -> FormulaAST -> ValueOrError
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
        Literal v ->
            Ok v

        BinOp op x y ->
            case op of
                PlusOp ->
                    intBinaryOperator (+) "(+) works only on IntValue" x y

                MinusOp ->
                    intBinaryOperator (-) "(-) works only on IntValue" x y

        RelativeReference cellName ->
            context.resolveRelative cellName

        AbsoluteReference sheetName cellName ->
            context.resolveAbsolute ( sheetName, cellName )
