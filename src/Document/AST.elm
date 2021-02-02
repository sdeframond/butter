module Document.AST exposing
    ( AST(..)
    , BinaryOp(..)
    , Error(..)
    , FormulaAST(..)
    , parseCell
    , parseName
    , renameSheets
    , toString
    )

import Document.Types exposing (Value(..))
import Parser as P exposing ((|.), (|=), Parser, backtrackable, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set
import String as S
import Tuple as T


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


renameSheets : (String -> String) -> AST -> AST
renameSheets f ast =
    case ast of
        RootLiteral _ ->
            ast

        Formula formula ->
            Formula (renameSheetsInFormula f formula)


renameSheetsInFormula : (String -> String) -> FormulaAST -> FormulaAST
renameSheetsInFormula f ast =
    case ast of
        BinOp op x y ->
            BinOp op (renameSheetsInFormula f x) (renameSheetsInFormula f y)

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


myInt : Parser Int
myInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


term : Parser FormulaAST
term =
    succeed identity
        |= oneOf
            [ map (Literal << IntValue) myInt
            , succeed (Literal << StringValue)
                |. symbol "\""
                |= variable { start = \c -> True, inner = \c -> c /= '"', reserved = Set.empty }
                |. symbol "\""
            , reference
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
