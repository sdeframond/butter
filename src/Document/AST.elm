module Document.AST exposing
    ( AST(..)
    , Error(..)
    , FormulaAST(..)
    , parseCell
    , parseName
    , renameSheets
    , toString
    )

import Parser as P exposing ((|.), (|=), Parser, backtrackable, end, int, lazy, map, oneOf, spaces, succeed, symbol, variable)
import Result as R
import Set
import String as S
import Tuple as T


type AST
    = RootLiteral String
    | Formula FormulaAST


type FormulaAST
    = Plus FormulaAST FormulaAST
    | Minus FormulaAST FormulaAST
    | IntLiteral Int
    | StringLiteral String
    | RelativeReference String
    | AbsoluteReference String String


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
        Plus x y ->
            Plus (renameSheetsInFormula f x) (renameSheetsInFormula f y)

        Minus x y ->
            Minus (renameSheetsInFormula f x) (renameSheetsInFormula f y)

        IntLiteral _ ->
            ast

        StringLiteral _ ->
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

        RootLiteral s ->
            s


formulaToString : FormulaAST -> String
formulaToString ast =
    case ast of
        Plus a b ->
            formulaToString a ++ "+" ++ formulaToString b

        Minus a b ->
            formulaToString a ++ "-" ++ formulaToString b

        IntLiteral i ->
            S.fromInt i

        StringLiteral s ->
            "\"" ++ s ++ "\""

        RelativeReference ref ->
            ref

        AbsoluteReference sheet ref ->
            sheet ++ "." ++ ref


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
        , map RootLiteral var
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
            [ map IntLiteral myInt
            , succeed StringLiteral
                |. symbol "\""
                |= variable { start = \c -> True, inner = \c -> c /= '"', reserved = Set.empty }
                |. symbol "\""
            , reference
            ]
        |. spaces


expression : Parser FormulaAST
expression =
    term |> P.andThen (expressionHelp [])


expressionHelp : List ( Operator, FormulaAST ) -> FormulaAST -> Parser FormulaAST
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


finalize : List ( Operator, FormulaAST ) -> FormulaAST -> FormulaAST
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( PlusOp, expr ) :: previousOps ->
            Plus (finalize previousOps expr) finalExpr

        ( MinusOp, expr ) :: previousOps ->
            Minus (finalize previousOps expr) finalExpr
