module Formula exposing
    ( Context
    , Formula
    , eval
    , fromSource
    , initialInput
    , isValid
    , parseInt
    , parseName
    , sourceView
    )

import Parser as P exposing ((|.), (|=))
import Set
import Types


type Formula
    = Formula UserInput ParsedAst


type alias ParsedAst =
    Result Types.Error (Ast (Result OriginalSheetName Types.SheetId))


type alias UserInput =
    String


type alias OriginalSheetName =
    String


isValid : Formula -> Bool
isValid (Formula _ astResult) =
    case astResult of
        Ok _ ->
            True

        Err _ ->
            False


initialInput : Formula -> UserInput
initialInput (Formula input _) =
    input


sourceView : (Types.SheetId -> Maybe Types.Name) -> Formula -> Maybe String
sourceView getSheetName (Formula originalSource astResult) =
    let
        referenceToString : Result OriginalSheetName Types.SheetId -> Maybe String
        referenceToString refResult =
            case refResult of
                Ok ref ->
                    getSheetName ref

                Err name ->
                    Just name

        astToString : Ast (Result OriginalSheetName Types.SheetId) -> Maybe String
        astToString ast =
            ast
                |> mapSheetReferences referenceToString
                |> toString
    in
    astResult
        |> Result.map astToString
        |> Result.withDefault (Just originalSource)


fromSource : (Types.Name -> Maybe Types.SheetId) -> UserInput -> Formula
fromSource getSheetId src =
    src
        |> parse
        |> Result.mapError (always Types.ParsingError)
        |> Result.map (mapSheetReferences (\sheetName -> getSheetId sheetName |> Result.fromMaybe sheetName))
        |> Formula src



-- EVAL


type alias Context sheetRefType =
    { resolveGlobalReference : ( sheetRefType, Types.Name ) -> List ( sheetRefType, Types.Name ) -> Types.ValueOrError
    , resolveLocalReference : Types.Name -> List ( sheetRefType, Types.Name ) -> Types.ValueOrError
    , prefix : sheetRefType
    , ancestors : List ( sheetRefType, Types.Name )
    }


eval : Context Types.SheetId -> Formula -> Types.ValueOrError
eval context (Formula _ astResult) =
    astResult
        |> Result.andThen (filterMapReferences (\r -> Result.mapError Types.UndefinedSheetError r))
        |> Result.andThen (evalAst context)


evalAst : Context Types.SheetId -> Ast Types.SheetId -> Types.ValueOrError
evalAst context formulaAst =
    let
        intBinaryOperator op errMsg x y =
            let
                xRes =
                    evalAst context x

                yRes =
                    evalAst context y

                applyOp xVal yVal =
                    case ( xVal, yVal ) of
                        ( Types.IntValue i, Types.IntValue j ) ->
                            Ok <| Types.IntValue (op i j)

                        _ ->
                            Err <| Types.TypeError errMsg

                andThen2 f a b =
                    Result.map2 f a b |> Result.andThen identity
            in
            andThen2 applyOp xRes yRes

        checkCycle path ancestors doEval =
            if List.member path ancestors then
                Err (Types.CyclicReferenceError ancestors)

            else
                doEval (path :: ancestors)
    in
    case formulaAst of
        FormulaLiteral lit ->
            Ok (evalLiteral lit)

        BinOp op x y ->
            case op of
                PlusOp ->
                    intBinaryOperator (+) "(+) works only on Types.IntValue" x y

                MinusOp ->
                    intBinaryOperator (-) "(-) works only on Types.IntValue" x y

        LocalReference cellName ->
            checkCycle ( context.prefix, cellName )
                context.ancestors
                (context.resolveLocalReference cellName)

        GlobalReference sheetRef cellName ->
            checkCycle ( sheetRef, cellName )
                context.ancestors
                (context.resolveGlobalReference ( sheetRef, cellName ))


evalLiteral : Literal -> Types.Value
evalLiteral lit =
    case lit of
        StringLiteral s ->
            Types.StringValue s

        IntLiteral i ->
            Types.IntValue i



-- AST


type Ast sheetRefType
    = BinOp BinaryOp (Ast sheetRefType) (Ast sheetRefType)
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
        BinOp op x y ->
            BinOp op (mapSheetReferences f x) (mapSheetReferences f y)

        FormulaLiteral lit ->
            FormulaLiteral lit

        LocalReference cellRef ->
            LocalReference cellRef

        GlobalReference sheetRef cellRef ->
            GlobalReference (f sheetRef) cellRef


filterMapReferences : (sheetRefType1 -> Result x sheetRefType2) -> Ast sheetRefType1 -> Result x (Ast sheetRefType2)
filterMapReferences fn ast =
    case ast of
        BinOp op x y ->
            Result.map2 (BinOp op) (filterMapReferences fn x) (filterMapReferences fn y)

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
        BinOp op a b ->
            toString a
                |> Maybe.map (\str -> str ++ binaryOpToString op)
                |> Maybe.andThen (\str -> toString b |> Maybe.map ((++) str))

        FormulaLiteral (IntLiteral i) ->
            Just (String.fromInt i)

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


parse : String -> Result Error (Ast String)
parse s =
    P.run (parser |. P.end) s |> Result.mapError (Error s)


parser : P.Parser (Ast String)
parser =
    term |> P.andThen (parserHelp [])


term : P.Parser (Ast String)
term =
    P.succeed identity
        |= P.oneOf
            [ referenceParser
            , P.map (FormulaLiteral << IntLiteral) intParser
            , P.succeed (FormulaLiteral << StringLiteral)
                |. P.symbol "\""
                |= P.variable { start = always True, inner = \c -> c /= '"', reserved = Set.empty }
                |. P.symbol "\""
            ]
        |. P.spaces


parserHelp : List ( BinaryOp, Ast String ) -> Ast String -> P.Parser (Ast String)
parserHelp reversedOps expr =
    P.oneOf
        [ P.succeed Tuple.pair
            |. P.spaces
            |= operatorParser
            |. P.spaces
            |= term
            |> P.andThen (\( op, newExpr ) -> parserHelp (( op, expr ) :: reversedOps) newExpr)
        , P.lazy (\_ -> P.succeed <| finalize reversedOps expr)
        ]


operatorParser : P.Parser BinaryOp
operatorParser =
    P.oneOf
        [ P.map (\_ -> PlusOp) (P.symbol "+")
        , P.map (\_ -> MinusOp) (P.symbol "-")
        ]


finalize : List ( BinaryOp, Ast String ) -> Ast String -> Ast String
finalize reversedOps finalExpr =
    case reversedOps of
        [] ->
            finalExpr

        ( op, expr ) :: previousOps ->
            BinOp op (finalize previousOps expr) finalExpr


nameParser : P.Parser String
nameParser =
    P.variable
        { start = Char.isAlpha
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


referenceParser : P.Parser (Ast String)
referenceParser =
    P.succeed identity
        |= nameParser
        |> P.andThen referenceHelp


referenceHelp : String -> P.Parser (Ast String)
referenceHelp str =
    P.oneOf
        [ P.succeed (GlobalReference str)
            |. P.symbol "."
            |= nameParser
        , P.succeed (LocalReference str)
        ]


intParser : P.Parser Int
intParser =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |. P.spaces
            |= P.int
        , P.int
        ]



-- EXPOSED PARSING HELPERS


parseName : String -> Result Error String
parseName s =
    let
        parser_ =
            P.succeed identity
                |. P.spaces
                |= nameParser
                |. P.spaces
                |. P.end
    in
    P.run parser_ s |> Result.mapError (Error s)


parseInt : String -> Result Error Int
parseInt str =
    let
        parser_ =
            P.succeed identity
                |. P.spaces
                |= intParser
                |. P.spaces
                |. P.end
    in
    P.run parser_ str |> Result.mapError (Error str)
