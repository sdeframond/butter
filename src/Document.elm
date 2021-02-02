module Document exposing
    ( Document
    , cellSource
    , fromList
    , get
    , insert
    , insertSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheets
    , singleSheet
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict as D exposing (Dict)
import Document.AST as AST exposing (AST(..), FormulaAST(..), parseName)
import Document.Cell as Cell exposing (Cell)
import Document.Types exposing (..)
import List as L
import Maybe as M
import Result as R
import Set exposing (Set)
import Tuple as T


type Document
    = Document DocData


type alias DocData =
    { cells : Dict LocatedName Cell
    , sheetsBefore : List Name
    , currentSheet : Name
    , sheetsAfter : List Name
    }


singleSheet : Name -> Document
singleSheet name =
    Document
        { cells = D.empty
        , currentSheet = name
        , sheetsBefore = []
        , sheetsAfter = []
        }


sheets : Document -> List (Position Name)
sheets (Document { sheetsBefore, currentSheet, sheetsAfter }) =
    L.concat
        [ L.map Before sheetsBefore
        , [ Current currentSheet ]
        , L.map After sheetsAfter
        ]


selectSheet : Name -> Document -> Result Error Document
selectSheet selectedName (Document data) =
    let
        empty =
            { data
                | currentSheet = selectedName
                , sheetsBefore = []
                , sheetsAfter = []
            }

        process sheet ( found, d ) =
            if sheet == selectedName then
                ( True, d )

            else
                ( found
                , if found then
                    { d | sheetsAfter = sheet :: d.sheetsAfter }

                  else
                    { d | sheetsBefore = sheet :: d.sheetsBefore }
                )

        ( foundBefore, processedBefore ) =
            L.foldl process ( False, empty ) data.sheetsBefore

        ( foundSheet, processed ) =
            L.foldl process
                ( foundBefore, processedBefore )
                (data.currentSheet :: data.sheetsAfter)
    in
    if foundSheet then
        Ok (Document processed)

    else
        Err (UndefinedSheetError selectedName)


sheetExists : Name -> DocData -> Bool
sheetExists name data =
    L.member name data.sheetsBefore
        || L.member name data.sheetsAfter
        || (name == data.currentSheet)


insertSheet : Name -> Document -> Result Error Document
insertSheet name (Document data) =
    if sheetExists name data then
        Err (DuplicateSheetNameError name)

    else
        Ok <|
            Document
                { data
                    | sheetsAfter = L.append data.sheetsAfter [ name ]
                }


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    if name == d.currentSheet then
        case ( d.sheetsBefore, d.sheetsAfter ) of
            ( _, head :: tail ) ->
                Ok <| Document { d | currentSheet = head, sheetsAfter = tail }

            ( head :: tail, _ ) ->
                Ok <| Document { d | currentSheet = head, sheetsBefore = tail }

            _ ->
                Err (RemovingLastSheetError name)

    else if sheetExists name d then
        Ok <|
            Document
                { d
                    | sheetsBefore = L.filter ((/=) name) d.sheetsBefore
                    , sheetsAfter = L.filter ((/=) name) d.sheetsAfter
                    , cells =
                        d.cells
                            |> D.filter (\( sheet, _ ) _ -> sheet /= name)
                }

    else
        Err (UndefinedSheetError name)


renameSheet : Name -> Name -> Document -> Result Error Document
renameSheet name newName (Document data) =
    let
        mapSheets f d =
            let
                renameCells ( sheetName, cellName ) cell renamed =
                    D.insert ( f sheetName, cellName ) (Cell.renameSheets f cell) renamed
            in
            { d
                | currentSheet = f d.currentSheet
                , sheetsBefore = L.map f d.sheetsBefore
                , sheetsAfter = L.map f d.sheetsAfter
                , cells = D.foldr renameCells D.empty d.cells
            }
    in
    if sheetExists name data then
        if name == newName then
            Ok (Document data)

        else if sheetExists newName data then
            Err (DuplicateSheetNameError newName)

        else
            parseName newName
                |> R.mapError (always InvalidSheetNameError)
                |> R.map
                    (\n ->
                        Document <|
                            mapSheets
                                (\sheet ->
                                    if sheet == name then
                                        n

                                    else
                                        sheet
                                )
                                data
                    )

    else
        Err <|
            UndefinedSheetError name


insert : Name -> String -> Document -> Document
insert cellName value (Document d) =
    case value of
        "" ->
            Document { d | cells = D.remove ( d.currentSheet, cellName ) d.cells }

        _ ->
            Document
                { d
                    | cells =
                        D.insert ( d.currentSheet, cellName )
                            (Cell.fromSource value)
                            d.cells
                }


fromList : Name -> List ( String, String ) -> Document
fromList sheet pairs =
    List.foldl (\( a, b ) -> insert a b) (singleSheet sheet) pairs


getCell : Name -> Name -> DocData -> Result Error Cell
getCell sheetName cellName data =
    if sheetExists sheetName data then
        D.get ( sheetName, cellName ) data.cells
            |> R.fromMaybe (UndefinedNameError ( sheetName, cellName ))

    else
        Err <| UndefinedSheetError sheetName


cellSource : Name -> Document -> Result Error String
cellSource cellName (Document d) =
    getCell d.currentSheet cellName d |> R.map Cell.source


get : Name -> Document -> ValueOrError
get name (Document d) =
    evalCell ( d.currentSheet, name ) D.empty d |> T.first


type alias Memo =
    Dict LocatedName ValueOrError


evalCell : LocatedName -> Memo -> DocData -> ( ValueOrError, Memo )
evalCell name memo data =
    evalHelp [] name memo data


evalHelp : List LocatedName -> LocatedName -> Memo -> DocData -> ( ValueOrError, Memo )
evalHelp ancestors name memo_ data =
    let
        intBinaryOperator f memo errMsg x y =
            let
                ( xRes, xMemo ) =
                    evalFormulaAst memo x

                ( yRes, yMemo ) =
                    evalFormulaAst xMemo y

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
                Formula x ->
                    evalFormulaAst memo x

                RootLiteral s ->
                    ( Ok <| StringValue s, memo )

        evalFormulaAst : Memo -> FormulaAST -> ( ValueOrError, Memo )
        evalFormulaAst memo ast =
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
                    evalHelp (name :: ancestors) ( T.first name, cellName ) memo data

                AbsoluteReference sheetName cellName ->
                    evalHelp (name :: ancestors) ( sheetName, cellName ) memo data

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
                getCell (T.first name) (T.second name) data
                    |> R.andThen Cell.parsed
                    |> R.map (evalAst memo_)
                    |> (\ast_ ->
                            case ast_ of
                                Err e ->
                                    ( Err e, memo_ )

                                Ok v ->
                                    v
                       )
                    |> memoize
