module Document exposing
    ( Document
    , Sheet(..)
    , cellSource
    , fromList
    , get
    , gridSheetItem
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
import Document.AST as AST
    exposing
        ( AST(..)
        , BinaryOp(..)
        , FormulaAST(..)
        , parseName
        )
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
    , sheetsBefore : List SheetItem
    , currentSheet : SheetItem
    , sheetsAfter : List SheetItem
    }


type alias SheetItem =
    { name : String
    , sheet : Sheet
    }


type Sheet
    = GridSheet
    | TableSheet


gridSheetItem name =
    { name = name, sheet = GridSheet }


singleSheet : Name -> Document
singleSheet name =
    Document
        { cells = D.empty
        , currentSheet = { name = name, sheet = GridSheet }
        , sheetsBefore = []
        , sheetsAfter = []
        }


sheets : Document -> List (Position SheetItem)
sheets (Document { sheetsBefore, currentSheet, sheetsAfter }) =
    L.concat
        [ L.map Before sheetsBefore
        , [ Current currentSheet ]
        , L.map After sheetsAfter
        ]


selectSheet : Name -> Document -> Result Error Document
selectSheet selectedName (Document data) =
    let
        process sheet ( before, current, after ) =
            if sheet.name == selectedName then
                ( before, Just sheet, after )

            else
                case current of
                    Just _ ->
                        ( before, current, L.append after [ sheet ] )

                    Nothing ->
                        ( L.append before [ sheet ], current, after )

        ( newBefore, newCurrent, newAfter ) =
            L.foldl process
                (L.foldl process ( [], Nothing, [] ) data.sheetsBefore)
                (data.currentSheet :: data.sheetsAfter)
    in
    case newCurrent of
        Just current ->
            Ok
                (Document
                    { data
                        | currentSheet = current
                        , sheetsBefore = newBefore
                        , sheetsAfter = newAfter
                    }
                )

        Nothing ->
            Err (UndefinedSheetError selectedName)


sheetExists : Name -> DocData -> Bool
sheetExists name data =
    L.member name (L.map .name data.sheetsBefore)
        || L.member name (L.map .name data.sheetsAfter)
        || (name == data.currentSheet.name)


insertSheet : Name -> Document -> Result Error Document
insertSheet name (Document data) =
    if sheetExists name data then
        Err (DuplicateSheetNameError name)

    else
        Ok <|
            Document
                { data
                    | sheetsAfter = L.append data.sheetsAfter [ gridSheetItem name ]
                }


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    if name == d.currentSheet.name then
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
                    | sheetsBefore = L.filter (.name >> (/=) name) d.sheetsBefore
                    , sheetsAfter = L.filter (.name >> (/=) name) d.sheetsAfter
                    , cells =
                        d.cells
                            |> D.filter (\( sheet, _ ) _ -> sheet /= name)
                }

    else
        Err (UndefinedSheetError name)


renameSheet : Name -> Name -> Document -> Result Error Document
renameSheet name newName (Document data) =
    let
        updateName f item =
            { item | name = f item.name }

        mapSheetNames f d =
            let
                renameCells ( sheetName, cellName ) cell renamed =
                    D.insert ( f sheetName, cellName ) (Cell.renameSheets f cell) renamed
            in
            { d
                | currentSheet = updateName f d.currentSheet
                , sheetsBefore = L.map (updateName f) d.sheetsBefore
                , sheetsAfter = L.map (updateName f) d.sheetsAfter
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
                    (\parsedName ->
                        Document <|
                            mapSheetNames
                                (\currentName ->
                                    if currentName == name then
                                        parsedName

                                    else
                                        currentName
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
            Document { d | cells = D.remove ( d.currentSheet.name, cellName ) d.cells }

        _ ->
            Document
                { d
                    | cells =
                        D.insert ( d.currentSheet.name, cellName )
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
    getCell d.currentSheet.name cellName d |> R.map Cell.source


get : Name -> Document -> ValueOrError
get name (Document d) =
    evalCell ( d.currentSheet.name, name ) D.empty d |> T.first


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

                RootLiteral v ->
                    ( Ok v, memo )

        evalFormulaAst : Memo -> FormulaAST -> ( ValueOrError, Memo )
        evalFormulaAst memo ast =
            case ast of
                Literal v ->
                    ( Ok v, memo )

                BinOp op x y ->
                    case op of
                        PlusOp ->
                            intBinaryOperator (+) memo "(+) works only on IntValue" x y

                        MinusOp ->
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
