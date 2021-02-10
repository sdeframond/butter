module Document exposing
    ( Document
    , Sheet(..)
    ,  cellSource
       --, commitEdit

    , currentSheet
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
    , tableSheetItem
    , updateTable
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
import Document.Table as Table exposing (Table)
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
    , sheetItemsBefore : List SheetItem
    , currentSheetItem : SheetItem
    , sheetItemsAfter : List SheetItem
    }


type alias SheetItem =
    { sheet : Sheet
    , name : String
    }


type Sheet
    = GridSheet
    | TableSheet Table


gridSheetItem : Name -> SheetItem
gridSheetItem =
    SheetItem GridSheet


tableSheetItem : Name -> SheetItem
tableSheetItem =
    SheetItem (TableSheet Table.empty)


updateTable : Table.State -> Document -> Document
updateTable s (Document data) =
    let
        upd item table =
            { item
                | sheet =
                    TableSheet <|
                        Table.update s table
            }
    in
    Document <|
        case data.currentSheetItem.sheet of
            TableSheet table ->
                { data
                    | currentSheetItem = upd data.currentSheetItem table
                }

            _ ->
                data


singleSheet : Name -> Document
singleSheet name =
    Document
        { cells = D.empty
        , currentSheetItem = { name = name, sheet = GridSheet }
        , sheetItemsBefore = []
        , sheetItemsAfter = []
        }


sheets : Document -> List (Position SheetItem)
sheets (Document { sheetItemsBefore, currentSheetItem, sheetItemsAfter }) =
    L.concat
        [ L.map Before sheetItemsBefore
        , [ Current currentSheetItem ]
        , L.map After sheetItemsAfter
        ]


currentSheet : Document -> Sheet
currentSheet (Document { currentSheetItem }) =
    currentSheetItem.sheet


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
                (L.foldl process ( [], Nothing, [] ) data.sheetItemsBefore)
                (data.currentSheetItem :: data.sheetItemsAfter)
    in
    case newCurrent of
        Just current ->
            Ok
                (Document
                    { data
                        | currentSheetItem = current
                        , sheetItemsBefore = newBefore
                        , sheetItemsAfter = newAfter
                    }
                )

        Nothing ->
            Err (UndefinedSheetError selectedName)


sheetExists : Name -> DocData -> Bool
sheetExists name data =
    L.member name (L.map .name data.sheetItemsBefore)
        || L.member name (L.map .name data.sheetItemsAfter)
        || (name == data.currentSheetItem.name)


insertSheet : SheetItem -> Document -> Result Error Document
insertSheet item (Document data) =
    if sheetExists item.name data then
        Err (DuplicateSheetNameError item.name)

    else
        Ok <|
            Document
                { data
                    | sheetItemsAfter = L.append data.sheetItemsAfter [ item ]
                }


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    if name == d.currentSheetItem.name then
        case ( d.sheetItemsBefore, d.sheetItemsAfter ) of
            ( _, head :: tail ) ->
                Ok <| Document { d | currentSheetItem = head, sheetItemsAfter = tail }

            ( head :: tail, _ ) ->
                Ok <| Document { d | currentSheetItem = head, sheetItemsBefore = tail }

            _ ->
                Err (RemovingLastSheetError name)

    else if sheetExists name d then
        Ok <|
            Document
                { d
                    | sheetItemsBefore = L.filter (.name >> (/=) name) d.sheetItemsBefore
                    , sheetItemsAfter = L.filter (.name >> (/=) name) d.sheetItemsAfter
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
                | currentSheetItem = updateName f d.currentSheetItem
                , sheetItemsBefore = L.map (updateName f) d.sheetItemsBefore
                , sheetItemsAfter = L.map (updateName f) d.sheetItemsAfter
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
            Document { d | cells = D.remove ( d.currentSheetItem.name, cellName ) d.cells }

        _ ->
            Document
                { d
                    | cells =
                        D.insert ( d.currentSheetItem.name, cellName )
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
    getCell d.currentSheetItem.name cellName d |> R.map Cell.source


get : Name -> Document -> ValueOrError
get name (Document d) =
    evalCell ( d.currentSheetItem.name, name ) D.empty d |> T.first


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
