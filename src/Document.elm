module Document exposing
    ( Document
    , Msg
    , Sheet(..)
    , cellSource
    , commitEdit
    , get
    , gridSheet
    , init
    ,  insert
       -- Not used but useful for testing.
       -- TODO: find a way to test without it.

    , insertSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetNames
    , tableSheet
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Document.AST as AST
    exposing
        ( AST(..)
        , BinaryOp(..)
        , FormulaAST(..)
        , parseName
        )
import Document.Cell as Cell exposing (Cell)
import Document.Grid as Grid exposing (Grid)
import Document.Table as Table exposing (Table)
import Document.Types as Types exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import List as L
import Maybe as M
import Result as R
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
    { name : String
    , sheet : Sheet
    }


type Sheet
    = GridSheet Grid
    | TableSheet Table


gridSheet : Sheet
gridSheet =
    GridSheet Grid.init


tableSheet : Sheet
tableSheet =
    TableSheet Table.empty


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg


update : Msg -> Document -> Document
update msg (Document data) =
    Document (updateData msg data)


updateData : Msg -> DocData -> DocData
updateData msg data =
    let
        { sheet, name } =
            data.currentSheetItem
    in
    case ( msg, sheet ) of
        ( GridMsg gridMsg, GridSheet grid ) ->
            updateGrid (Grid.update gridMsg grid) data

        ( TableMsg tableMsg, TableSheet table ) ->
            { data
                | currentSheetItem =
                    SheetItem name (TableSheet <| Table.update tableMsg table)
            }

        ( _, _ ) ->
            data


commitEdit : Document -> Document
commitEdit (Document ({ currentSheetItem } as data)) =
    Document <|
        case currentSheetItem.sheet of
            GridSheet grid ->
                updateGrid (Grid.commit grid) data

            _ ->
                data


updateGrid : ( Grid, Grid.Cmd ) -> DocData -> DocData
updateGrid ( newGrid, gridCmd ) data =
    let
        newItem =
            SheetItem data.currentSheetItem.name (GridSheet newGrid)

        newData =
            { data | currentSheetItem = newItem }
    in
    case gridCmd of
        Grid.NoCmd ->
            newData

        Grid.CommitChangesCmd cellName content ->
            insertHelp cellName content newData


init : Name -> Sheet -> Document
init name sheet =
    Document
        { cells = D.empty
        , currentSheetItem = SheetItem name sheet
        , sheetItemsBefore = []
        , sheetItemsAfter = []
        }


sheetNames : Document -> List (Types.Position Name)
sheetNames (Document { sheetItemsBefore, currentSheetItem, sheetItemsAfter }) =
    L.concat
        [ L.map (.name >> Before) sheetItemsBefore
        , [ Current currentSheetItem.name ]
        , L.map (.name >> After) sheetItemsAfter
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


insertSheet : Name -> Sheet -> Document -> Result Error Document
insertSheet name sheet (Document data) =
    if sheetExists name data then
        Err (DuplicateSheetNameError name)

    else
        Ok <|
            Document
                { data
                    | sheetItemsAfter = L.append data.sheetItemsAfter [ SheetItem name sheet ]
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
insert cellName value (Document data) =
    Document <| insertHelp cellName value data


insertHelp : Name -> String -> DocData -> DocData
insertHelp cellName value d =
    case value of
        "" ->
            { d | cells = D.remove ( d.currentSheetItem.name, cellName ) d.cells }

        _ ->
            { d
                | cells =
                    D.insert ( d.currentSheetItem.name, cellName )
                        (Cell.fromSource value)
                        d.cells
            }


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
    evalCell D.empty d [] ( d.currentSheetItem.name, name ) |> T.first


evalCell : AST.Memo -> DocData -> List LocatedName -> LocatedName -> ( ValueOrError, AST.Memo )
evalCell memo data ancestors name =
    let
        memoize ( v, m ) =
            ( v, D.insert name v m )

        resolveAbsolute =
            evalCell memo data (name :: ancestors)

        context =
            { resolveAbsolute = resolveAbsolute
            , resolveRelative = T.pair (T.first name) >> resolveAbsolute
            }
    in
    if List.member name ancestors then
        ( Err <| CyclicReferenceError ancestors, memo )

    else
        D.get name memo
            |> M.map (\v -> ( v, memo ))
            |> M.withDefault
                (getCell (T.first name) (T.second name) data
                    |> R.andThen Cell.parsed
                    |> R.map (AST.eval context memo)
                    |> (\result ->
                            case result of
                                Err e ->
                                    ( Err e, memo )

                                Ok v ->
                                    v
                       )
                    |> memoize
                )


type alias Config msg =
    { toMsg : Msg -> msg }


view : Config msg -> Document -> Html msg
view { toMsg } ((Document { currentSheetItem }) as doc) =
    let
        gridConfig =
            { toMsg = GridMsg >> toMsg
            , getCellValue = \name -> get name doc
            , getCellSource = \name -> cellSource name doc |> R.withDefault ""
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow auto
            ]
        ]
        [ case currentSheetItem.sheet of
            GridSheet grid ->
                Grid.view gridConfig grid

            TableSheet table ->
                Table.view (TableMsg >> toMsg) table
        ]
