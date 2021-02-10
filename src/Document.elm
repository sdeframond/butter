module Document exposing
    ( Document
    , Msg
    , Sheet(..)
    , cellSource
    , commitEdit
    ,  fromList
       -- Not used but useful for testing.
       -- TODO: find a way to test without it.

    , get
    , gridSheet
    ,  insert
       -- Not used but useful for testing.
       -- TODO: find a way to test without it.

    , insertSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetNames
    , singleSheet
    , tableSheet
    , update
    , view
    )

import Char exposing (isUpper)
import Css exposing (..)
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
import Document.Grid as Grid exposing (Grid)
import Document.Table as Table exposing (Table)
import Document.Types as Types exposing (..)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css)
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
    = GridMsg Grid
    | TableMsg Table.State


update : Msg -> Document -> Document
update msg (Document data) =
    let
        { sheet, name } =
            data.currentSheetItem
    in
    Document <|
        case ( msg, sheet ) of
            ( GridMsg grid, GridSheet _ ) ->
                { data | currentSheetItem = SheetItem name (GridSheet grid) }

            ( TableMsg state, TableSheet table ) ->
                { data
                    | currentSheetItem =
                        SheetItem name (TableSheet <| Table.update state table)
                }

            ( _, _ ) ->
                data


commitEdit : Document -> Document
commitEdit (Document ({ currentSheetItem } as data)) =
    case currentSheetItem.sheet of
        GridSheet grid ->
            let
                commiter editState =
                    case editState of
                        Nothing ->
                            data

                        Just ( name, src ) ->
                            insertHelp name src data

                newItem =
                    { currentSheetItem | sheet = GridSheet newGrid }

                ( newData, newGrid ) =
                    Grid.commit commiter grid
            in
            Document { newData | currentSheetItem = newItem }

        _ ->
            Document data


singleSheet : Name -> Document
singleSheet name =
    Document
        { cells = D.empty
        , currentSheetItem = SheetItem name gridSheet
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


fromList : Name -> List ( String, String ) -> Document
fromList sheet pairs =
    List.foldl (\( a, b ) (Document data) -> Document <| insertHelp a b data) (singleSheet sheet) pairs


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


type alias Config msg =
    { toMsg : Msg -> msg
    , toCommitMsg : Msg -> msg
    }


view : Config msg -> Document -> Html msg
view { toMsg, toCommitMsg } ((Document { currentSheetItem }) as doc) =
    let
        gridConfig =
            { toMsg = GridMsg >> toMsg
            , commitMsg = GridMsg >> toCommitMsg
            , getCellValue = \name -> get name doc
            , getCellSource = \name -> cellSource name doc |> R.withDefault ""
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow hidden
            ]
        ]
        [ case currentSheetItem.sheet of
            GridSheet grid ->
                Grid.view gridConfig grid

            TableSheet table ->
                fromUnstyled (Table.view (TableMsg >> toMsg) table)
        ]
