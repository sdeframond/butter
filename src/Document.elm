module Document exposing
    ( Document
    , Msg
    , Position(..)
    , commitEdit
    , init
    , insertSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetsWithIds
    , subscriptions
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Formula
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Result as R
import Sheet exposing (Sheet)
import Tuple as T
import Types exposing (Name)
import ZipList as ZL exposing (ZipList)



-- DOCUMENT


type Document
    = Document DocData


type alias DocData =
    { sheets : ZipList ( Types.SheetId, Sheet )
    , sheetIds : Dict Types.Name Types.SheetId
    , nextSheetId : Types.SheetId
    }



-- INIT


init : Sheet -> Document
init sheet =
    Document
        { sheets = ZL.singleton ( 0, sheet )
        , sheetIds = D.fromList [ ( Sheet.getName sheet, 0 ) ]
        , nextSheetId = 1
        }



-- SHEETS


currentSheet : DocData -> Sheet
currentSheet { sheets } =
    ZL.current sheets |> T.second


currentSheetId : DocData -> Types.SheetId
currentSheetId { sheets } =
    ZL.current sheets |> T.first


currentSheetName : DocData -> Types.Name
currentSheetName { sheets } =
    ZL.current sheets |> T.second |> Sheet.getName


sheetsWithIds : Document -> List (Position ( Types.SheetId, Sheet ))
sheetsWithIds (Document { sheets }) =
    sheets
        |> ZL.toListWithPosition
            { before = Before
            , current = Current
            , after = After
            }


type Position a
    = Before a
    | Current a
    | After a


selectSheet : Types.SheetId -> Document -> Result Types.Error Document
selectSheet selectedId (Document ({ sheets } as data)) =
    ZL.select (T.first >> (==) selectedId) sheets
        |> R.fromMaybe (Types.UndefinedSheetError (selectedId |> String.fromInt))
        |> R.map (\newSheets -> Document { data | sheets = newSheets })


sheetExists : Types.Name -> DocData -> Bool
sheetExists name { sheetIds } =
    D.member name sheetIds


getSheet : Types.SheetId -> DocData -> Maybe Sheet
getSheet sheetId data =
    ZL.get (T.first >> (==) sheetId) data.sheets
        |> Maybe.map T.second


getSheetName : DocData -> Types.SheetId -> Maybe Types.Name
getSheetName data sheetId =
    getSheet sheetId data |> Maybe.map Sheet.getName


insertSheet : Sheet -> Document -> Result Types.Error Document
insertSheet sheet (Document data) =
    insertSheetHelp sheet data |> R.map Document


insertSheetHelp : Sheet -> DocData -> Result Types.Error DocData
insertSheetHelp sheet data =
    if sheetExists (Sheet.getName sheet) data then
        Err (Types.DuplicateSheetNameError (Sheet.getName sheet))

    else
        Ok <|
            { data
                | sheets = ZL.append [ ( data.nextSheetId, sheet ) ] data.sheets
                , nextSheetId = data.nextSheetId + 1
                , sheetIds = D.insert (Sheet.getName sheet) data.nextSheetId data.sheetIds
            }


removeSheet : Types.SheetId -> Document -> Result Types.Error Document
removeSheet sheetId (Document d) =
    let
        newSheetsRes =
            if sheetId == currentSheetId d then
                ZL.removeCurrent d.sheets
                    |> Maybe.map
                        (\newSheets ->
                            { newSheets = newSheets
                            , maybeSheetName = Just (currentSheetName d)
                            }
                        )
                    |> R.fromMaybe (Types.RemovingLastSheetError (currentSheetName d))

            else if ZL.map T.first d.sheets |> ZL.member sheetId then
                Ok
                    { newSheets =
                        ZL.filter (T.first >> (/=) sheetId) d.sheets
                            |> Maybe.withDefault d.sheets
                    , maybeSheetName = getSheetName d sheetId
                    }

            else
                Err (Types.UndefinedSheetError (String.fromInt sheetId))
    in
    newSheetsRes
        |> R.map
            (\{ newSheets, maybeSheetName } ->
                Document
                    { d
                        | sheets = newSheets
                        , sheetIds =
                            maybeSheetName
                                |> Maybe.map (\name -> D.remove name d.sheetIds)
                                |> Maybe.withDefault d.sheetIds
                    }
            )


renameSheet : Types.SheetId -> Types.Name -> Document -> Result Types.Error Document
renameSheet sheetId newName (Document data) =
    let
        updateSheetName : ( Name, Name ) -> DocData -> DocData
        updateSheetName ( validNewName, oldName ) d =
            let
                renameSheet_ ( currentId, sheet ) =
                    T.pair currentId <|
                        if currentId == sheetId then
                            Sheet.rename validNewName sheet

                        else
                            sheet
            in
            { d
                | sheets = ZL.map renameSheet_ d.sheets
                , sheetIds =
                    d.sheetIds
                        |> D.remove oldName
                        |> D.insert validNewName sheetId
            }
    in
    if sheetExists newName data then
        Err (Types.DuplicateSheetNameError newName)

    else
        Formula.parseName newName
            |> R.mapError (always Types.InvalidSheetNameError)
            |> R.andThen
                (\validNewName ->
                    getSheetName data sheetId
                        |> Maybe.map (T.pair validNewName)
                        |> R.fromMaybe (Types.UnexpectedError ("Invalid SheetId: " ++ String.fromInt sheetId))
                )
            |> R.map updateSheetName
            |> R.map (\updater -> Document (updater data))



-- SUSCRIPTIONS


subscriptions : Document -> Sub Msg
subscriptions (Document data) =
    currentSheet data
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
      -- | MakePivotTable
    | InsertSheet Sheet


update : Msg -> Document -> ( Document, Cmd Msg )
update msg (Document data) =
    updateData msg data |> Tuple.mapBoth Document identity


updateData : Msg -> DocData -> ( DocData, Cmd Msg )
updateData msg data =
    let
        updateSheet d newSheet =
            { d
                | sheets = ZL.setCurrent ( currentSheetId data, newSheet ) d.sheets
            }
    in
    case msg of
        SheetMsg sheetMsg ->
            Sheet.update (getSheetId data) sheetMsg (currentSheet data)
                |> Tuple.mapFirst (updateSheet data)
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet sheet ->
            ( insertSheetHelp sheet data
                |> Result.withDefault data
            , Cmd.none
            )


commitEdit : Document -> Document
commitEdit (Document data) =
    let
        setCurrentSheet sheet =
            { data
                | sheets =
                    ZL.setCurrent
                        ( currentSheetId data, sheet )
                        data.sheets
            }
    in
    currentSheet data
        |> Sheet.commitEdit
        |> setCurrentSheet
        |> Document


getSheetId : DocData -> Types.Name -> Maybe Types.SheetId
getSheetId data name =
    D.get name data.sheetIds



-- EVAL


eval : DocData -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval data ( sheetId, ref ) ancestors =
    let
        context : Sheet.Context
        context =
            { ancestors = ancestors
            , prefix = sheetId
            , resolveGlobalReference = eval data
            }
    in
    getSheet sheetId data
        |> Result.fromMaybe
            -- Is it realy unexpected though ? Eg what happens when some sheet is removed ?
            (Types.UnexpectedError "Found an orphan sheet")
        |> Result.andThen (Sheet.eval ref context)



-- VIEW


type alias Config msg =
    { toMsg : Msg -> msg }


view : Config msg -> Document -> Html msg
view { toMsg } (Document data) =
    let
        config : Sheet.Config msg
        config =
            { toMsg = SheetMsg >> toMsg
            , insertSheet = InsertSheet >> toMsg
            , getSheetName = getSheetName data
            , context =
                { prefix = currentSheetId data
                , ancestors = []
                , resolveGlobalReference = eval data
                }
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow hidden
            ]
        ]
        [ Sheet.view config (currentSheet data)
        ]
