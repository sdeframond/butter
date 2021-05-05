module Main exposing (main)

import Browser
import Css exposing (..)
import Css.Global as Global
import Dict as D exposing (Dict)
import Formula
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Result as R
import Sheet exposing (Sheet)
import Tuple as T
import Types exposing (Name)
import ZipList as ZL exposing (ZipList)



-- MAIN


main : Program {} Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- DOCUMENT


type alias Model =
    { sheets : ZipList ( Types.SheetId, Sheet )
    , sheetIds : Dict Types.Name Types.SheetId
    , nextSheetId : Types.SheetId
    , edit : EditStatus
    }


type EditStatus
    = NotEditing
    | EditingSheetName Types.SheetId Name



-- INIT


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        sheet =
            Sheet.initTable "Sheet1"

        initId =
            1
    in
    ( { sheets = ZL.singleton ( initId, sheet )
      , sheetIds = D.fromList [ ( Sheet.getName sheet, initId ) ]
      , nextSheetId = initId + 1
      , edit = NotEditing
      }
    , Cmd.none
    )



-- SHEETS


currentSheet : Model -> Sheet
currentSheet { sheets } =
    ZL.current sheets |> T.second


currentSheetId : Model -> Types.SheetId
currentSheetId { sheets } =
    ZL.current sheets |> T.first


currentSheetName : Model -> Types.Name
currentSheetName { sheets } =
    ZL.current sheets |> T.second |> Sheet.getName


sheetsWithIds : Model -> List (Position ( Types.SheetId, Sheet ))
sheetsWithIds model =
    model.sheets
        |> ZL.toListWithPosition
            { before = Before
            , current = Current
            , after = After
            }


type Position a
    = Before a
    | Current a
    | After a


selectSheet : Types.SheetId -> Model -> Result Types.Error Model
selectSheet selectedId model =
    ZL.select (T.first >> (==) selectedId) model.sheets
        |> R.fromMaybe (Types.UndefinedSheetError (selectedId |> String.fromInt))
        |> R.map (\newSheets -> { model | sheets = newSheets })


sheetExists : Types.Name -> Model -> Bool
sheetExists name { sheetIds } =
    D.member name sheetIds


getSheet : Types.SheetId -> Model -> Maybe Sheet
getSheet sheetId model =
    ZL.get (T.first >> (==) sheetId) model.sheets
        |> Maybe.map T.second


getSheetName : Model -> Types.SheetId -> Maybe Types.Name
getSheetName model sheetId =
    getSheet sheetId model |> Maybe.map Sheet.getName


insertSheetHelp : Sheet -> Model -> Result Types.Error Model
insertSheetHelp sheet model =
    if sheetExists (Sheet.getName sheet) model then
        Err (Types.DuplicateSheetNameError (Sheet.getName sheet))

    else
        Ok <|
            { model
                | sheets = ZL.append [ ( model.nextSheetId, sheet ) ] model.sheets
                , nextSheetId = model.nextSheetId + 1
                , sheetIds = D.insert (Sheet.getName sheet) model.nextSheetId model.sheetIds
            }


removeSheet : Types.SheetId -> Model -> Result Types.Error Model
removeSheet sheetId model =
    let
        newSheetsRes =
            if sheetId == currentSheetId model then
                ZL.removeCurrent model.sheets
                    |> Maybe.map
                        (\newSheets ->
                            { newSheets = newSheets
                            , maybeSheetName = Just (currentSheetName model)
                            }
                        )
                    |> R.fromMaybe (Types.RemovingLastSheetError (currentSheetName model))

            else if ZL.map T.first model.sheets |> ZL.member sheetId then
                Ok
                    { newSheets =
                        ZL.filter (T.first >> (/=) sheetId) model.sheets
                            |> Maybe.withDefault model.sheets
                    , maybeSheetName = getSheetName model sheetId
                    }

            else
                Err (Types.UndefinedSheetError (String.fromInt sheetId))
    in
    newSheetsRes
        |> R.map
            (\{ newSheets, maybeSheetName } ->
                { model
                    | sheets = newSheets
                    , sheetIds =
                        maybeSheetName
                            |> Maybe.map (\name -> D.remove name model.sheetIds)
                            |> Maybe.withDefault model.sheetIds
                }
            )


renameSheet : Types.SheetId -> Types.Name -> Model -> Result Types.Error Model
renameSheet sheetId newName model =
    let
        updateSheetName : ( Name, Name ) -> Model -> Model
        updateSheetName ( validNewName, oldName ) m =
            let
                renameSheet_ ( currentId, sheet ) =
                    T.pair currentId <|
                        if currentId == sheetId then
                            Sheet.rename validNewName sheet

                        else
                            sheet
            in
            { m
                | sheets = ZL.map renameSheet_ m.sheets
                , sheetIds =
                    m.sheetIds
                        |> D.remove oldName
                        |> D.insert validNewName sheetId
            }
    in
    if sheetExists newName model then
        Err (Types.DuplicateSheetNameError newName)

    else
        Formula.parseName newName
            |> R.mapError (always Types.InvalidSheetNameError)
            |> R.andThen
                (\validNewName ->
                    getSheetName model sheetId
                        |> Maybe.map (T.pair validNewName)
                        |> R.fromMaybe (Types.UnexpectedError ("Invalid SheetId: " ++ String.fromInt sheetId))
                )
            |> R.map updateSheetName
            |> R.map (\updater -> updater model)



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    currentSheet model
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
      -- | MakePivotTable
    | InsertSheet Sheet
    | InsertGridSheet
    | InsertTableSheet
    | SelectSheet Types.SheetId
    | RemoveSheet Types.SheetId
    | EditSheet ( Types.SheetId, Sheet )
    | UpdateSheetName Types.Name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateSheet m newSheet =
            { m
                | sheets = ZL.setCurrent ( currentSheetId model, newSheet ) model.sheets
            }

        insertSheet_ sheet =
            ( insertSheetHelp sheet model |> Result.withDefault model, Cmd.none )

        commitSheetName m =
            case m.edit of
                EditingSheetName sheetId newName ->
                    { m | edit = NotEditing }
                        |> renameSheet sheetId newName
                        -- TODO log errors
                        |> R.withDefault m

                NotEditing ->
                    m
    in
    case Debug.log "update msg" msg of
        SheetMsg sheetMsg ->
            Sheet.update (getSheetId model) sheetMsg (currentSheet model)
                |> Tuple.mapFirst (updateSheet model >> commitSheetName)
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet sheet ->
            insertSheet_ sheet

        InsertGridSheet ->
            insertSheet_ (Sheet.initGrid <| "Sheet" ++ String.fromInt model.nextSheetId)

        InsertTableSheet ->
            insertSheet_ (Sheet.initTable <| "Sheet" ++ String.fromInt model.nextSheetId)

        SelectSheet sheetId ->
            ( model
                |> commitEdit
                |> commitSheetName
                |> selectSheet sheetId
                |> Result.withDefault model
            , Cmd.none
            )

        RemoveSheet sheetId ->
            ( removeSheet sheetId model
                |> Result.withDefault model
            , Cmd.none
            )

        EditSheet ( sheetId, sheet ) ->
            ( { model | edit = EditingSheetName sheetId (Sheet.getName sheet) }
                |> commitEdit
            , Cmd.none
            )

        UpdateSheetName sheetName ->
            ( { model
                | edit =
                    case model.edit of
                        EditingSheetName oldName _ ->
                            EditingSheetName oldName sheetName

                        _ ->
                            model.edit
              }
            , Cmd.none
            )


commitEdit : Model -> Model
commitEdit model =
    let
        setCurrentSheet sheet =
            { model
                | sheets =
                    ZL.setCurrent
                        ( currentSheetId model, sheet )
                        model.sheets
            }
    in
    currentSheet model
        |> Sheet.commitEdit
        |> setCurrentSheet


getSheetId : Model -> Types.Name -> Maybe Types.SheetId
getSheetId model name =
    D.get name model.sheetIds



-- EVAL


eval : Model -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval model ( sheetId, ref ) ancestors =
    let
        context : Sheet.Context
        context =
            { ancestors = ancestors
            , prefix = sheetId
            , resolveGlobalReference = eval model
            }
    in
    getSheet sheetId model
        |> Result.fromMaybe
            -- Is it realy unexpected though ? Eg what happens when some sheet is removed ?
            (Types.UnexpectedError "Found an orphan sheet")
        |> Result.andThen (Sheet.eval ref context)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Butter Spreadsheet"
    , body =
        List.map toUnstyled
            [ Global.global
                [ Global.html [ height (pct 100) ]
                , Global.body [ height (pct 100) ]
                ]
            , documentView model
            ]
    }


documentView : Model -> Html Msg
documentView model =
    let
        sheetConfig : Sheet.Config Msg
        sheetConfig =
            { toMsg = SheetMsg
            , insertSheet = InsertSheet
            , getSheetName = getSheetName model
            , context =
                { prefix = currentSheetId model
                , ancestors = []
                , resolveGlobalReference = eval model
                }
            }
    in
    div
        [ css
            [ displayFlex
            , flexDirection column
            , height (pct 100)
            ]
        ]
        [ div
            [ css
                [ width (pct 100)
                , height (pct 100)
                , overflow hidden
                ]
            ]
            [ Sheet.view sheetConfig (currentSheet model)
            ]
        , sheetSelector model
        ]


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        itemCss =
            css
                [ border3 (px 1) solid (rgb 100 100 100)
                , display inlineBlock
                , padding2 (px 5) (px 5)
                ]

        sheetItem positionedName =
            let
                defaultItem ( sheetId, sheet ) isCurrent =
                    li
                        [ itemCss
                        , css
                            [ if isCurrent then
                                fontWeight bold

                              else
                                fontWeight normal
                            ]
                        , Events.onClick <| SelectSheet sheetId
                        , Events.onDoubleClick <| EditSheet ( sheetId, sheet )
                        ]
                        [ text (Sheet.getName sheet)
                        , span [ Events.onClick <| RemoveSheet sheetId ]
                            [ text "[x]" ]
                        ]
            in
            case ( positionedName, model.edit ) of
                ( Current _, EditingSheetName _ newName ) ->
                    li [ itemCss ]
                        [ input
                            [ Attr.value newName
                            , Events.onInput UpdateSheetName
                            ]
                            []
                        ]

                ( Current sheetWithId, _ ) ->
                    defaultItem sheetWithId True

                ( Before sheetWithId, _ ) ->
                    defaultItem sheetWithId False

                ( After sheetWithId, _ ) ->
                    defaultItem sheetWithId False

        addSheet msg label =
            li
                [ itemCss
                , Events.onClick msg
                ]
                [ text label ]
    in
    ul
        [ css
            [ borderTop3 (px 1) solid (rgb 0 0 0)
            , margin (px 0)
            , padding2 (px 10) (px 10)
            ]
        ]
        (addSheet InsertTableSheet "+table"
            :: addSheet InsertGridSheet "+grid"
            :: (sheetsWithIds model |> List.map sheetItem)
        )
