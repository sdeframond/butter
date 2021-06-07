module Document exposing
    ( Model
    , Msg(..)
    , currentSheetId
    , currentSheetName
    , decoder
    , encode
    , init
    , isEditing
    , subscriptions
    , update
    , view
    )

import Css exposing (..)
import Css.Global as Global
import DecodeHelpers
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Name exposing (Name)
import PositiveInt
import Result as R
import Sheet exposing (Sheet)
import Tuple as T
import Types
import ZipList as ZL exposing (ZipList)



-- DOCUMENT


type alias Model =
    { sheetIds : Name.Store Types.SheetId
    , edit : EditStatus
    , sheets : ZipList ( Types.SheetId, Sheet )
    , nextSheetId : Types.SheetId
    }


type EditStatus
    = NotEditing
    | EditingSheetName String



-- INIT


init : Model
init =
    let
        sheet =
            Sheet.initTable (Name.fromSheetId initId)

        initId =
            PositiveInt.one

        model =
            { sheets = ZL.singleton ( initId, sheet )
            , sheetIds = Name.fromList [ ( Sheet.getName sheet, initId ) ]
            , nextSheetId = PositiveInt.next initId
            , edit = NotEditing
            }
    in
    model



-- SHEETS


isEditing : Model -> Bool
isEditing { edit } =
    case edit of
        EditingSheetName _ ->
            True

        NotEditing ->
            False


currentSheet : Model -> Sheet
currentSheet { sheets } =
    ZL.current sheets |> T.second


currentSheetId : Model -> Types.SheetId
currentSheetId { sheets } =
    ZL.current sheets |> T.first


currentSheetName : Model -> Name
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


selectSheet : Types.SheetId -> Model -> Maybe Model
selectSheet selectedId model =
    ZL.select (T.first >> (==) selectedId) model.sheets
        -- |> R.fromMaybe (Types.UndefinedSheetError (selectedId |> String.fromInt))
        |> Maybe.map (\newSheets -> { model | sheets = newSheets })


sheetExists : Name -> Model -> Bool
sheetExists name { sheetIds } =
    Name.member name sheetIds


getSheet : Types.SheetId -> Model -> Maybe Sheet
getSheet sheetId model =
    ZL.get (T.first >> (==) sheetId) model.sheets
        |> Maybe.map T.second


getSheetName : Model -> Types.SheetId -> Maybe Name
getSheetName model sheetId =
    getSheet sheetId model |> Maybe.map Sheet.getName


insertSheet : Sheet -> Model -> Model
insertSheet sheet model =
    if sheetExists (Sheet.getName sheet) model then
        model

    else
        { model
            | sheets = ZL.append [ ( model.nextSheetId, sheet ) ] model.sheets
            , nextSheetId = PositiveInt.next model.nextSheetId
            , sheetIds = Name.insert (Sheet.getName sheet) model.nextSheetId model.sheetIds
        }


removeSheet : Types.SheetId -> Model -> Model
removeSheet sheetId model =
    let
        maybeNewSheets =
            if sheetId == currentSheetId model then
                ZL.removeCurrent model.sheets
                    |> Maybe.map
                        (\newSheets ->
                            { newSheets = newSheets
                            , maybeSheetName = Just (currentSheetName model)
                            }
                        )

            else if ZL.map T.first model.sheets |> ZL.member sheetId then
                Just
                    { newSheets =
                        ZL.filter (T.first >> (/=) sheetId) model.sheets
                            |> Maybe.withDefault model.sheets
                    , maybeSheetName = getSheetName model sheetId
                    }

            else
                Nothing
    in
    maybeNewSheets
        |> Maybe.map
            (\{ newSheets, maybeSheetName } ->
                { model
                    | sheets = newSheets
                    , sheetIds =
                        maybeSheetName
                            |> Maybe.map (\name -> Name.remove name model.sheetIds)
                            |> Maybe.withDefault model.sheetIds
                }
            )
        |> Maybe.withDefault model


renameSheet : Types.SheetId -> String -> Model -> Result Types.Error Model
renameSheet sheetId input model =
    let
        updateSheetName : ( Name, Name ) -> Model -> Model
        updateSheetName ( newName, oldName ) m =
            let
                renameSheet_ ( currentId, sheet ) =
                    T.pair currentId <|
                        if currentId == sheetId then
                            Sheet.rename newName sheet

                        else
                            sheet
            in
            { m
                | sheets = ZL.map renameSheet_ m.sheets
                , sheetIds =
                    m.sheetIds
                        |> Name.remove oldName
                        |> Name.insert newName sheetId
            }

        help name =
            if sheetExists name model then
                Err (Types.DuplicateSheetNameError name)

            else
                getSheetName model sheetId
                    |> Maybe.map (T.pair name)
                    |> R.fromMaybe
                        (Types.UnexpectedError
                            ("Invalid SheetId: " ++ PositiveInt.toString sheetId)
                        )
                    |> R.map updateSheetName
                    |> R.map (\updater -> updater model)
    in
    Name.fromString input
        |> Result.fromMaybe Types.InvalidSheetNameError
        |> Result.andThen help



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    currentSheet model
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
    | InsertSheet Sheet
    | InsertGridSheet
    | InsertTableSheet
    | SelectSheet Types.SheetId
    | RemoveSheet Types.SheetId
    | EditSheet
    | UpdateSheetName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateSheet m newSheet =
            { m
                | sheets = ZL.setCurrent ( currentSheetId model, newSheet ) model.sheets
            }

        commitSheetName m =
            case m.edit of
                EditingSheetName input ->
                    renameSheet (currentSheetId m) input m
                        -- TODO log errors
                        |> R.withDefault m
                        |> (\renamed -> { renamed | edit = NotEditing })

                NotEditing ->
                    m

        commitSheet : Model -> Model
        commitSheet m =
            let
                setCurrentSheet sheet =
                    { m
                        | sheets =
                            ZL.setCurrent
                                ( currentSheetId m, sheet )
                                m.sheets
                    }
            in
            currentSheet m
                |> Sheet.commitEdit
                |> setCurrentSheet
    in
    case msg of
        SheetMsg sheetMsg ->
            Sheet.update (\name -> Name.get name model.sheetIds) sheetMsg (currentSheet model)
                |> Tuple.mapFirst (updateSheet model >> commitSheetName)
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet sheet ->
            ( insertSheet sheet model
            , Cmd.none
            )

        InsertGridSheet ->
            ( insertSheet (Sheet.initGrid <| Name.fromSheetId model.nextSheetId) model
            , Cmd.none
            )

        InsertTableSheet ->
            ( insertSheet (Sheet.initTable <| Name.fromSheetId model.nextSheetId) model
            , Cmd.none
            )

        SelectSheet sheetId ->
            ( model
                |> commitSheet
                |> commitSheetName
                |> (\m ->
                        selectSheet sheetId m
                            |> Maybe.withDefault m
                   )
            , Cmd.none
            )

        RemoveSheet sheetId ->
            ( removeSheet sheetId model
            , Cmd.none
            )

        EditSheet ->
            ( currentSheetName model
                |> (\name ->
                        { model | edit = EditingSheetName (Name.toString name) }
                            |> commitSheet
                   )
            , Cmd.none
            )

        UpdateSheetName input ->
            ( { model
                | edit =
                    case model.edit of
                        EditingSheetName _ ->
                            EditingSheetName input

                        _ ->
                            model.edit
              }
            , Cmd.none
            )



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


view : Model -> List (Html.Html Msg)
view model =
    List.map toUnstyled
        [ Global.global
            [ Global.html [ height (pct 100) ]
            , Global.body [ height (pct 100) ]
            ]
        , documentView model
        ]


documentView : Model -> Html Msg
documentView model =
    let
        sheetConfig : Sheet.Config Msg
        sheetConfig =
            { toMsg = SheetMsg
            , insertPivotTable =
                Sheet.initPivotTable (Name.fromSheetId model.nextSheetId)
                    >> InsertSheet
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
                        , Events.onDoubleClick <| EditSheet
                        ]
                        [ text (Sheet.getName sheet |> Name.toString)
                        , span [ Events.onClick <| RemoveSheet sheetId ]
                            [ text "[x]" ]
                        ]
            in
            case ( positionedName, model.edit ) of
                ( Current _, EditingSheetName newName ) ->
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



-- JSON


jsonKeys :
    { sheets : String
    , nextSheetId : String
    , sheetIds : String
    , id : String
    , sheet : String
    , edit : String
    , statusType : String
    , statusEditingSheetName : String
    , statusSheetId : String
    , statusInput : String
    , statusNotEditing : String
    }
jsonKeys =
    { sheets = "sheets"
    , nextSheetId = "nextSheetId"
    , sheetIds = "sheetIds"
    , id = "id"
    , sheet = "sheet"
    , edit = "edit"
    , statusType = "statusType"
    , statusEditingSheetName = "statusEditingSheetName"
    , statusSheetId = "statusSheetId"
    , statusInput = "statusInput"
    , statusNotEditing = "statusNotEditing"
    }


decoder : Decode.Decoder Model
decoder =
    let
        getSheetId ids name =
            Name.get name ids

        finalize sheetIds =
            Decode.map3 (Model sheetIds)
                (Decode.field jsonKeys.edit editStatusDecoder)
                (Decode.field jsonKeys.sheets <|
                    sheetListDecoder (getSheetId sheetIds)
                )
                (Decode.field jsonKeys.nextSheetId PositiveInt.decoder)
    in
    Decode.field jsonKeys.sheetIds (Name.storeDecoder PositiveInt.decoder)
        |> Decode.andThen finalize


editStatusDecoder : Decode.Decoder EditStatus
editStatusDecoder =
    Decode.field jsonKeys.statusType Decode.string
        |> DecodeHelpers.switch "Invalid status type"
            [ ( jsonKeys.statusNotEditing, Decode.succeed NotEditing )
            , ( jsonKeys.statusEditingSheetName
              , Decode.map EditingSheetName
                    (Decode.field jsonKeys.statusInput Decode.string)
              )
            ]


sheetListDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder (ZipList ( Types.SheetId, Sheet ))
sheetListDecoder getSheetId =
    let
        itemDecoder =
            Decode.map2 Tuple.pair
                (Decode.field jsonKeys.id PositiveInt.decoder)
                (Decode.field jsonKeys.sheet <| Sheet.decoder getSheetId)
    in
    ZL.decoder itemDecoder


encode : Model -> Encode.Value
encode model =
    Encode.object
        [ ( jsonKeys.edit, encodeEditStatus model.edit )
        , ( jsonKeys.nextSheetId, PositiveInt.encode model.nextSheetId )
        , ( jsonKeys.sheetIds, Name.encodeStore PositiveInt.encode model.sheetIds )
        , ( jsonKeys.sheets, encodeSheetList (getSheetName model) model.sheets )
        ]


encodeEditStatus : EditStatus -> Encode.Value
encodeEditStatus status =
    case status of
        NotEditing ->
            Encode.object
                [ ( jsonKeys.statusType, Encode.string jsonKeys.statusNotEditing ) ]

        EditingSheetName input ->
            Encode.object
                [ ( jsonKeys.statusType, Encode.string jsonKeys.statusEditingSheetName )
                , ( jsonKeys.statusInput, Encode.string input )
                ]


encodeSheetList : (Types.SheetId -> Maybe Name) -> ZipList ( Types.SheetId, Sheet ) -> Encode.Value
encodeSheetList getSheetName_ list =
    let
        encodeItem ( sheetId, sheet ) =
            Encode.object
                [ ( jsonKeys.id, PositiveInt.encode sheetId )
                , ( jsonKeys.sheet, Sheet.encode getSheetName_ sheet )
                ]
    in
    ZL.encode encodeItem list
