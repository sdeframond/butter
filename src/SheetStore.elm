module SheetStore exposing
    ( SheetStore
    , commitSheetName
    , createSheet
    , currentSheet
    , currentSheetId
    , currentSheetName
    , decoder
    , editCurrentSheetName
    , editStatus
    , encode
    , getSheet
    , getSheetId
    , getSheetName
    , init
    , isCurrentId
    , isEditing
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetsWithIds
    , updateEdit
    , updateSheet
    )

import DecodeHelpers
import Json.Decode as Decode
import Json.Encode as Encode
import Name exposing (Name)
import PositiveInt
import Sheet exposing (Sheet)
import Types
import ZipList as ZL exposing (ZipList)


type SheetStore
    = SheetStore Data


type alias Data =
    { sheetIds : Name.Store Types.SheetId
    , edit : Maybe String
    , sheets : ZipList ( Types.SheetId, Sheet )
    , nextSheetId : Types.SheetId
    }



-- INIT


init : SheetStore
init =
    let
        initSheet =
            Sheet.initTable (Name.fromSheetId initId)

        initId =
            PositiveInt.one
    in
    SheetStore
        { sheets = ZL.singleton ( initId, initSheet )
        , sheetIds = Name.fromList [ ( Sheet.getName initSheet, initId ) ]
        , nextSheetId = PositiveInt.next initId
        , edit = Nothing
        }



-- SHEETS


isEditing : SheetStore -> Bool
isEditing (SheetStore { edit }) =
    case edit of
        Just _ ->
            True

        Nothing ->
            False


currentSheet : SheetStore -> Sheet
currentSheet (SheetStore { sheets }) =
    ZL.current sheets |> Tuple.second


currentSheetId : SheetStore -> Types.SheetId
currentSheetId (SheetStore { sheets }) =
    ZL.current sheets |> Tuple.first


currentSheetName : SheetStore -> Name
currentSheetName (SheetStore { sheets }) =
    ZL.current sheets |> Tuple.second |> Sheet.getName


sheetsWithIds : SheetStore -> List ( Types.SheetId, Sheet )
sheetsWithIds (SheetStore { sheets }) =
    sheets |> ZL.toList


isCurrentId : Types.SheetId -> SheetStore -> Bool
isCurrentId id model =
    id == currentSheetId model


selectSheet : Types.SheetId -> SheetStore -> Maybe SheetStore
selectSheet selectedId store =
    let
        (SheetStore model) =
            store |> commitSheet |> commitSheetName
    in
    ZL.select (Tuple.first >> (==) selectedId) model.sheets
        |> Maybe.map (\newSheets -> { model | sheets = newSheets })
        |> Maybe.map SheetStore


sheetExists : Name -> Data -> Bool
sheetExists name { sheetIds } =
    Name.member name sheetIds


getSheet : Types.SheetId -> SheetStore -> Maybe Sheet
getSheet sheetId (SheetStore { sheets }) =
    ZL.get (Tuple.first >> (==) sheetId) sheets
        |> Maybe.map Tuple.second


getSheetName : SheetStore -> Types.SheetId -> Maybe Name
getSheetName model sheetId =
    getSheet sheetId model |> Maybe.map Sheet.getName


getSheetId : SheetStore -> Name -> Maybe Types.SheetId
getSheetId (SheetStore { sheetIds }) name =
    Name.get name sheetIds


createSheet : Sheet.Params -> SheetStore -> SheetStore
createSheet params (SheetStore model) =
    let
        name =
            Name.fromSheetId model.nextSheetId

        sheet =
            Sheet.fromParams name params
    in
    SheetStore <|
        { model
            | sheets = ZL.append [ ( model.nextSheetId, sheet ) ] model.sheets
            , nextSheetId = PositiveInt.next model.nextSheetId
            , sheetIds = Name.insert (Sheet.getName sheet) model.nextSheetId model.sheetIds
        }


removeSheet : Types.SheetId -> SheetStore -> SheetStore
removeSheet sheetId ((SheetStore data) as model) =
    ZL.filter (Tuple.first >> (/=) sheetId) data.sheets
        |> Maybe.map
            (\newSheets ->
                { data
                    | sheets = newSheets
                    , sheetIds =
                        Name.remove (currentSheetName model) data.sheetIds
                }
            )
        |> Maybe.withDefault data
        |> SheetStore


renameSheet : Types.SheetId -> String -> SheetStore -> Result Types.Error SheetStore
renameSheet sheetId input ((SheetStore data) as model) =
    let
        updateSheetName : ( Name, Name ) -> Data -> SheetStore
        updateSheetName ( newName, oldName ) m =
            let
                renameSheet_ ( currentId, sheet ) =
                    Tuple.pair currentId <|
                        if currentId == sheetId then
                            Sheet.rename newName sheet

                        else
                            sheet
            in
            SheetStore
                { m
                    | sheets = ZL.map renameSheet_ m.sheets
                    , sheetIds =
                        m.sheetIds
                            |> Name.remove oldName
                            |> Name.insert newName sheetId
                }

        help name =
            if sheetExists name data then
                Err (Types.DuplicateSheetNameError name)

            else
                getSheetName model sheetId
                    |> Maybe.map (Tuple.pair name)
                    |> Result.fromMaybe
                        (Types.UnexpectedError
                            ("Invalid SheetId: " ++ PositiveInt.toString sheetId)
                        )
                    |> Result.map updateSheetName
                    |> Result.map (\updater -> updater data)
    in
    Name.fromString input
        |> Result.fromMaybe Types.InvalidSheetNameError
        |> Result.andThen help


updateSheet : SheetStore -> Sheet -> SheetStore
updateSheet ((SheetStore data) as model) newSheet =
    { data
        | sheets = ZL.setCurrent ( currentSheetId model, newSheet ) data.sheets
    }
        |> SheetStore


commitSheetName : SheetStore -> SheetStore
commitSheetName ((SheetStore data) as model) =
    case data.edit of
        Just input ->
            renameSheet (currentSheetId model) input model
                |> Result.withDefault model
                |> (\(SheetStore renamed) -> SheetStore { renamed | edit = Nothing })

        Nothing ->
            model


commitSheet : SheetStore -> SheetStore
commitSheet ((SheetStore data) as model) =
    let
        setCurrentSheet sheet =
            { data
                | sheets =
                    ZL.setCurrent
                        ( currentSheetId model, sheet )
                        data.sheets
            }
    in
    currentSheet model
        |> Sheet.commitEdit
        |> setCurrentSheet
        |> SheetStore


editCurrentSheetName : SheetStore -> SheetStore
editCurrentSheetName ((SheetStore data) as model) =
    { data | edit = Just (Name.toString <| currentSheetName model) }
        |> SheetStore
        |> commitSheet


updateEdit : String -> SheetStore -> SheetStore
updateEdit input (SheetStore data) =
    SheetStore <|
        case data.edit of
            Just _ ->
                { data | edit = Just input }

            _ ->
                data


editStatus : SheetStore -> Maybe String
editStatus (SheetStore { edit }) =
    edit



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


decoder : Decode.Decoder SheetStore
decoder =
    let
        getSheetId_ ids name =
            Name.get name ids

        finalize sheetIds =
            Decode.map3 (Data sheetIds)
                (Decode.field jsonKeys.edit editStatusDecoder)
                (Decode.field jsonKeys.sheets <|
                    sheetListDecoder (getSheetId_ sheetIds)
                )
                (Decode.field jsonKeys.nextSheetId PositiveInt.decoder)
    in
    Decode.field jsonKeys.sheetIds (Name.storeDecoder PositiveInt.decoder)
        |> Decode.andThen finalize
        |> Decode.map SheetStore


editStatusDecoder : Decode.Decoder (Maybe String)
editStatusDecoder =
    Decode.field jsonKeys.statusType Decode.string
        |> DecodeHelpers.switch "Invalid status type"
            [ ( jsonKeys.statusNotEditing, Decode.succeed Nothing )
            , ( jsonKeys.statusEditingSheetName
              , Decode.map Just
                    (Decode.field jsonKeys.statusInput Decode.string)
              )
            ]


sheetListDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder (ZipList ( Types.SheetId, Sheet ))
sheetListDecoder getSheetId_ =
    let
        itemDecoder =
            Decode.map2 Tuple.pair
                (Decode.field jsonKeys.id PositiveInt.decoder)
                (Decode.field jsonKeys.sheet <| Sheet.decoder getSheetId_)
    in
    ZL.decoder itemDecoder


encode : SheetStore -> Encode.Value
encode ((SheetStore data) as model) =
    Encode.object
        [ ( jsonKeys.edit, encodeEditStatus data.edit )
        , ( jsonKeys.nextSheetId, PositiveInt.encode data.nextSheetId )
        , ( jsonKeys.sheetIds, Name.encodeStore PositiveInt.encode data.sheetIds )
        , ( jsonKeys.sheets, encodeSheetList (getSheetName model) data.sheets )
        ]


encodeEditStatus : Maybe String -> Encode.Value
encodeEditStatus status =
    case status of
        Nothing ->
            Encode.object
                [ ( jsonKeys.statusType, Encode.string jsonKeys.statusNotEditing ) ]

        Just input ->
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
