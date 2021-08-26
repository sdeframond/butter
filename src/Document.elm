module Document exposing
    ( Model
    , Msg(..)
    , decoder
    , encode
    , fromBytes
    , init
    , subscriptions
    , update
    , view
    )

import Bytes exposing (Bytes)
import Bytes.Encode
import Css exposing (..)
import File.Download exposing (bytes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Json.Encode
import Name exposing (Name)
import NamedAndOrderedStore exposing (NamedAndOrderedStore)
import Sheet exposing (Sheet)
import Time
import Types
import Ui
import Zip
import Zip.Entry as Entry



-- DOCUMENT


type alias Model =
    NamedAndOrderedStore Sheet


defaultSheetName : Name
defaultSheetName =
    Name.sanitize "Sheet"


init : Model
init =
    Sheet.initTable
        |> NamedAndOrderedStore.init defaultSheetName


toBytes : Model -> Bytes
toBytes doc =
    Json.Encode.encode 2 (encode doc)
        |> (Bytes.Encode.encode << Bytes.Encode.string)
        |> Entry.compress
            { path = "/main.json"

            -- We don't care about this.
            , lastModified = ( Time.utc, Time.millisToPosix 0 )
            , comment = Nothing
            }
        |> List.singleton
        |> Zip.fromEntries
        |> Zip.toBytes


fromBytes : Bytes -> Maybe Model
fromBytes bytes =
    Zip.fromBytes bytes
        |> Maybe.andThen (Zip.getEntry "/main.json")
        |> Maybe.andThen
            (\entry ->
                case Entry.toString entry of
                    Ok string ->
                        Json.Decode.decodeString decoder string
                            |> Result.toMaybe

                    Err _ ->
                        Nothing
            )



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    NamedAndOrderedStore.current model
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
    | InsertSheet Sheet.Params
    | SelectSheet Types.SheetId
    | RemoveSheet Types.SheetId
    | EditSheet
    | UpdateSheetName String
    | DownloadDocument


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetMsg sheetMsg ->
            Sheet.update (NamedAndOrderedStore.getIdByName model) sheetMsg (NamedAndOrderedStore.current model)
                |> Tuple.mapFirst (NamedAndOrderedStore.setCurrent model >> NamedAndOrderedStore.commitName)
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet params ->
            ( NamedAndOrderedStore.insert defaultSheetName (Sheet.fromParams params) model
            , Cmd.none
            )

        SelectSheet sheetId ->
            ( NamedAndOrderedStore.selectById Sheet.commitEdit sheetId model |> Maybe.withDefault model
            , Cmd.none
            )

        RemoveSheet sheetId ->
            ( NamedAndOrderedStore.remove sheetId model
            , Cmd.none
            )

        EditSheet ->
            ( NamedAndOrderedStore.editCurrentName Sheet.commitEdit model
            , Cmd.none
            )

        UpdateSheetName input ->
            ( NamedAndOrderedStore.updateEdit input model
            , Cmd.none
            )

        DownloadDocument ->
            ( model
            , toBytes model
                |> File.Download.bytes "document.butter" "application/butter"
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
    NamedAndOrderedStore.getById sheetId model
        |> Result.fromMaybe
            -- Is it realy unexpected though ? Eg what happens when some sheet is removed ?
            (Types.UnexpectedError "Found an orphan sheet")
        |> Result.andThen (Sheet.eval ref context)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , height (pct 100)
            , width (pct 100)
            , overflow hidden
            ]
        ]
        [ viewCurrentSheet model
        , Ui.row
            [ css
                [ borderTop3 (px 1) solid (rgb 0 0 0)
                , justifyContent spaceBetween
                , padding2 (px 10) (px 10)
                ]
            ]
            [ sheetSelector model
            , Ui.row []
                [ Ui.button [ Events.onClick DownloadDocument ] [ text "Download" ]
                ]
            ]
        ]


viewCurrentSheet : Model -> Html Msg
viewCurrentSheet model =
    let
        sheetConfig : Sheet.Config Msg
        sheetConfig =
            { toMsg = SheetMsg
            , insertPivotTable =
                Sheet.allParams.pivotTable
                    >> InsertSheet
            , getSheetName = NamedAndOrderedStore.getNameById model
            , context =
                { prefix = NamedAndOrderedStore.currentId model
                , ancestors = []
                , resolveGlobalReference = eval model
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
        [ Sheet.view sheetConfig (NamedAndOrderedStore.current model)
        ]


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        sheetItem item =
            Ui.editableListItem
                { onSelect = SelectSheet
                , onEdit = EditSheet
                , onRemove = RemoveSheet
                , onUpdate = UpdateSheetName
                }
                { id = item.id
                , name = Name.toString item.name
                , isCurrent = NamedAndOrderedStore.isCurrentId item.id model
                , editStatus = NamedAndOrderedStore.editStatus model
                }

        addSheet msg label =
            Ui.button
                [ Events.onClick msg
                ]
                [ text label ]
    in
    Ui.row []
        (addSheet (InsertSheet Sheet.allParams.table) "+table"
            :: addSheet (InsertSheet Sheet.allParams.grid) "+grid"
            :: (NamedAndOrderedStore.toItemList model |> List.map sheetItem)
        )


decoder : Json.Decode.Decoder Model
decoder =
    NamedAndOrderedStore.decoder Sheet.decoder


encode : Model -> Json.Encode.Value
encode =
    NamedAndOrderedStore.encode Sheet.encode
