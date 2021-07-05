module NamedAndOrderedStore exposing
    ( NamedAndOrderedStore
    , commitName
    , current
    , currentId
    , currentName
    , decoder
    , editCurrentName
    , editStatus
    , encode
    , getById
    , getIdByName
    , getNameById
    , init
    , insert
    , isCurrentId
    , isEditing
    , remove
    , rename
    , selectById
    , setCurrent
    , toItemList
    , updateEdit
    )

import DecodeHelpers
import Json.Decode as Decode
import Json.Encode as Encode
import Name exposing (Name)
import PositiveInt exposing (PositiveInt)
import Types
import ZipList as ZL exposing (ZipList)


type NamedAndOrderedStore a
    = Store (Data a)


type alias Data a =
    { nameIndex : Name.Store PositiveInt
    , edit : Maybe String
    , items : ZipList (Item a)
    , nextId : PositiveInt
    }


type alias Item a =
    { id : PositiveInt, name : Name, value : a }



-- INIT


init : a -> NamedAndOrderedStore a
init item =
    let
        initId =
            PositiveInt.one

        initName =
            Name.fromSheetId initId
    in
    Store
        { items = ZL.singleton { id = initId, name = initName, value = item }
        , nameIndex = Name.fromList [ ( initName, initId ) ]
        , nextId = PositiveInt.next initId
        , edit = Nothing
        }


isEditing : NamedAndOrderedStore a -> Bool
isEditing (Store { edit }) =
    case edit of
        Just _ ->
            True

        Nothing ->
            False


currentItem : NamedAndOrderedStore a -> Item a
currentItem (Store { items }) =
    ZL.current items


current : NamedAndOrderedStore a -> a
current store =
    currentItem store |> .value


currentId : NamedAndOrderedStore a -> PositiveInt
currentId store =
    currentItem store |> .id


currentName : NamedAndOrderedStore a -> Name
currentName store =
    currentItem store |> .name


toItemList : NamedAndOrderedStore a -> List (Item a)
toItemList (Store { items }) =
    items |> ZL.toList


isCurrentId : PositiveInt -> NamedAndOrderedStore a -> Bool
isCurrentId id model =
    id == currentId model


selectById : (a -> a) -> PositiveInt -> NamedAndOrderedStore a -> Maybe (NamedAndOrderedStore a)
selectById onBlur selectedId store =
    let
        (Store model) =
            store |> mapCurrent onBlur |> commitName
    in
    ZL.select (.id >> (==) selectedId) model.items
        |> Maybe.map (\items -> { model | items = items })
        |> Maybe.map Store


getById : PositiveInt -> NamedAndOrderedStore a -> Maybe a
getById id (Store { items }) =
    ZL.get (.id >> (==) id) items
        |> Maybe.map .value


getNameById : NamedAndOrderedStore a -> PositiveInt -> Maybe Name
getNameById (Store { items }) id =
    ZL.get (.id >> (==) id) items
        |> Maybe.map .name


getIdByName : NamedAndOrderedStore a -> Name -> Maybe PositiveInt
getIdByName (Store { items }) name =
    ZL.get (.name >> (==) name) items
        |> Maybe.map .id


insert : a -> NamedAndOrderedStore a -> NamedAndOrderedStore a
insert item (Store model) =
    let
        name =
            Name.fromSheetId model.nextId
    in
    Store <|
        { model
            | items = ZL.append [ { id = model.nextId, name = name, value = item } ] model.items
            , nextId = PositiveInt.next model.nextId
            , nameIndex = Name.insert name model.nextId model.nameIndex
        }


remove : PositiveInt -> NamedAndOrderedStore a -> NamedAndOrderedStore a
remove id ((Store data) as model) =
    ZL.filter (.id >> (/=) id) data.items
        |> Maybe.map
            (\items ->
                { data
                    | items = items
                    , nameIndex =
                        Name.remove (currentName model) data.nameIndex
                }
            )
        |> Maybe.withDefault data
        |> Store


rename : PositiveInt -> String -> NamedAndOrderedStore a -> Result Types.Error (NamedAndOrderedStore a)
rename id input ((Store data) as model) =
    let
        updateSheetName : ( Name, Name ) -> Data a -> NamedAndOrderedStore a
        updateSheetName ( newName, oldName ) m =
            let
                renameSheet_ item =
                    { id = item.id
                    , name =
                        if item.id == id then
                            newName

                        else
                            item.name
                    , value = item.value
                    }
            in
            Store
                { m
                    | items = ZL.map renameSheet_ m.items
                    , nameIndex =
                        m.nameIndex
                            |> Name.remove oldName
                            |> Name.insert newName id
                }

        doRename name =
            if Name.member name data.nameIndex then
                Err (Types.DuplicateSheetNameError name)

            else
                getNameById model id
                    |> Maybe.map (Tuple.pair name)
                    |> Result.fromMaybe
                        (Types.UnexpectedError
                            ("Invalid SheetId: " ++ PositiveInt.toString id)
                        )
                    |> Result.map updateSheetName
                    |> Result.map (\updater -> updater data)
    in
    Name.fromString input
        |> Result.fromMaybe Types.InvalidSheetNameError
        |> Result.andThen doRename


setCurrent : NamedAndOrderedStore a -> a -> NamedAndOrderedStore a
setCurrent ((Store data) as model) newSheet =
    { data
        | items = ZL.setCurrent { id = currentId model, name = currentName model, value = newSheet } data.items
    }
        |> Store


commitName : NamedAndOrderedStore a -> NamedAndOrderedStore a
commitName ((Store data) as model) =
    case data.edit of
        Just input ->
            rename (currentId model) input model
                |> Result.withDefault model
                |> (\(Store renamed) -> Store { renamed | edit = Nothing })

        Nothing ->
            model


mapCurrent : (a -> a) -> NamedAndOrderedStore a -> NamedAndOrderedStore a
mapCurrent fn ((Store data) as model) =
    let
        mapValue item =
            { item | value = fn item.value }

        setCurrentItem item =
            { data | items = ZL.setCurrent item data.items }
    in
    currentItem model
        |> mapValue
        |> setCurrentItem
        |> Store


editCurrentName : (a -> a) -> NamedAndOrderedStore a -> NamedAndOrderedStore a
editCurrentName afterEdit ((Store data) as model) =
    { data | edit = Just (Name.toString <| currentName model) }
        |> Store
        |> mapCurrent afterEdit


updateEdit : String -> NamedAndOrderedStore a -> NamedAndOrderedStore a
updateEdit input (Store data) =
    Store <|
        case data.edit of
            Just _ ->
                { data | edit = Just input }

            _ ->
                data


editStatus : NamedAndOrderedStore a -> Maybe String
editStatus (Store { edit }) =
    edit



-- JSON


jsonKeys :
    { items : String
    , nextId : String
    , nameIndex : String
    , id : String
    , value : String
    , name : String
    , edit : String
    , statusType : String
    , statusEditingSheetName : String
    , statusSheetId : String
    , statusInput : String
    , statusNotEditing : String
    }
jsonKeys =
    { items = "items"
    , nextId = "nextId"
    , nameIndex = "nameIndex"
    , id = "id"
    , value = "value"
    , name = "name"
    , edit = "edit"
    , statusType = "statusType"
    , statusEditingSheetName = "statusEditingSheetName"
    , statusSheetId = "statusSheetId"
    , statusInput = "statusInput"
    , statusNotEditing = "statusNotEditing"
    }


decoder : ((Name -> Maybe PositiveInt) -> Decode.Decoder a) -> Decode.Decoder (NamedAndOrderedStore a)
decoder makeValueDecoder =
    let
        getId ids name =
            Name.get name ids

        finalize nameIndex =
            Decode.map3 (Data nameIndex)
                (Decode.field jsonKeys.edit editStatusDecoder)
                (getId nameIndex
                    |> makeValueDecoder
                    |> itemDecoder
                    |> ZL.decoder
                    |> Decode.field jsonKeys.items
                )
                (Decode.field jsonKeys.nextId PositiveInt.decoder)
    in
    Decode.field jsonKeys.nameIndex (Name.storeDecoder PositiveInt.decoder)
        |> Decode.andThen finalize
        |> Decode.map Store


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


itemDecoder : Decode.Decoder a -> Decode.Decoder (Item a)
itemDecoder valueDecoder =
    Decode.map3 Item
        (Decode.field jsonKeys.id PositiveInt.decoder)
        (Decode.field jsonKeys.name Name.decoder)
        (Decode.field jsonKeys.value <| valueDecoder)


encode : ((PositiveInt -> Maybe Name) -> a -> Encode.Value) -> NamedAndOrderedStore a -> Encode.Value
encode makeValueEncoder ((Store data) as store) =
    let
        getName id =
            getNameById store id
    in
    Encode.object
        [ ( jsonKeys.edit, encodeEditStatus data.edit )
        , ( jsonKeys.nextId, PositiveInt.encode data.nextId )
        , ( jsonKeys.nameIndex, Name.encodeStore PositiveInt.encode data.nameIndex )
        , ( jsonKeys.items, ZL.encode (encodeItem (makeValueEncoder getName)) data.items )
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


encodeItem : (a -> Encode.Value) -> Item a -> Encode.Value
encodeItem encodeValue { id, name, value } =
    Encode.object
        [ ( jsonKeys.id, PositiveInt.encode id )
        , ( jsonKeys.name, Name.encode name )
        , ( jsonKeys.value, encodeValue value )
        ]
