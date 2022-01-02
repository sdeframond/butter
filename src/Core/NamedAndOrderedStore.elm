module Core.NamedAndOrderedStore exposing
    ( Id
    , NamedAndOrderedStore
    , applyContentFrom
    , cancelEdits
    , commitEdits
    , current
    , currentId
    , currentName
    , decoder
    , editCurrentName
    , encode
    , getById
    , getCurrentEditStatus
    , getIdByName
    , getNameById
    , init
    , insert
    , remove
    , setCurrent
    , setName
    , toItemList
    , updateCurrentEditedName
    , zipMap
    )

import Core.Name as Name exposing (Name)
import Core.PositiveInt as Id exposing (PositiveInt)
import Core.Types as Types
import Core.ZipList as ZL exposing (ZipList)
import Json.Decode as Decode
import Json.Encode as Encode


type NamedAndOrderedStore a
    = Store (Data a)


type alias Data a =
    { nameIndex : Name.Store PositiveInt
    , items : ZipList (Item a)
    , nextId : PositiveInt
    }


type alias Item a =
    { id : PositiveInt, name : Name, value : a, editStatus : Maybe String }


type alias Id =
    PositiveInt



-- INIT


init : Name -> a -> NamedAndOrderedStore a
init initName item =
    let
        initId =
            Id.one
    in
    Store
        { items = ZL.singleton { id = initId, name = initName, value = item, editStatus = Nothing }
        , nameIndex = Name.fromList [ ( initName, initId ) ]
        , nextId = Id.next initId
        }


currentItem : NamedAndOrderedStore a -> Item a
currentItem (Store { items }) =
    ZL.current items


current : NamedAndOrderedStore a -> a
current store =
    currentItem store |> .value


currentId : NamedAndOrderedStore a -> Id
currentId store =
    currentItem store |> .id


currentName : NamedAndOrderedStore a -> Name
currentName store =
    currentItem store |> .name


cancelEdits : NamedAndOrderedStore a -> NamedAndOrderedStore a
cancelEdits (Store data) =
    let
        items =
            data.items
                |> ZL.map (\i -> { i | editStatus = Nothing })
    in
    Store { data | items = items }


commitEdits : NamedAndOrderedStore a -> NamedAndOrderedStore a
commitEdits ((Store data) as store) =
    let
        commitItem nameIndex item =
            case item.editStatus of
                Just str ->
                    let
                        newName =
                            Name.fromString str |> Maybe.withDefault item.name
                    in
                    if Name.member newName nameIndex then
                        ( nameIndex, { item | editStatus = Nothing } )

                    else
                        ( data.nameIndex
                            |> Name.remove item.name
                            |> Name.insert newName (currentId store)
                        , { item
                            | name = newName
                            , editStatus = Nothing
                          }
                        )

                Nothing ->
                    ( nameIndex, item )

        ( newNameIndex, newItems ) =
            ZL.mapState commitItem data.nameIndex data.items
    in
    Store { data | items = newItems, nameIndex = newNameIndex }


setCurrentItem : Item a -> NamedAndOrderedStore a -> NamedAndOrderedStore a
setCurrentItem item (Store data) =
    Store { data | items = ZL.setCurrent item data.items }


updateCurrentItem : (Item a -> Item a) -> NamedAndOrderedStore a -> NamedAndOrderedStore a
updateCurrentItem f store =
    let
        item =
            currentItem store |> f
    in
    setCurrentItem item store


editCurrentName : NamedAndOrderedStore a -> NamedAndOrderedStore a
editCurrentName store =
    updateCurrentItem
        (\item -> { item | editStatus = store |> currentName |> Name.toString |> Just })
        store


updateCurrentEditedName : String -> NamedAndOrderedStore a -> NamedAndOrderedStore a
updateCurrentEditedName input store =
    updateCurrentItem (\item -> { item | editStatus = Just input }) store


getCurrentEditStatus : NamedAndOrderedStore a -> Maybe String
getCurrentEditStatus =
    currentItem >> .editStatus


toItemList : NamedAndOrderedStore a -> List (Item a)
toItemList (Store { items }) =
    items |> ZL.toList


zipMap : (NamedAndOrderedStore a -> Bool -> b) -> NamedAndOrderedStore a -> List b
zipMap f (Store data) =
    let
        go items isCurrent =
            f (Store { data | items = items }) isCurrent
    in
    ZL.zipMap go data.items


applyContentFrom : (a -> a -> a) -> NamedAndOrderedStore a -> NamedAndOrderedStore a -> NamedAndOrderedStore a
applyContentFrom mergeValue remote origin =
    let
        mergeSubItems (Store data) =
            Store { data | items = ZL.map mergeItem data.items }

        mergeItem item =
            getById item.id origin
                |> Maybe.map (\value -> { item | value = mergeValue item.value value })
                |> Maybe.withDefault item
    in
    remote
        |> selectById identity (currentId origin)
        |> Maybe.withDefault remote
        |> mergeSubItems


selectById : (a -> a) -> Id -> NamedAndOrderedStore a -> Maybe (NamedAndOrderedStore a)
selectById onBlur selectedId store =
    let
        (Store model) =
            store |> mapCurrent onBlur
    in
    ZL.select (.id >> (==) selectedId) model.items
        |> Maybe.map (\items -> { model | items = items })
        |> Maybe.map Store


getById : Id -> NamedAndOrderedStore a -> Maybe a
getById id (Store { items }) =
    ZL.get (.id >> (==) id) items
        |> Maybe.map .value


getNameById : NamedAndOrderedStore a -> Id -> Maybe Name
getNameById (Store { items }) id =
    ZL.get (.id >> (==) id) items
        |> Maybe.map .name


getIdByName : NamedAndOrderedStore a -> Name -> Maybe Id
getIdByName (Store { items }) name =
    ZL.get (.name >> (==) name) items
        |> Maybe.map .id


insert : Name -> a -> NamedAndOrderedStore a -> NamedAndOrderedStore a
insert name item (Store model) =
    let
        finalName =
            if Name.member name model.nameIndex then
                nameHelp name 1

            else
                name

        nameHelp name_ i =
            let
                nextName =
                    Name.appendInt name_ i
            in
            if Name.member nextName model.nameIndex then
                nameHelp name_ (i + 1)

            else
                nextName
    in
    Store <|
        { model
            | items = ZL.insertAfter { id = model.nextId, name = finalName, value = item, editStatus = Nothing } model.items
            , nextId = Id.next model.nextId
            , nameIndex = Name.insert finalName model.nextId model.nameIndex
        }


remove : Id -> NamedAndOrderedStore a -> NamedAndOrderedStore a
remove id (Store data) =
    ZL.get (.id >> (==) id) data.items
        |> Maybe.andThen
            (\toDelete ->
                ZL.filter (.id >> (/=) id) data.items
                    |> Maybe.map
                        (\items ->
                            { data
                                | items = items
                                , nameIndex =
                                    Name.remove toDelete.name data.nameIndex
                            }
                        )
            )
        |> Maybe.withDefault data
        |> Store


setName : Id -> Name -> NamedAndOrderedStore a -> Result Types.Error (NamedAndOrderedStore a)
setName id name ((Store data) as model) =
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
                    , editStatus = Nothing
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

        isDuplicate =
            Name.get name data.nameIndex
                |> Maybe.map ((/=) id)
                |> Maybe.withDefault False
    in
    if isDuplicate then
        Err (Types.DuplicateSheetNameError name)

    else
        getNameById model id
            |> Maybe.map (Tuple.pair name)
            |> Result.fromMaybe
                (Types.UnexpectedError
                    ("Invalid SheetId: " ++ Id.toString id)
                )
            |> Result.map updateSheetName
            |> Result.map (\updater -> updater data)


setCurrent : NamedAndOrderedStore a -> a -> NamedAndOrderedStore a
setCurrent ((Store data) as model) newSheet =
    { data
        | items = ZL.setCurrent { id = currentId model, name = currentName model, value = newSheet, editStatus = Nothing } data.items
    }
        |> Store


mapCurrent : (a -> a) -> NamedAndOrderedStore a -> NamedAndOrderedStore a
mapCurrent fn store =
    let
        mapValue item =
            { item | value = fn item.value }
    in
    currentItem store
        |> mapValue
        |> (\item -> setCurrentItem item store)



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


decoder : ((Name -> Maybe Id) -> Decode.Decoder a) -> Decode.Decoder (NamedAndOrderedStore a)
decoder makeValueDecoder =
    let
        getId ids name =
            Name.get name ids

        finalize nameIndex =
            Decode.map2 (Data nameIndex)
                (getId nameIndex
                    |> makeValueDecoder
                    |> itemDecoder
                    |> ZL.decoder
                    |> Decode.field jsonKeys.items
                )
                (Decode.field jsonKeys.nextId Id.decoder)
    in
    Decode.field jsonKeys.nameIndex (Name.storeDecoder Id.decoder)
        |> Decode.andThen finalize
        |> Decode.map Store


itemDecoder : Decode.Decoder a -> Decode.Decoder (Item a)
itemDecoder valueDecoder =
    Decode.map4 Item
        (Decode.field jsonKeys.id Id.decoder)
        (Decode.field jsonKeys.name Name.decoder)
        (Decode.field jsonKeys.value <| valueDecoder)
        (Decode.succeed Nothing)


encode : ((Id -> Maybe Name) -> a -> Encode.Value) -> NamedAndOrderedStore a -> Encode.Value
encode makeValueEncoder ((Store data) as store) =
    let
        getName id =
            getNameById store id
    in
    Encode.object
        [ ( jsonKeys.nextId, Id.encode data.nextId )
        , ( jsonKeys.nameIndex, Name.encodeStore Id.encode data.nameIndex )
        , ( jsonKeys.items, ZL.encode (encodeItem (makeValueEncoder getName)) data.items )
        ]


encodeItem : (a -> Encode.Value) -> Item a -> Encode.Value
encodeItem encodeValue { id, name, value } =
    Encode.object
        [ ( jsonKeys.id, Id.encode id )
        , ( jsonKeys.name, Name.encode name )
        , ( jsonKeys.value, encodeValue value )
        ]
