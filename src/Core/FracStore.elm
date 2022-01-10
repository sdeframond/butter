module Core.FracStore exposing
    ( Id
    , Store
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
    , updateCurrentEditedName
    , zipMap
    )

import Core.Frac as Frac exposing (Frac)
import Core.Name as Name exposing (Name)
import Core.PositiveInt as Id
import Core.ZipIdDict as ZipIdDict exposing (ZipIdDict)
import Json.Decode
import Json.Encode


type Store a
    = Store (Data a)


type alias Item a =
    { name : Name
    , value : a
    , editStatus : Maybe String
    , position : Frac
    }


type alias Data a =
    { nameIndex : Name.Store Id
    , items : ZipIdDict (Item a)
    , nextId : Id
    }


type alias Id =
    ZipIdDict.Id



-- INIT


init : Name -> a -> Store a
init initName value =
    let
        initId =
            Id.one
    in
    Store
        { items = ZipIdDict.singleton initId { name = initName, value = value, editStatus = Nothing, position = Frac.init }
        , nextId = Id.next initId
        , nameIndex = Name.fromList [ ( initName, initId ) ]
        }



-- SYNCHRONIZATION


applyContentFrom : (a -> a -> a) -> Store a -> Store a -> Store a
applyContentFrom mergeValue remote origin =
    let
        mergeSubItems (Store data) =
            Store { data | items = ZipIdDict.map mergeItem data.items }

        mergeItem id item =
            getById id origin
                |> Maybe.map (\value -> { item | value = mergeValue item.value value })
                |> Maybe.withDefault item

        selectById id (Store data) =
            Store { data | items = ZipIdDict.select id data.items |> Maybe.withDefault data.items }
    in
    remote
        |> selectById (currentId origin)
        |> mergeSubItems



-- GETTERS/SETTERS


current : Store a -> a
current (Store data) =
    ZipIdDict.getCurrentItem data.items |> .value


setCurrent : Store a -> a -> Store a
setCurrent ((Store data) as store) value =
    let
        newItems =
            data.items
                |> ZipIdDict.setCurrentItem
                    { name = currentName store
                    , value = value
                    , editStatus = Nothing
                    , position = currentPosition store
                    }
    in
    Store { data | items = newItems }


currentId : Store a -> Id
currentId (Store data) =
    ZipIdDict.getCurrentId data.items


currentName : Store a -> Name
currentName (Store data) =
    ZipIdDict.getCurrentItem data.items |> .name


currentPosition : Store a -> Frac
currentPosition (Store data) =
    ZipIdDict.getCurrentItem data.items |> .position


getById : Id -> Store a -> Maybe a
getById id (Store { items }) =
    ZipIdDict.get id items |> Maybe.map .value


getNameById : Store a -> Id -> Maybe Name
getNameById (Store data) id =
    ZipIdDict.get id data.items
        |> Maybe.map .name


getIdByName : Store a -> Name -> Maybe Id
getIdByName (Store data) name =
    Name.get name data.nameIndex



-- ITEMS MANIPULATION


compareItems : Item a -> Item a -> Order
compareItems a b =
    Frac.compare a.position b.position


insert : Name -> a -> Store a -> Store a
insert name value (Store data) =
    let
        finalName =
            if Name.member name data.nameIndex then
                nameHelp name 1

            else
                name

        nameHelp name_ i =
            let
                nextName =
                    Name.appendInt name_ i
            in
            if Name.member nextName data.nameIndex then
                nameHelp name_ (i + 1)

            else
                nextName

        item : Item a
        item =
            { name = finalName
            , value = value
            , editStatus = Nothing
            , position =
                ZipIdDict.getNextItem compareItems data.items
                    |> Maybe.map .position
                    |> Maybe.withDefault Frac.max
                    |> Frac.between (ZipIdDict.getCurrentItem data.items |> .position)
            }
    in
    Store
        { data
            | items = ZipIdDict.insert data.nextId item data.items
            , nameIndex = Name.insert finalName data.nextId data.nameIndex
            , nextId = Id.next data.nextId
        }


remove : Id -> Store a -> Store a
remove id ((Store data) as store) =
    let
        newItems =
            ZipIdDict.remove id data.items
    in
    Store
        { data
            | items = newItems
            , nameIndex =
                if newItems == data.items then
                    data.nameIndex

                else
                    getNameById store id
                        |> Maybe.map (\name -> Name.remove name data.nameIndex)
                        |> Maybe.withDefault data.nameIndex
        }



-- NAME EDITION


cancelEdits : Store a -> Store a
cancelEdits (Store data) =
    let
        cancel _ item =
            { item | editStatus = Nothing }
    in
    Store { data | items = ZipIdDict.map cancel data.items }


apply : a -> (a -> b) -> b
apply a f =
    f a


updateCurrentItem : (Item a -> Item a) -> Store a -> Store a
updateCurrentItem func (Store data) =
    Store
        { data
            | items =
                data.items
                    |> ZipIdDict.getCurrentItem
                    |> func
                    |> ZipIdDict.setCurrentItem
                    |> apply data.items
        }


editCurrentName : Store a -> Store a
editCurrentName store =
    let
        newCurrentItem currentItem =
            { currentItem
                | editStatus =
                    currentItem.name
                        |> Name.toString
                        |> Just
            }
    in
    updateCurrentItem newCurrentItem store


updateCurrentEditedName : String -> Store a -> Store a
updateCurrentEditedName input store =
    updateCurrentItem (\item -> { item | editStatus = Just input }) store


getCurrentEditStatus : Store a -> Maybe String
getCurrentEditStatus (Store data) =
    ZipIdDict.getCurrentItem data.items |> .editStatus


commitEdits : Store a -> Store a
commitEdits (Store data) =
    let
        commitItem : Name.Store Id -> Id -> Item a -> ( Name.Store Id, Item a )
        commitItem nameIndex id item =
            case item.editStatus of
                Nothing ->
                    ( nameIndex, item )

                Just str ->
                    renameItem id nameIndex str item

        renameItem : Id -> Name.Store Id -> String -> Item a -> ( Name.Store Id, Item a )
        renameItem id nameIndex str item =
            let
                newName =
                    Name.fromString str |> Maybe.withDefault item.name
            in
            if Name.member newName nameIndex then
                ( nameIndex, { item | editStatus = Nothing } )

            else
                ( data.nameIndex
                    |> Name.remove item.name
                    |> Name.insert newName id
                , { item
                    | name = newName
                    , editStatus = Nothing
                  }
                )

        ( newIndex, newItems ) =
            ZipIdDict.mapState commitItem data.nameIndex data.items
    in
    Store { data | items = newItems, nameIndex = newIndex }



-- SELECTION


zipMap : (Store a -> Bool -> b) -> Store a -> List b
zipMap f (Store data) =
    let
        go items isCurrent =
            f (Store { data | items = items }) isCurrent
    in
    ZipIdDict.zipMap compareItems go data.items



-- JSON


encode : ((Id -> Maybe Name) -> a -> Json.Encode.Value) -> Store a -> Json.Encode.Value
encode makeValueEncoder ((Store data) as store) =
    let
        getName id =
            getNameById store id
    in
    Json.Encode.object
        [ ( "nextId", Id.encode data.nextId )
        , ( "nameIndex", Name.encodeStore Id.encode data.nameIndex )
        , ( "items", ZipIdDict.encode (encodeItem (makeValueEncoder getName)) data.items )
        ]


encodeItem : (a -> Json.Encode.Value) -> Item a -> Json.Encode.Value
encodeItem encodeValue { name, value, position } =
    Json.Encode.object
        [ ( "name", Name.encode name )
        , ( "value", encodeValue value )
        , ( "position", Frac.encode position )
        ]


decoder : ((Name -> Maybe Id) -> Json.Decode.Decoder a) -> Json.Decode.Decoder (Store a)
decoder makeValueDecoder =
    let
        getId ids name =
            Name.get name ids

        finalize nameIndex =
            Json.Decode.map2 (Data nameIndex)
                (getId nameIndex
                    |> makeValueDecoder
                    |> itemDecoder
                    |> ZipIdDict.decoder
                    |> Json.Decode.field "items"
                )
                (Json.Decode.field "nextId" Id.decoder)
    in
    Json.Decode.field "nameIndex" (Name.storeDecoder Id.decoder)
        |> Json.Decode.andThen finalize
        |> Json.Decode.map Store


itemDecoder : Json.Decode.Decoder a -> Json.Decode.Decoder (Item a)
itemDecoder valueDecoder =
    Json.Decode.map4 Item
        (Json.Decode.field "name" Name.decoder)
        (Json.Decode.field "value" valueDecoder)
        (Json.Decode.succeed Nothing)
        (Json.Decode.field "position" Frac.decoder)
