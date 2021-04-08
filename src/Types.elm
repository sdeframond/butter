module Types exposing
    ( DataType(..)
    , Error(..)
    , LocatedName
    , Name
    , Value(..)
    , ValueOrError
    , valueOrErrorToString
    , valueToString
    )
import Dict exposing (Dict)


type Error
    = ParsingError
    | UndefinedGlobalReferenceError LocatedName
    | UndefinedLocalReferenceError Name
    | TypeError String
    | CyclicReferenceError (List LocatedName)
    | UndefinedSheetError Name
    | RemovingLastSheetError Name
    | InvalidSheetNameError
    | DuplicateSheetNameError Name


type alias Name =
    String


type alias LocatedName =
    ( Name, Name )


type Value
    = IntValue Int
    | StringValue String
    | TableValue Table

type alias Table =
    { fields : List Name
    , rows : List Record
    }

type alias Record = Dict Name ValueOrError

type alias ValueOrError =
    Result Error Value


type DataType
    = IntType
    | StringType


valueToString : Value -> String
valueToString value =
    case value of
        IntValue i ->
            String.fromInt i

        StringValue s ->
            s
        TableValue _ ->
            "<table>"


valueOrErrorToString : ValueOrError -> String
valueOrErrorToString val =
    case val of
        Err e ->
            case e of
                UndefinedGlobalReferenceError _ ->
                    ""

                r ->
                    Debug.toString r

        Ok v ->
            valueToString v
