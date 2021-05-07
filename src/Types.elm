module Types exposing
    ( DataType(..)
    , Error(..)
    , LocatedName
    , SheetId
    , Table
    , Value(..)
    , ValueOrError
    , valueOrErrorToString
    , valueToString
    )

import Name


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
    | UnexpectedError String


type alias Name =
    Name.Name


type alias LocatedName =
    ( SheetId, Name )


type alias SheetId =
    Int


type Value
    = IntValue Int
    | StringValue String
    | TableValue Table


type alias Table =
    { fields : List Name
    , rows : List (Name.Store ValueOrError)
    }


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
                UndefinedLocalReferenceError _ ->
                    ""

                r ->
                    Debug.toString r

        Ok v ->
            valueToString v
