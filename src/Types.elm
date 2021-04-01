module Types exposing
    ( DataType(..)
    , Error(..)
    , LocatedName
    , Name
    , Position(..)
    , Value(..)
    , ValueOrError
    , valueOrErrorToString
    , valueToString
    )


type Error
    = ParsingError
    | UndefinedNameError LocatedName
    | UndefinedLocalReference Name
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


valueOrErrorToString : ValueOrError -> String
valueOrErrorToString val =
    case val of
        Err e ->
            case e of
                UndefinedNameError _ ->
                    ""

                r ->
                    Debug.toString r

        Ok v ->
            valueToString v


type Position a
    = Before a
    | Current a
    | After a
