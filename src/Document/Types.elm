module Document.Types exposing
    ( Error(..)
    , LocatedName
    , Name
    , Position(..)
    , Value(..)
    , ValueOrError, valueOrErrorToString
    )


type Error
    = ParsingError
    | UndefinedNameError LocatedName
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
            case v of
                IntValue i ->
                    String.fromInt i

                StringValue s ->
                    s

type Position a
    = Before a
    | Current a
    | After a
