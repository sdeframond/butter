module Document.Types exposing
    ( Error(..)
    , LocatedName
    , Name
    , Position(..)
    , Value(..)
    , ValueOrError
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


type Position a
    = Before a
    | Current a
    | After a
