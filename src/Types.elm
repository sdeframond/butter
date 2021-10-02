module Types exposing
    ( DataType(..)
    , Error(..)
    , LocatedName
    , SheetId
    , Table
    , Value(..)
    , ValueOrError
    , dataTypeDecoder
    , encodeDataType
    , encodeTable
    , errorToString
    , tableDecoder
    , valueOrErrorToString
    , valueToString
    )

import DecodeHelpers
import Json.Decode as Decode
import Json.Encode as Encode
import Name exposing (Name)
import PositiveInt exposing (PositiveInt)


type Error
    = ParsingError
    | UndefinedGlobalReferenceError String
    | UndefinedLocalReferenceError Name
    | TypeError String
    | CyclicReferenceError (List String)
    | UndefinedSheetError Name
    | RemovingLastSheetError Name -- what is this doing here ?
    | InvalidSheetNameError
    | DuplicateSheetNameError Name
    | UnexpectedError String


errorToString : Error -> String
errorToString error =
    error |> encodeError |> Encode.encode 0


errorDecoder : Decode.Decoder Error
errorDecoder =
    let
        decoders =
            [ ( "ParsingError"
              , Decode.succeed ParsingError
              )
            , ( "UndefinedGlobalReferenceError"
              , Decode.map UndefinedGlobalReferenceError (Decode.field "msg" Decode.string)
              )
            , ( "UndefinedLocalReferenceError"
              , Decode.map UndefinedLocalReferenceError (Decode.field "name" Name.decoder)
              )
            , ( "TypeError"
              , Decode.map TypeError (Decode.field "msg" Decode.string)
              )
            , ( "CyclicReferenceError"
              , Decode.map CyclicReferenceError (Decode.field "cycle" <| Decode.list Decode.string)
              )
            , ( "UndefinedSheetError"
              , Decode.map UndefinedSheetError (Decode.field "name" Name.decoder)
              )
            , ( "RemovingLastSheetError"
              , Decode.map RemovingLastSheetError (Decode.field "name" Name.decoder)
              )
            , ( "InvalidSheetNameError"
              , Decode.succeed InvalidSheetNameError
              )
            , ( "DuplicateSheetNameError"
              , Decode.map DuplicateSheetNameError (Decode.field "name" Name.decoder)
              )
            , ( "UnexpectedError"
              , Decode.map UnexpectedError (Decode.field "msg" Decode.string)
              )
            ]
    in
    Decode.field "type" Decode.string
        |> DecodeHelpers.switch "Invalid error" decoders


encodeError : Error -> Encode.Value
encodeError error =
    case error of
        ParsingError ->
            Encode.object [ ( "type", Encode.string "ParsingError" ) ]

        UndefinedGlobalReferenceError msg ->
            Encode.object
                [ ( "type", Encode.string "UndefinedGlobalReferenceError" )
                , ( "msg", Encode.string msg )
                ]

        UndefinedLocalReferenceError name ->
            Encode.object
                [ ( "type", Encode.string "UndefinedLocalReferenceError" )
                , ( "name", Name.encode name )
                ]

        TypeError msg ->
            Encode.object
                [ ( "type", Encode.string "TypeError" )
                , ( "msg", Encode.string msg )
                ]

        CyclicReferenceError refList ->
            Encode.object
                [ ( "type", Encode.string "CyclicReferenceError" )
                , ( "cycle", Encode.list Encode.string refList )
                ]

        UndefinedSheetError name ->
            Encode.object
                [ ( "type", Encode.string "UndefinedSheetError" )
                , ( "name", Name.encode name )
                ]

        RemovingLastSheetError name ->
            Encode.object
                [ ( "type", Encode.string "RemovingLastSheetError" )
                , ( "name", Name.encode name )
                ]

        InvalidSheetNameError ->
            Encode.object [ ( "type", Encode.string "InvalidSheetNameError" ) ]

        DuplicateSheetNameError name ->
            Encode.object
                [ ( "type", Encode.string "DuplicateSheetNameError" )
                , ( "name", Name.encode name )
                ]

        UnexpectedError msg ->
            Encode.object
                [ ( "type", Encode.string "UnexpectedError" )
                , ( "msg", Encode.string msg )
                ]


type alias LocatedName =
    ( SheetId, Name )


type alias SheetId =
    PositiveInt


type Value
    = IntValue Int
    | StringValue String


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.field "int" <| Decode.map IntValue Decode.int
        , Decode.field "string" <| Decode.map StringValue Decode.string
        ]


encodeValue : Value -> Encode.Value
encodeValue value =
    case value of
        IntValue i ->
            Encode.object [ ( "int", Encode.int i ) ]

        StringValue s ->
            Encode.object [ ( "string", Encode.string s ) ]


type alias Table =
    { fields : List Name
    , rows : List (Name.Store ValueOrError)
    }


tableDecoder : Decode.Decoder Table
tableDecoder =
    Decode.map2 Table
        (Decode.field "fields" <| Decode.list Name.decoder)
        (Decode.field "rows" <|
            Decode.list (Name.storeDecoder valueOrErrorDecoder)
        )


encodeTable : Table -> Encode.Value
encodeTable table =
    Encode.object
        [ ( "fields", Encode.list Name.encode table.fields )
        , ( "rows", Encode.list (Name.encodeStore encodeValueOrError) table.rows )
        ]


type alias ValueOrError =
    Result Error Value


valueOrErrorDecoder : Decode.Decoder ValueOrError
valueOrErrorDecoder =
    Decode.oneOf
        [ Decode.field "value" (Decode.map Ok valueDecoder)
        , Decode.field "error" (Decode.map Err errorDecoder)
        ]


encodeValueOrError : ValueOrError -> Encode.Value
encodeValueOrError voe =
    case voe of
        Ok value ->
            Encode.object [ ( "value", encodeValue value ) ]

        Err error ->
            Encode.object [ ( "value", encodeError error ) ]


type DataType
    = IntType
    | StringType


dataTypeDecoder : Decode.Decoder DataType
dataTypeDecoder =
    let
        switchType type_ =
            case type_ of
                "int" ->
                    Decode.succeed IntType

                "string" ->
                    Decode.succeed StringType

                _ ->
                    Decode.fail ("Invalid data type: " ++ type_)
    in
    Decode.string |> Decode.andThen switchType


encodeDataType : DataType -> Encode.Value
encodeDataType dt =
    case dt of
        IntType ->
            Encode.string "int"

        StringType ->
            Encode.string "string"


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
                UndefinedLocalReferenceError _ ->
                    ""

                r ->
                    Debug.toString r

        Ok v ->
            valueToString v
