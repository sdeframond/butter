module DecodeHelpers exposing (switch)

import Dict
import Json.Decode as Decode exposing (Decoder)


switch : String -> List ( String, Decoder a ) -> Decoder String -> Decoder a
switch notFoundMsg groups keyDecoder =
    let
        getDecoder key =
            Dict.get key (groups |> Dict.fromList)
                |> Maybe.withDefault (Decode.fail <| notFoundMsg ++ ": " ++ key)
    in
    Decode.andThen getDecoder keyDecoder
