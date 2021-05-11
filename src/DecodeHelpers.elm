module DecodeHelpers exposing (switch)

import Dict
import Json.Decode as Decode exposing (Decoder)


switch : String -> List ( String, Decoder a ) -> Decoder String -> Decoder a
switch notFoundMsg groups =
    Decode.andThen
        (\g ->
            Dict.get g (groups |> Dict.fromList)
                |> Maybe.withDefault (Decode.fail <| notFoundMsg ++ ": " ++ g)
        )
