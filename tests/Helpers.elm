module Helpers exposing (testCollection)

import Expect
import Test exposing (Test, test)


testCollection : (input -> out) -> List ( String, input, out ) -> List Test
testCollection f collection =
    List.map
        (\( caseName, input, output ) ->
            test caseName
                (\_ -> Expect.equal (f input) output)
        )
        collection
