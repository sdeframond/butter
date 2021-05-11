module Helpers exposing (testCollection)

import Expect
import Test exposing (Test, describe, test)


testCollection : String -> (input -> out) -> List ( String, input, out ) -> Test
testCollection title f collection =
    describe title <|
        List.map
            (\( caseName, input, output ) ->
                test caseName
                    (\_ -> Expect.equal (f input) output)
            )
            collection
