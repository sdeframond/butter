module DocumentTest exposing (..)

import Dict as D
import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    let
        expectValue val ( res, _ ) =
            Expect.equal res (Ok val)

        expectError val ( res, _ ) =
            Expect.equal res (Err val)
    in
    describe "Document"
        [ describe "insert"
            [ test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (empty |> insert "a" "qwe")
                        (empty |> insert "a" "qwe" |> insert "a" "qwe")
            ]
        , describe "eval"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedNameError "a")
                        (eval "a" D.empty empty)
            , test "simgle value" <|
                \_ ->
                    expectValue (StringValue "1")
                        (eval "a" D.empty <| fromList [ ( "a", "1" ) ])
            , test "simple formula" <|
                \_ ->
                    expectValue (IntValue -7)
                        (eval "a" D.empty <| fromList [ ( "a", "=1+1-10+1" ) ])
            , test "simple reference" <|
                \_ ->
                    let
                        doc =
                            fromList
                                [ ( "a", "1" )
                                , ( "b", "=a" )
                                ]
                    in
                    expectValue (StringValue "1")
                        (eval "b" D.empty doc)
            , test "cyclic reference" <|
                \_ ->
                    let
                        doc =
                            fromList
                                [ ( "a", "=b" )
                                , ( "b", "=a" )
                                ]
                    in
                    expectError (CyclicReferenceError [ "a", "b" ])
                        (eval "b" D.empty doc)
            , test "complex document" <|
                \_ ->
                    let
                        doc =
                            fromList
                                [ ( "a", "1" )
                                , ( "b", "=1" )
                                , ( "c", "= \"1\" " )
                                , ( "d", "=a" )

                                -- type error in reference
                                , ( "e", "=d+d+1" )

                                -- a more complex cycle
                                , ( "f", "=h+1" )
                                , ( "g", "=b+f" )
                                , ( "h", "=g+1" )
                                ]

                        expected =
                            D.fromList
                                [ ( "a", Ok (StringValue "1") )
                                , ( "b", Ok (IntValue 1) )
                                , ( "c", Ok (StringValue "1") )
                                , ( "d", Ok (StringValue "1") )
                                , ( "e", Err (TypeError "(+) works only on IntValue") )
                                , ( "f", Err (CyclicReferenceError [ "g", "h", "f" ]) )
                                , ( "g", Err (CyclicReferenceError [ "g", "h", "f" ]) )
                                , ( "h", Err (CyclicReferenceError [ "g", "h", "f" ]) )
                                ]
                    in
                    Expect.equal expected (evalAll doc)
            , test "memoized values should be used" <|
                \_ ->
                    let
                        value =
                            IntValue 1

                        memo =
                            D.singleton "memoized" (Ok value)
                    in
                    expectValue value
                        (eval "memoized" memo empty)
            ]
        ]
