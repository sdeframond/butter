module DocumentTest exposing (..)

import Dict as D
import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import List as L
import Result as R
import String as S
import Test exposing (..)


suite : Test
suite =
    let
        expectValue val res =
            Expect.equal res (Ok val)

        expectError val res =
            Expect.equal res (Err val)
    in
    describe "Document"
        [ describe "insert"
            [ test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (empty |> insert "sheet" "a" "qwe")
                        (empty |> insert "sheet" "a" "qwe" |> insert "sheet" "a" "qwe")
            , test "does not change sheet order" <|
                \_ ->
                    let
                        doc =
                            singleSheet "sheet1" |> insertSheet "sheet2"
                    in
                    Expect.equal
                        (sheets doc)
                        (insert "sheet1" "a" "1" doc |> sheets)
            ]
        , describe "cellSource"
            [ test "retrieves the source of a cell" <|
                \_ ->
                    expectValue "src"
                        (empty |> insert "sheet" "a" "src" |> cellSource "sheet" "a")
            , test "returns an error when the cell is not set" <|
                \_ ->
                    expectError (UndefinedSheetError "sheet")
                        (empty |> cellSource "sheet" "a")
            ]
        , describe "sheets"
            [ test "returns all sheets" <|
                \_ -> Expect.equal (sheets <| singleSheet "sheet") [ "sheet" ]
            ]
        , describe "insertSheet"
            [ test "should insert a sheet" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet" |> insertSheet "toto" |> sheets)
                        [ "sheet", "toto" ]
            , test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (empty |> insertSheet "sheet")
                        (empty |> insertSheet "sheet" |> insertSheet "sheet")
            ]
        , describe "removeSheet"
            [ test "removes a given sheet" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> removeSheet "sheet"
                            |> sheets
                        )
                        [ "toto" ]
            , test "does not removes the other sheets' cells" <|
                \_ ->
                    expectValue (StringValue "1")
                        (singleSheet "sheet"
                            |> insert "other sheet" "a" "1"
                            |> get "other sheet" "a"
                        )
            , test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> removeSheet "sheet"
                            |> removeSheet "sheet"
                            |> sheets
                        )
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> removeSheet "sheet"
                            |> sheets
                        )
            ]
        , describe "renameSheet"
            [ test "it renames a sheet" <|
                \_ ->
                    Expect.equal (Ok [ "renamed" ])
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "renamed"
                            |> R.map sheets
                        )
            , test "it renames only if the sheet exists" <|
                \_ ->
                    Expect.equal (Ok [ "sheet" ])
                        (singleSheet "sheet"
                            |> renameSheet "doesNotExist" "renamed"
                            |> R.map sheets
                        )
            , test "it renames only one sheet" <|
                \_ ->
                    Expect.equal (Ok [ "sheet1", "renamed", "sheet3" ])
                        (singleSheet "sheet1"
                            |> insertSheet "sheet2"
                            |> insertSheet "sheet3"
                            |> renameSheet "sheet2" "renamed"
                            |> R.map sheets
                        )
            , test "it enforces name unicity" <|
                \_ ->
                    Expect.equal (Err DuplicateSheetNameError)
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "sheet"
                        )
            , test "it enforces name format" <|
                \_ ->
                    Expect.equal (Err InvalidSheetNameError)
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "not a valid sheet name"
                        )
            ]
        , describe "get"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (singleSheet "sheet" |> get "sheet" "a")
            , test "single value" <|
                \_ ->
                    expectValue (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ] |> get "sheet" "a")
            , test "simple formula" <|
                \_ ->
                    expectValue (IntValue -7)
                        (get "sheet" "a" <| fromList "sheet" [ ( "a", "=1+1-10+1" ) ])
            , fuzz (tuple ( int, int )) "Fuzzing substraction" <|
                \( i, j ) ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "-", String.fromInt j ] ) ]
                    in
                    expectValue (IntValue (i - j)) (get "sheet" "a" doc)
            , fuzz (tuple ( int, int )) "Fuzzing addition" <|
                \( i, j ) ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "+", String.fromInt j ] ) ]
                    in
                    expectValue (IntValue (i + j)) (get "sheet" "a" doc)
            , test "empty string" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (get "sheet" "a" <| fromList "sheet" [ ( "a", "" ) ])
            , test "simple reference" <|
                \_ ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", "1" )
                                , ( "b", "=a" )
                                ]
                    in
                    expectValue (StringValue "1")
                        (get "sheet" "b" doc)
            , test "cyclic reference" <|
                \_ ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", "=b" )
                                , ( "b", "=a" )
                                ]
                    in
                    expectError (CyclicReferenceError [ ( "sheet", "a" ), ( "sheet", "b" ) ])
                        (get "sheet" "b" doc)
            , test "absolute reference" <|
                \_ ->
                    expectValue (StringValue "1")
                        (fromList "sheet1" [ ( "a", "=sheet2.b" ) ] |> insert "sheet2" "b" "1" |> get "sheet1" "a")
            , test "missing sheet" <|
                \_ ->
                    expectError (UndefinedSheetError "sheet")
                        (empty |> get "sheet" "a")
            , describe "complex document" <|
                let
                    data =
                        [ ( "a", "1", Ok (StringValue "1") )
                        , ( "b", "=1", Ok (IntValue 1) )
                        , ( "c", "= \"1\" ", Ok (StringValue "1") )
                        , ( "d", "=a", Ok (StringValue "1") )
                        , ( "e", "=d+d+1", Err (TypeError "(+) works only on IntValue") )
                        , ( "f"
                          , "=h+1"
                          , Err
                                (CyclicReferenceError
                                    [ ( "sheet", "g" )
                                    , ( "sheet", "h" )
                                    , ( "sheet", "f" )
                                    ]
                                )
                          )
                        , ( "g"
                          , "=b+f"
                          , Err
                                (CyclicReferenceError
                                    [ ( "sheet", "h" )
                                    , ( "sheet", "f" )
                                    , ( "sheet", "g" )
                                    ]
                                )
                          )
                        , ( "h"
                          , "=g+1"
                          , Err
                                (CyclicReferenceError
                                    [ ( "sheet", "f" )
                                    , ( "sheet", "g" )
                                    , ( "sheet", "h" )
                                    ]
                                )
                          )
                        ]

                    doc =
                        data |> L.map (\( name, src, res ) -> ( name, src )) |> fromList "sheet"
                in
                L.map
                    (\( name, src, res ) ->
                        test (name ++ ":" ++ src) (\_ -> Expect.equal (get "sheet" name doc) res)
                    )
                    data
            ]
        ]
