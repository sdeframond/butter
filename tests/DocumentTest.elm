module DocumentTest exposing (..)

import Dict as D
import Document exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import List as L
import Maybe as M
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
                        (singleSheet "sheet" |> insert "a" "qwe")
                        (singleSheet "sheet" |> insert "a" "qwe" |> insert "a" "qwe")
            , test "does not change sheet order" <|
                \_ ->
                    let
                        doc =
                            singleSheet "sheet1" |> insertSheet "sheet2"
                    in
                    Expect.equal
                        (sheets doc)
                        (insert "a" "1" doc |> sheets)
            ]
        , describe "cellSource"
            [ test "retrieves the source of a cell" <|
                \_ ->
                    expectValue "src"
                        (singleSheet "sheet" |> insert "a" "src" |> cellSource "a")

            --, test "returns an error when the cell is not set" <|
            --    \_ ->
            --        expectError (UndefinedSheetError "sheet")
            --            (empty |> cellSource "sheet" "a")
            ]
        , describe "sheets"
            [ test "returns all sheets" <|
                \_ -> Expect.equal (sheets <| singleSheet "sheet") [ Current "sheet" ]
            ]
        , describe "selectSheet"
            [ test "selects a sheet" <|
                \_ ->
                    expectValue [ Before "sheet", Current "sheet2", After "sheet3" ]
                        (singleSheet "sheet"
                            |> insertSheet "sheet2"
                            |> insertSheet "sheet3"
                            |> selectSheet "sheet2"
                            |> R.map sheets
                        )
            , test "returns an error if the selected sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (singleSheet "sheet"
                            |> selectSheet "toto"
                        )
            ]
        , describe "insertSheet"
            [ test "should insert a sheet" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet" |> insertSheet "toto" |> sheets)
                        [ Current "sheet", After "toto" ]
            , test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet")
                        (singleSheet "sheet" |> insertSheet "sheet")
            ]
        , describe "removeSheet"
            [ test "removes a given sheet" <|
                \_ ->
                    Expect.equal
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> removeSheet "sheet"
                            |> R.map sheets
                        )
                        (Ok [ Current "toto" ])
            , test "does not removes the other sheets' cells" <|
                \_ ->
                    expectValue (StringValue "1")
                        (singleSheet "sheet"
                            |> insertSheet "otherSheet"
                            |> selectSheet "otherSheet"
                            |> R.map (insert "a" "1")
                            |> R.andThen (removeSheet "sheet")
                            |> R.andThen (get "a")
                        )
            , test "returns an error when the sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> removeSheet "toto"
                            |> R.andThen (removeSheet "toto")
                        )
            , test "returns an error when removing the last sheet" <|
                \_ ->
                    expectError (RemovingLastSheetError "sheet")
                        (singleSheet "sheet"
                            |> removeSheet "sheet"
                        )
            ]
        , describe "renameSheet"
            [ test "renames a sheet" <|
                \_ ->
                    Expect.equal (Ok [ Current "renamed" ])
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "renamed"
                            |> R.map sheets
                        )
            , test "renames only if the sheet exists" <|
                \_ ->
                    Expect.equal (Ok [ Current "sheet" ])
                        (singleSheet "sheet"
                            |> renameSheet "doesNotExist" "renamed"
                            |> R.map sheets
                        )
            , test "renames only one sheet" <|
                \_ ->
                    Expect.equal (Ok [ Current "sheet1", After "renamed", After "sheet3" ])
                        (singleSheet "sheet1"
                            |> insertSheet "sheet2"
                            |> insertSheet "sheet3"
                            |> renameSheet "sheet2" "renamed"
                            |> R.map sheets
                        )
            , test "enforces name unicity" <|
                \_ ->
                    Expect.equal (Err DuplicateSheetNameError)
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "sheet"
                        )
            , test "enforces name format" <|
                \_ ->
                    Expect.equal (Err InvalidSheetNameError)
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "not a valid sheet name"
                        )
            , test "does not break cellSource" <|
                \_ ->
                    expectValue "1"
                        (singleSheet "sheet"
                            |> insert "a" "1"
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (cellSource "a")
                        )
            , test "does not break get" <|
                \_ ->
                    expectValue (StringValue "1")
                        (singleSheet "sheet"
                            |> insert "a" "1"
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (get "a")
                        )
            ]
        , describe "get"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (singleSheet "sheet" |> get "a")
            , test "single value" <|
                \_ ->
                    expectValue (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ] |> get "a")
            , test "simple formula" <|
                \_ ->
                    expectValue (IntValue -7)
                        (get "a" <| fromList "sheet" [ ( "a", "=1+1-10+1" ) ])
            , fuzz (tuple ( int, int )) "Fuzzing substraction" <|
                \( i, j ) ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "-", String.fromInt j ] ) ]
                    in
                    expectValue (IntValue (i - j)) (get "a" doc)
            , fuzz (tuple ( int, int )) "Fuzzing addition" <|
                \( i, j ) ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "+", String.fromInt j ] ) ]
                    in
                    expectValue (IntValue (i + j)) (get "a" doc)
            , test "empty string" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (get "a" <| fromList "sheet" [ ( "a", "" ) ])
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
                        (get "b" doc)
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
                        (get "b" doc)
            , test "absolute reference" <|
                \_ ->
                    expectValue (StringValue "1")
                        (fromList "sheet1" [ ( "a", "=sheet2.b" ) ]
                            |> insertSheet "sheet2"
                            |> selectSheet "sheet2"
                            |> R.map (insert "b" "1")
                            |> R.andThen (selectSheet "sheet1")
                            |> R.andThen (get "a")
                        )
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
                        test (name ++ ":" ++ src) (\_ -> Expect.equal (get name doc) res)
                    )
                    data
            ]
        ]
