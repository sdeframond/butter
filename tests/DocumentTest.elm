module DocumentTest exposing (..)

import Dict as D
import Document exposing (..)
import Document.Types exposing (..)
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
        expectOk val res =
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
                        (R.map sheets doc)
                        (R.map (insert "a" "1") doc |> R.map sheets)
            ]
        , describe "cellSource"
            [ test "retrieves the source of a cell" <|
                \_ ->
                    expectOk "src"
                        (singleSheet "sheet" |> insert "a" "src" |> cellSource "a")
            ]
        , describe "sheets"
            [ test "returns all sheets" <|
                \_ -> Expect.equal (sheets <| singleSheet "sheet") [ Current "sheet" ]
            ]
        , describe "selectSheet"
            [ test "selects a sheet" <|
                \_ ->
                    expectOk [ Before "sheet", Current "sheet2", After "sheet3" ]
                        (singleSheet "sheet"
                            |> insertSheet "sheet2"
                            |> R.andThen (insertSheet "sheet3")
                            |> R.andThen (selectSheet "sheet2")
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
                    expectOk [ Current "sheet", After "toto" ]
                        (singleSheet "sheet" |> insertSheet "toto" |> R.map sheets)
            , test "cannot create duplicates" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet")
                        (singleSheet "sheet" |> insertSheet "sheet")
            ]
        , describe "removeSheet"
            [ test "removes a given sheet" <|
                \_ ->
                    expectOk [ Current "toto" ]
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> R.andThen (removeSheet "sheet")
                            |> R.map sheets
                        )
            , test "does not removes the other sheets' cells" <|
                \_ ->
                    expectOk (StringValue "1")
                        (singleSheet "sheet"
                            |> insertSheet "otherSheet"
                            |> R.andThen (selectSheet "otherSheet")
                            |> R.map (insert "a" "1")
                            |> R.andThen (removeSheet "sheet")
                            |> R.andThen (get "a")
                        )
            , test "returns an error when the sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (singleSheet "sheet"
                            |> insertSheet "toto"
                            |> R.andThen (removeSheet "toto")
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
                    expectError (UndefinedSheetError "doesNotExist")
                        (singleSheet "sheet"
                            |> renameSheet "doesNotExist" "renamed"
                        )
            , test "renames only one sheet" <|
                \_ ->
                    expectOk [ Current "sheet1", After "renamed", After "sheet3" ]
                        (singleSheet "sheet1"
                            |> insertSheet "sheet2"
                            |> R.andThen (insertSheet "sheet3")
                            |> R.andThen (renameSheet "sheet2" "renamed")
                            |> R.map sheets
                        )
            , test "enforces name unicity" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet2")
                        (singleSheet "sheet"
                            |> insertSheet "sheet2"
                            |> R.andThen (renameSheet "sheet" "sheet2")
                        )
            , test "enforces name format" <|
                \_ ->
                    Expect.equal (Err InvalidSheetNameError)
                        (singleSheet "sheet"
                            |> renameSheet "sheet" "not a valid sheet name"
                        )
            , test "does not break cellSource" <|
                \_ ->
                    expectOk "1"
                        (singleSheet "sheet"
                            |> insert "a" "1"
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (cellSource "a")
                        )
            , test "does not break get" <|
                \_ ->
                    expectOk (StringValue "1")
                        (singleSheet "sheet"
                            |> insert "a" "1"
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (get "a")
                        )
            , test "does not affect references to that sheet" <|
                \_ ->
                    expectOk (StringValue "1")
                        (singleSheet "sheet"
                            |> insert "a" "1"
                            |> insert "b" "=sheet.a"
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (get "b")
                        )
            ]
        , describe "get"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (singleSheet "sheet" |> get "a")
            , test "single value" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ] |> get "a")
            , test "simple formula" <|
                \_ ->
                    expectOk (IntValue -7)
                        (get "a" <| fromList "sheet" [ ( "a", "=1+1-10+1" ) ])
            , fuzz2 int int "Fuzzing substraction" <|
                \i j ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "-", String.fromInt j ] ) ]
                    in
                    expectOk (IntValue (i - j)) (get "a" doc)
            , fuzz2 int int "Fuzzing addition" <|
                \i j ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "+", String.fromInt j ] ) ]
                    in
                    expectOk (IntValue (i + j)) (get "a" doc)
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
                    expectOk (StringValue "1")
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
                    expectOk (StringValue "1")
                        (fromList "sheet1" [ ( "a", "=sheet2.b" ) ]
                            |> insertSheet "sheet2"
                            |> R.andThen (selectSheet "sheet2")
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
