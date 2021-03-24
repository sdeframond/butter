module DocumentTest exposing (..)

import Document exposing (..)
import Document.Types exposing (..)
import Expect
import Fuzz exposing (int)
import List as L
import Result as R
import Test exposing (..)


suite : Test
suite =
    let
        expectOk val res =
            Expect.equal res (Ok val)

        expectError val res =
            Expect.equal res (Err val)

        fromList name pairs =
            List.foldl (\( a, b ) -> insert a b) (init name gridSheet) pairs
    in
    describe "Document"
        [ describe "cellSource"
            [ test "retrieves the source of a cell" <|
                \_ ->
                    expectOk "src"
                        (fromList "sheet" [ ( "a", "src" ) ] |> cellSource "a")
            ]
        , describe "insert"
            [ test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (init "sheet" gridSheet |> insert "a" "qwe")
                        (init "sheet" gridSheet |> insert "a" "qwe" |> insert "a" "qwe")
            , test "does not change sheet order" <|
                \_ ->
                    let
                        doc =
                            init "sheet1" gridSheet |> insertSheet "sheet2" gridSheet
                    in
                    Expect.equal
                        (R.map sheetNames doc)
                        (R.map (insert "a" "1") doc |> R.map sheetNames)
            ]
        , describe "sheetNames"
            [ test "returns all sheets" <|
                \_ -> Expect.equal [ Current "sheet" ] (sheetNames <| init "sheet" gridSheet)
            ]

        --, describe "currentSheet"
        --    [ test "returns the current sheet" <|
        --        \_ -> init "sheet" gridSheet |> currentSheet |> Expect.equal GridSheet
        --    ]
        , describe "selectSheet"
            [ test "selects a sheet and preserves sheet order" <|
                \_ ->
                    expectOk [ Before "sheet", Current "sheet2", After "sheet3", After "sheet4" ]
                        (init "sheet" gridSheet
                            |> insertSheet "sheet2" gridSheet
                            |> R.andThen (insertSheet "sheet3" gridSheet)
                            |> R.andThen (insertSheet "sheet4" gridSheet)
                            -- select twice to make sure the sheet order before
                            -- and after is preserved
                            |> R.andThen (selectSheet "sheet4")
                            |> R.andThen (selectSheet "sheet2")
                            |> R.map sheetNames
                        )
            , test "returns an error if the selected sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (init "sheet" gridSheet
                            |> selectSheet "toto"
                        )
            ]
        , describe "insertSheet"
            [ test "should insert a sheet" <|
                \_ ->
                    expectOk [ Current "sheet", After "toto" ]
                        (init "sheet" gridSheet |> insertSheet "toto" gridSheet |> R.map sheetNames)
            , test "cannot create duplicates" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet")
                        (init "sheet" gridSheet |> insertSheet "sheet" gridSheet)
            ]
        , describe "removeSheet"
            [ test "removes a given sheet" <|
                \_ ->
                    expectOk [ Current "toto" ]
                        (init "sheet" gridSheet
                            |> insertSheet "toto" gridSheet
                            |> R.andThen (removeSheet "sheet")
                            |> R.map sheetNames
                        )
            , test "does not removes the other sheets' cells" <|
                \_ ->
                    expectOk (StringValue "1")
                        (init "sheet" gridSheet
                            |> insertSheet "otherSheet" gridSheet
                            |> R.andThen (selectSheet "otherSheet")
                            |> R.map (insert "a" "1")
                            |> R.andThen (removeSheet "sheet")
                            |> R.andThen (get "a")
                        )
            , test "returns an error when the sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (init "sheet" gridSheet
                            |> insertSheet "toto" gridSheet
                            |> R.andThen (removeSheet "toto")
                            |> R.andThen (removeSheet "toto")
                        )
            , test "returns an error when removing the last sheet" <|
                \_ ->
                    expectError (RemovingLastSheetError "sheet")
                        (init "sheet" gridSheet
                            |> removeSheet "sheet"
                        )
            ]
        , describe "renameSheet"
            [ test "renames a sheet" <|
                \_ ->
                    Expect.equal (Ok [ Current "renamed" ])
                        (init "sheet" gridSheet
                            |> renameSheet "sheet" "renamed"
                            |> R.map sheetNames
                        )
            , test "renames only if the sheet exists" <|
                \_ ->
                    expectError (UndefinedSheetError "doesNotExist")
                        (init "sheet" gridSheet
                            |> renameSheet "doesNotExist" "renamed"
                        )
            , test "renames only one sheet" <|
                \_ ->
                    expectOk [ Current "sheet1", After "renamed", After "sheet3" ]
                        (init "sheet1" gridSheet
                            |> insertSheet "sheet2" gridSheet
                            |> R.andThen (insertSheet "sheet3" gridSheet)
                            |> R.andThen (renameSheet "sheet2" "renamed")
                            |> R.map sheetNames
                        )
            , test "enforces name unicity" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet2")
                        (init "sheet" gridSheet
                            |> insertSheet "sheet2" gridSheet
                            |> R.andThen (renameSheet "sheet" "sheet2")
                        )
            , test "enforces name format" <|
                \_ ->
                    Expect.equal (Err InvalidSheetNameError)
                        (init "sheet" gridSheet
                            |> renameSheet "sheet" "not a valid sheet name"
                        )
            , test "does not break cellSource" <|
                \_ ->
                    expectOk "1"
                        (fromList "sheet"
                            [ ( "a", "1" ) ]
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (cellSource "a")
                        )
            , test "does not break get" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ]
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (get "a")
                        )
            , test "does not affect references to that sheet" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet"
                            [ ( "a", "1" )
                            , ( "b", "=sheet.a" )
                            ]
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (get "b")
                        )
            ]
        , describe "get"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedNameError ( "sheet", "a" ))
                        (init "sheet" gridSheet |> get "a")
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
                            |> insertSheet "sheet2" gridSheet
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
                        data |> L.map (\( name, src, _ ) -> ( name, src )) |> fromList "sheet"
                in
                L.map
                    (\( name, src, res ) ->
                        test (name ++ ":" ++ src) (\_ -> Expect.equal (get name doc) res)
                    )
                    data
            ]
        ]
