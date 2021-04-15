module DocumentTest exposing (..)

import Document exposing (..)
import Expect
import Fuzz exposing (int)
import List as L
import Result as R
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    let
        expectOk val res =
            Expect.equal res (Ok val)

        expectError val res =
            Expect.equal res (Err val)

        fromList name pairs =
            List.foldl (\( a, b ) -> insertCellSource a b) (init (gridSheet name)) pairs
    in
    describe "Document"
        [ describe "cellSource"
            [ test "retrieves the source of a cell" <|
                \_ ->
                    expectOk "src"
                        (fromList "sheet" [ ( "a", "src" ) ] |> cellSource "a")
            ]
        , describe "insertSource"
            [ test "is idempotent" <|
                \_ ->
                    Expect.equal
                        (init (gridSheet "sheet") |> insertCellSource "a" "qwe")
                        (init (gridSheet "sheet") |> insertCellSource "a" "qwe" |> insertCellSource "a" "qwe")
            , test "does not change sheet order" <|
                \_ ->
                    let
                        doc =
                            init (gridSheet "sheet1") |> insertSheet (gridSheet "sheet2")
                    in
                    Expect.equal
                        (R.map sheetsWithIds doc)
                        (R.map (insertCellSource "a" "1") doc |> R.map sheetsWithIds)
            ]
        , describe "sheetNames"
            [ test "returns all sheets" <|
                \_ -> Expect.equal [ Current "sheet" ] (sheetsWithIds <| init (gridSheet "sheet"))
            ]

        --, describe "currentSheet"
        --    [ test "returns the current sheet" <|
        --        \_ -> init ( gridSheet "sheet") |> currentSheet |> Expect.equal GridSheet
        --    ]
        , describe "insertSheet"
            [ test "should insert a sheet" <|
                \_ ->
                    expectOk [ Current "sheet", After "toto" ]
                        (init (gridSheet "sheet") |> insertSheet (gridSheet "toto") |> R.map sheetsWithIds)
            , test "cannot create duplicates" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet")
                        (init (gridSheet "sheet") |> insertSheet (gridSheet "sheet"))
            ]
        , describe "removeSheet"
            [ test "removes a given sheet" <|
                \_ ->
                    expectOk [ Current "toto" ]
                        (init (gridSheet "sheet")
                            |> insertSheet (gridSheet "toto")
                            |> R.andThen (removeSheet "sheet")
                            |> R.map sheetsWithIds
                        )
            , test "does not removes the other sheets' cells" <|
                \_ ->
                    expectOk (StringValue "1")
                        (init (gridSheet "sheet")
                            |> insertSheet (gridSheet "otherSheet")
                            |> R.andThen (selectSheet "otherSheet")
                            |> R.map (insertCellSource "a" "1")
                            |> R.andThen (removeSheet "sheet")
                            |> R.andThen (getValue "a")
                        )
            , test "returns an error when the sheet does not exist" <|
                \_ ->
                    expectError (UndefinedSheetError "toto")
                        (init (gridSheet "sheet")
                            |> insertSheet (gridSheet "toto")
                            |> R.andThen (removeSheet "toto")
                            |> R.andThen (removeSheet "toto")
                        )
            , test "returns an error when removing the last sheet" <|
                \_ ->
                    expectError (RemovingLastSheetError "sheet")
                        (init (gridSheet "sheet")
                            |> removeSheet "sheet"
                        )
            ]
        , describe "renameSheet"
            [ test "renames a sheet" <|
                \_ ->
                    Expect.equal (Ok [ Current "renamed" ])
                        (init (gridSheet "sheet")
                            |> renameSheet "sheet" "renamed"
                            |> R.map sheetsWithIds
                        )
            , test "renames only if the sheet exists" <|
                \_ ->
                    expectError (UndefinedSheetError "doesNotExist")
                        (init (gridSheet "sheet")
                            |> renameSheet "doesNotExist" "renamed"
                        )
            , test "renames only one sheet" <|
                \_ ->
                    expectOk [ Current "sheet1", After "renamed", After "sheet3" ]
                        (init (gridSheet "sheet1")
                            |> insertSheet (gridSheet "sheet2")
                            |> R.andThen (insertSheet (gridSheet "sheet3"))
                            |> R.andThen (renameSheet "sheet2" "renamed")
                            |> R.map sheetsWithIds
                        )
            , test "enforces name unicity" <|
                \_ ->
                    expectError (DuplicateSheetNameError "sheet2")
                        (init (gridSheet "sheet")
                            |> insertSheet (gridSheet "sheet2")
                            |> R.andThen (renameSheet "sheet" "sheet2")
                        )
            , test "enforces name format" <|
                \_ ->
                    Expect.equal (Err InvalidSheetNameError)
                        (init (gridSheet "sheet")
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
            , test "does not break getValue" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ]
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (getValue "a")
                        )
            , test "does not affect references to that sheet" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet"
                            [ ( "a", "1" )
                            , ( "b", "=sheet.a" )
                            ]
                            |> renameSheet "sheet" "toto"
                            |> R.andThen (getValue "b")
                        )
            ]
        , describe "getValue"
            [ test "empty value" <|
                \_ ->
                    expectError (UndefinedGlobalReferenceError ( "sheet", "a" ))
                        (init (gridSheet "sheet") |> getValue "a")
            , test "single value" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet" [ ( "a", "1" ) ] |> getValue "a")
            , test "simple formula" <|
                \_ ->
                    expectOk (IntValue -7)
                        (getValue "a" <| fromList "sheet" [ ( "a", "=1+1-10+1" ) ])
            , fuzz2 int int "Fuzzing substraction" <|
                \i j ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "-", String.fromInt j ] ) ]
                    in
                    expectOk (IntValue (i - j)) (getValue "a" doc)
            , fuzz2 int int "Fuzzing addition" <|
                \i j ->
                    let
                        doc =
                            fromList "sheet"
                                [ ( "a", String.concat [ "=", String.fromInt i, "+", String.fromInt j ] ) ]
                    in
                    expectOk (IntValue (i + j)) (getValue "a" doc)
            , test "empty string" <|
                \_ ->
                    expectError (UndefinedGlobalReferenceError ( "sheet", "a" ))
                        (getValue "a" <| fromList "sheet" [ ( "a", "" ) ])
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
                        (getValue "b" doc)
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
                        (getValue "b" doc)
            , test "absolute reference" <|
                \_ ->
                    expectOk (StringValue "1")
                        (fromList "sheet1" [ ( "a", "=sheet2.b" ) ]
                            |> insertSheet (gridSheet "sheet2")
                            |> R.andThen (selectSheet "sheet2")
                            |> R.map (insertCellSource "b" "1")
                            |> R.andThen (selectSheet "sheet1")
                            |> R.andThen (getValue "a")
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
                        test (name ++ ":" ++ src) (\_ -> Expect.equal (getValue name doc) res)
                    )
                    data
            ]
        ]
