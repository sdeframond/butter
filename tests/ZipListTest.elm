module ZipListTest exposing (..)

import Core.ZipList as ZL
import Expect
import Test exposing (..)


suite : Test
suite =
    let
        zlist =
            ZL.singleton 1 |> append [ 2, 3, 4, 5 ]

        append l zl =
            case l of
                [] ->
                    zl

                head :: rest ->
                    ZL.insertAfter head zl |> append rest

        select f zl =
            case ZL.select f zl of
                Just selected ->
                    selected

                Nothing ->
                    Debug.todo "Error: no item match"
    in
    describe "ZipList"
        [ describe ".get" <|
            [ test "returns the first match" <|
                \_ -> Expect.equal (ZL.get (always True) zlist) (Just 1)
            , test "returns the first match, even when it is not the select item" <|
                \_ -> Expect.equal (ZL.get ((==) 2) zlist) (Just 2)
            , test "returns Nothing when nothing matches" <|
                \_ -> Expect.equal (ZL.get (always False) zlist) Nothing
            ]
        , describe ".select" <|
            [ test "selects an item" <|
                \_ -> Expect.equal (ZL.select ((==) 3) zlist |> Maybe.map ZL.current) (Just 3)
            , test "preserves ordering" <|
                \_ -> Expect.equal (ZL.select ((==) 3) zlist |> Maybe.map ZL.toList) (Just [ 1, 2, 3, 4, 5 ])
            , test "returns nothing when nothing matches" <|
                \_ -> Expect.equal (ZL.select (always False) zlist |> Maybe.map ZL.toList) Nothing
            ]
        , describe ".filter" <|
            [ test "returns Nothing when removeing the last item" <|
                \_ -> ZL.singleton False |> ZL.filter identity |> Expect.equal Nothing
            , test "removes all matching items" <|
                \_ ->
                    ZL.singleton False
                        |> append [ True, True, False ]
                        |> ZL.filter identity
                        |> Maybe.map ZL.toList
                        |> Expect.equal (Just [ True, True ])
            ]
        , describe ".zipMap" <|
            [ test "selects each item and passes them to the callback function" <|
                \_ ->
                    zlist
                        |> ZL.zipMap (\zl cur -> ( ZL.current zl, cur ))
                        |> Expect.equal [ ( 1, False ), ( 2, False ), ( 3, False ), ( 4, False ), ( 5, True ) ]
            ]
        , describe ".insertAfter" <|
            [ test "inserts a new item after the current one" <|
                \_ ->
                    zlist
                        |> select ((==) 1)
                        |> ZL.insertAfter 10
                        |> ZL.toList
                        |> Expect.equal [ 1, 10, 2, 3, 4, 5 ]
            , test "selects the inserted item" <|
                \_ ->
                    zlist |> ZL.insertAfter 10 |> ZL.current |> Expect.equal 10
            ]
        ]
