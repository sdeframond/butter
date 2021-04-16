module ZipListTest exposing (..)

import Expect
import Test exposing (..)
import ZipList as ZL


suite : Test
suite =
    let
        zlist =
            ZL.singleton 1 |> ZL.append [ 2, 3, 4, 5 ]
    in
    describe "ZipList"
        [ describe "get" <|
            [ test "returns the first match" <|
                \_ -> Expect.equal (ZL.get (always True) zlist) (Just 1)
            , test "returns the first match, even when it is not the select item" <|
                \_ -> Expect.equal (ZL.get ((==) 2) zlist) (Just 2)
            , test "returns Nothing when nothing matches" <|
                \_ -> Expect.equal (ZL.get (always False) zlist) Nothing
            ]
        , describe "select" <|
            [ test "selects an item" <|
                \_ -> Expect.equal (ZL.select ((==) 3) zlist |> Maybe.map ZL.current) (Just 3)
            , test "preserves ordering" <|
                \_ -> Expect.equal (ZL.select ((==) 3) zlist |> Maybe.map ZL.toList) (Just [ 1, 2, 3, 4, 5 ])
            , test "returns nothing when nothing matches" <|
                \_ -> Expect.equal (ZL.select (always False) zlist |> Maybe.map ZL.toList) Nothing
            ]
        , todo "filter"
        , todo "map"
        , todo "member"
        , todo "removeCurrent"
        , todo "setCurrent"
        , todo "toListWithPosition"
        ]
