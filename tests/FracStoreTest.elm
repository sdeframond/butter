module FracStoreTest exposing (..)

import Core.FracStore as Store
import Core.Name as Name
import Expect
import Test exposing (..)


suite : Test
suite =
    let
        toNameList : Store.Store a -> List String
        toNameList =
            Store.zipMap (\store _ -> Store.currentName store)
                >> List.map Name.toString
    in
    describe "FracStore"
        [ test "cancelEdits clears editStatus" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.updateCurrentEditedName "changed"
                    |> Store.cancelEdits
                    |> Store.getCurrentEditStatus
                    |> Expect.equal Nothing
        , test "cancelEdits does not change name" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.updateCurrentEditedName "changed"
                    |> Store.cancelEdits
                    |> Store.currentName
                    |> Name.toString
                    |> Expect.equal "initialName"
        , test "commitEdits does nothing when there is no ongoing edition" <|
            \_ ->
                let
                    store =
                        Store.init (Name.sanitize "first") 1
                            |> Store.insert (Name.sanitize "second") 2
                            |> Store.insert (Name.sanitize "third") 3
                in
                Store.commitEdits store
                    |> Expect.equal store
        , test "commitEdits clears the current edit status" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.updateCurrentEditedName "changed"
                    |> Store.commitEdits
                    |> Store.getCurrentEditStatus
                    |> Expect.equal Nothing
        , test "commitEdits changes current name when it is valid" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.updateCurrentEditedName "changed"
                    |> Store.commitEdits
                    |> Store.currentName
                    |> Name.toString
                    |> Expect.equal "changed"
        , test "commitEdits does not change current name when it is invalid" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.updateCurrentEditedName "not valid"
                    |> Store.commitEdits
                    |> Store.currentName
                    |> Name.toString
                    |> Expect.equal "initialName"
        , test "commitEdits does not change current name when it already exists" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.updateCurrentEditedName "first"
                    |> Store.commitEdits
                    |> Store.currentName
                    |> Name.toString
                    |> Expect.equal "second"
        , test "commitEdits cancels edition when the name already exists" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.updateCurrentEditedName "second"
                    |> Store.commitEdits
                    |> Store.getCurrentEditStatus
                    |> Expect.equal Nothing
        , test "commitEdits does not update the name when it is a duplicate" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.updateCurrentEditedName "second"
                    |> Store.commitEdits
                    |> toNameList
                    |> Expect.equal [ "first", "second" ]
        , test "commitEdits updates the name index" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.updateCurrentEditedName "second"
                    |> Store.commitEdits
                    |> Store.insert (Name.sanitize "second") 2
                    |> toNameList
                    |> Expect.equal [ "second", "second1" ]
        , test "editCurrentName set current edit status to the current item's name" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.editCurrentName
                    |> Store.getCurrentEditStatus
                    |> Expect.equal (Just "initialName")
        , test ".insert selects the inserted item" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.currentName
                    |> Name.toString
                    |> Expect.equal "second"
        , test ".insert inserts after the current item" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.insert (Name.sanitize "third") 3
                    |> toNameList
                    |> Expect.equal [ "first", "second", "third" ]
        , test ".current returns the current item" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.current
                    |> Expect.equal 1
        , test ".getIdByName returns an Id given an existing name" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> (\s -> Store.getIdByName s (Name.sanitize "first"))
                    |> Expect.notEqual Nothing
        , test ".getIdByName returns Nothing when the name doesn't exist" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> (\s -> Store.getIdByName s (Name.sanitize "somethingElse"))
                    |> Expect.equal Nothing
        , test ".setCurrent sets the current value" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> (\s -> Store.setCurrent s 2)
                    |> Store.current
                    |> Expect.equal 2
        ]
