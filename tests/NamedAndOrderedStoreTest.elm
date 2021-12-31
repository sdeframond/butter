module NamedAndOrderedStoreTest exposing (..)

import Expect
import Name
import NamedAndOrderedStore as Store
import Test exposing (..)


suite : Test
suite =
    describe "NamedAndOrderedStore"
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
        , test "commitEdits does not change current name when it is already exists" <|
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
                    |> Store.toItemList
                    |> List.map (.name >> Name.toString)
                    |> Expect.equal [ "first", "second" ]
        , test "commitEdits updates the name index" <|
            \_ ->
                Store.init (Name.sanitize "first") 1
                    |> Store.updateCurrentEditedName "second"
                    |> Store.commitEdits
                    |> Store.insert (Name.sanitize "second") 2
                    |> Store.toItemList
                    |> List.map (.name >> Name.toString)
                    |> Expect.equal [ "second", "second1" ]
        , test "editCurrentName set current edit status to the current item's name" <|
            \_ ->
                Store.init (Name.sanitize "initialName") 1
                    |> Store.editCurrentName
                    |> Store.getCurrentEditStatus
                    |> Expect.equal (Just "initialName")
        ]
