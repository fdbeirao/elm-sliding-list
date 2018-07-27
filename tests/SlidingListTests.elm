module SlidingListTests exposing (..)

import Expect exposing (Expectation)
import SlidingList
import Test exposing (Test, describe, test)


usageExample : Test
usageExample =
    describe "Usage examples"
        [ test "Inserting 4 elements, one at a time, onto a list with maximum capacity 2" <|
            \_ ->
                let
                    emptyList =
                        SlidingList.new 2

                    list1 =
                        emptyList |> SlidingList.insert "a"

                    list2 =
                        list1 |> SlidingList.insert "b"

                    list3 =
                        list2 |> SlidingList.insert "c"

                    list4 =
                        list3 |> SlidingList.insert "d"
                in
                ()
                    |> Expect.all
                        [ \_ -> emptyList |> SlidingList.items |> Expect.equal []
                        , \_ -> list1 |> SlidingList.items |> Expect.equal [ "a" ]
                        , \_ -> list2 |> SlidingList.items |> Expect.equal [ "a", "b" ]
                        , \_ -> list3 |> SlidingList.items |> Expect.equal [ "b", "c" ]
                        , \_ -> list4 |> SlidingList.items |> Expect.equal [ "c", "d" ]
                        ]
        , test "InsertAll works like if you had run insert for each element of the incomming list" <|
            \_ ->
                let
                    emptyList =
                        SlidingList.new 2

                    list1 =
                        emptyList |> SlidingList.insertAll [ "a" ]

                    list2 =
                        list1 |> SlidingList.insertAll [ "b", "c" ]

                    list3 =
                        list2 |> SlidingList.insertAll [ "d" ]
                in
                ()
                    |> Expect.all
                        [ \_ -> emptyList |> SlidingList.items |> Expect.equal []
                        , \_ -> list1 |> SlidingList.items |> Expect.equal [ "a" ]
                        , \_ -> list2 |> SlidingList.items |> Expect.equal [ "b", "c" ]
                        , \_ -> list3 |> SlidingList.items |> Expect.equal [ "c", "d" ]
                        ]
        , test "Creating a sliding list from an existing list will slide it" <|
            \_ ->
                [ "a", "b", "c" ]
                    |> SlidingList.fromList 2
                    |> SlidingList.items
                    |> Expect.equal [ "b", "c" ]
        ]


newTests : Test
newTests =
    describe "new tests"
        [ test "A new list is empty by default" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.items
                    |> Expect.equal []
        , test "A new list keeps its specified maximum size" <|
            \_ ->
                SlidingList.new 5
                    |> SlidingList.maximumSize
                    |> Expect.equal 5
        , test "A new list with size 0 will have size 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new 0
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "A new list with negative size will have size 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new -5
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        ]


isEmptyTests : Test
isEmptyTests =
    describe "isEmpty tests"
        [ test "Initial list is empty" <|
            \_ ->
                SlidingList.new 5
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        , test "Non empty list should not be empty" <|
            \_ ->
                SlidingList.new 5
                    |> SlidingList.insert "str"
                    |> SlidingList.isEmpty
                    |> Expect.equal False
        ]


fromListTests : Test
fromListTests =
    describe "fromList tests"
        [ test "fromList adds items from a list" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 5
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "fromList sets the maximum size of the sliding list" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 5
                    |> SlidingList.maximumSize
                    |> Expect.equal 5
        , test "fromList with a maximum size of zero will be empty" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 0
                    |> SlidingList.items
                    |> Expect.equal []
        , test "fromList with a maximum size of zero will have its maximum size become 1, as specified in the documentation" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 0
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "fromList with a negative maximum size will be empty" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList -5
                    |> SlidingList.items
                    |> Expect.equal []
        , test "fromList with a negative maximum size will have its maximum size become 1, as specified in the documentation" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList -2
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "fromList slides if the input is greater than its maximum size" <|
            \_ ->
                [ "A", "B", "C" ]
                    |> SlidingList.fromList 2
                    |> SlidingList.items
                    |> Expect.equal [ "B", "C" ]
        , test "fromList with an input with the maximum size doesn't slide" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 2
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        ]


insertTests : Test
insertTests =
    describe "insert tests"
        [ test "Inserting 2 items into a maximum size 1 sliding list should only keep the last one" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.insert 1
                    |> SlidingList.insert 2
                    |> SlidingList.items
                    |> Expect.equal [ 2 ]
        , test "Inserting fewer items than the maximum size of the sliding list should keep their order and not slide them" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        ]


insertAllTests : Test
insertAllTests =
    describe "insertAll tests"
        [ test "insertAll treats each element of the new list as if they had been individually inserted" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insertAll [ "B", "C", "D" ]
                    |> SlidingList.items
                    |> Expect.equal [ "B", "C", "D" ]
        , test "insertAll keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insertAll [ "B", "C", "D" ]
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "insertAll of an empty list keeps the original list" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 3
                    |> SlidingList.insertAll []
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "insertAll with enough space does not slide elements out" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insertAll [ "B" ]
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        ]


lengthTests : Test
lengthTests =
    describe "length tests"
        [ test "The length of an empty sliding list is 0" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.length
                    |> Expect.equal 0
        , test "The length of a sliding list with maximum size 5, but only 1 element is 1" <|
            \_ ->
                SlidingList.new 5
                    |> SlidingList.insert "item"
                    |> SlidingList.length
                    |> Expect.equal 1
        , test "The length of a sliding list with maximum size 1, after trying to insert 2 elements is still 1" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.insert "1"
                    |> SlidingList.insert "2"
                    |> SlidingList.length
                    |> Expect.equal 1
        ]


reverseTests : Test
reverseTests =
    describe "reverse tests"
        [ test "Reversing a symetric list results in the same list" <|
            \_ ->
                let
                    initialList =
                        SlidingList.new 3
                            |> SlidingList.insert "A"
                            |> SlidingList.insert "B"
                            |> SlidingList.insert "A"

                    reversedList =
                        initialList
                            |> SlidingList.reverse
                in
                reversedList
                    |> Expect.equal initialList
        , test "Reversing a sliding list reverses its items" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.reverse
                    |> SlidingList.items
                    |> Expect.equal [ "B", "A" ]
        , test "After reversing a list, insert still works as expected" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.reverse
                    |> SlidingList.insert "C"
                    |> SlidingList.items
                    |> Expect.equal [ "B", "A", "C" ]
        , test "Reversing a sliding list keeps its maximum size" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.reverse
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        ]


availableSpaceTests : Test
availableSpaceTests =
    describe "availableSpace tests"
        [ test "The available space of an empty sliding list is its maximum size" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.availableSpace
                    |> Expect.equal 3
        , test "The available space of a list with maximum size 3 and one element is 2" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert "A"
                    |> SlidingList.availableSpace
                    |> Expect.equal 2
        , test "The available space of a full list is zero" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.insert "A"
                    |> SlidingList.availableSpace
                    |> Expect.equal 0
        ]


maximumSizeTests : Test
maximumSizeTests =
    describe "maximumSize tests"
        [ test "It is possible to obtain the maximum size of a sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        ]


resizeTests : Test
resizeTests =
    describe "resize tests"
        [ test "Resizing a sliding list changes its maximum size" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.resize 1
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "Shortening a sliding list makes it slide to the new available space" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize 1
                    |> SlidingList.items
                    |> Expect.equal [ "B" ]
        , test "Increasing the capacity of a sliding list makes keeps its items untouched" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize 4
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "Resizing a sliding list to zero clears it, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize 0
                    |> SlidingList.items
                    |> Expect.equal []
        , test "Resizing a sliding list to a negative number clears it, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize -5
                    |> SlidingList.items
                    |> Expect.equal []
        , test "Resizing a sliding list to zero makes its maximum size become 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize 0
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "Resizing a sliding list to a negative number makes its maximum size become 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.resize -5
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        ]


memberTests : Test
memberTests =
    describe "member tests"
        [ test "An existing item should be a member of the sliding list" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.member "A"
                    |> Expect.equal True
        , test "A non-existing item should not be a member of the sliding list" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.insert "A"
                    |> SlidingList.member "B"
                    |> Expect.equal False
        , test "An item that has been slided out of the list is no longer a member" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.insert "A"
                    |> SlidingList.insert "B"
                    |> SlidingList.member "A"
                    |> Expect.equal False
        ]


mapTests : Test
mapTests =
    describe "map tests"
        [ test "Map works as expected" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 3
                    |> SlidingList.map String.toLower
                    |> SlidingList.items
                    |> Expect.equal [ "a", "b" ]
        , test "Map keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.insert 1
                    |> SlidingList.map ((*) 2)
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Map allows changing the type of the sliding list" <|
            \_ ->
                [ 1, 2 ]
                    |> SlidingList.fromList 3
                    |> SlidingList.map toString
                    |> SlidingList.items
                    |> Expect.equal [ "1", "2" ]
        ]
