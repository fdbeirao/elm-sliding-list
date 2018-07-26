module SlidingListTests exposing (..)

import Expect exposing (Expectation)
import SlidingList
import Test exposing (Test, describe, test)


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
                    |> SlidingList.cons "str"
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


consTests : Test
consTests =
    describe "cons (::) tests"
        [ test "Adding 2 items to a maximum size 1 sliding list should only keep the last one" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.cons 1
                    |> SlidingList.cons 2
                    |> SlidingList.items
                    |> Expect.equal [ 2 ]
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
                    |> SlidingList.cons "item"
                    |> SlidingList.length
                    |> Expect.equal 1
        , test "The length of a sliding list with maximum size 1, after trying to cons 2 elements is 1" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.cons "1"
                    |> SlidingList.cons "2"
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
                            |> SlidingList.cons "A"
                            |> SlidingList.cons "B"
                            |> SlidingList.cons "A"

                    reversedList =
                        initialList
                            |> SlidingList.reverse
                in
                reversedList
                    |> Expect.equal initialList
        , test "Reversing a sliding list reverses its items" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.reverse
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "After reversing a list, cons still works as expected" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.reverse
                    |> SlidingList.cons "C"
                    |> SlidingList.items
                    |> Expect.equal [ "C", "A", "B" ]
        , test "Reversing a sliding list keeps its maximum size" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
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
                    |> SlidingList.cons "A"
                    |> SlidingList.availableSpace
                    |> Expect.equal 2
        , test "The available space of a full list is zero" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.cons "A"
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
        , test "Shortening a sliding list truncates it" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.resize 1
                    |> SlidingList.items
                    |> Expect.equal [ "B" ]
        , test "Resizing a sliding list to zero clears it" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.resize 0
                    |> SlidingList.items
                    |> Expect.equal []
        , test "Resizing a sliding list to a negative number clears it" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.resize -5
                    |> SlidingList.items
                    |> Expect.equal []
        , test "Resizing a sliding list to zero makes its maximum size become 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.resize 0
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "Resizing a sliding list to a negative number makes its maximum size become 1, as specified in the documentation" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
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
                    |> SlidingList.cons "A"
                    |> SlidingList.member "A"
                    |> Expect.equal True
        , test "A non-existing item should not be a member of the sliding list" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.member "B"
                    |> Expect.equal False
        , test "An item that has been slided out of the list is no longer a member" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.member "A"
                    |> Expect.equal False
        ]


headTests : Test
headTests =
    describe "head tests"
        [ test "The head of an empty sliding list is Nothing" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.head
                    |> Expect.equal Nothing
        , test "The head of a non empty list should be the last added item" <|
            \_ ->
                SlidingList.new 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.head
                    |> Expect.equal (Just "B")
        ]


tailTests : Test
tailTests =
    describe "tail tests"
        [ test "The tail of an empty sliding list is Nothing" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.tail
                    |> Expect.equal Nothing
        , test "The tail of a sliding list with one single element is an empty list" <|
            \_ ->
                SlidingList.new 1
                    |> SlidingList.cons "A"
                    |> SlidingList.tail
                    |> Expect.equal (Just [])
        , test "The tail of a sliding list with multiple elements is the expected tail" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.cons "C"
                    |> SlidingList.tail
                    |> Expect.equal (Just [ "B", "A" ])
        ]


filterTests : Test
filterTests =
    describe "filter tests"
        [ test "Filter works as expected" <|
            \_ ->
                SlidingList.new 4
                    |> SlidingList.cons 1
                    |> SlidingList.cons 2
                    |> SlidingList.cons 3
                    |> SlidingList.cons 4
                    |> SlidingList.filter (\i -> i % 2 == 0)
                    |> SlidingList.items
                    |> Expect.equal [ 4, 2 ]
        , test "Filter keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 4
                    |> SlidingList.cons 1
                    |> SlidingList.cons 2
                    |> SlidingList.cons 3
                    |> SlidingList.cons 4
                    |> SlidingList.filter (\i -> i % 2 == 0)
                    |> SlidingList.maximumSize
                    |> Expect.equal 4
        ]


takeTests : Test
takeTests =
    describe "take tests"
        [ test "Take works as expected" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.take 1
                    |> SlidingList.items
                    |> Expect.equal [ "B" ]
        , test "Take keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.take 1
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Take from an empty list is still an empty list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.take 1
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        ]


dropTests : Test
dropTests =
    describe "drop tests"
        [ test "Drop works as expected" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.drop 1
                    |> SlidingList.items
                    |> Expect.equal [ "A" ]
        , test "Drop keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.drop 1
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Drop from an empty list is still an empty list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.drop 1
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        ]


appendTests : Test
appendTests =
    describe "append tests"
        [ test "Append treats each element of the new list as if they had been cons'd" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.append [ "B", "C", "D" ]
                    |> SlidingList.items
                    |> Expect.equal [ "D", "C", "B" ]
        , test "Append keeps the maximum size of the sliding list" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.append [ "B", "C", "D" ]
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Append of an empty list keeps the original list" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList 3
                    |> SlidingList.append []
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "Append with enough space does not slide elements out" <|
            \_ ->
                SlidingList.new 3
                    |> SlidingList.cons "A"
                    |> SlidingList.append [ "B" ]
                    |> SlidingList.items
                    |> Expect.equal [ "B", "A" ]
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
                    |> SlidingList.cons 1
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


foldrTests : Test
foldrTests =
    describe "foldr tests"
        [ test "Foldr works as expected" <|
            \_ ->
                [ "B", "C" ]
                    |> SlidingList.fromList 3
                    |> SlidingList.foldr (++) "A"
                    |> Expect.equal "BCA"
        ]


foldlTests : Test
foldlTests =
    describe "foldl tests"
        [ test "Foldl works as expected" <|
            \_ ->
                [ "B", "C" ]
                    |> SlidingList.fromList 3
                    |> SlidingList.foldl (++) "A"
                    |> Expect.equal "CBA"
        ]
