module SlidingListTests exposing (..)

import SlidingList
import Expect exposing (Expectation)
import Test exposing (Test, test, describe)


positiveIntTests : Test
positiveIntTests =
    describe "positiveInt tests"
        [ test "Trying to construct a positive int with a negative number will return nothing" <|
            \_ ->
                SlidingList.positiveInt -5
                    |> Expect.equal Nothing
        , test "Tying to construct a positive int with zero will return nothing" <|
            \_ ->
                SlidingList.positiveInt 0
                    |> Expect.equal Nothing
        , test "Trying to construct a positive int with a positive number will not return 'Nothing'" <|
            \_ ->
                SlidingList.positiveInt 5
                    |> Expect.notEqual Nothing
        ]


{-| This function is meant to make testing easier.

It leverages Debug.crash to override the compiler guarantees, while bypassing the returned Maybe

-}
testSlidingListWithSize : Int -> SlidingList.SlidingList a
testSlidingListWithSize size =
    size
        |> adHocPositiveInt
        |> SlidingList.newSlidingList


{-| This function is meant to make testing easier.

It leverages Debug.crash to override the compiler guarantees, while bypassing the returned Maybe

-}
adHocPositiveInt : Int -> SlidingList.PositiveInt
adHocPositiveInt size =
    case SlidingList.positiveInt size of
        Just a ->
            a

        Nothing ->
            Debug.crash "An error occurred creating a positiveInt"


isEmptyTests : Test
isEmptyTests =
    describe "isEmpty tests"
        [ test "Initial list is empty" <|
            \_ ->
                testSlidingListWithSize 5
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        , test "Non empty list should not be empty" <|
            \_ ->
                testSlidingListWithSize 5
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
                    |> SlidingList.fromList (adHocPositiveInt 5)
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "fromList sets the maximumSize of the SlidingList" <|
            \_ ->
                [ "A", "B" ]
                    |> SlidingList.fromList (adHocPositiveInt 5)
                    |> SlidingList.maximumSize
                    |> Expect.equal 5
        , test "fromList slides if the input is greater than its maximum size" <|
            \_ ->
                [ "A", "B", "C" ]
                    |> SlidingList.fromList (adHocPositiveInt 2)
                    |> SlidingList.items
                    |> Expect.equal [ "B", "C" ]
        ]


consTests : Test
consTests =
    describe "cons (::) tests"
        [ test "Adding 2 items to a size 1 sliding list should only keep the last one" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.cons 1
                    |> SlidingList.cons 2
                    |> SlidingList.items
                    |> Expect.equal [ 2 ]
        ]


lengthTests : Test
lengthTests =
    describe "length tests"
        [ test "The length of an empty list is 0" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.length
                    |> Expect.equal 0
        , test "The length of a list with size 5, but only 1 element is 1" <|
            \_ ->
                testSlidingListWithSize 5
                    |> SlidingList.cons "item"
                    |> SlidingList.length
                    |> Expect.equal 1
        , test "The length of a list with size 1, after trying to cons 2 element is 1" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.cons "1"
                    |> SlidingList.cons "2"
                    |> SlidingList.length
                    |> Expect.equal 1
        ]


reverseTests : Test
reverseTests =
    describe "reverse tests"
        [ test "Reversing a symetric list is the same list" <|
            \_ ->
                let
                    initialList =
                        testSlidingListWithSize 3
                            |> SlidingList.cons "A"
                            |> SlidingList.cons "B"
                            |> SlidingList.cons "A"

                    reversedList =
                        initialList
                            |> SlidingList.reverse
                in
                    reversedList
                        |> Expect.equal initialList
        , test "Reversing a list reverses it" <|
            \_ ->
                testSlidingListWithSize 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.reverse
                    |> SlidingList.items
                    |> Expect.equal [ "A", "B" ]
        , test "Reversing a list keeps its maximum size" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.reverse
                    |> SlidingList.cons "C"
                    |> SlidingList.items
                    |> Expect.equal [ "C", "A", "B" ]
        ]


maximumSizeTests : Test
maximumSizeTests =
    describe "maximumSize tests"
        [ test "It is possible to obtain the maximum size of a SlidingList" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        ]


resizeTests : Test
resizeTests =
    describe "resize tests"
        [ test "Resizing a SlidingList changes its maximumSize" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.resize (adHocPositiveInt 1)
                    |> SlidingList.maximumSize
                    |> Expect.equal 1
        , test "Shortening a SlidingList truncates it" <|
            \_ ->
                testSlidingListWithSize 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.resize (adHocPositiveInt 1)
                    |> SlidingList.items
                    |> Expect.equal [ "B" ]
        ]


memberTests : Test
memberTests =
    describe "member tests"
        [ test "An existing item should be a member of the SlidingList" <|
            \_ ->
                testSlidingListWithSize 2
                    |> SlidingList.cons "A"
                    |> SlidingList.member "A"
                    |> Expect.equal True
        , test "A non-existing item should not be a member of the SlidingList" <|
            \_ ->
                testSlidingListWithSize 2
                    |> SlidingList.cons "A"
                    |> SlidingList.member "B"
                    |> Expect.equal False
        , test "An item that has been slided out of the list is no longer a member" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.member "A"
                    |> Expect.equal False
        ]


headTests : Test
headTests =
    describe "head tests"
        [ test "The head of an empty SlidingList is Nothing" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.head
                    |> Expect.equal Nothing
        , test "The head of a non empty list should be the last added item" <|
            \_ ->
                testSlidingListWithSize 2
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.head
                    |> Expect.equal (Just "B")
        ]


tailTests : Test
tailTests =
    describe "tail tests"
        [ test "The tail of an empty SlidingList is Nothing" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.tail
                    |> Expect.equal Nothing
        , test "The tail of a SlidingList with one single element is an empty list" <|
            \_ ->
                testSlidingListWithSize 1
                    |> SlidingList.cons "A"
                    |> SlidingList.tail
                    |> Expect.equal (Just [])
        , test "The tail of a SlidingList with multiple elements is the expected tail" <|
            \_ ->
                testSlidingListWithSize 3
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
                testSlidingListWithSize 4
                    |> SlidingList.cons 1
                    |> SlidingList.cons 2
                    |> SlidingList.cons 3
                    |> SlidingList.cons 4
                    |> SlidingList.filter (\i -> i % 2 == 0)
                    |> SlidingList.items
                    |> Expect.equal [ 4, 2 ]
        , test "Filter keeps the maximumSize of the list" <|
            \_ ->
                testSlidingListWithSize 4
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
                testSlidingListWithSize 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.take 1
                    |> SlidingList.items
                    |> Expect.equal [ "B" ]
        , test "Take keeps the maximumSize of the list" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.take 1
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Take from an empty list is still an empty list" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.take 1
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        ]


dropTests : Test
dropTests =
    describe "drop tests"
        [ test "Drop works as expected" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.drop 1
                    |> SlidingList.items
                    |> Expect.equal [ "A" ]
        , test "Drop keeps the maximumSize of the list" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.cons "A"
                    |> SlidingList.cons "B"
                    |> SlidingList.drop 1
                    |> SlidingList.maximumSize
                    |> Expect.equal 3
        , test "Drop from an empty list is still an empty list" <|
            \_ ->
                testSlidingListWithSize 3
                    |> SlidingList.drop 1
                    |> SlidingList.isEmpty
                    |> Expect.equal True
        ]
