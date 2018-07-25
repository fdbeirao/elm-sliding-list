module SlidingList
    exposing
        ( SlidingList
        , PositiveInt
        , positiveInt
        , newSlidingList
        , fromList
        , isEmpty
        , cons
        , items
        , length
        , reverse
        , maximumSize
        , resize
        , member
        , head
        , tail
        , filter
        , take
        , drop
        , append
        , map
        )

{-| A data type that holds an upper bounded sliding list.

When you create a `SlidingList` you specify the maximum
number of items that it can hold. This module is heavily
inspired by the [elm-lang/core/List](http://package.elm-lang.org/packages/elm-lang/core/latest/List)
module.

When you create a sliding list, you can `cons` items into it, and it will slide after
the maximum size:

    positiveInt 2
        |> Maybe.map (
            newSlidingList
            >> cons "A"
            >> cons "B"
            >> cons "C"
            >> items
        ) == Just [ "C", "B" ]


# Creating a sliding list

@docs positiveInt, newSlidingList, fromList, resize, maximumSize


# Basics

@docs cons, items, isEmpty, length, reverse, member


# Sub-lists

@docs head, tail, filter, take, drop


# Putting lists together

@docs append


# Mapping

@docs map


# Opaque data types

@docs SlidingList, PositiveInt

-}


{-| The opaque data type that holds the `List a`.
-}
type SlidingList a
    = SlidingList (List a) Int


{-| An opaque data type to hold an integer that is greater than 1.
-}
type PositiveInt
    = PositiveInt Int


{-| Obtain a `PositiveInt`. The argument must be greater than 0.

    positiveInt 5 == Just PositiveInt
    positiveInt 0 == Nothing
    positiveInt -10 == Nothing

-}
positiveInt : Int -> Maybe PositiveInt
positiveInt i =
    if i > 0 then
        Just (PositiveInt i)
    else
        Nothing


{-| Create a new empty SlidingList.
-}
newSlidingList : PositiveInt -> SlidingList a
newSlidingList size =
    fromList size []


{-| Create a new SlidingList from an existing List.

If the size of the initial list is greater than the maximum
size of the sliding list, the sliding list will slide, as
expected.

-}
fromList : PositiveInt -> List a -> SlidingList a
fromList (PositiveInt size) list =
    SlidingList
        (list
            |> List.reverse
            |> List.take size
            |> List.reverse
        )
        size


{-| Determine if this sliding list is empty.
-}
isEmpty : SlidingList a -> Bool
isEmpty (SlidingList list _) =
    List.isEmpty list


{-| Add an element to the front of the sliding list, sliding
if it exceeds the maximum size of the sliding list.
-}
cons : a -> SlidingList a -> SlidingList a
cons item (SlidingList list size) =
    SlidingList (item :: list |> List.take size) size


{-| Obtain the current items held by this sliding list.
-}
items : SlidingList a -> List a
items (SlidingList list _) =
    list


{-| Obtain the count of items help by this sliding list.
-}
length : SlidingList a -> Int
length (SlidingList list _) =
    List.length list


{-| Reverse this sliding list in place, returning a new
sliding list with the same maximum size.
-}
reverse : SlidingList a -> SlidingList a
reverse (SlidingList list size) =
    SlidingList (list |> List.reverse) size


{-| Obtain the current maximum size allowed for this
sliding list.
-}
maximumSize : SlidingList a -> Int
maximumSize (SlidingList _ size) =
    size


{-| Resize this sliding list. If the new size is smaller than
the current held number of items in the list, it will slide, as
expected.
-}
resize : PositiveInt -> SlidingList a -> SlidingList a
resize (PositiveInt size) (SlidingList list _) =
    SlidingList (list |> List.take size) size


{-| Determine if an item is a member of this current sliding list.
-}
member : a -> SlidingList a -> Bool
member item (SlidingList list _) =
    list |> List.member item


{-| Extract the head of this sliding list.
-}
head : SlidingList a -> Maybe a
head (SlidingList list _) =
    list |> List.head


{-| Extract the tail of this list.
-}
tail : SlidingList a -> Maybe (List a)
tail (SlidingList list _) =
    list |> List.tail


{-| Filter this sliding list in place, keeping only the items
that satisfy the predicate. The new sliding list will have the
same maximum items as the previous one.
-}
filter : (a -> Bool) -> SlidingList a -> SlidingList a
filter fn (SlidingList list size) =
    SlidingList (list |> List.filter fn) size


{-| Take the first *n* items of this list into a new sliding list.
The new sliding list will have the same maximum items as the previous
one.
-}
take : Int -> SlidingList a -> SlidingList a
take howMany (SlidingList list size) =
    SlidingList (list |> List.take howMany) size


{-| Drop the first *n* items of this list. The new sliding list will have
the same maximum items as the previous one.
-}
drop : Int -> SlidingList a -> SlidingList a
drop howMany (SlidingList list size) =
    SlidingList (list |> List.drop howMany) size


{-| Append a list of elements to this sliding list.
Each element will be treated as if it had been cons'd by their
order in the incoming list.
-}
append : List a -> SlidingList a -> SlidingList a
append newItems list =
    newItems |> List.foldl cons list


{-| Apply a function to every element of this sliding list.
-}
map : (a -> b) -> SlidingList a -> SlidingList b
map fn (SlidingList items size) =
    fromList
        (PositiveInt size)
        (items |> List.map fn)
