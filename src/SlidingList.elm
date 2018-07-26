module SlidingList
    exposing
        ( SlidingList
        , append
        , availableSpace
        , cons
        , drop
        , filter
        , foldl
        , foldr
        , fromList
        , head
        , isEmpty
        , items
        , length
        , map
        , maximumSize
        , member
        , new
        , resize
        , reverse
        , tail
        , take
        )

{-| A data type that holds an upper bounded sliding list.

When you create a `SlidingList` you specify the maximum number of items that it can hold.

⚠ Keep in mind that zero or negative size lists do notreally make sense so the minimum size of a sliding list is 1.

The public interface of module is heavily inspired by the [elm-lang/core/List](http://package.elm-lang.org/packages/elm-lang/core/latest/List) module.

When you create a sliding list, you can `cons` items intoit, and it will slide after the maximum size:

    new 2
        |> cons "A"
        |> cons "B"
        |> cons "C"
        |> items
        == [ "C", "B" ]

    [ "A", "B", "C" ]
        |> fromList 2
        |> items
        == [ "B", "C" ]


# Creating a sliding list

@docs new, fromList, resize, availableSpace, maximumSize


# Basics

@docs cons, items, isEmpty, length, reverse, member


# Sub-lists

@docs head, tail, filter, take, drop


# Putting lists together

@docs append


# Mapping

@docs map


# Folding

@docs foldr, foldl


# Opaque data types

@docs SlidingList

-}


{-| The opaque data type that holds the `List a`.
-}
type SlidingList a
    = SlidingList (List a) Int


{-| Create a new empty sliding list with the specified maximum size.

Keep in mind that zero or negative size lists do not really make sense so the minimum size of a sliding list is 1.

-}
new : Int -> SlidingList a
new size =
    fromList size []


{-| Create a new sliding list from an existing list, with the specified maximum size.

Keep in mind that zero or negative size lists do not really make sense so the minimum size of a sliding list is 1.

If the size of the initial list is greater than the maximum size of the sliding list, the sliding list will slide, as expected.

-}
fromList : Int -> List a -> SlidingList a
fromList size list =
    let
        newList =
            if List.length list <= size then
                list
            else
                list
                    |> List.reverse
                    |> List.take size
                    |> List.reverse
    in
    SlidingList newList (max 1 size)


{-| Determine if this sliding list is empty.
-}
isEmpty : SlidingList a -> Bool
isEmpty (SlidingList list _) =
    List.isEmpty list


{-| Add an element to the front of the sliding list, sliding if it exceeds the maximum size of the sliding list.
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


{-| Reverse this sliding list in place, returning a new sliding list with the same maximum size.

Keep in mind that zero or negative size lists do not really make sense so the minimum size of a sliding list is 1. Additionally, if you resize the list to zero or a negative number, it will be cleared.

-}
reverse : SlidingList a -> SlidingList a
reverse (SlidingList list size) =
    SlidingList (list |> List.reverse) size


{-| Obtain how much available space there is left, before this list starts to slide.
-}
availableSpace : SlidingList a -> Int
availableSpace (SlidingList list size) =
    size - (list |> List.length)


{-| Obtain the current maximum size allowed for this sliding list.
-}
maximumSize : SlidingList a -> Int
maximumSize (SlidingList _ size) =
    size


{-| Resize this sliding list. If the new size is smaller than the current held number of items in the list, it will slide, as expected.
-}
resize : Int -> SlidingList a -> SlidingList a
resize newSize (SlidingList list _) =
    fromList newSize (list |> List.take newSize)


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


{-| Filter this sliding list in place, keeping only the items that satisfy the predicate.

The new sliding list will have the same maximum items as the previous one.

-}
filter : (a -> Bool) -> SlidingList a -> SlidingList a
filter fn (SlidingList list size) =
    SlidingList (list |> List.filter fn) size


{-| Take the first _n_ items of this list into a new sliding list.

The new sliding list will have the same maximum items as the previous one.

-}
take : Int -> SlidingList a -> SlidingList a
take howMany (SlidingList list size) =
    SlidingList (list |> List.take howMany) size


{-| Drop the first _n_ items of this list. The new sliding list will have the same maximum items as the previous one.
-}
drop : Int -> SlidingList a -> SlidingList a
drop howMany (SlidingList list size) =
    SlidingList (list |> List.drop howMany) size


{-| Append a list of elements to this sliding list.

Each element will be treated as if it had been cons'd by their order in the incoming list.

-}
append : List a -> SlidingList a -> SlidingList a
append newItems list =
    newItems |> List.foldl cons list


{-| Apply a function to every element of this sliding list.
-}
map : (a -> b) -> SlidingList a -> SlidingList b
map fn (SlidingList items size) =
    fromList
        size
        (items |> List.map fn)


{-| Reduce the items of this sliding list from the right.
-}
foldr : (a -> b -> b) -> b -> SlidingList a -> b
foldr fn init (SlidingList items _) =
    List.foldr fn init items


{-| Reduce the items of this sliding list from the left.
-}
foldl : (a -> b -> b) -> b -> SlidingList a -> b
foldl fn init (SlidingList items _) =
    List.foldl fn init items
