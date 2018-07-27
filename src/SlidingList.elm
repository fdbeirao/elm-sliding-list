module SlidingList
    exposing
        ( SlidingList
        , availableSpace
        , fromList
        , insert
        , insertAll
        , isEmpty
        , items
        , length
        , map
        , maximumSize
        , member
        , new
        , resize
        , reverse
        )

{-| A data type that holds an upper bounded sliding list.

When you create a `SlidingList` you specify the maximum number of items that it can hold.

When you `insert` elements into this list, they will be added to the _end_ of the sliding list. This means than when you have more items than what your sliding list can hold, they will be dropped from the beginning of the list. `insertAll` works the same way, as if you had called `insert` for each element of the incoming list.

⚠ Keep in mind that zero or negative size lists do not really make sense so the minimum size of a sliding list is 1.

When you create a sliding list, you can `insert` items into it, and it will slide after the maximum size:

    new 2
        |> insert "A"
        |> insert "B"
        |> insert "C"
        |> items
        == [ "B", "C" ]

    [ "A", "B", "C" ]
        |> fromList 2
        |> items
        == [ "B", "C" ]


# Creating a sliding list

@docs new, fromList


# Obtaining information about a sliding list

@docs availableSpace, maximumSize, items


# Resizing a sliding list

@docs resize


# Basics

@docs insert, insertAll, isEmpty, length, reverse, member


# Mapping

@docs map


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
fromList unsafeSize list =
    SlidingList
        (list |> List.drop (List.length list - unsafeSize))
        (max 1 unsafeSize)


{-| Determine if this sliding list is empty.
-}
isEmpty : SlidingList a -> Bool
isEmpty (SlidingList list _) =
    List.isEmpty list


{-| Add an element to the end of this sliding list, dropping elements from the beginning of the list if it exceeds the maximum size of the sliding list.
-}
insert : a -> SlidingList a -> SlidingList a
insert item (SlidingList list size) =
    [ item ]
        |> List.append list
        |> fromList size


{-| Append a list of elements to the end of this sliding list.

Each element will be treated as if it had been inserted by their order in the incoming list.

-}
insertAll : List a -> SlidingList a -> SlidingList a
insertAll newItems (SlidingList list size) =
    newItems
        |> List.append list
        |> fromList size


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

Keep in mind that zero or negative size lists do not really make sense so the minimum size of a sliding list is 1.

Additionally, if you resize the list to zero or a negative number, it will be cleared.

-}
resize : Int -> SlidingList a -> SlidingList a
resize newSize (SlidingList list _) =
    fromList newSize list


{-| Determine if an item is a member of this current sliding list.
-}
member : a -> SlidingList a -> Bool
member item (SlidingList list _) =
    list |> List.member item


{-| Apply a function to every element of this sliding list.
-}
map : (a -> b) -> SlidingList a -> SlidingList b
map fn (SlidingList items size) =
    fromList
        size
        (items |> List.map fn)
