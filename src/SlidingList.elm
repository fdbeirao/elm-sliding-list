module SlidingList
    exposing
        ( SlidingList
        , positiveInt
        , newSlidingList
        , isEmpty
        , cons
        , items
        , length
        , reverse
        , maximumSize
        , resize
        , member
        , head
        )


type SlidingList a
    = SlidingList (List a) Int


type PositiveInt
    = PositiveInt Int


positiveInt : Int -> Maybe PositiveInt
positiveInt i =
    if i > 0 then
        Just (PositiveInt i)
    else
        Nothing


newSlidingList : PositiveInt -> SlidingList a
newSlidingList (PositiveInt size) =
    SlidingList [] size


isEmpty : SlidingList a -> Bool
isEmpty (SlidingList list _) =
    List.isEmpty list


cons : a -> SlidingList a -> SlidingList a
cons item (SlidingList list size) =
    SlidingList (item :: list |> List.take size) size


items : SlidingList a -> List a
items (SlidingList list _) =
    list


length : SlidingList a -> Int
length (SlidingList list _) =
    List.length list


reverse : SlidingList a -> SlidingList a
reverse (SlidingList list size) =
    SlidingList (list |> List.reverse) size


maximumSize : SlidingList a -> Int
maximumSize (SlidingList _ size) =
    size


resize : PositiveInt -> SlidingList a -> SlidingList a
resize (PositiveInt size) (SlidingList list _) =
    SlidingList (list |> List.take size) size


member : a -> SlidingList a -> Bool
member item (SlidingList list _) =
    list |> List.member item


head : SlidingList a -> Maybe a
head (SlidingList list _) =
    list |> List.head
