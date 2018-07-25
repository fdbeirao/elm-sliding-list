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
newSlidingList size =
    fromList size []


fromList : PositiveInt -> List a -> SlidingList a
fromList (PositiveInt size) list =
    SlidingList
        (list
            |> List.reverse
            |> List.take size
            |> List.reverse
        )
        size


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


tail : SlidingList a -> Maybe (List a)
tail (SlidingList list _) =
    list |> List.tail


filter : (a -> Bool) -> SlidingList a -> SlidingList a
filter f (SlidingList list size) =
    SlidingList (list |> List.filter f) size


take : Int -> SlidingList a -> SlidingList a
take howMany (SlidingList list size) =
    SlidingList (list |> List.take howMany) size
