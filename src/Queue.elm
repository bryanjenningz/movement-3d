module Queue exposing (Queue, add, empty, fromList, remove, toList)


type Queue a
    = Queue (List a) (List a)


empty : Queue a
empty =
    Queue [] []


fromList : List a -> Queue a
fromList oldestToNewest =
    Queue oldestToNewest []


toList : Queue a -> List a
toList (Queue oldestToNewest newestToOldest) =
    oldestToNewest ++ List.reverse newestToOldest


add : a -> Queue a -> Queue a
add newest (Queue oldestToNewest newestToOldest) =
    Queue oldestToNewest (newest :: newestToOldest)


remove : Queue a -> ( Maybe a, Queue a )
remove queue =
    case queue of
        Queue [] [] ->
            ( Nothing, queue )

        Queue [] newestToOldest ->
            remove (Queue (List.reverse newestToOldest) [])

        Queue (oldest :: oldestToNewest) newestToOldest ->
            ( Just oldest, Queue oldestToNewest newestToOldest )
