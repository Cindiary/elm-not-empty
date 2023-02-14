module NotEmpty.Set exposing
  ( Set
  , singleton, fromSet, toSet
  , withValue, fromListWithValue
  , head, last, tail, hasTail
  , insert, remove
  , member, size
  , union, diff, intersect
  , toList, fromList
  , map, foldl, foldr, filter
  )


{-|
@docs Set

# Create

@docs singleton, withValue, fromList, fromListWithValue

# Transforming from and to regular sets

@docs fromSet, toSet

# New functions

@docs head, last, tail, hasTail

# Modified functions

@docs remove, filter, diff, intersect

# Unchanged functions

@docs member, insert, size

@docs map, foldl, foldr

@docs toList, union

-}

import Set
import NotEmpty.List

{-| Not empty version of `Set` -}
type alias Set item =
  ( item, Set.Set item )


{-| Create a non empty set with one value. -}
singleton : a -> Set a
singleton value =
  ( value, Set.empty )


{-| Create a non empty set from a regular set and a given value.

In contrast to [`fromSet`](NotEmpty.Set#fromSet) this will always return a value since there is always at least 1 value.

    set = withValue 5 ( Set.fromList [ 4, 3 ] )
    toSet set == Set.fromList [ 3, 4, 5 ]
-}
withValue : comparable -> Set.Set comparable -> Set comparable
withValue first set =
  reorder ( first, set )


{-| Same as `Set.fromList` except using a [`NotEmpty.List`](NotEmpty.List#List) -}
fromList : NotEmpty.List.List comparable -> Set comparable
fromList list =
  let
    headItem =
      NotEmpty.List.minimum list
  in
  ( headItem
  , NotEmpty.List.toList list
    |> Set.fromList
    |> Set.remove headItem
  )


{-| Same as [`withValue`](NotEmpty.Set#withValue) but with a list instead of a set -}
fromListWithValue : comparable -> List comparable -> Set comparable
fromListWithValue first list =
  fromList ( first, list )


getHead : Set.Set a -> Maybe a
getHead set =
  let
    helper item res =
      Just ( Maybe.withDefault item res )
  in
  Set.foldl helper Nothing set


reorder : Set comparable -> Set comparable
reorder ( ( headItem, set ) as og ) =
  case getHead set of
    Just setLow ->
      if setLow < headItem then
        ( setLow, Set.insert headItem ( Set.remove setLow set ) )
      else
        og
    
    Nothing ->
      og

{-| Convert a regular set into a not empty set, or Nothing if the set is empty

    fromSet ( Set.fromList [] ) == Nothing
    fromSet ( Set.fromList [ 1, 2, 3 ] ) == Just ( fromListWithValue 1 [ 2, 3 ] )
-}
fromSet : Set.Set comparable -> Maybe ( Set comparable )
fromSet set =
  getHead set
  |> Maybe.map ( \headItem -> ( headItem, Set.remove headItem set ) )


{-| Convert a not empty set into a regular set

    toSet ( fromListWithValue 1 [ 2, 3 ] ) == Set.fromList [ 1, 2, 3 ]
    toSet ( singleton 1 ) == Set.singleton 1
-}
toSet : Set comparable -> Set.Set comparable
toSet ( first, set ) =
  Set.insert first set


{-| Extract the first value from a not empty set.

    head ( fromListWithValue 1 [ 2, 3 ] ) == 1
    head ( singleton 1 ) == 1
-}
head : Set a -> a
head ( first, _ ) =
  first


{-| Extract the last value from a not empty set.

    head ( fromListWithValue 1 [ 2, 3 ] ) == 3
    head ( singleton 1 ) == 1
-}
last : Set a -> a
last ( first, set ) =
  Set.foldl always first set


{-| Returns the elements after the first value as a regular set

    tail ( fromListWithValue 1 [ 2, 3 ] ) == Set.fromList [ 2, 3 ]
    tail ( singleton 1 ) == Set.fromList []
-}
tail : Set a -> Set.Set a
tail ( _, set ) =
  set


{-| Returns wether the set has a tail with elements

    hasTail ( fromListWithValue 1 [ 2, 3 ] ) == True
    hasTail ( singleton 1 ) == False
    hasTail ( fromListWithHead 1 [] ) == False
-}
hasTail : Set a -> Bool
hasTail ( _, set ) =
  not ( Set.isEmpty set )


{-| Same as `Set.toList` -}
toList : Set a -> NotEmpty.List.List a
toList ( headItem, set ) =
  ( headItem, Set.toList set )


{-| Same as `Set.insert` -}
insert : comparable -> Set comparable -> Set comparable
insert value ( ( headItem, set ) as startSet ) =
  if value < headItem then
    ( value, Set.insert headItem set )
  else if value ==
    headItem then
    startSet
  else
    ( headItem, Set.insert value set )


{-| Same as `Set.remove` but will return Nothing if the last value is removed

    remove 1 ( fromListWithValue 1 [ 2, 3 ] ) == Just ( fromListWithValue 2 [ 3 ] )
    remove 1 ( singleton 1 ) == Nothing
    remove 2 ( singleton 1 ) == Just ( singleton 1 )
-}
remove : comparable -> Set comparable -> Maybe ( Set comparable )
remove value ( headItem, set ) =
  if value ==
    headItem then
    fromSet set
  else
    Just ( headItem, Set.remove value set )


{-| Same as `Set.member` -}
member : comparable -> Set comparable -> Bool
member value ( headItem, set ) =
  value ==
    headItem || Set.member value set

{-| Same as `Set.size` -}
size : Set a -> Int
size ( _, set ) =
  Set.size set + 1


{-| Same as `Set.union` -}
union : Set comparable -> Set comparable -> Set comparable
union ( head1, set1 ) ( head2, set2 ) =
  let
    ( lowHead, highHead ) =
      if head1 < head2 then
        ( head1, head2 )
      else
        ( head2, head1 )
  in
  ( lowHead, Set.union ( Set.insert highHead set1 ) set2 )


{-| Same as `Set.diff` but will return Nothing if the resulting set is empty

    diff ( fromListWithValue 1 [ 2, 3 ] ) ( singleton 1 ) == Just ( fromListWithValue 2 2 [ ( 3, 3 ) ] )
    diff ( singleton 1 ) ( singleton 1 ) == Nothing
    diff ( singleton 2 ) ( singleton 1 ) == Just ( singleton 2 )
-}
diff : Set comparable -> Set comparable -> Maybe ( Set comparable )
diff dict1 dict2 =
  Set.diff ( toSet dict1 ) ( toSet dict2 )
  |> fromSet


{-| Same as `Set.intersect` but will return Nothing if the resulting set is empty

    intersect ( fromListWithValue 1 [ 2, 3 ] ) ( singleton 1 ) == Just ( fromListWithValue 1 1 [] )
    intersect ( singleton 1 ) ( singleton 1 ) == Just ( singleton 1 )
    intersect ( singleton 2 ) ( singleton 1 ) == Nothing
-}
intersect : Set comparable -> Set comparable -> Maybe ( Set comparable )
intersect dict1 dict2 =
  Set.intersect ( toSet dict1 ) ( toSet dict2 )
  |> fromSet


{-| Same as `Set.foldr` -}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr fold acc ( headItem, set ) =
  Set.foldr fold acc set
  |> fold headItem


{-| Same as `Set.foldl` -}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl fold acc ( headItem, set ) =
  Set.foldl fold ( fold headItem acc ) set


{-| Same as `Set.map` -}
map : ( comparable1 -> comparable2 ) -> Set comparable1 -> Set comparable2
map func ( headItem, set ) =
  reorder ( func headItem, Set.map func set )


{-| Same as `Set.filter` but will return Nothing if no values satisfy the test

    filter isKeyEven ( fromListWithValue 1 [ 2, 3 ] ) == Just ( fromListWithValue 2 [] )
    filter isKeyEven ( singleton 1 ) == Nothing
-}
filter : ( comparable -> Bool ) -> Set comparable -> Maybe ( Set comparable )
filter func ( headItem, arr ) =
  if func headItem then
    Just ( headItem, Set.filter func arr )
  else
    fromSet ( Set.filter func arr )