module NotEmpty.Dict exposing
  ( Dict
  , singleton, fromDict, toDict, withPair
  , head, last, tail, hasTail
  , get, insert, remove
  , member, size
  , union, diff, intersect, merge
  , toList, fromList, fromListWithPair
  , map, foldl, foldr, filter
  )

{-|
@docs Dict

# Create

@docs singleton, withPair, fromList, fromListWithPair

# Transforming from and to regular dictionaries

@docs fromDict, toDict

# New functions

@docs head, last, tail, hasTail

# Modified functions

@docs remove, filter, diff, intersect

# Unchanged functions

@docs get, member, insert, size, toList

@docs map, foldl, foldr

@docs union, merge

-}

import Dict
import NotEmpty.List

{-| Not empty version of `Dict` -}
type alias Dict key value =
  ( ( key, value ), Dict.Dict key value )


{-| Create a non empty dictionary with one key-value pair. -}
singleton : key -> value -> Dict key value
singleton key value =
  ( ( key, value ), Dict.empty )


{-| Create a non empty dictionary from a regular dictionary and a given pair.

In contrast to [`fromDict`](NotEmpty.Dict#fromDict) this will always return a value since there is always at least 1 pair.

    dict = withPair 5 "Five" ( Dict.fromList [ ( 4, "Four" ), ( 3, "Three" ) ] )
    toDict dict == Dict.fromList [ ( 3, "Three" ), ( 4, "Four" ), ( 5, "Five" ) ]
-}
withPair : comparable -> value -> Dict.Dict comparable value -> Dict comparable value
withPair key value dict =
  reorder ( ( key, value ), dict )


reorder : Dict comparable value -> Dict comparable value
reorder ( ( ( headKey, headValue ), dict ) as og ) =
  case getHead dict of
    Just ( ( lowKey, _ ) as lowPair ) ->
      if lowKey < headKey then
        ( lowPair, Dict.insert headKey headValue ( Dict.remove lowKey dict ) )
      else
        og
    
    Nothing ->
      og


getHead : Dict.Dict key value -> Maybe ( key, value )
getHead dict =
  let
    helper key value res =
      case res of
        Just _ -> res
        Nothing -> Just ( key, value )
  in
  Dict.foldl helper Nothing dict


{-| Same as `Dict.fromList` except using a [`NotEmpty.List`](NotEmpty.List#List) -}
fromList : NotEmpty.List.List ( comparable, value ) -> Dict comparable value
fromList ( ( headPair, _ ) as list ) =
  NotEmpty.List.toList list
  |> Dict.fromList
  |> fromDict
  |> Maybe.withDefault ( headPair, Dict.empty )


{-| Same as [`withPair`](NotEmpty.Dict#withPair) but with a list or pairs instead of a dict -}
fromListWithPair : comparable -> value -> List ( comparable, value ) -> Dict comparable value
fromListWithPair headKey headValue pairs =
  withPair headKey headValue ( Dict.fromList pairs )


{-| Convert a regular dict into a not empty dict, or Nothing if the dict is empty

    fromDict ( Dict.fromList [] ) == Nothing
    fromDict ( Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ] ) == Just ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] )
-}
fromDict : Dict.Dict comparable value -> Maybe ( Dict comparable value )
fromDict dict =
  getHead dict
  |> Maybe.map ( \headPair -> ( headPair, Dict.remove ( Tuple.first headPair ) dict ) )


{-| Convert a not empty dict into a regular dict

    toDict ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ]
    toDict ( singleton 1 "one" ) == Dict.fromList [ ( 1, "one" ) ]
-}
toDict : Dict comparable value -> Dict.Dict comparable value
toDict ( ( key, value ), dict ) =
  Dict.insert key value dict


{-| Extract the first pair from a not empty dictionary.

    head ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == ( 1, "one" )
    head ( singleton 1 "one" ) == ( 1, "one" )
-}
head : Dict key value -> ( key, value )
head ( first, _ ) =
  first


{-| Extract the last pair from a not empty dictionary.

    last ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == ( 3, "three" )
    last ( singleton 1 "one" ) == ( 1, "one" )
-}
last : Dict key value -> ( key, value )
last ( first, dict ) =
  let
    helper key value _ =
      ( key, value )
  in
  Dict.foldl helper first dict


{-| Returns the elements after the first pair as a regular dictionary

    tail ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == Dict.fromList [ ( 2, "two" ), ( 3, "three" ) ]
    tail ( singleton 1 "one" ) == Dict.fromList []
-}
tail : Dict key value -> Dict.Dict key value
tail ( _, dict ) =
  dict


{-| Returns wether the dictionary has a tail with elements

    hasTail ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == True
    hasTail ( singleton 1 "one" ) == False
    hasTail ( fromListWithHead 1 "one" [] ) == False
-}
hasTail : Dict key value -> Bool
hasTail ( _, dict ) =
  not ( Dict.isEmpty dict )


{-| Same as `Dict.toList` -}
toList : Dict key value -> NotEmpty.List.List ( key, value )
toList ( headItem, dict ) =
  ( headItem, Dict.toList dict )


{-| Same as `Dict.get` -}
get : comparable -> Dict comparable value -> Maybe value
get key ( ( headKey, headValue ), dict ) =
  if key ==
    headKey then
    Just headValue
  else
    Dict.get key dict


{-| Same as `Dict.insert` -}
insert : comparable -> value -> Dict comparable value -> Dict comparable value
insert key value ( ( headKey, headValue ) as headItem, dict ) =
  if key ==
    headKey then
    ( ( key, value ), dict )
  else if key < headKey then
    ( ( key, value), Dict.insert headKey headValue dict )
  else
    ( headItem, Dict.insert key value dict )


{-| Same as `Dict.remove` but will return Nothing if the last value is removed

    remove 1 ( fromListWithPair 1 "one" [ ( 2, "two" ), ( 3, "three" ) ] ) == Just ( fromListWithPair 2 "two" [ ( 3, "three" ) ] )
    remove 1 ( singleton 1 "one" ) == Nothing
    remove 2 ( singleton 1 "one" ) == Just ( singleton 1 "one" )
-}
remove : comparable -> Dict comparable value -> Maybe ( Dict comparable value )
remove key ( headItem, dict ) =
  if key ==
    Tuple.first headItem then
    fromDict dict
  else
    Just ( headItem, Dict.remove key dict )


{-| Same as `Dict.member` -}
member : comparable -> Dict comparable value -> Bool
member key ( ( headKey, _ ), dict ) =
  key ==
    headKey || Dict.member key dict


{-| Same as `Dict.size` -}
size : Dict key value -> Int
size ( _, dict ) =
  Dict.size dict + 1


{-| Same as `Dict.union` -}
union : Dict comparable value -> Dict comparable value -> Dict comparable value
union ( head1, dict1 ) ( head2, dict2 ) =
  let
    ( lowHead, ( highKey, highValue ) ) =
      if Tuple.first head1 < Tuple.first head2 then
        ( head1, head2 )
      else
        ( head2, head1 )
  in
  ( lowHead, Dict.union ( Dict.insert highKey highValue dict1 ) dict2 )


{-| Same as `Dict.diff` but will return Nothing if the resulting dictionary is empty

    diff ( fromListWithPair 1 1 [ ( 2, 2 ), ( 3, 3 ) ] ) ( singleton 1 "one" ) == Just ( fromListWithPair 2 2 [ ( 3, 3 ) ] )
    diff ( singleton 1 1 ) ( singleton 1 "one" ) == Nothing
    diff ( singleton 2 2 ) ( singleton 1 "one" ) == Just ( singleton 2 2 )
-}
diff : Dict comparable value -> Dict comparable a -> Maybe ( Dict comparable value )
diff dict1 dict2 =
  Dict.diff ( toDict dict1 ) ( toDict dict2 )
  |> fromDict


{-| Same as `Dict.intersect` but will return Nothing if the resulting dictionary is empty

    intersect ( fromListWithPair 1 1 [ ( 2, 2 ), ( 3, 3 ) ] ) ( singleton 1 "one" ) == Just ( fromListWithPair 1 1 [] )
    intersect ( singleton 1 1 ) ( singleton 1 "one" ) == Just ( singleton 1 1 )
    intersect ( singleton 2 2 ) ( singleton 1 "one" ) == Nothing
-}
intersect : Dict comparable value -> Dict comparable a -> Maybe ( Dict comparable value )
intersect dict1 dict2 =
  filter ( \k _ -> member k dict2 ) dict1


{-| Same as `Dict.merge` -}
merge
  :  (comparable -> a -> result -> result)
  -> (comparable -> a -> b -> result -> result)
  -> (comparable -> b -> result -> result)
  -> Dict comparable a
  -> Dict comparable b
  -> result
  -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
  Dict.merge leftStep bothStep rightStep ( toDict leftDict ) ( toDict rightDict ) initialResult



{-| Same as `Dict.foldr` -}
foldr : ( key -> value -> b -> b) -> b -> Dict key value -> b
foldr fold acc ( ( headKey, headValue ), dict ) =
  Dict.foldr fold acc dict
  |> fold headKey headValue


{-| Same as `Dict.foldl` -}
foldl : ( key -> value -> b -> b ) -> b -> Dict key value -> b
foldl fold acc ( ( headKey, headValue ), dict ) =
  Dict.foldl fold ( fold headKey headValue acc ) dict


{-| Same as `Dict.map` -}
map : ( key -> a -> b ) -> Dict key a -> Dict key b
map func ( ( headKey, headValue ), dict ) =
  ( ( headKey, func headKey headValue ), Dict.map func dict )


{-| Same as `Dict.filter` but will return Nothing if no pairs satisfy the test

    filter isKeyEven ( fromListWithPair 1 1 [ ( 2, 2 ), ( 3, 3 ) ] ) == Just ( fromListWithPair 2 2 [] )
    filter isKeyEven ( singleton 1 1 ) == Nothing
-}
filter : ( comparable -> value -> Bool ) -> Dict comparable value -> Maybe ( Dict comparable value )
filter func ( ( headKey, headValue ) as headItem, arr ) =
  if func headKey headValue then
    Just ( headItem, Dict.filter func arr )
  else
    fromDict ( Dict.filter func arr )