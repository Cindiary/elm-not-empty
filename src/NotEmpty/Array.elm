module NotEmpty.Array exposing
  ( Array
  , singleton
  , fromArray
  , withHead
  , toArray
  , length
  , initialize
  , repeat
  , fromList
  , fromListWithHead
  , get
  , set
  , push
  , toList
  , toIndexedList
  , foldr
  , foldl
  , map
  , filter
  , indexedMap
  , append, appendArray, prependArray
  , head, last, tail, hasTail
  )

{-|
@docs Array

# Create

@docs singleton, initialize, repeat, withHead, fromList, fromListWithHead

# Transforming from and to regular arrays

@docs fromArray, toArray

# New functions

@docs head, last, tail, hasTail, appendArray, prependArray

# Modified functions

@docs filter

# Unchanged functions

@docs get, set, push, append, length

@docs map, indexedMap, foldl, foldr

@docs toList, toIndexedList

-}


import Array
import NotEmpty.List


{-| Not empty version of `Array` -}
type alias Array item =
  ( item, Array.Array item )


{-| A not empty array containing only the given value.

    toList ( singleton "a" ) == [ "a" ]
-}
singleton : a -> Array a
singleton value =
  ( value, Array.empty )


{-| Same as `Array.initialize`.

Will always include at least 1 value ( initialized with index 0 ).

    initialize 4 identity    == fromListWithHead 0 [ 1, 2, 3 ]
    initialize 4 ( \n -> n * n ) == fromListWithHead 0 [ 1, 4, 9 ]
    initialize 4 ( always 0 ) == fromListWithHead 0 [ 0, 0, 0 ]
    initialize 0 ( \n -> n + 4 ) == singleton 4

-}
initialize : Int -> ( Int -> a ) -> Array a
initialize len fn =
  ( fn 0, Array.initialize ( len - 1 ) ( \i -> fn ( i + 1 ) ) )


{-| Same as `Array.repeat`.

Will always include at least 1 value.

    repeat 4 "Value" == fromListWithHead "Value" [ "Value", "Value", "Value" ]
    repeat 0 "Test" == singleton "Test"

-}
repeat : Int -> a -> Array a
repeat len val =
  ( val, Array.repeat ( len - 1 ) val )


{-| A not empty array list with the given head and tail

    array = withHead 5 ( Array.fromList [ 4, 3 ] )
    toArray array == Array.fromList [ 5, 4, 3 ]
    head array == 5
    tail array == Array.fromList [ 4, 3 ]
-}
withHead : a -> Array.Array a -> Array a
withHead first array =
  ( first, array )


{-| Same as `Array.fromList` except using a [`NotEmpty.List`](NotEmpty.List#List) -}
fromList : NotEmpty.List.List a -> Array a
fromList ( headItem, ls ) =
  ( headItem, Array.fromList ls )


{-| Same as [`withHead`](NotEmpty.Array#withHead) but with a list instead of a array -}
fromListWithHead : a -> List a -> Array a
fromListWithHead first list =
  ( first, Array.fromList list )


{-| Convert a regular array into a not empty array, or Nothing if the array is empty

    fromArray ( Array.fromList [] ) == Nothing
    fromArray ( Array.fromList [ 1, 2, 3 ] ) == Just ( fromListWithHead 1 [ 2, 3 ] )
-}
fromArray : Array.Array a -> Maybe ( Array a )
fromArray array =
  Array.get 0 array
  |> Maybe.map ( \headItem -> ( headItem, Array.slice 1 ( Array.length array ) array ) )


{-| Convert a not empty array into a regular array

  toArray ( fromListWithHead 1 [ 2, 3 ] ) == Array.fromList [ 1, 2, 3 ]
  toArray ( singleton 1 ) == Array.fromList [ 1 ]
-}
toArray : Array a -> Array.Array a
toArray ( first, arr ) =
  Array.append ( Array.repeat 1 first ) arr


{-| Same as `Array.length` -}
length : Array a -> Int
length ( _, arr ) =
  Array.length arr + 1


{-| Same as `Array.get` -}
get : Int -> Array a -> Maybe a
get index ( headItem, arr ) =
  if index ==
    0 then
    Just headItem
  else
    Array.get ( index - 1 ) arr


{-| Extract the first element from a not empty array.

    head ( fromListWithHead 1 [ 2, 3 ] ) == 1
    head ( singleton 1 ) == 1
-}
head : Array a -> a
head ( first, _ ) =
  first


{-| Extract the last element from a not empty array.

    last ( fromListWithHead 1 [ 2, 3 ] ) == 3
    last ( singleton 1 ) == 1
-}
last : Array a -> a
last ( first, arr ) =
  Array.foldl always first arr


{-| Returns the elements after the first element as a regular array

    tail ( fromListWithHead 1 [ 2, 3 ] ) == Array.fromList [ 2, 3 ]
    tail ( singleton 1 ) == Array.fromList []
-}
tail : Array a -> Array.Array a
tail ( _, arr ) =
  arr


{-| Returns wether the array has a tail with elements

    hasTail ( fromListWithHead 1 [ 2, 3 ] ) == True
    hasTail ( singleton 1 ) == False
    hasTail ( fromListWithHead 1 [] ) == False
-}
hasTail : Array a -> Bool
hasTail ( _, arr ) =
  not ( Array.isEmpty arr )


{-| Same as `Array.set` -}
set : Int -> a -> Array a -> Array a
set index value ( headItem, arr ) =
  if index ==
    0 then
    ( value, arr )
  else
    ( headItem, Array.set ( index - 1 ) value arr )


{-| Same as `Array.push` -}
push : a -> Array a -> Array a
push value ( headItem, arr ) =
  ( headItem, Array.push value arr )


{-| Same as `Array.toList` -}
toList : Array a -> NotEmpty.List.List a
toList ( headItem, arr ) =
  ( headItem, Array.toList arr )


{-| Same as `Array.toIndexedList` -}
toIndexedList : Array a -> NotEmpty.List.List (Int, a)
toIndexedList ( headItem, arr ) =
  let
    helper entry ( index, list ) =
      ( index - 1, (index,entry) :: list )
  in
  ( ( 0, headItem )
  , Array.foldr helper ( Array.length arr, [] ) arr
    |> Tuple.second
  )


{-| Same as `Array.foldr` -}
foldr : (a -> b -> b) -> b -> Array a -> b
foldr fold acc ( headItem, arr ) =
  Array.foldr fold acc arr
  |> fold headItem


{-| Same as `Array.foldl` -}
foldl : (a -> b -> b) -> b -> Array a -> b
foldl fold acc ( headItem, arr ) =
  Array.foldl fold ( fold headItem acc ) arr


{-| Same as `Array.map` -}
map : ( a -> b ) -> Array a -> Array b
map func ( headItem, arr ) =
  ( func headItem, Array.map func arr )


{-| Same as `Array.filter` but will return Nothing if no elements satisfy the test

    filter isEven ( fromListWithHead 1 [ 2, 3, 4, 5, 6 ] ) == Just ( fromListWithHead 2 [ 4, 6 ] )
    filter isEven ( fromListWithHead 1 [ 3, 5 ] ) == Nothing

-}
filter : ( a -> Bool ) -> Array a -> Maybe ( Array a )
filter func ( headItem, arr ) =
  if func headItem then
    Just ( headItem, Array.filter func arr )
  else
    fromArray ( Array.filter func arr )


{-| Same as `Array.indexedMap` -}
indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap func ( headItem, arr ) =
  ( func 0 headItem
  , Array.indexedMap ( \i val -> func ( i + 1 ) val ) arr
  )


{-| Same as `Array.append` -}
append : Array a -> Array a -> Array a
append ( head1, arr1 ) ( head2, arr2 ) =
  ( head1, Array.append ( Array.push head2 arr1 ) arr2 )


{-| Append a regular array to a not empty array

    appendArray ( repeat 2 42 ) ( Array.repeat 3 81 ) == fromListWithHead 42 [ 42, 81, 81, 81 ]
-}
appendArray : Array a -> Array.Array a -> Array a
appendArray ( head_, arr1 ) arr2 =
  ( head_, Array.append arr1 arr2 )


{-| Prepend a regular array to a not empty array

    prependArray ( Array.repeat 2 42 ) ( repeat 3 81 ) == fromListWithHead 42 [ 42, 81, 81, 81 ]
-}
prependArray : Array.Array a -> Array a -> Array a
prependArray arr1 arr2 =
  case fromArray arr1 of
    Just notEmptyArray ->
      append notEmptyArray arr2
    
    Nothing ->
      arr2

