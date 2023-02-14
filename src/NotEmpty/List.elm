module NotEmpty.List exposing
  ( List, PartitionResult(..)
  , singleton, repeat, range, withHead
  , fromList, toList
  , map, indexedMap, foldl, foldr, filter, filterMap, partition
  , head, last, tail, take, drop, unzip
  , hasTail, length, reverse, member, all, any, maximum, minimum, sum, product
  , cons, unCons, append, appendList, concat, concatMap, intersperse, map2, map3, map4, map5
  , sort, sortBy, sortWith
  )

{-|

@docs List

# Create

@docs singleton, repeat, range, withHead, cons

# Transforming from and to regular lists

@docs fromList, toList

# New functions

@docs unCons, last, hasTail, appendList

# Modified functions

@docs head, tail, maximum, minimum

@docs take, drop

@docs filter, filterMap, partition, PartitionResult

# Unchanged functions

@docs length, reverse, member, all, any, sum, product

@docs map, indexedMap, foldl, foldr

@docs append, concat, concatMap, intersperse, unzip

@docs sort, sortBy, sortWith

@docs map2, map3, map4, map5

-}

import Internal.CoreList exposing(CoreList)


{-| Not empty version of `List` -}
type alias List item =
  ( item, CoreList item )


{-| A not empty list containing only the given value.

    toList ( singleton "a" ) == [ "a" ]
-}
singleton : a -> List a
singleton value =
  ( value, [] )


{-| Create a not empty list with *n* copies of a value.

Will always include at least 1 value.

    toList ( repeat 4 "Value" ) == [ "Value", "Value", "Value", "Value" ]
    toList ( repeat 0 "Test" ) == [ "Test" ]
-}
repeat : Int -> a -> List a
repeat n value =
  ( value
  , List.repeat ( n - 1 ) value
  )


{-| Create a list of numbers, every element increasing by one.
You give the lowest and highest number that should be in the list.

Will always include the low value.

    toList ( range 3 6 ) == [ 3, 4, 5, 6 ]
    toList ( range 3 3 ) == [ 3 ]
    toList ( range 6 3 ) == [ 6 ]
-}
range : Int -> Int -> List Int
range lo hi =
  ( lo, List.range ( lo + 1 ) hi )


{-| A not empty list with the given head and tail

    ls = withHead 5 [ 4, 3 ]
    toList ls == [ 5, 4, 3 ]
    head ls == 5
    tail ls == [ 4, 3 ]
-}
withHead : a -> CoreList a -> List a
withHead first list =
  ( first, list )


{-| Deconstruct a not empty list into it's head and tail

    unCons ( singleton 5 ) = ( 5, [] )
    unCons ( withHead 1 [ 2, 3 ] ) = ( 1, [ 2, 3 ] )

-}
unCons : List a -> ( a, CoreList a )
unCons =
  identity


{-| Convert a regular list into a not empty list, or Nothing if the list is empty

    fromList [] == Nothing
    fromList [ 1, 2, 3 ] == Just ( withHead 1 [ 2, 3 ] )
-}
fromList : CoreList a -> Maybe ( List a )
fromList list =
  case list of
    first :: ls -> Just ( first, ls )
    [] -> Nothing


{-| Convert a not empty list into a regular list

  toList ( withHead 1 [ 2, 3 ] ) == [ 1, 2, 3 ]
  toList ( singleton 1 ) == [ 1 ]
-}
toList : List a -> CoreList a
toList ( first, list ) =
  first :: list


{-| Add an element to the front of a not empty list.

  cons 1 ( singleton 2 ) == withHead 1 [ 2 ]
-}
cons : a -> List a -> List a
cons value ( first, list ) =
  ( value, first :: list )


{-| Same as `List.map` -}
map : ( a -> b ) -> List a -> List b
map f ( first, list ) =
  ( f first, List.map f list )


{-| Same as `List.indexedMap` -}
indexedMap : ( Int -> a -> b ) -> List a -> List b
indexedMap f ( first, list ) =
  ( f 0 first, iMapHelp f 1 list )


iMapHelp : ( Int -> a -> b ) -> Int -> CoreList a -> CoreList b
iMapHelp f index list =
  case list of
    item :: ls ->
      f index item :: iMapHelp f ( index + 1 ) ls
    
    [] ->
      []


{-| Same as `List.foldl` -}
foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc ( first, list ) =
  List.foldl func ( func first acc ) list


{-| Same as `List.foldr` -}
foldr : ( a -> b -> b ) -> b -> List a -> b
foldr func acc ( first, list ) =
  func first ( List.foldr func acc list )


{-| Same as `List.filter` but will return Nothing if no elements satisfy the test

    filter isEven ( withHead 1 [ 2, 3, 4, 5, 6 ] ) == Just ( withHead 2 [ 4, 6 ] )
    filter isEven ( withHead 1 [ 3, 5 ] ) == Nothing

-}
filter : ( a -> Bool ) -> List a -> Maybe ( List a )
filter pred ( first, list ) =
  if pred first then
    Just ( first, List.filter pred list )
  else
    fromList ( List.filter pred list )


{-| Same as `List.filterMap` but will return Nothing if all elements return Nothing

    numbers1 : Maybe ( List Int )
    numbers1 =
      filterMap String.toInt ( withHead "3" [ "hi", "12", "4th", "May" ] )

    numbers2 : Maybe ( List Int )
    numbers2 =
      filterMap String.toInt ( withHead "hello" [ "world" ] )

    -- numbers1 == Just [3, 12]
    -- numbers2 == Nothing

-}
filterMap : ( a -> Maybe b ) -> List a -> Maybe ( List b )
filterMap func ( first, list ) =
  case func first of
    Just newFirst ->
      Just ( newFirst, List.filterMap func list )
    
    Nothing ->
      fromList ( List.filterMap func list )


{-| Same as `List.head` but will always always return a value -}
head : List a -> a
head ( first, _ ) =
  first


{-| Extract the last element from a not empty list.

    last ( withHead 1 [ 2, 3 ] ) == 3
    last ( singleton 1 ) == 1
-}
last : List a -> a
last ( first, ls ) =
  lastHelp first ls


lastHelp : a -> CoreList a -> a
lastHelp prevItem list =
  case list of
    item :: ls -> lastHelp item ls
    [] -> prevItem


{-| Same as `List.tail` but will always return a value

    tail ( withHead 1 [ 2, 3 ] ) == [ 2, 3 ]
    tail ( singleton 1 ) == []
-}
tail : List a -> CoreList a
tail ( _, ls ) =
  ls


{-| Same as `List.take` but will always include the first element

    take 2 ( withHead 1 [ 2, 3 ] ) == withHead 1 [ 2 ]
    take 0 ( withHead 1 [ 2, 3 ] ) == singleton 1

-}
take : Int -> List a -> List a
take amount ( first, ls ) =
  ( first, List.take ( amount - 1) ls )


{-| Same as `List.drop` but will always include the last element

    drop 2 ( withHead 1 [ 2, 3 ] ) == withHead 2 [ 3 ]
    drop 0 ( withHead 1 [ 2, 3 ] ) == singleton 3

-}
drop : Int -> List a -> List a
drop amount ls =
  if amount <= 0 then
    ls
  else
    case fromList ( List.drop ( amount - 1 ) ( tail ls ) ) of
      Just res ->
        res
      
      Nothing ->
        ( last ls, [] )


{-| Returns wether the list has a tail with elements

    hasTail ( withHead 1 [ 2, 3 ] ) == True
    hasTail ( singleton 1 ) == False
    hasTail ( withHead 1 [] ) == False
-}
hasTail : List a -> Bool
hasTail ( _, list ) =
  not ( List.isEmpty list )


{-| Same as `List.length` -}
length : List a -> Int
length ( _, list ) =
  List.length list + 1


reorder : ( CoreList a -> CoreList a ) -> List a -> List a
reorder func start =
  if not ( hasTail start ) then
    start
  else
    case func ( toList start ) of
      newFirst :: newLs -> ( newFirst, newLs )
      [] -> start


{-| Same as `List.reverse` -}
reverse : List a -> List a
reverse =
  reorder List.reverse


{-| Same as `List.member` -}
member : a -> List a -> Bool
member item ( first, list ) =
  item == first || List.member item list


{-| Same as `List.all` -}
all : ( a -> Bool ) -> List a -> Bool
all isOkay ( first, list ) =
  isOkay first && List.all isOkay list


{-| Same as `List.any` -}
any : ( a -> Bool ) -> List a -> Bool
any isOkay ( first, list ) =
  isOkay first || List.any isOkay list


{-| Same as `List.maximum` but will always return a value -}
maximum : List comparable -> comparable
maximum ( first, list ) =
  List.foldl max first list


{-| Same as `List.minimum` but will always return a value -}
minimum : List comparable -> comparable
minimum ( first, list ) =
  List.foldl min first list


{-| Same as `List.sum` -}
sum : List number -> number
sum ( first, ls ) =
  List.foldl (+) first ls


{-| Same as `List.product` -}
product : List number -> number
product ( first, ls ) =
  List.foldl (*) first ls


{-| Same as `List.append` -}
append : List a -> List a -> List a
append ( first1, list1 ) ( first2, list2 ) =
  ( first1, list1 ++ first2 :: list2 )


{-| Appends a regular list to a not empty list

    appendList ( singleton 1 ) [ 2, 3 ] == withHead 1 [ 2, 3 ]
    appendList ( withHead 1 [ 2, 3 ] ) [ 4, 5 ] == withHead 1 [ 2, 3, 4, 5 ]

-}
appendList : List a -> CoreList a -> List a
appendList ( first, list1 ) list2 =
  ( first, list1 ++ list2 )


{-| Same as `List.concat` -}
concat : List ( List a ) -> List a
concat ( ( first, ls1 ), lists ) =
  ( first, ls1 ++ List.concatMap toList lists )


{-| Same as `List.concatMap` -}
concatMap : ( a -> List b ) -> List a -> List b
concatMap f ( first, lists ) =
  let
    ( newFirst, ls ) = f first
    fold item list =
      case f item of
        ( x, xs ) -> x :: xs ++ list
  in
  ( newFirst, ls ++ List.foldr fold [] lists )


{-| Same as `List.intersperse` -}
intersperse : a -> List a -> List a
intersperse sep ( ( first, list ) as start ) =
  if List.isEmpty list then
    start
  else
    ( first, sep :: List.intersperse sep list )


{-| Same as `List.map2` -}
map2 : ( a -> b -> result ) -> List a -> List b -> List result
map2 func ( first1, ls1 ) ( first2, ls2 ) =
  ( func first1 first2, List.map2 func ls1 ls2 )


{-| Same as `List.map3` -}
map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 func ( first1, ls1 ) ( first2, ls2 ) ( first3, ls3 ) =
  ( func first1 first2 first3, List.map3 func ls1 ls2 ls3 )


{-| Same as `List.map4` -}
map4 : (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 func ( first1, ls1 ) ( first2, ls2 ) ( first3, ls3 ) ( first4, ls4 ) =
  ( func first1 first2 first3 first4, List.map4 func ls1 ls2 ls3 ls4 )


{-| Same as `List.map5` -}
map5 : (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 func ( first1, ls1 ) ( first2, ls2 ) ( first3, ls3 ) ( first4, ls4 ) ( first5, ls5 ) =
  ( func first1 first2 first3 first4 first5, List.map5 func ls1 ls2 ls3 ls4 ls5 )


{-| Same as `List.sort` -}
sort : List comparable -> List comparable
sort =
  reorder List.sort


{-| Same as `List.sortBy` -}
sortBy : (a -> comparable) -> List a -> List a
sortBy func list =
  reorder ( List.sortBy func ) list


{-| Same as `List.sortWith` -}
sortWith : (a -> a -> Order) -> List a -> List a
sortWith func list =
  reorder ( List.sortWith func ) list


{-| Return type for [`NotEmpty.List.partition`](NotEmpty.List#partition)

Will be `NoMatches` when no elements in the list satisfy the test.

Will be `OnlyMatches` when all elements in the list satisfy the test.

Will be `Both` if there a both elements that do and don't satisfy the test.
The first list are the elements that satisfy the test and the second list are the elements that don't.

-}
type PartitionResult a
  = NoMatches ( List a )
  | OnlyMatches ( List a )
  | Both ( List a ) ( List a )


{-| Same as `List.partition` but will return a [`PartitionResult`](NotEmpty.List#PartitionResult) since either but not both lists could be empty. -}
partition : ( a -> Bool ) -> List a -> PartitionResult a
partition predicate ( first, list ) =
  let
    ( matchList, nonMatchList ) = List.partition predicate list
  in
  if predicate first then
    case fromList nonMatchList of
      Just nonMatches ->
        Both ( first, matchList ) nonMatches
      
      Nothing ->
        OnlyMatches ( first, matchList )
  else
    case fromList matchList of
      Just matches ->
        Both matches ( first, nonMatchList )
      
      Nothing ->
        NoMatches ( first, nonMatchList )


{-| Same as `List.unzip` -}
unzip : List ( a, b ) -> ( List a, List b )
unzip ( ( first1, first2 ), list ) =
  case List.unzip list of
    ( ls1, ls2 ) ->
      ( ( first1, ls1 ), ( first2, ls2 ) )

