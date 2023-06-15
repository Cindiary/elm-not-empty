module NotEmpty.String exposing
  ( String
  , fromString, toString
  , head, tail
  , length, reverse, repeat, replace
  , append, appendString, appendToString, concat, split, join, words, lines
  , slice, left, right, dropLeft, dropRight
  , contains, startsWith, endsWith, indexes, indices
  , toInt, fromInt
  , toFloat, fromFloat
  , fromChar, cons, consString, uncons
  , toList, fromList
  , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
  , map, filter, foldl, foldr, any, all
  )

{-|
# Strings
@docs String, fromString, toString, length, reverse, repeat, replace

# Building and Splitting
@docs append, concat, split, join, words, lines

# Get Substrings
@docs slice, left, right, dropLeft, dropRight

# Check for Substrings
@docs contains, startsWith, endsWith, indexes, indices

# Int Conversions
@docs toInt, fromInt

# Float Conversions
@docs toFloat, fromFloat

# Char Conversions
@docs fromChar, cons, uncons, head, tail

# List Conversions
@docs toList, fromList

# Formatting
Cosmetic operations such as padding with extra characters or trimming whitespace.

@docs toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight

# Higher-Order Functions
@docs map, filter, foldl, foldr, any, all
-}

import Internal.CoreTypes exposing(CoreString)
import NotEmpty.List


{-| Not empty version of `String` -}
type alias String =
  ( Char, CoreString )




{-| Convert a regular string into a not empty string, or Nothing if the string is empty

    fromString "" == Nothing
    fromString "hello" == Just ( consString 'h' "ello" )
-}
fromString : CoreString -> Maybe String
fromString =
  String.uncons


{-| Convert a not empty string into a regular string

  toString ( consString 'h' "ello" ) == "hello"
  toString ( consString 'a' "" ) == "a"
-}
toString : String -> CoreString
toString ( head_, tail_ ) =
  String.cons head_ tail_


{-| Get the first character from a not empty string

  head ( consString 'h' "ello" ) == 'h'
-}
head : String -> Char
head = Tuple.first


{-| Get the tail from a not empty string

  tail ( consString 'h' "ello" ) == "ello"
-}
tail : String -> CoreString
tail = Tuple.second


{-| Same as String.length -}
length : String -> Int
length str =
  String.length ( tail str )


{-| Same as String.reverse -}
reverse : String -> String
reverse str =
  case fromString ( String.reverse ( toString str ) ) of
    Just res ->
      res
    
    Nothing ->
      ( head str, "" )


{-| Same as String.repeat but the string is repeated at least once -}
repeat : Int -> String -> String
repeat n chunk =
  ( head chunk, tail chunk ++ String.repeat ( n - 1 ) ( toString chunk ) )


{-| Same as String.replace -}
replace : String -> String -> String -> String
replace before after string =
  String.replace ( toString before ) ( toString after ) ( toString string )
  |> fromString
  |> Maybe.withDefault after


{-| Same as String.append -}
append : String -> String -> String
append str1 str2 =
  ( head str1, tail str1 ++ toString str2 )


{-| Append a regular string to a not empty string -}
appendString : String -> CoreString -> String
appendString str1 str2 =
  ( head str1, tail str1 ++ str2 )


{-| Append a not empty string to a regular string -}
appendToString : CoreString -> String -> String
appendToString str1 str2 =
  case fromString str1 of
    Just notEmpty ->
      append notEmpty str2
    
    Nothing ->
      str2


{-| Same as String.concat -}
concat : NotEmpty.List.List String -> String
concat strings =
  join "" strings


{-| Same as String.split, returns regular strings -}
split : String -> String -> NotEmpty.List.List CoreString
split sep string =
  case NotEmpty.List.fromList ( String.split ( toString sep ) ( toString string ) ) of
    Just strings ->
      strings
    
    Nothing ->
      NotEmpty.List.singleton ( toString string )


{-| Same as String.join -}
join : CoreString -> NotEmpty.List.List String -> String
join sep chunks =
  ( head ( NotEmpty.List.head chunks ), String.join sep ( tail ( NotEmpty.List.head chunks ) :: List.map toString ( NotEmpty.List.tail chunks ) ) )


{-| Same as String.words but will not return empty words -}
words : String -> List String
words string =
  String.words ( toString string )
  |> List.filterMap fromString


{-| Same as String.lines, returns regular strings -}
lines : String -> List CoreString
lines string =
  String.lines ( toString string )



-- SUBSTRINGS


{-| Same as String.slice -}
slice : Int -> Int -> String -> Maybe String
slice lo hi str =
  String.slice lo hi ( toString str )
  |> fromString


{-| Same as String.left but will take at least 1 character -}
left : Int -> String -> String
left n string =
  ( head string, String.left ( n - 1 ) ( tail string ) )


{-| Same as String.right but will take at least 1 character -}
right : Int -> String -> String
right n string =
  case fromString ( String.right ( max n 1 ) ( toString string ) ) of
    Just res ->
      res
    
    Nothing ->
      ( head string, "" )


{-| Same as String.dropLeft -}
dropLeft : Int -> String -> Maybe String
dropLeft n string =
  if n < 1 then
    Just string
  else
    String.dropLeft n ( toString string )
    |> fromString


{-| Same as String.dropRight -}
dropRight : Int -> String -> Maybe String
dropRight n string =
  if n < 1 then
    Just string
  else
    String.dropRight n ( toString string )
    |> fromString



-- DETECT SUBSTRINGS


{-| Same as String.contains -}
contains : String -> String -> Bool
contains search string =
  String.contains ( toString search ) ( toString string )


{-| Same as String.startsWith -}
startsWith : String -> String -> Bool
startsWith search string =
  String.startsWith ( toString search ) ( toString string )


{-| Same as String.endsWith -}
endsWith : String -> String -> Bool
endsWith search string =
  String.endsWith ( toString search ) ( toString string )


{-| Same as String.indexes -}
indexes : String -> String -> List Int
indexes search string =
  String.indexes ( toString search ) ( toString string )


{-| Alias for `indexes`. -}
indices : String -> String -> List Int
indices search string =
  String.indices ( toString search ) ( toString string )



-- FORMATTING


{-| Same as String.toUpper -}
toUpper : String -> String
toUpper string =
  ( Char.toUpper ( head string ), String.toUpper ( tail string ) )


{-| Same as String.toLower -}
toLower : String -> String
toLower string =
  ( Char.toLower ( head string ), String.toLower ( tail string ) )


{-| Same as String.pad -}
pad : Int -> Char -> String -> String
pad n char string =
  let
    half =
      Basics.toFloat (n - length string) / 2
  in
  if half <= 0 then
    string
  else
    ( char, String.repeat ( ceiling half - 1 ) ( String.fromChar char ) ++ toString string ++ String.repeat ( floor half ) ( String.fromChar char ) )


{-| Same as String.padLeft -}
padLeft : Int -> Char -> String -> String
padLeft n char string =
  let
    chars = n - length string
  in
  if chars < 1 then
    string
  else
    ( char, String.repeat ( chars - 1 ) ( String.fromChar char ) ++ toString string )


{-| Same as String.padRight -}
padRight : Int -> Char -> String -> String
padRight n char string =
  append string ( repeat (n - length string) (fromChar char) )


{-| Same as String.trim -}
trim : String -> Maybe String
trim string =
  String.trim ( toString string )
  |> fromString


{-| Same as String.trimLeft -}
trimLeft : String -> Maybe String
trimLeft string =
  String.trimLeft ( toString string )
  |> fromString


{-| Same as String.trimRight -}
trimRight : String -> Maybe String
trimRight string =
  String.trimRight ( toString string )
  |> fromString



-- INT CONVERSIONS


{-| Same as String.toInt -}
toInt : String -> Maybe Int
toInt str =
  String.toInt ( toString str )


{-| Same as String.fromInt -}
fromInt : Int -> String
fromInt int =
  String.fromInt int
  |> fromString
  |> Maybe.withDefault ( '0', "" )



-- FLOAT CONVERSIONS


{-| Same as String.toFloat -}
toFloat : String -> Maybe Float
toFloat str =
  String.toFloat ( toString str )


{-| Same as String.fromFloat -}
fromFloat : Float -> String
fromFloat float =
  String.fromFloat float
  |> fromString
  |> Maybe.withDefault ( '0', "" )



-- LIST CONVERSIONS


{-| Same as String.toList, returns a not empty list -}
toList : String -> NotEmpty.List.List Char
toList string =
  ( head string, String.toList ( tail string ) )


{-| Same as String.fromList, but takes a not empty list -}
fromList : NotEmpty.List.List Char -> String
fromList list =
  ( NotEmpty.List.head list, String.fromList ( NotEmpty.List.tail list ) )



-- CHAR CONVERSIONS


{-| Same as String.fromChar -}
fromChar : Char -> String
fromChar char =
  ( char, "" )


{-| Same as String.cons -}
cons : Char -> String -> String
cons char string =
  ( char, toString string )


{-| Create a not empty string from a char and a regular string -}
consString : Char -> CoreString -> String
consString =
  Tuple.pair


{-| Same as String.uncons, but always returns a value -}
uncons : String -> ( Char, CoreString )
uncons =
  identity



-- HIGHER-ORDER FUNCTIONS


{-| Same as String.map -}
map : (Char -> Char) -> String -> String
map func string =
  ( func ( head string ), String.map func ( tail string ) )


{-| Same as String.filter -}
filter : (Char -> Bool) -> String -> Maybe String
filter predicate string =
  String.filter predicate ( toString string )
  |> fromString


{-| Same as String.foldl -}
foldl : (Char -> b -> b) -> b -> String -> b
foldl func start string =
  String.foldl func ( func ( head string ) start ) ( tail string )


{-| Same as String.foldr -}
foldr : (Char -> b -> b) -> b -> String -> b
foldr func start string =
  String.foldr func start ( tail string )
  |> func ( head string )


{-| Same as String.any -}
any : (Char -> Bool) -> String -> Bool
any predicate string =
  predicate ( head string ) || String.any predicate ( tail string )


{-| Same as String.all -}
all : (Char -> Bool) -> String -> Bool
all predicate string =
  predicate ( head string ) && String.all predicate ( tail string )

