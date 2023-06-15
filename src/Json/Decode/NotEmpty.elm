module Json.Decode.NotEmpty exposing (list, array, dict, set, string)

{-|

Functions for encoding not empty collections into JSON values

@docs list, array, dict, set, string
-}


import NotEmpty
import NotEmpty.List
import NotEmpty.Array
import NotEmpty.Dict
import NotEmpty.Set

import Json.Decode as D exposing (Decoder)
import NotEmpty.String


{-| Decode a JSON array into a [`NotEmpty.List`](NotEmpty.List#List)

    decodeString ( list int ) "[ 1, 2, 3 ]"   == Ok ( NotEmpty.List.withHead 1 [ 2, 3 ] )
    decodeString ( list bool ) "[]"           == Err ...
-}
list : Decoder item -> Decoder ( NotEmpty.List item )
list itemDecoder =
  D.oneOrMore NotEmpty.List.withHead itemDecoder


{-| Decode a JSON array into a [`NotEmpty.Array`](NotEmpty.Array#Array)-}
array : Decoder item -> Decoder ( NotEmpty.Array item )
array itemDecoder =
  notEmptyDecoder NotEmpty.Array.fromArray ( D.array itemDecoder ) "expected at least 1 element"


{-| Decode a JSON object into a [`NotEmpty.Dict`](NotEmpty.Dict#Dict)

    import Json.Decode as D
    import Json.Decode.NotEmpty exposing(dict)

    D.decodeString ( dict D.int ) "{ \"alice\": 42, \"bob\": 99 }" == Ok ( NotEmpty.Dict.fromListWithPair "alice" 42 [ ( "bob", 99 ) ] )
    
    D.decodeString ( dict D.int ) "{}" == Err ...
-}
dict : Decoder value -> Decoder ( NotEmpty.Dict String value )
dict itemDecoder =
  notEmptyDecoder NotEmpty.Dict.fromDict ( D.dict itemDecoder ) "expected at least 1 field"


{-| Decode a JSON array into a [`NotEmpty.Set.Set`](NotEmpty.Set#Set) -}
set : Decoder comparable -> Decoder ( NotEmpty.Set comparable )
set itemDecoder =
  D.map NotEmpty.Set.fromList ( list itemDecoder )


{-| Decode a JSON string into a [`NotEmpty.String.String`](NotEmpty.String#String) -}
string : Decoder NotEmpty.String
string =
  notEmptyDecoder NotEmpty.String.fromString D.string "String is empty"


notEmptyDecoder : ( collection -> Maybe notEmptyCollection ) -> Decoder collection -> String -> Decoder notEmptyCollection
notEmptyDecoder getNotEmpty decoder failMessage =
  let
    fromCollection collection =
      case getNotEmpty collection of
        Just notEmpty ->
          D.succeed notEmpty
        
        Nothing ->
          D.fail failMessage
  in
  D.andThen fromCollection decoder

