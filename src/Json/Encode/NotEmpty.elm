module Json.Encode.NotEmpty exposing (list, array, dict, set, string)

{-|

Functions for encoding not empty collections into JSON values

@docs list, array, set, dict, string
-}

import NotEmpty
import NotEmpty.List
import NotEmpty.Array
import NotEmpty.Dict
import NotEmpty.Set
import NotEmpty.String

import Json.Encode as E exposing (Value)


{-| Turn a [`NotEmpty.List`](NotEmpty.List#List) into a JSON array. -}
list : ( item -> Value ) -> NotEmpty.List item -> Value
list itemEncoder list_ =
  E.list itemEncoder ( NotEmpty.List.toList list_ )


{-| Turn a [`NotEmpty.Array`](NotEmpty.Array#Array) into a JSON array. -}
array : ( item -> Value ) -> NotEmpty.Array item -> Value
array itemEncoder array_ =
  E.array itemEncoder ( NotEmpty.Array.toArray array_ )


{-| Turn a [`NotEmpty.Set.Set`](NotEmpty.Set#Set) into a JSON array. -}
set : ( comparable -> Value ) -> NotEmpty.Set comparable -> Value
set itemEncoder set_ =
  E.set itemEncoder ( NotEmpty.Set.toSet set_ )


{-| Turn a [`NotEmpty.Dict`](NotEmpty.Dict#Dict) into a JSON object. -}
dict : ( comparable -> String ) -> ( value -> Value ) -> NotEmpty.Dict comparable value -> Value
dict keyEncoder valueEncoder dict_ =
  E.dict keyEncoder valueEncoder ( NotEmpty.Dict.toDict dict_ )


{-| Turn a [`NotEmpty.String`](NotEmpty.String#String) into a JSON array. -}
string : NotEmpty.String -> Value
string str =
  E.string ( NotEmpty.String.toString str )

