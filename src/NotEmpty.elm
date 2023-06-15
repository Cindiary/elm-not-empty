module NotEmpty exposing (List, Array, Set, Dict, String)

{-|
Module with aliases for the different collection types so you can type

    foo : NotEmpty.List Int
instead of

    foo : NotEmpty.List.List Int

@docs List, Array, Dict, Set, String

-}

import NotEmpty.List
import NotEmpty.Array
import NotEmpty.Dict
import NotEmpty.Set
import NotEmpty.String

{-| Alias for [`NotEmpty.List.List`](NotEmpty.List#List) -}
type alias List a =
  NotEmpty.List.List a


{-| Alias for [`NotEmpty.Array.Array`](NotEmpty.Array#Array) -}
type alias Array a =
  NotEmpty.Array.Array a


{-| Alias for [`NotEmpty.Dict.Dict`](NotEmpty.Dict#Dict) -}
type alias Dict key value =
  NotEmpty.Dict.Dict key value


{-| Alias for [`NotEmpty.Set.Set`](NotEmpty.Set#Set) -}
type alias Set a =
  NotEmpty.Set.Set a


{-| Alias for [`NotEmpty.String.String`](NotEmpty.String#String) -}
type alias String =
  NotEmpty.String.String