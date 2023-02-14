# Not empty

This library provides variation of core collections ( List, Array, Dict and Set ) that are not empty ( contain at least 1 element ).

This can help ensure that certain collections are never empty, prevents impossible states and makes for cleaner and simpler code.

For example `NotEmpty.List.maximum` always return a value where `List.maximum` returns Maybe
