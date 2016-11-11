module Key exposing (..)


type Key
    = Space
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
      32 ->
          Space

      _ ->
          Unknown
