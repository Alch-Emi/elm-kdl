module Kdl exposing (LocatedNode, Node(..), Position, SourceRange, Value(..))

import Dict exposing (Dict)
import BigRational exposing (BigRational)

type Node l v =
    Node
        String {- Name -}
        (List (l, v)) {- Args -}
        (Dict String (l, v)) {- props -}
        (List (Node l v)) {- children -}
        l

type Value
    = StringVal String
    | NumberVal BigRational
    | BoolVal Bool
    | NullVal

type alias Position = (Int {- LineNo -}, Int {- Col -})

type alias SourceRange = (Position {- First Character -}, Position {- Last Character -})

type alias LocatedNode = Node SourceRange Value
