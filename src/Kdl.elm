module Kdl exposing (LocatedNode, LocatedValue, Node(..), Position, SourceRange, Value, ValueContents(..))

import Dict exposing (Dict)
import BigRational exposing (BigRational)

type Node l v =
    Node
        String {- Name -}
        (Maybe String) {- Type -}
        (List (Value l v)) {- Args -}
        (Dict String (Value l v)) {- props -}
        (List (Node l v)) {- children -}
        l

type ValueContents
    = StringVal String
    | NumberVal BigRational
    | BoolVal Bool
    | NullVal

type alias Value l v =
    { location: l
    , typestr: Maybe String
    , contents: v
    }

type alias Position = (Int {- LineNo -}, Int {- Col -})

type alias SourceRange = (Position {- First Character -}, Position {- Last Character -})

type alias LocatedNode = Node SourceRange ValueContents
type alias LocatedValue = Value SourceRange ValueContents
