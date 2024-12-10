module Kdl.Types exposing (Document, KdlNumber(..), Node(..), Position, SourceRange, Value, ValueContents(..))

{-| Types for representing KDL nodes!

# Core Types
@docs Node
@docs Value
@docs ValueContents
@docs KdlNumber

# Source Tracking Helpers
@docs Position
@docs SourceRange
@docs LocatedNode
@docs LocatedValue
-}

import Dict exposing (Dict)
import BigRational exposing (BigRational)

{-| The foundation of KDL is the node.  All documents can be understood as a nested heirarchy of nodes, where each node has children, values, and properties.
-}
type Node = Node
    { name      : String
    , typ       : Maybe String
    , args      : List Value
    , props     : Dict String Value
    , children  : Document
    , location  : SourceRange
    }

{-| A document (or equivalently, a child block) is simply an ordered list of [`Node`](#Node)s 

There can be several nodes with the same name, or no nodes at all!
-}
type alias Document = (List Node, SourceRange)

{-| A basic KDL value.  Represents anything that can be used as an argument to a node or the value of a property.

This will almost always appear within a [`Value`](Kdl#Value)
-}
type ValueContents
    = StringVal String
    | NumberVal KdlNumber
    | BoolVal Bool
    | NullVal

{-| A KDL number

KDL does not commit to having any specific restrictions on what kinds of numbers can appear in its documents, so this type attempts to encompass all possibilities.
-}
type KdlNumber
    = Rational BigRational
    | PositiveInfinity
    | NegativeInfinity
    | NaN

{-| A KDL value, including metadata

Additionally contains some extra metadata about the value, including it's type string, if it has one, and it's location within the source file.
-}
type alias Value =
    { location: SourceRange
    , typestr: Maybe String
    , contents: ValueContents
    }

{-| A position within the source file.

Consists of the line number and column of a specific character
-}
type alias Position = (Int {- LineNo -}, Int {- Col -})

{-| A range of characters within the source file

Note that this is and *inclusive* range, which means that both the characters at both the first and second position are considered to be within the range, as well as all characters between them.
-}
type alias SourceRange = (Position {- First Character -}, Position {- Last Character -})
