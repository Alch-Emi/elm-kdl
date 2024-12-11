module Kdl exposing (Document, SourceRange)

{-| General types and functions which are used throughout the library

@docs Document
@docs SourceRange
-}

import Kdl.Types

{-| A KDL document, held within Elm

This is the core type of this library, and all of the tooling revolves around it.  Check out some of the sub-modules to see things you can do with this document, and how you can create them.
-}
type alias Document = Kdl.Types.Document

{-| A range of characters within the source file

This is a type alias for `((Int, Int), (Int, Int))`, where the first pair of ints is the row and column of the first character, and the second pair of ints is the row and column of the final character.

Note that this is and *inclusive* range, which means that both the characters at both the first and second position are considered to be within the range, as well as all characters between them.
-}
type alias SourceRange = Kdl.Types.SourceRange
