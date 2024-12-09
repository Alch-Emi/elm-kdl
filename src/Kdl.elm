module Kdl exposing (Document)

{-| General types and functions which are used throughout the library

@docs Document
-}

import Kdl.Types

{-| A KDL document, held within Elm

This is the core type of this library, and all of the tooling revolves around it.  Check out some of the sub-modules to see things you can do with this document, and how you can create them.
-}
type alias Document = Kdl.Types.Document
