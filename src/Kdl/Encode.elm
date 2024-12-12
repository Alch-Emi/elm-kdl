module Kdl.Encode exposing (Value, string, int, float, rational, bool, null, Node, node, emptyNode, nodeWithProps, nodeWithArgs, document, documentFromDict, emptyDocument)

{-| Tools for turning Elm values into KDL values

# Encoding Values

@docs Value
@docs string
@docs int
@docs float
@docs rational
@docs bool
@docs null

# Encoding Nodes

@docs Node
@docs emptyNode
@docs nodeWithProps
@docs nodeWithArgs
@docs node

# Encoding Documents

@docs emptyDocument
@docs document
@docs documentFromDict
-}

import Kdl exposing (Document)
import Kdl.Types exposing (KdlNumber(..), Node(..), SourceRange, Value, ValueContents(..))
import Kdl.Util exposing (k, uncurry)

import BigRational exposing (BigRational)
import Dict exposing (Dict)

voidSourceRange : SourceRange
voidSourceRange = ((0,0),(0,0))

toValue : ValueContents -> Value
toValue = Value voidSourceRange Nothing

{-| A KDL Value

These are the specific values used in argument and properties, like numbers and strings
-}
type alias Value = Kdl.Types.Value

{-| Encode a String into a value
-}
string : String -> Value
string = StringVal >> toValue

{-| Encode a integer into a value
-}
int : Int -> Value
int = BigRational.fromInt >> Rational >> NumberVal >> toValue

{-| Encode a float into a value
-}
float : Float -> Value
float f =
    let
        kdlNum = if isNaN f
            then NaN
            else if isInfinite f
                then if f > 0
                    then PositiveInfinity
                    else NegativeInfinity
            else BigRational.fromFloat f |> Rational
    in kdlNum |> NumberVal |> toValue

{-| Encode a [`BigRational`](https://package.elm-lang.org/packages/nunntom/elm-bigrational/latest/BigRational#BigRational) into a value
-}
rational : BigRational -> Value
rational = Rational >> NumberVal >> toValue

{-| Encode a boolean into a value
-}
bool : Bool -> Value
bool = BoolVal >> toValue

{-| The value `#null`
-}
null : Value
null = toValue NullVal

{-| A KDL Node, containing arguments, properties, and children

Notably, this representation of nodes does not contain their name, only their
arguments, properties, and children.  The name of the node will be encoded as
part of the document.  If you wish to track a node's name as well, use the type
`(String, Node)`.
-}
type alias Node = (String -> Kdl.Types.Node)

{-| A node without arguments, children, or properties
-}
emptyNode : Node
emptyNode = node [] Dict.empty emptyDocument

{-| Encode a node with properties, but no children or arguments
-}
nodeWithProps : (propType -> Value) -> List propType -> Node
nodeWithProps encodeProps props = node (List.map encodeProps props) Dict.empty emptyDocument

{-| Encode a node with arguments, but no children or properties
-}
nodeWithArgs : (argType -> Value) -> Dict String argType -> Node
nodeWithArgs encodeArgs args = node [] (Dict.map (k encodeArgs) args) emptyDocument

{-| A fallback method for creating your own node encoders when you need a little more power.
-}
node : (List Value) -> (Dict String Value) -> Document -> Node
node args props children name = Node
    { name = name
    , args = args
    , props = props
    , children = children
    , typ = Nothing
    , location = voidSourceRange
    }

{-| A document with no nodes
-}
emptyDocument : Document
emptyDocument = document []

{-| Encode a document full of order-signifigant nodes
-}
document : List (String, Node) -> Document
document n = (List.map (uncurry (|>)) n, voidSourceRange)


{-| A document full of unique, order-insignifigant nodes
-}
documentFromDict : (nodeType -> Node) -> Dict String nodeType -> Document 
documentFromDict f = Dict.map (k f) >> Dict.toList >> document
