module Kdl.Serialize exposing (document)

{-| Turn KDL documents into `String`s

@docs document
-}

import Kdl.Types exposing (Document, KdlNumber(..), Node(..), Value, ValueContents(..))
import Kdl.Shared exposing (checkForIllegalBareStrings, illegalCharacter)
import Kdl.Util exposing (flip, toHex)

import BigInt
import BigRational exposing (BigRational)
import Dict exposing (Dict)
import Maybe exposing (withDefault)

---------------[ Bags ]-------------------------------

type Bag t
    = Leaf t
    | Bunch (List (Bag t))

foldrBag : (i -> o -> o) -> o -> Bag i -> o
foldrBag f init b = case b of
    Leaf v -> f v init
    Bunch [] -> init
    Bunch (h :: t) -> 
        let
            foldedTail = (foldrBag f init (Bunch t))
        in foldrBag f foldedTail h

emptyBag : Bag i
emptyBag = Bunch []

singletonBag : i -> Bag i
singletonBag = Leaf

concatBags : List (Bag i) -> Bag i
concatBags = Bunch

concatSepBags : Bag i -> List (Bag i) -> Bag i
concatSepBags = List.intersperse >> (<<) concatBags

prependBag : Bag i -> Bag i -> Bag i
prependBag b1 b2 = concatBags [b1, b2]

strConcatBag : Bag String -> String
strConcatBag = foldrBag (++) ""

bindBag : (i -> Bag o) -> Bag i -> Bag o
bindBag f b = case b of
    Leaf v -> Bunch [f v]
    Bunch cs -> Bunch <| List.map (bindBag f) cs

mapBag : (i -> o) -> Bag i -> Bag o
mapBag f b = case b of
    Leaf v -> Leaf (f v)
    Bunch bs -> Bunch <| List.map (mapBag f) bs

anyBag : (t -> Bool) -> Bag t -> Bool
anyBag f = mapBag f >> foldrBag (||) False

-------------------------------------------------------

type alias Line = Bag String

type alias Lines = Bag Line

indentB : Bag String
indentB = singletonBag <| String.repeat 4 " "

indent : Lines -> Lines
indent = mapBag (prependBag indentB)

linesToString : Lines -> String
linesToString = bindBag (flip prependBag newlineB) >> strConcatBag

----------------------------------------------------

escapes : Dict Char Char
escapes = Dict.fromList
    [ ('\n', 'n')
    , ('\r', 'r')
    , ('\t', 't')
    , ('"', '"')
    , ('\u{0008}', 'b')
    , ('\u{000C}', 'f')
    , ('\\', '\\')
    ]

strEscape : String -> String
strEscape =
    let
        escape c = case Dict.get c escapes of
            Just escCode -> String.fromList ['\\', escCode]
            Nothing -> if illegalCharacter c
                then ("\\u{" ++ toHex (Char.toCode c) ++ "}")
                else String.fromChar c
    in String.toList >> List.map escape >> String.concat

quoteB : Bag String
quoteB = singletonBag "\""

eB : Bag String
eB = singletonBag "E"

minusB : Bag String
minusB = singletonBag "-"

newlineB : Bag String
newlineB = singletonBag "\n"

eqB : Bag String
eqB = singletonBag "="

spaceB : Bag String
spaceB = singletonBag " "

ocurlB : Bag String
ocurlB = singletonBag "{"

ccurlB : Bag String
ccurlB = singletonBag "}"

oparenB : Bag String
oparenB = singletonBag "("

cparenB : Bag String
cparenB = singletonBag ")"

serializeStr : String -> Line
serializeStr s = if checkForIllegalBareStrings True True True True s |> withDefault False
    then concatBags [quoteB, singletonBag (strEscape s), quoteB]
    else singletonBag s

zero : BigRational
zero = BigRational.fromInt 0

ten : BigRational
ten = BigRational.fromInt 10

serializeNumberVal : BigRational -> Line
serializeNumberVal n =
    let
        (intPart, fractionalPart) = BigRational.toMixedFraction n
        aux rat exp =
            if BigRational.lt rat zero
                then concatBags
                    [ minusB
                    , aux (BigRational.abs rat) exp
                    ]
            else if not <| BigRational.gt ten rat
                then aux (BigRational.div rat ten) (exp + 1)
            else if not <| BigRational.gt rat (BigRational.fromInts 1 10)
                then aux (BigRational.mul rat ten) (exp - 1)
            else if exp == 0
                then BigRational.toDecimalString 30 rat |> singletonBag
                else concatBags
                    [ BigRational.toDecimalString 30 rat |> singletonBag
                    , eB
                    , singletonBag <| if exp > 0 then "+" else ""
                    , String.fromInt exp |> singletonBag
                    ]
    in if BigRational.compare fractionalPart zero == EQ
        then BigInt.toString intPart |> singletonBag
        else aux n 0

serializeType : Maybe String -> Line
serializeType t_ = case t_ of
    Just t -> concatBags
        [ oparenB
        , serializeStr t
        , cparenB
        ]
    Nothing -> emptyBag

serializeVal : Value -> Line
serializeVal {typestr, contents} =
    let
        typeSerialized = serializeType typestr
        contentsSerialized = case contents of
            StringVal s -> serializeStr s
            NumberVal PositiveInfinity -> singletonBag "#inf"
            NumberVal NegativeInfinity -> singletonBag "#-inf"
            NumberVal NaN -> singletonBag "#nan"
            NumberVal (Rational n) -> serializeNumberVal n
            BoolVal True -> singletonBag "#true"
            BoolVal False -> singletonBag "#false"
            NullVal -> singletonBag "#null"
    in concatBags [typeSerialized, contentsSerialized]

serializeProp : (String, Value) -> Line
serializeProp (k, v) = concatBags
    [ serializeStr k
    , eqB
    , serializeVal v
    ]

serializeNode : Node -> Lines
serializeNode (Node {name, typ, args, props, children}) =
    let
        serializedType = serializeType typ
        serializedName = concatBags [serializedType, serializeStr name]
        serializedArgs = concatSepBags spaceB <| List.map serializeVal args
        serializedProps = Dict.toList props
            |> List.sortBy Tuple.first
            |> List.map serializeProp
            |> concatSepBags spaceB
        serializedChildren = List.map serializeNode children
            |> concatBags
        components = [serializedName, serializedArgs, serializedProps]
            |> List.filter (anyBag (not << String.isEmpty))
            |> concatSepBags spaceB
    in if List.length children > 0
        then concatBags
            [ singletonBag <| concatSepBags spaceB [components, ocurlB]
            , indent serializedChildren
            , singletonBag ccurlB
            ]
        else singletonBag components

serializeDocument : List Node -> Lines
serializeDocument = List.map serializeNode >> concatBags

{-| Convert a KDL document into a `String`
-}
document : Document -> String
document = serializeDocument >> linesToString
