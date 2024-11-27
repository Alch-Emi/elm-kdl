module Kdl.Serialize exposing (..)

import Kdl exposing (Node(..), Value, ValueContents(..))
import Kdl.Shared exposing (identifierCharacter, initialCharacter)
import Kdl.Util exposing (flip, maybe)

import BigInt
import BigRational exposing (BigRational)
import BigInt exposing (BigInt)
import Dict

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

replaceToBag : String -> String -> String -> Line
replaceToBag from to =
    String.split from
    >> List.map singletonBag
    >> List.intersperse (singletonBag to)
    >> concatBags

lineReplace : String -> String -> Line -> Line
lineReplace from to = bindBag (replaceToBag from to)

----------------------------------------------------

escapes : List ( Char, Char )
escapes =
    [ ('\n', 'n')
    , ('\r', 'r')
    , ('\t', 't')
    , ('"', '"')
    , ('\u{0008}', 'b')
    , ('\u{000C}', 'f')
    , ('\\', '\\')
    ]

strEscape : String -> Line
strEscape =
    let
        doEscape (f, t) = lineReplace (String.fromChar f) (String.fromList ['\\', t])
        doAllEscapes = List.foldl (doEscape >> (>>)) identity escapes
    in singletonBag >> doAllEscapes

quoteB : Bag String
quoteB = singletonBag "\""

eB : Bag String
eB = singletonBag "e"

dotB : Bag String
dotB = singletonBag "."

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

serializeStrVal : String -> Line
serializeStrVal s = concatBags [quoteB, strEscape s, quoteB]

zero : BigRational
zero = BigRational.fromInt 0

ten : BigRational
ten = BigRational.fromInt 10

eqZI : BigInt -> Bool
eqZI = BigInt.compare (BigInt.fromInt 0) >> (==) EQ

eqZR : BigRational -> Bool
eqZR = BigRational.compare zero >> (==) EQ

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
                    , String.fromInt exp |> singletonBag
                    ]
    in if BigRational.compare fractionalPart zero == EQ
        then BigInt.toString intPart |> singletonBag
        else aux n 0

serializeBoolVal : Bool -> Line
serializeBoolVal v = singletonBag <| if v then "true" else "false"

serializeNullVal : Bag String
serializeNullVal = singletonBag "null"

serializeType : Maybe String -> Line
serializeType t_ = case t_ of
    Just t -> concatBags
        [ oparenB
        , serializeIdent t
        , cparenB
        ]
    Nothing -> emptyBag

serializeVal : Value l ValueContents -> Line
serializeVal {typestr, contents} =
    let
        typeSerialized = serializeType typestr
        contentsSerialized = case contents of
            StringVal s -> serializeStrVal s
            NumberVal n -> serializeNumberVal n
            BoolVal b -> serializeBoolVal b
            NullVal -> serializeNullVal
    in concatBags [typeSerialized, contentsSerialized]

serializeIdent : String -> Line
serializeIdent v =
    let
        canSerializeAux i = case String.uncons i of
            Nothing -> True
            Just (h, t) -> List.foldl (&&) (initialCharacter h) <| List.map identifierCharacter <| String.toList t
        canSerializeBare i = case String.uncons i of
            Nothing -> False
            Just (h, t) -> if h == '-' || h == '+'
                then canSerializeAux t
                else canSerializeAux i
    in if canSerializeBare v then v |> singletonBag else serializeStrVal v

serializeProp : (String, Value l ValueContents) -> Line
serializeProp (k, v) = concatBags
    [ serializeIdent k
    , eqB
    , serializeVal v
    ]

serializeNode : Node l ValueContents -> Lines
serializeNode (Node name typ args props children _) = 
    let
        serializedType = serializeType typ
        serializedName = concatBags [serializedType, serializeIdent name]
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

serializeDocument : List (Node l ValueContents) -> Lines
serializeDocument = List.map serializeNode >> concatBags

serialize : List (Node l ValueContents) -> String
serialize = serializeDocument >> linesToString
