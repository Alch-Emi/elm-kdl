module Kdl.Decode exposing (decode, ValueDecoder, NodeDecoder, DocumentDecoder, map, map2, andApply, andRequire, andThen, succeed, rational, float, int, string, bool, null, argument, optionalArgument, arguments, noOtherArguments, property, optionalProperty, properties, noOtherProperties, nodeName, children, noChildren, document, documentF, singleNodeDocument, guardV, guardN, failV, failN, oneOfV, noOtherEntries, multidictDocument, dictDocument, entry, entries, optionalEntry, singleNodeDocumentF)

{-| Tools for building a translator from KDL to your Elm datastructure

This module follow's the same decoding philosophy as Elm uses for its JSON
decoders.  If you haven't read the [JSON chapter of Elm's official guide][1],
I recommend it!  It's a 5-10 minute read, and it'll give you a great high-level
overview that'll make working with this module a lot easier.

In short, this philosophy centers around Decoders, which are objects that
convert data from KDL (or another format) into Elm data structures.  At first,
you start out with very simple decoders, like a decoder that simply converts a
KDL string into an Elm string.  What makes this technique powerful, though, is
that decoders snap together like legos to construct bigger decoders.  The
functions that combine these decoders are known as *combinators*.

By piecing together these simple decoders with simple combinators, you can
construct a decoder for pretty much any datastructure you can imagine, and
adding validation and intuitive error messages is a snap!

One distinction between this library and Elm's JSON library is that JSON
objects all belong to one "class", so Elm only needs one type of decoder.  In
KDL, values, properties, and nodes behave differently, so this library uses a
different decoder type for each.  Fear not though!  A simple set of combinators
is provided for each, and the documentation will make it clear which one you
need.

[1]: https://guide.elm-lang.org/effects/json.html

# Where The Magic Happens

@docs decode

# Types

These are the decoder types.  They fit together much like you would
expect:  [`DocumentDecoder`][]s are built out of [`NodeDecoder`][], and
[`NodeDecoder`][]s are built out of [`ValueDecoder`][]s and [`DocumentDecoder`]s
(for decoding child nodes).

You'll notice that all of the decoders here are generic over two types.
- The last type, `t` is the output type of the decoder--  The datastructure that
the decoder spits out.
- The `e` type is your custom error type.  For maximum flexibility, you can
choose whatever type you want here.  The quickest and easiest way to get started
is to just use the unit type (aka the empty tuple) `()`.  This will let you
build fast, but you won't get much information about what went wrong when a
decode fails.  At some point, you'll probably want actual error messages, so you
may want to use a `String` or even a custom enum to represent possible errors.

[`ValueDecoder`]: #ValueDecoder
[`NodeDecoder`]: #NodeDecoder
[`DocumentDecoder`]: #DocumentDecoder

@docs ValueDecoder
@docs NodeDecoder
@docs DocumentDecoder

# Combinators

Combinators are what you use to combine decoders.  The ones in this section are generic, so you can use them with any of the five decoders.

@docs map
@docs map2
@docs andApply
@docs andRequire
@docs andThen
@docs succeed

# Values

Values are the most basic building block of KDL, and can be used to construct decoders for arguments and properties.

@docs rational
@docs float
@docs int
@docs string
@docs bool
@docs null
@docs guardV
@docs failV
@docs oneOfV

# Arguments

Arguments are values applied directly to nodes, like `my-node 1 2 "hello" myprop=10`.  In this example, `1`, `2`, and `"hello"` are all arguments. `myprop=10` is **not** an argument, but a property, which is the other way of associating values with a node.

The decoders in this section are concerned with making sure that nodes have the right number of arguments, in the right order (if there is one).

@docs argument
@docs optionalArgument
@docs arguments
@docs noOtherArguments

# Properties

Properties are key/value associations tied to nodes.  For example in `my-node hello=world favorite-number=1080`, there's two properties:  One called `"hello"` with the value `"world"`, and one called "favorite-number" with the value `1080`.  Property keys are always strings, while property values can be any KDL value.

This section is dedicated to making sure that all the right properties are specified, and none of the wrong ones.

@docs property
@docs optionalProperty
@docs properties
@docs noOtherProperties

# Children

These decoders help you decode the children of a node.  If you don't specify either of these, then by default your decoder will simply ignore any children which are present.  It's recommended that you do specify a behavior though, so that accidental misuse is met by a helpful error rather than silent acceptance.

@docs children
@docs noChildren

# Nodes

These decoders help you get a little bit more information about your nodes.  Note that the [Arguments](#arguments), [Properties](#properties), and [Children](#Children) all also contain [`NodeDecoder`](#NodeDecoder)s as well, but this section is dedicated to decoders for node metadata alone.

@docs nodeName
@docs guardN
@docs failN

# Documents & Child Blocks

There are several strategies you can use to decode documents and child blocks
(which are treated as equivalent by this decoder).  This section is broken up
by strategy.

## Property-List Documents

One of the more common types of documents you'll see has a series of nodes
which each encode some property.  For example, the block of child nodes for an
employee record might contain a `name` node, an optional `birthday` node, and a
several `project` nodes.  Note that this does not fit neatly as a dict-like node
(from the next section) since there are a fixed number of keys, and each key has
a special meaning.

In parse this example, you could parse the employee's name with [`entry`]
(#entry), their birthday with [`optionalEntry`](#optionalEntry), and their
project with [`entries`](#entries).  This method would be agnostic to the order
of the properties in the block, but would require exactly one `name` and no more
more one `birthday`.

    type alias EmployeeData =
        { name : String
        , birthday : Maybe Int
        , projects : List String
        }

    decodeEmployeeData = succeed EmployeeData
        |> andApply (entry "no name provided" "employees can only have one name" "name" (argument "name takes one argument" (string "name must be a string")))
        |> andApply (optionalEntry "employees can only have one birthday" "birthday" (argument "birthday takes one argument" (int "birthday must be a unix timestamp")))
        |> andApply (entries "project" (argument "projects takes one argument" (string "projects must be strings")))
        |> andRequire (noOtherEntries "unrecognized property")

This decoder could successfully decode this document:

```
name bob
birthday 944937799
project big-freaking-contraption
project evil-laser-beam
```

One thing to note about this parsing strategy is that while most of the
rest of the parsing strategies simply take arguments and return a complete
[`Documentdecoder`](#DocumentDecoder), this parsing strategy looks a little
bit more like the parsing strategy for arguments and values, where smaller unit
decoders are combined with [`andApply`](#andApply) to form a bigger decoder.

@docs entry
@docs entries
@docs optionalEntry
@docs noOtherEntries

## Dict-Like Documents

A common use of documents is to represent dict-like structures.  In these cases,
the names of nodes are arbitrary strings which represent keys in a dictionary,
and the bodies of those nodes are simple types corresponding to the values in
that dictionary.

Crucially, the keys shouldn't be preset keywords, and the values should all be
decoded in the same way.

@docs dictDocument
@docs multidictDocument

## Single-Node Documents

Sometimes your document only has one node, so you don't need much to parse it!

@docs singleNodeDocument
@docs singleNodeDocumentF

## Order-Signifigant Documents

The most powerful document decoders are these, which preserve the order of
the nodes which they parse rather than forming them into a more ergonomic
datastructure.  This means that you can arrange the nodes into your own
order-aware datastructure, although it forces you to do a little bit more work.

@docs document
@docs documentF
-}

import Kdl.Types exposing (Document, KdlNumber(..), Node(..), SourceRange, Value, ValueContents(..))
import Kdl.Util exposing (flip, k, maybe, result, uncons, uncurry)

import BigRational exposing (BigRational)
import Dict exposing (Dict)
import List exposing (member)
import Tuple exposing (pair)
import BigInt
import Maybe exposing (withDefault)

-- [ Types ] --

{-| A generic decoder (private)

All other decoders are actually specializations of this one, but I can't make it
public otherwise Evan's spirit will come out of my computer and strangle me for
making my library too generic, so for now we keep it as our own little secret.
-}
type Decoder f e t = Decoder (f -> Result (e, SourceRange) (t, f))

{-| A decoder for simple KDL values, like strings, numbers, and booleans
-}
type alias ValueDecoder e t = Decoder Value e t

{-| A decoder for KDL nodes
-}
type alias NodeDecoder e t = Decoder Node e t

{-| A decoder for KDL documents, and equivalently, child blocks of KDL nodes

'cause they're both just lists of Nodes.
-}
type alias DocumentDecoder e t = Decoder Document e t

-- [ Internal Utils ] --

{-| Replace the carry part of a Decoder result with a new value (internal)
-}
resultSetCarry : n -> Result e ( t, z ) -> Result e ( t, n )
resultSetCarry = Result.map << Tuple.mapSecond << k

{-| Map the type of a Decoder result (internal)
-}
resultMapItem : (a -> x) -> Result b ( a, c ) -> Result b ( x, c )
resultMapItem = Result.map << Tuple.mapFirst

-- [ Core ] --

run : Decoder f e t -> f -> Result (e, SourceRange) (t, f)
run (Decoder d) = d

-- "don't make your library too generic" my ass
decode_ : Decoder f e t -> f -> Result (e, SourceRange) t
decode_ (Decoder d) = d >> Result.map Tuple.first

{-| Use a decoder to decode a KDL document

At the end of the day, once you've built up your KDL decoder, this function is
how you use it.  Just provide your decoder and a [`Document`](Kdl#Document) to
decode, and you'll either get back the data structure your decoder was designed
for, or one of your error types if something went wrong.
-}
decode : DocumentDecoder e t -> Document -> Result (e, SourceRange) t
decode = decode_

-- [ Combinators ] --

{-| Alter the output type of an existing combinator

Sometimes a simple existing decoder isn't enough.  Suppose you want to decode a
percentage, but in KDL it's represented as an integer between 0 and 100, but in
your datastructure, you want to represent it as a floating point between 0.0 and
1.0. You could represent that decoder like this:

    -- the `float` decoder automatically converts integers to floats
    decodePercent = float "the data wasn't a number!"
        |> map (\input -> input / 100.0)

**Note:**  This combinator is generic, and can be used with any of the three
decoders in this module.
-}
map : (i -> o) -> Decoder f e i -> Decoder f e o
map f (Decoder d)= Decoder (d >> resultMapItem f)

{-| Lift a function to a decoder

The carry value is set to be the unmodified original

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html
-}
arr : (f -> t) -> Decoder f z t
arr f = Decoder (\i -> Ok (f i, i) )

{-| A version of [`arr`](#arr) that also allows setting a modified carry
-}
fromFuncWithCarry : (a -> (b, a)) -> Decoder a never b
fromFuncWithCarry f = Decoder (f >> Ok)

{-| "Compose" two decoders

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html
-}
comp : Decoder m e t -> Decoder f e m -> Decoder f e t
comp (Decoder dInner) (Decoder dOuter) = Decoder (
        dOuter
        >> Result.andThen (\(resOuter, carryOuter) ->
            dInner resOuter
                |> resultSetCarry carryOuter
        )
    )

{-| "Unzip" on a fixed-length list of two elements

Helper function for [`both`](#both)
-}
tUnzip : ((a1, a2), (b1, b2)) -> ((a1, b1), (a2, b2))
tUnzip   ((a1, a2), (b1, b2)) = ((a1, b1), (a2, b2))

{-| Run two decoders in parallel on twin data

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html#v:-42--42--42-
-}
both : Decoder a1 e b1 -> Decoder a2 e b2 -> Decoder (a1, a2) e (b1, b2)
both (Decoder left) (Decoder right) = Decoder (\(l, r) -> Result.map2 pair (left l) (right r) |> Result.map tUnzip)

{-| Combine two seperate decoders that share an input type

The carry from the 1st argument will be used as the carry for the combined decoder.

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html#v:-38--38--38-
-}
fanout : Decoder a e b1 -> Decoder a e b2 -> Decoder a e (b1, b2)
fanout (Decoder d1) (Decoder d2) = Decoder (\i ->
        d1 i
        |> Result.andThen (\(d1Res, d1Carry) ->
            d2 i
            |> Result.map (\(d2Res, _) ->
                ((d1Res, d2Res), d1Carry)
            )
        )
    )

{-| Decoding generalized over Choice

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html#v:-43--43--43-
-}
cBoth : Decoder a1 e b1 -> Decoder a2 e b2 -> Decoder (Result a1 a2) e (Result b1 b2)
cBoth (Decoder left) (Decoder right) = Decoder (\v ->
        case v of
            Ok r -> right r   |> resultMapItem Ok |> (Result.map << Tuple.mapSecond) Ok
            Err l -> left l |> resultMapItem Err |> (Result.map << Tuple.mapSecond) Err
    )

{-| ArrowApply for Decoders

https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Arrow.html#t:ArrowApply
-}
app : Decoder (Decoder b e c, b) e c
app = Decoder (\((Decoder toApply, arg) as input) ->
        toApply arg
        |> resultSetCarry input
    )

{-| Delay computation of a decoder for recursive purposes
-}
lazy : (() -> Decoder f e t) -> Decoder f e t
lazy d = Decoder (\i -> run (d ()) i)

{-| Require two things to be true of a value

For those familiar with the JSON decoding module, this function is included to
make you feel at home.  But most of the time you'll probably want to use the
[`andApply`](#andApply) function instead.

This function runs TWO decoders on one object.  The decode succeeds only if
*both* of the component decoders succeed.  If that's the case, then the output of
each constituent decoder is fed to the provided function.

**Note:**  This combinator is generic, and can be used with any of the three
decoders in this module.
-}
map2 : (t1 -> t2 -> o) -> Decoder f e t1 -> Decoder f e t2 -> Decoder f e o
map2 f d1 d2 = andThen (f >> flip map d2) d1

{-| Add another requirement to a decoder and combine their values through application

This can be very helpful when defining decoders for documents, properties, and arguments, especially when combined with `succeed`.  This can be a little easier to see through example:

    type MyWeirdDatatype = MyWeirdDatatype String Int

    mkMyWeirdDatatype : Int -> String -> Int -> MyWeirdDatatype
    mkMyWeirdDatatype x str y = MyWeirdDatatype str (x - y)

    decodeMyWeirdData : DocumentDecoder String MyWeirdDatatype
    decodeMyWeirdData =
        succeed mkMyWeirdDatatype
            |> andApply (argument "needs 3 args" (int "expected an int"))
            |> andApply (argument "needs 3 args" (string "expected a string"))
            |> andApply (argument "needs 3 args" (int "expected an int"))
            |> andRequire (noOtherArguments "too many arguments!")
            |> andRequire (noOtherProperties "shouldnt be any properties")
            |> andRequire (noChildren "shouldnt be any children!")
            |> singleNodeDocument "no node provided!" "only expected one node!" "unrecognized node!" "my-weird-data"

    (parse "my-weird-data 5 meowy 2" |> Result.map (decode decodeMyWeirdData)) == Ok (Ok (MyWeirdDatatype "meowy" 3))

**Note:**  This combinator is generic, and can be used with any of the three decoders in this module, though most of the time you'll be using it with [`NodeDecoder`](#NodeDecoder)s.
-}
andApply : Decoder f e i -> Decoder f e (i -> o) -> Decoder f e o
andApply = flip <| map2 (<|)

{-| A version of [`andApply`](#andApply) that ignores the output of its first argument and while still adding the constraints

**Note:**  This combinator is generic, and can be used with any of the three decoders in this module, though most of the time you'll be using it with [`NodeDecoder`](#NodeDecoder)s.
-}
andRequire : Decoder f e ignore -> Decoder f e t -> Decoder f e t
andRequire = flip <| map2 k

{-| Run a decoder, then run a function on the result to produce a new decoder, which is *also* run.

Sometimes the behaviour of the decoder going forward is contingent on values already decoded.  This is the function to turn to in that case.  Analagous to [`Json.Decode.andThen`](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#andThen).

**Note:**  This combinator is generic, and can be used with any of the three decoders in this module.
-}
andThen : (i -> Decoder f e o) -> Decoder f e i -> Decoder f e o
andThen f i = Decoder
    (
        (run i) >> (
            \res1 -> case res1 of
                Err e -> Err e
                Ok (v, carry) -> run (f v) carry
        )
    )

{-| A decoder that always succeeds and produces a value of your choice

**Note:**  This combinator is generic, and can be used with any of the three decoders in this module.
-}
succeed : t -> Decoder f e t
succeed v = pair v >> Ok |> Decoder

{-| The abstraction that gives us [`guardV`](#guardV) and [`guardN`](#guardN)
-}
guard : (e -> Decoder f e Never) -> e -> (t -> Bool) -> Decoder f e t -> Decoder f e t
guard fail_ ifFail predicate = andThen (\v ->
        if predicate v
            then succeed v
            else fail_ ifFail |> map never
    )

{-| A decoder that always fails regardless of input
-}
fail : (e, SourceRange) -> Decoder any e never
fail = Err >> k >> Decoder

-- [ Values ] --

{-| The basis of all value decoders
-}
value : (ValueContents -> Maybe t) -> e -> ValueDecoder e t
value f typeError = andThen (f >> maybe (failV typeError) succeed) (arr .contents)
    -- (\({contents} as v) -> case f contents of
    --     Nothing -> Err typeError
    --     Just result -> Ok (result, v)
    -- )

{-| Decode any rational number

This will handle integers, numbers with decimals, and even numbers with exponents, like `120.9312e100`, however, it can't decode special "numbers" like NaN (`#nan`) or infinity (`#inf`), and will return its first argument as an error if applied to any of these.

Of course, this will also give its first argument as an error when applied to non-numeric data, like strings or booleans.
-}
rational : e -> ValueDecoder e BigRational
rational = value (\v -> case v of
        NumberVal (Rational r) -> Just r
        _ -> Nothing
    )

{-| Decode a number as a floating point

Unlike [`rational`](#rational), this can decode values like NaN (`#nan`) and infinity (`#inf`), but may not necessarily represent a number with maximal position, and can introduce minor numeric errors, so isn't ideal for situations where absolute accuracy is important.

This will also give its first argument as an error when applied to non-numeric data, like strings or booleans.
-}
float : e -> ValueDecoder e Float
float = value
    (\v ->
        case v of
            NumberVal (Rational r) -> Just (BigRational.toFloat r)
            NumberVal PositiveInfinity -> Just (1/0)
            NumberVal NegativeInfinity -> Just (-1/0)
            NumberVal NaN -> Just (0/0)
            _ -> Nothing
    )


{-| Decode a number as an integer

This will decode a value as an integer.  However, it will fail (and return its first argument as an error) if used to decode non-integer data, or any numbers too large or to small to be represented by Elm's `Int` type.

Note that `1.1e1` and `1.0` *are* considered integers.

This will also give its first argument as an error when applied to non-numeric data, like strings or booleans.
-}
int : e -> ValueDecoder e Int
int = value
    (\v ->
        case v of
            NumberVal (Rational r) ->
                let
                    (numerator, denominator) = BigRational.toBigInts r
                    intMin = BigInt.fromInt -(2^31)
                    intMax = BigInt.fromInt (2^31 - 1)
                    one = BigInt.fromInt 1
                in if denominator == one && BigInt.lt intMin numerator && BigInt.lt numerator intMax
                    then BigInt.toString numerator
                        |> String.toInt
                        |> withDefault 0
                        |> Just
                    else Nothing
            _ -> Nothing
    )

{-| Decode a string

This will decode any quoted string, bare string, raw string, or multiline string of any variety into an Elm `String`

This will return its first argument as an error when applied to non-string data, like numbers or booleans.
-}
string : e -> ValueDecoder e String
string = value
    (\v ->
        case v of
            StringVal s -> Just s
            _ -> Nothing
    )

{-| Decode a Boolean

This will decode `#true` and `#false` into their Elm `Bool` equivalents

This will return its first argument as an error when applied to non-boolean data, like numbers or strings.
-}
bool : e -> ValueDecoder e Bool
bool = value
    (\v ->
        case v of
            BoolVal b -> Just b
            _ -> Nothing
    )


{-| Require a value to be `#null`

This decoder only succeeds when applied to the KDL value `#null`.  Its return value is always `unit`, aka the empty tuple, aka `()`.

This will return its first argument as an error when applied to anything other then `#null`.
-}
null : e -> ValueDecoder e ()
null = value
    (\v ->
        case v of
            NullVal -> Just ()
            _ -> Nothing
    )

{-| Require that a condition be true of the value returned by a [`ValueDecoder`](#ValueDecoder)

If that condition isn't true, then an error of your choice is raised.  Here's
a simple example.  Say you want a node `count` that only accepts non-negative
integers as an argument.  You could describe this node like this:

    decodeCount =
        argument "count needs 1 argument" (
            int "argument must be an integer"
            |> guardV "can't have a negative count!" (\x -> x >= 0)
        )
        |> andRequire (noOtherArguments "too many arguments on count!")
        |> singleNodeDocument "missing node!" "extra node!" "unrecognized node!" "count"

See also: [`guardN`](#guardN), the [`NodeDecoder`](#NodeDecoder) equivalent of this function
-}
guardV : e -> (t -> Bool) -> ValueDecoder e t -> ValueDecoder e t
guardV = guard failV

{-| A decoder which always fails with an error of your choice rather than parsing a value
-}
failV : e -> ValueDecoder e never
failV e = Decoder (.location >> pair e >> Err)

{-| Try several different decoders to see if any work

Sometimes, you'll need to decode a value which could have one of several
possible types.  For example, say you have a `date` node which can be followed
by either an integer (which should be interpretted as a UNIX timestamp) or
a string (which should be interpretted as a string).  For example, say you have
a `date` node which can be followed by either an integer (which should be
interpretted as a UNIX timestamp) or a string (which should be interpretted
as a string).  In this case, you can't just decode that value using
[`string`](#string) or [`int`](#int) — you'll need to try both to see which one
works. Here's how you can do that with `oneOfV`:

    type MyDate
        = Datestring String
        | UnixTime Int

    decodeDate : DocumentDecoder String MyDate
    decodeDate =
        argument "date takes one argument"
            (
                oneOfV "dates must be either strings or ints!"
                    [ string () |> map Datestring
                    , int () |> map UnixTime
                    ]
            )
        |> andRequire (noOtherArguments "too many arguments!")
        |> singleNodeDocument "missing node" "too many nodes" "unrecognized node" "date"

    (parse "date 1733882937" |> Result.map (decode decodeDate)) == Ok (Ok (UnixTime 1733882937))

`oneOfV` takes two arguments.  The first is an error to return if all other
errors fail.  The second is a list of decoders that should be tried.  Note that
all errors from the internal parsers are discarded if those parsers fail.
-}
oneOfV : e -> List (ValueDecoder ignored t) -> ValueDecoder e t
oneOfV ifAllFail possibleDecoders =
    case possibleDecoders of
        [] -> failV ifAllFail
        (Decoder h) :: t -> Decoder (\i ->
                case h i of
                    Err _ -> run (oneOfV ifAllFail t) i
                    Ok yay -> Ok yay
            )

-- [ Node Helpers ] --

{-| Decode a node, taking its first argument if it exists (and removing it from the carry)
-}
takeFirstArg : NodeDecoder e (Maybe Value)
takeFirstArg = fromFuncWithCarry (\(Node ({args} as n)) ->
        case args of
            h :: t -> (Just h, Node {n | args = t})
            [] -> (Nothing, Node n)
    )

{-| Decode a node, taking its first argument if it exists (and removing it from the carry)
-}
takeProp : String -> NodeDecoder e (Maybe Value)
takeProp propName = fromFuncWithCarry (\(Node ({props} as n)) ->
        case Dict.get propName props of
            (Just _) as o -> (o, Node {n | props = Dict.remove propName props})
            Nothing -> (Nothing, Node n)
    )

{-| When decoding a Maybe, unpack its value, or produce an error at the attached location
-}
just : e -> Decoder (Maybe v, SourceRange) e v
just ifAbsent = arr (\(m, l) -> maybe (fail (ifAbsent, l)) succeed m) |> andThen identity

{-| A version of `Maybe.map` lifted to Decoders
-}
optional : Decoder a e b -> Decoder (Maybe a) e (Maybe b)
optional d =
    arr (Result.fromMaybe ())
    |> comp (cBoth (arr identity) d)
    |> comp (arr Result.toMaybe)

{-| A version of `List.map` lifted to Decoders
-}
multiple : Decoder a e b -> Decoder (List a) e (List b)
multiple decodeVal =
    let
        aux () =
            arr (uncons >> Result.fromMaybe ())
            |> comp (cBoth (succeed []) (comp (arr <| uncurry (::)) (both decodeVal (lazy aux))))
            |> comp (arr (result identity identity))
    in aux ()

-- [ Arguments ] --

{-| Add a required argument to a node

The argument takes the form of a [`ValueDecoder`](#ValueDecoder), which will be used to decode the argument itself.  If the argument is absent, however, the provided error will be returned instead.

One thing to keep in mind is that arguments are *order sensitive*.  Arguments will be expected to appear in the same order that they are applied to the [`NodeDecoder`](#NodeDecoder) being built.  That is:

    -- Decodes `tuple hi 1` but not `tuple 1 hi`
    tuple1 = succeed pair
        |> andApply (argument "two args required!" (string "first argument should be a string")) 
        |> andApply (argument "two args required!" (int "second argument should be an int")) 
        |> andRequire (noOtherArguments "only 2-tuples are supported")
        |> singleNodeDocument "missing node" "too many nodes" "wrong node name" "tuple"

    -- Decodes `tuple 1 hi` but not `tuple hi 1`
    tuple2 = succeed pair
        |> andApply (argument "two args required!" (int "first argument should be an int")) 
        |> andApply (argument "two args required!" (string "second argument should be a string")) 
        |> andRequire (noOtherArguments "only 2-tuples are supported")
        |> singleNodeDocument "missing node" "too many nodes" "wrong node name" "tuple"

Similarly, if [`noOtherArguments`](#noOtherArguments) or [`arguments`](#arguments) is used, they should be come *after* all other arguments.

If you'd rather the argument be completely optional, see [`optionalArgument`](#optionalArgument).  If you would like to decode an arbitrary number of arguments all using the same decoder, see [`arguments`](#arguments). 

This function mirrors the behavior of [`property`](#property), but for arguments rather than properties.
-}
argument : e -> ValueDecoder e t -> NodeDecoder e t
argument = just >> flip comp (fanout takeFirstArg nodeLocation) >> flip comp

{-| Add an optional argument to a node

If any (not-yet-decoded) argument is present, this decodes it with the provided decoder.  Notice that the returned parser decodes to `Maybe t` rather than just `t`.  This is because `Nothing` will be the result when the argument is absent.

Please read the documentation for [`argument`](#argument) for important details regarding ordering.

This mirrors the behavior of [`optionalProperty`](#optionalProperty) for arguments rather than properties.
-}
optionalArgument : ValueDecoder e t -> NodeDecoder e (Maybe t)
optionalArgument decodeVal =
    takeFirstArg
    |> comp (optional decodeVal)

{-| Decodes all remaining arguments into a list

Requires that you provide a decoder which will be used on all arguments which have not yet been decoded.  The results of each decode will be compiled into a list.

This mirrors the behavior of [`properties`](#properties) but for arguments rather than properties.
-}
arguments : ValueDecoder e t -> NodeDecoder e (List t)
arguments = multiple >> flip comp (arr (\(Node {args}) -> args))

{-| Automatically fails if there are any arguments which have not yet been decoded

By default, if there are trailing arguments which don't get decoded, they are simply ignored.  For example, the `pair` parser from the [`argument`](#argument) documentation will parse `tuple 1 hi`, but also `tuple 1 hi #nan etc etc`.  This behavior isn't always optimal, since it can lead to users not realizing they're misusing something.  Applying this function causes your parser to error if there are any trailing arguments.

Also notice that `noOtherArguments` and [`arguments`][] mutually make the other redundant.  When `noOtherArguments` comes first, [`arguments`][] is guaranteed to decode nothing (and produce the empty list).  Meanwhile, if [`arguments`][] comes first, `noOtherArguments` is guaranteed to succeed, as [`arguments`][] is guaranteed to decode all the arguments if it succeeds.

This mirrors the behavior of [`noOtherProperties`](#noOtherProperties) but for arguments rather than properties.

[`arguments`]: #arguments
-}
noOtherArguments : e -> NodeDecoder e ()
noOtherArguments = failV >> optionalArgument >> map (k ())

-- [ Properties ] --

{-| Require that a node have a certain property

This adds a requirement of a certain property to a node, which must be present on the node for the parse to succeed.  The `ifAbsent` argument is returned as an error if the property is missing from the node.  Otherwise, it's decoded using the `decodePropertyValue` node.  `propertyKey` is the name of the property.

Note that this is *order sensitive*.  When building a decoder, if you first decode a property, then use [`properties`](#properties) or [`noOtherProperties`][], this property will be "accounted for", and not count towards the properties decoded by either of those two functions.  Inversely, if you run [`noOtherProperties`][] before running [`property`](#property), the resulting decoder will *always* fail, either because the property was present, but got passed to `noOtherProperties` first, or because it was absent.

This function mirrors the behavior of [`argument`](#argument) but for properties rather than arguments.

[`noOtherProperties`]: #noOtherProperties
-}
property : e -> String -> ValueDecoder e t -> NodeDecoder e t
property ifAbsent propertyKey decodePropertyValue =
    (fanout (takeProp propertyKey) nodeLocation)
    |> comp (just ifAbsent)
    |> comp decodePropertyValue

{-| Optionally decode a property

The resulting node decoder will produce `Nothing` when the property is absent, or `Just` when the property is present and can be decoded by the provided decoder.

See [`property`](#property) for important notes regarding ordering

As with other node decoders, you'll probably want to chain this with other decoders using [`andThen`](#andThen) and [`succeed`](#succeed).

This mirrors the behavior of [`optionalArgument`](#optionalArgument) for properties rather than arguments.
-}
optionalProperty : String -> ValueDecoder e t -> NodeDecoder e (Maybe t)
optionalProperty propertyKey decodePropertyValue =
    takeProp propertyKey
    |> comp (optional decodePropertyValue)

{-| Decodes an arbitrary number of properties agnostic regardless of key

You provide a decoder for the value attached to each key, and the decoder decodes each value, and produces a `Dict` mapping each property name to the resulting value.

Pay attention to the ordering between this and [`property`][]/[`optionalProperty`][].  Putting either [`property`][] or [`optionalProperty`][] will exclude that property from the set of properties decoded by this function.

This is, in effect, mutually exclusive with [`noOtherProperties`](#noOtherProperties)

This mirrors the behavior of [`arguments`](#arguments) but for properties rather than arguments.

[`property`]: #property
[`optionalProperty`]: #optionalProperty
-}
properties : ValueDecoder e t -> NodeDecoder e (Dict String t)
properties decodePropertyValue =
    arr (\(Node {props}) -> Dict.toList props)
    |> comp (multiple (both (arr identity) decodePropertyValue))
    |> map Dict.fromList

{-| Automatically fail if any properties have yet to be decoded

By default, if there are properties which don't get decoded, they are simply ignored.  This behavior isn't always optimal, since it can lead to users not realizing they're misusing something.  Applying this function causes your parser to error if there are any arguments that are unaccounted for.

This mirrors the behavior of [`noOtherArguments`](#noOtherArguments) but for properties rather than arguments.
-}
noOtherProperties : e -> NodeDecoder e ()
noOtherProperties = failV >> properties >> comp (arr <| k ())

-- [ Children ] --

{-| Parse the children of a node

The children of a node is parsed as just another document.  Any of the functions you use to create [`DocumentDecoder`](#DocumentDecoder)s can be used to construct a document decoder which works for the children of a node.

By default, any child nodes are simply ignored without fanfare, but you can specify a specific expectation by either using this function or [`noChildren`](#noChildren), which can ensure that the child nodes are what you expect.

See also:  [`document`](#document)
-}
children : DocumentDecoder e t -> NodeDecoder e t
children = arr (\(Node n) -> n.children) |> flip comp

{-| Assert that a node does not have any children

Instead of simply ignoring the children of a node, it's often preferable to ensure that the node definitely does not have children, and therefore is less likely to be being used incorrectly.  This function concisely accomplishes that.

Equivalent to `children (noOtherNodes <error>)`

See also:  [`children`](#children), [`noOtherEntries`](#noOtherEntries)
-}
noChildren : e -> NodeDecoder e ()
noChildren = noOtherEntries >> children

-- [ Nodes ] --

{-| Decode a node, yeilding its name

It's still possible to decode other parts of the node using other node decoders.  It's often helpful to use a pattern like the one demonstrated in the example of [`andApply`](#andApply).  If you just need to check that the name is equal to a certain value, you might prefer [`assertNodeName`](#assertNodeName).
-}
nodeName : NodeDecoder e String
nodeName = Decoder
    (\((Node {name}) as inputNode) ->
        Ok (name, inputNode)
    )

{-| Get the location of a node
-}
nodeLocation : NodeDecoder never SourceRange
nodeLocation = Decoder
    (\((Node {location}) as inputNode) ->
        Ok (location, inputNode)
    )

oneOfNodeF : (String -> NodeDecoder e t) -> NodeDecoder e t
oneOfNodeF lookupNode = andThen lookupNode nodeName

{-| Require that a condition be true of the value returned by a [`NodeDecoder`](#NodeDecoder)

If that condition isn't true, then an error of your choice is raised.  Here's
one way you could use this to parse a list that needs at least three elments:

    -- Decodes "favorite-things \"raindrops on roses\" \"whiskers on kittens\" \"bright copper kettles\""
    -- But fails on "favorite-things \"pussy willows\" \"christmas\""
    decodeFavoriteThings =
        arguments (string "your favorite things must be strings!")
        |> guardN "You need to least at least three of your favorite things" (\l -> length l >= 3)
        |> singleNodeDocument "missing node!" "extra node!" "unrecognized node!" "favorite-things"

See also: [`guardV`](#guardV), the [`ValueDecoder`](#ValueDecoder) equivalent of this function
-}
guardN : e -> (t -> Bool) -> NodeDecoder e t -> NodeDecoder e t
guardN = guard failN

{-| A decoder which always fails with an error of your choice rather than parsing a node
-}
failN : e -> NodeDecoder e never
failN e = Decoder (\(Node {location}) -> Err (e, location))

-- [ Document ] --

nodes : DocumentDecoder never (List Node)
nodes = arr Tuple.first

{-| Take without replace the first node of a document
-}
takeFirstChild : DocumentDecoder never (Maybe Node)
takeFirstChild = fromFuncWithCarry (\((l, p) as d) ->
        case l of
            [] -> (Nothing, d)
            h :: t -> (Just h, (t, p))
    )

documentLoc : DocumentDecoder never SourceRange
documentLoc = arr Tuple.second

{-| Decode a docmunt with an arbitrary number of nodes

This is the most basic, unspecialized type of document decoder.  It simply
decodes all of the nodes that appear in the document, in order, choosing a
[`NodeDecoder`](#NodeDecoder) based on the provided lookup table.

For each node to be decoded, its name will be checked against the provided `Dict`.  If a decoder is found there, it will be returned.  Otherwise, the the `ifUnrecognized` error is returned.

It is often helpful to set `fallbackNodeDecoder` to [`fail`](#fail) with some error.

For a more powerful version of this, see [`documentF`](#documentF).
-}
document : e -> Dict String (NodeDecoder e t) -> DocumentDecoder e (List t)
document ifUnrecognized namedNodeDecoders =
    documentF (flip Dict.get namedNodeDecoders >> Maybe.withDefault (failN ifUnrecognized))

{-| Decode a document with an arbitrary number of nodes, and the power to choose which decoder to use based on an algorithm

Similar to [`document`][], `documentF` allows for parsing an arbitrary number of
nodes in a document, ultimately yeilding a list.  But while [`document`][] chose
how to decode a node based on a `Dict` lookup, and always failed when the node
was absent from the dict, `documentF` allows you to specify your own algorithm
for choosing a subsequent decoder.

[`document`]: #document
-}
documentF : (String -> NodeDecoder e t) -> DocumentDecoder e (List t)
documentF f =
    nodes
    |> comp (multiple (oneOfNodeF f))

{-| Decode a document with exactly one node with a fixed name

Best used when your document only has one node, and that node should always have the same name.  If you have multiple types of nodes that could appear, you might prefer to use [`singleNodeDocumentF`](#singleNodeDocumentF).

The first three arguments are error values to return if the document is empty (contains no nodes), if there is an extra node that's unaccounted for, and if the name of the node in the document isn't the expected one.

The next two arguments specify the expected name of the node and the [`NodeDecoder`](#NodeDecoder) that should be used for it.
-}
singleNodeDocument : e -> e -> e -> String -> NodeDecoder e t -> DocumentDecoder e t
singleNodeDocument ifAbsent ifTooMany ifUnrecognized expectedName decodeNode = singleNodeDocumentF ifAbsent ifTooMany (\n -> if n == expectedName then decodeNode else failN ifUnrecognized)

{-| Decode a document with exactly one node, but where there might be several possible types of nodes.

Rather than only parse a single type of node (with a fixed name), this function lets you conditionally accept or deny multiple types of nodes based on their name.

The first two arguments are errors to return if the node is absent, or if there is more than one node present.  Then the third and final argument is a function which decides what [`NodeDecoder`](#NodeDecoder) to use for a given node name.

Keep in mind that you can return [`failN`](#failN) to reject a node that has a name you don't support.

See also:
- [`singleNodeDocument`](#singleNodeDocument), a version of this function for a fixed node name
- [`documentF`](#documentF), a version of this which can accept multiple nodes, not just one.
-}
singleNodeDocumentF : e -> e -> (String -> NodeDecoder e t) -> DocumentDecoder e t
singleNodeDocumentF ifAbsent ifTooMany getDecoder =
    nodeName
    |> andThen getDecoder
    |>
        (
            fanout takeFirstChild documentLoc
            |> comp (just ifAbsent)
            |> flip comp
        )
    |> andRequire (noOtherEntries ifTooMany)

{-| Only succeed when a document has no nodes left

Any nodes proccessed by [`entry`](#entry), [`optionalEntry`](#optionalEntry), and [`entries`](#entries) are omitted from the count.

The first argument is the error to produce if any nodes are present at all.

This can also be used to decode an empty document.
-}
noOtherEntries : e -> DocumentDecoder e ()
noOtherEntries = failN >> k >> documentF >> map (k ())

{-| Utility method from creating a multidict from a list of key-value pairs
-}
multidictFromList : List (comparable, t) -> Dict comparable (List t)
multidictFromList = List.foldr (\(key, v) -> Dict.update key (withDefault [] >> (::) v >> Just)) Dict.empty

{-| Decode a dict-like document

For each node in the document, that node is treated like an entry in a
dictionary.  The name of the node is seen as the key, and the value is what
that node decodes to.  Consider this subsection of the `cargo.kdl` official
KDL example:

```kdl
dependencies {
    nom "6.0.1"
    thiserror "1.0.22"
}
```

We could parse this like so:

    decodeDependencyVersion : NodeDecoder String String
    decodeDependencyVersion =
        argument "missing version" (string "version should be a string")
        |> andRequire (noOtherArguments "a dependency should only have one version")
        |> andRequire (noOtherProperties "dependency properties aren't supported")
        |> andRequire (noChildren "dependencies can't have children")

    decodeDependencies : DocumentDecoder String (Dict String String)
    decodeDependencies =
        dictDocument "dependencies can only be listed once" decodeDependencyVersion 
        |> children
        |> andRequire (noOtherArguments "specify dependencies in the child block")
        |> andRequire (noOtherProperties "specify dependencies in the child block")
        |> singleNodeDocument "missing dependency block" "multiple dependency blocks" "unrecognized node" "dependencies"

Notice that this will return `"dependencies can only be listed once"` if someone
tries to specify a dependency twice.
-}
dictDocument : e -> NodeDecoder e t -> DocumentDecoder e (Dict String t)
dictDocument ifDuplicate decodeNode =
    let
        aux : List String -> Decoder (List Node) e (List (String, t))
        aux forbiddenNames = 
            let
                decodeOne : NodeDecoder e (String, t)
                decodeOne =
                    (nodeName |> guardN ifDuplicate (not << flip member forbiddenNames))
                    |> map pair
                    |> andApply decodeNode
                decodeMany : Decoder (Node, List Node) e (List (String, t))
                decodeMany =
                    both
                        (
                            decodeOne
                            |> map
                                (\((usedName, _) as firstNode) ->
                                    aux (usedName :: forbiddenNames)
                                    |> map ((::) firstNode)
                                )
                        )
                        (arr identity)
                    |> comp app
            in
                arr (uncons >> Result.fromMaybe ())
                |> comp (cBoth (arr <| k []) decodeMany)
                |> map (result identity identity)
    in nodes |> comp (aux []) |> map Dict.fromList

{-| Decode a document as if it were a multidict (a dict that can have multiple values associated with a single key)

This behaves much like [`dictDocument`](#dictDocument), but rather than raise an error when there are multiple nodes with one key, 
-}
multidictDocument : NodeDecoder e t -> DocumentDecoder e (Dict String (List t))
multidictDocument =
    map2 pair nodeName
    >> k
    >> documentF
    >> map multidictFromList

takeEntries : String -> DocumentDecoder e Document
takeEntries desiredName = Decoder (\(childNodes, originalLocation) ->
        let
            matchesName (Node n) = n.name == desiredName
            (matchingNodes, remainingNodes) = List.partition matchesName childNodes
        in Ok ((matchingNodes, originalLocation), (remainingNodes, originalLocation))
    )

{-| Decode one entry which should occur exactly once

Takes four arguments:
 - An error to return when the entry is absent
 - An error to return when the entry occurs more than once
 - The name of the entry (aka the name of the node to look for)
 - A decoder for the node once it's found
-}
entry : e -> e -> String -> NodeDecoder e t -> DocumentDecoder e t
entry ifAbsent ifTooMany desiredName decodeNode = comp (singleNodeDocumentF ifAbsent ifTooMany (k decodeNode)) (takeEntries desiredName)

{-| Decode an entry which may occur any number of times (including zero)

Takes two arguments:
 - The name of the entries (aka the name of the nodes to look for)
 - A decoder for the node once it's found
-}
entries : String -> NodeDecoder e t -> DocumentDecoder e (List t)
entries desiredName decodeNode = comp (documentF (k decodeNode)) (takeEntries desiredName)

{-| Decode an entry which can occur zero or one times

Takes two arguments:
 - An error to return when the entry occurs more than once
 - The name of the entry (aka the name of the node to look for)
 - A decoder for the node once it's found
-}
optionalEntry : e -> String -> NodeDecoder e t -> DocumentDecoder e (Maybe t)
optionalEntry ifTooMany desiredName decodeNode =
    let
        parseDoc =
            takeFirstChild
            |> comp (optional decodeNode)
            |> andRequire (noOtherEntries ifTooMany)
    in comp parseDoc (takeEntries desiredName)
