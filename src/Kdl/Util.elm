module Kdl.Util exposing (andf, bool, either, flip, k, liftA2f, maybe, orf, parseRadix, result, sequenceListF, sequenceListMaybe, toHex, traverseListResult, triple, unlines, withinRange)

import BigInt exposing (BigInt)
import List exposing (drop, head)
import Maybe exposing (withDefault)

liftA2f : (a -> b -> o) -> (i -> a) -> (i -> b) -> i -> o
liftA2f g f1 f2 a = g (f1 a) (f2 a)

orf : (i -> Bool) -> (i -> Bool) -> i -> Bool
orf = liftA2f (||)

andf : (i -> Bool) -> (i -> Bool) -> i -> Bool
andf = liftA2f (&&)

flip : (b -> a -> c) -> a -> b -> c
flip f b a = f a b

k : t -> z -> t
k v _ = v

maybe : o -> (v -> o) -> Maybe v -> o
maybe d f m = case m of
    Just v -> f v
    Nothing -> d

result : (e -> o) -> (v -> o) -> Result e v -> o
result ifErr ifOk r = case r of
    Err e -> ifErr e
    Ok v -> ifOk v

parseHexDigit : Char -> Maybe Int
parseHexDigit h = case h of
    '0' -> Just 0
    '1' -> Just 1
    '2' -> Just 2
    '3' -> Just 3
    '4' -> Just 4
    '5' -> Just 5
    '6' -> Just 6
    '7' -> Just 7
    '8' -> Just 8
    '9' -> Just 9
    'a' -> Just 10
    'b' -> Just 11
    'c' -> Just 12
    'd' -> Just 13
    'e' -> Just 14
    'f' -> Just 15
    _ -> Nothing

sequenceListMaybe : List (Maybe a) -> Maybe (List a)
sequenceListMaybe l = case l of
    [] -> Just []
    (Just x) :: rest ->
        sequenceListMaybe rest
        |> Maybe.map ((::) x)
    Nothing :: _ -> Nothing

traverseListResult : (i -> Result e o) -> List i -> Result e (List o)
traverseListResult f l = case l of
    [] -> Ok []
    e :: rest -> Result.map2 (::) (f e) (traverseListResult f rest)

sequenceListF : List (a -> b) -> (a -> List b)
sequenceListF l a = List.map ((|>) a) l

-- Up to 16
parseRadix : Int -> String -> Maybe BigInt
parseRadix radix = let bigRadix = BigInt.fromInt radix in
    String.toList
    >> List.map parseHexDigit 
    >> sequenceListMaybe
    >> Maybe.map
        ( List.map BigInt.fromInt
        >> List.foldl (\v acc -> BigInt.add v (BigInt.mul bigRadix acc)) (BigInt.fromInt 0)
        )

toHexDigit : Int -> Char
toHexDigit = flip drop (String.toList "0123456789abcdef") >> head >> withDefault 'X'

toHexAux : List Char -> Int -> List Char
toHexAux acc i =
    if i == 0
        then acc
        else toHexAux (toHexDigit (modBy 16 i) :: acc) (i // 16)

toHex : Int -> String
toHex i = case toHexAux [] i of
    [] -> "0"
    chars -> String.fromList chars

unlines : List String -> String
unlines = String.join "\n"

either : Maybe a -> Maybe a -> Maybe a
either = maybe identity (Just >> k)

withinRange : comparable -> comparable -> comparable -> Bool
withinRange startInclusive endInclusive v = startInclusive <= v && v <= endInclusive

bool : a -> a -> Bool -> a
bool f t b = if b then t else f

triple : a -> b -> c -> (a, b, c)
triple a b c = (a, b, c)
