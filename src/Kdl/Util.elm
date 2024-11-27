module Kdl.Util exposing (andf, flip, k, liftA2f, maybe, orf, parseRadix, result, sequenceListMaybe)

import BigInt exposing (BigInt)

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
