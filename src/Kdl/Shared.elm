module Kdl.Shared exposing (bom, checkForIllegalBareStrings, identifierCharacter, identifyKeyword, illegalCharacter, initialCharacter, isAnyWhitespace, nameWhitespace, posPlus, unicodeNewline, unicodeScalarValue, unicodeScalarValueByCodepoint, unicodeSpace, legalCharacter)

import Kdl.Types exposing (KdlNumber(..), Position, ValueContents(..))
import Kdl.Util exposing (andf, orf, flip, sequenceListF, toHex, withinRange)

import Char exposing (isDigit)
import List exposing (member)
import Kdl.Util exposing (bool, k, liftA2f)

unicodeSpace : Char -> Bool
unicodeSpace = Char.toCode >> flip member
    [ 0x0009
    , 0x0020
    , 0x00A0
    , 0x1680
    , 0x2000
    , 0x2001
    , 0x2002
    , 0x2003
    , 0x2004
    , 0x2005
    , 0x2006
    , 0x2007
    , 0x2008
    , 0x2009
    , 0x200A
    , 0x202F
    , 0x205F
    , 0x3000
    , 0x000B
    ]

unicodeNewline : Char -> Bool
unicodeNewline = Char.toCode >> flip member
    [ 0x000D
    , 0x000A
    , 0x0085
    , 0x000C
    , 0x2028
    , 0x2029
    ]

unicodeScalarValue : Char -> Bool
unicodeScalarValue = Char.toCode >> unicodeScalarValueByCodepoint

unicodeScalarValueByCodepoint : Int -> Bool
unicodeScalarValueByCodepoint cp =
    (0 <= cp && cp <= 0xD7FF)
    ||  (0xE000 <= cp && cp <= 0x10FFFF)

isAnyWhitespace : Char -> Bool
isAnyWhitespace = orf unicodeSpace unicodeNewline

bom : Char
bom = '\u{FeFF}'

illegalCharacter : Char -> Bool
illegalCharacter =
    let
        controlRange1 = withinRange 0x0000 0x0008
        controlRange2 = withinRange 0x000E 0x001F
        delete        = (==) 0x007F
        notUSV        = withinRange 0xD800 0xDFFF
        direction1    = withinRange 0x200E 0x200F
        direction2    = withinRange 0x202A 0x202E
        direction3    = withinRange 0x2066 0x2069
        isBom         = (==) 0xFEFF
    in Char.toCode >> (
        controlRange1
        |> orf controlRange2
        |> orf controlRange2
        |> orf delete
        |> orf notUSV
        |> orf direction1
        |> orf direction2
        |> orf direction3
        |> orf isBom
    )

legalCharacter : Char -> Bool
legalCharacter = not << illegalCharacter

identifierCharacter : Char -> Bool
identifierCharacter =
    let
        bannedChars = ['\\', '/', '(', ')', '{', '}', ';', '[', ']', '=', '"', '#']
    in (not << flip member bannedChars)
        |> andf legalCharacter
        |> andf (not << isAnyWhitespace)

nameWhitespace : Char -> String
nameWhitespace c = case c of
    '\t'       -> "tab"
    '\u{000b}' -> "vertical tab"
    ' '        -> "space"
    '\u{00a0}' -> "non-breaking space"
    '\u{1680}' -> "ogham space" -- in case anyone accidentally types some medival irish
    '\u{2000}' -> "en quad"
    '\u{2001}' -> "em quad (aka the mutton quad)"
    '\u{2002}' -> "en space (aka the nut)"
    '\u{2003}' -> "em space (aka mutton)"
    '\u{2004}' -> "thick (three-per-em) space"
    '\u{2005}' -> "mid (four-per-em) space"
    '\u{2006}' -> "six-per-em space"
    '\u{2007}' -> "figure space"
    '\u{2008}' -> "punctuation space"
    '\u{2009}' -> "thin space"
    '\u{200A}' -> "hair space"
    '\u{202f}' -> "narrow no-break space"
    '\u{205f}' -> "medium mathematical space"
    '\u{3000}' -> "ideographic space"
    _          -> "U+" ++ toHex (Char.toCode c)

posPlus : Int -> Position -> Position
posPlus i (r, c) = (r, c + i)

initialCharacter : Char -> Bool
initialCharacter = andf (not << isDigit) identifierCharacter

identifyKeyword : String -> Maybe ValueContents
identifyKeyword keyword = case keyword of
    "true" -> Just <| BoolVal True
    "false" -> Just <| BoolVal False
    "null" -> Just NullVal
    "inf" -> Just (NumberVal PositiveInfinity)
    "-inf" -> Just (NumberVal NegativeInfinity)
    "nan" -> Just (NumberVal NaN)
    _ -> Nothing

type alias MiniParser = String -> List String

miniparse1 : (Char -> Bool) -> MiniParser
miniparse1 f s = case String.uncons s of
    Nothing -> []
    Just (c, rest) -> if f c then [rest] else []

miniparseSucceed : MiniParser
miniparseSucceed = List.singleton

miniparseEither : MiniParser -> MiniParser -> MiniParser
miniparseEither = liftA2f (++)

miniparseThen : MiniParser -> MiniParser -> MiniParser
miniparseThen p2 p1 cs = List.concatMap p2 (p1 cs)

miniparseOpt : MiniParser -> MiniParser
miniparseOpt = miniparseEither miniparseSucceed

miniparseRun : MiniParser -> String -> Bool
miniparseRun = (<<) (List.isEmpty >> not)

sign : Char -> Bool
sign = flip List.member ['+', '-']

dot : Char -> Bool
dot = (==) '.'

checkForIllegalBareStrings : e -> e -> e -> e -> String -> Maybe e
checkForIllegalBareStrings ifConfusableWithNumber ifConfusableWithKeyword ifEmpty ifContainsNonidentifierChars =
    let
        miniparseIllegalStart =
            miniparseOpt (miniparse1 sign)
            |> miniparseThen (miniparseOpt <| miniparse1 dot)
            |> miniparseThen (miniparse1 isDigit)
        checkIllegalStart = miniparseRun miniparseIllegalStart >> bool Nothing (Just ifConfusableWithNumber)
        checkKeyword = identifyKeyword >> Maybe.map (k ifConfusableWithKeyword)
        checkEmpty = String.isEmpty >> bool Nothing (Just ifEmpty)
        checkIdentChar = String.toList >> List.map identifierCharacter >> List.foldl (&&) True >> bool (Just ifContainsNonidentifierChars) Nothing
    in sequenceListF [checkIllegalStart, checkKeyword, checkEmpty, checkIdentChar] >> List.filterMap identity >> List.head
