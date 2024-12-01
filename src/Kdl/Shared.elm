module Kdl.Shared exposing (bom, identifierCharacter, initialCharacter, isAnyWhitespace, nameWhitespace, posPlus, unicodeNewline, unicodeScalarValue, unicodeSpace, legalCharacter)

import Kdl exposing (Position)
import Kdl.Util exposing (andf, orf, flip, toHex)

import Char exposing (isDigit)
import List exposing (member)

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
unicodeScalarValue c =
    let
        cp = Char.toCode c
    in
        (0 <= cp && cp <= 0xD7FF)
        ||  (0xE000 <= cp && cp <= 0x10FFFF)

isAnyWhitespace : Char -> Bool
isAnyWhitespace = orf unicodeSpace unicodeNewline

bom : Char
bom = '\u{FeFF}'

legalCharacter : Char -> Bool
legalCharacter =
    let
        over20OrWhitespace = orf (Char.toCode >> (<=) 0x20) isAnyWhitespace
        notTooBig = Char.toCode >> (>=) 0x10FFFF
        notDelete = (/=) '\u{007F}'
        unicodeDirectionControls =
            [ '\u{2066}'
            , '\u{2067}'
            , '\u{2068}'
            , '\u{202A}'
            , '\u{202B}'
            , '\u{202D}'
            , '\u{202E}'
            , '\u{2069}'
            , '\u{202C}'
            ]
        notBOM = (/=) bom
        notUnicodeDirectionControl = not << flip member unicodeDirectionControls
    in over20OrWhitespace
        |> andf notTooBig
        |> andf notDelete
        |> andf notUnicodeDirectionControl
        |> andf unicodeScalarValue
        |> andf notBOM

identifierCharacter : Char -> Bool
identifierCharacter c =
    let
        code = Char.toCode c
        bannedChars = ['\\', '/', '(', ')', '{', '}', ';', '[', ']', '=', '"', '#']
    in code > 0x20 && code <= 0x10FFFF && (not <| member c bannedChars)

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
