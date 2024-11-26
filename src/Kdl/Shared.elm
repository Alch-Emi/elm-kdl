module Kdl.Shared exposing (identifierCharacter, initialCharacter, unicodeNewline, unicodeSpace)

import Kdl.Util exposing (andf, flip)

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
    , 0xFEFF -- BOM
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

identifierCharacter : Char -> Bool
identifierCharacter c =
    let
        code = Char.toCode c
        bannedChars = ['\\', '/', '(', ')', '{', '}', '<', '>', ';', '[', ']', '=', ',', '"']
    in code > 0x20 && code <= 0x10FFFF && (not <| member c bannedChars)

initialCharacter : Char -> Bool
initialCharacter = andf (not << isDigit) identifierCharacter
