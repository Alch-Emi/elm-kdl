module Kdl.Parse exposing (parse, Problem(..), Message, MessageComponent(..), getErrorMessage, messageToString)

import Kdl exposing (KdlNumber(..), Node(..), Value, ValueContents(..), LocatedNode, LocatedValue, Position, SourceRange)
import Kdl.Shared exposing (bom, identifierCharacter, initialCharacter, isAnyWhitespace, legalCharacter, nameWhitespace, unicodeNewline, unicodeScalarValue, unicodeSpace)
import Kdl.Util exposing (andf, flip, k, maybe, orf, parseRadix, result, toHex, traverseListResult, unlines)

import BigInt exposing (BigInt)
import BigRational exposing (BigRational)

import Char exposing (isDigit, toCode)
import Dict
import List exposing (member)
import Maybe exposing (withDefault)
import Parser.Advanced as Parser exposing (DeadEnd, Parser, Nestable(..), Token(..), (|.), (|=), andThen, backtrackable, chompIf, chompUntil, chompWhile, commit, end, getChompedString, getOffset, getSource, getPosition, inContext, lazy, problem, keyword, oneOf, succeed, symbol, variable)
import String
import Set
import Parser.Advanced exposing (chompUntilEndOr)

type PProblem
    = PExpecting String
    | PUnrecognizedEscapeCode Char
    | PUnicodeEscapeNotOpened
    | PUnicodeEscapeEmpty
    | PUnicodeEscapeInvalidCharacters
    | PUnicodeEscapeTooLong
    | PUnicodeEscapeNotClosed
    | PIllegalUnicodeCodepoint
    | PUnclosedString
    | PUnclosedType
    | PUnrecognizedKeyword
    | PForbiddenBareString
    | PMalformedRawStringOpening
    | PUnclosedMultilineComment
    | PUnclosedChildBlock
    | PMissingValue
    | PMalformedNodeComponent
    | PInvalidIdentifier
    | PUnfinishedEscline
    | PMalformedNumber
    | PPrelocated Problem

type StringType
    = Raw
    | Quoted

type TypeableThing
    = TNode
    | TProperty SourceRange
    | TArgument

type PropOrArg
    = Prop String LocatedValue
    | Arg LocatedValue

type Context
    = WithinNode
    | WithinProp
    | WithinComment
    | WithinType
    | WithinString
    | WithinValue
    | WithinNumber
    | WithinExponent
    | WithinFractional
    | WithinRawString
    | WithinQuotedString
    | WithinChildBlock
    | WithinRadixNumber Int
    | WithinStrEscape
    | WithinEscline
    | WithinKeyword
    | WithinPropValue SourceRange

type StrToken
    = STNormal
    | STEscaped

type StrLexeme = StrLexeme StrToken SourceRange String

mkStrLexeme : StrToken -> Position -> String -> Position -> StrLexeme
mkStrLexeme token start val (endrow, endcol) = StrLexeme token (start, (endrow, endcol - 1)) val

stringTypeToString : StringType -> String
stringTypeToString s = case s of
    Raw -> "raw"
    Quoted -> "quoted"

optional : a -> Parser c x a -> Parser c x a
optional default p = oneOf [p, succeed default]

star : (i -> o -> o) -> o -> Parser d p i -> Parser d p o
star f d p = oneOf
    [ succeed f
        |= p
        |= lazy (\() -> star f d p)
    , succeed d
    ]

starL : Parser d p a -> Parser d p (List a)
starL = star (::) []

plus : (i -> o -> o) -> o -> Parser c p i -> Parser c p o
plus f d p = succeed f |= p |= star f d p

orProblem : a -> Parser c a b -> Parser c a b
orProblem ifFail p = oneOf
    [ p
    , commit identity |= problem ifFail
    ]

lookAhead1 : Bool -> p -> (Char -> Bool) -> Parser c p (Maybe Char)
lookAhead1 succeedOnEof prob pred =
    let
        decideFate source offset =
            let charM = (String.dropLeft offset source |> String.uncons |> Maybe.map Tuple.first)
            in
                case charM of
                    Just char -> if pred char then succeed (Just char) else problem prob
                    Nothing -> if succeedOnEof then succeed Nothing else problem prob
    in
        succeed decideFate
        |= getSource
        |= getOffset
        |> Parser.andThen identity

parseEscapeCode : Char -> Parser c PProblem String
parseEscapeCode c = case c of
    'n' -> succeed "\n"
    'r' -> succeed "\r"
    't' -> succeed "\t"
    's' -> succeed " "
    '\\' -> succeed "\\"
    '"' -> succeed "\""
    'b' -> succeed "\u{0008}"
    'f' -> succeed "\u{000C}"
    'u' ->
        succeed identity
        |. symbol (Token "{" PUnicodeEscapeNotOpened)
        |=
            (
                chompWhile (not << flip member ['}', '"'])
                |. lookAhead1 False PUnicodeEscapeNotClosed ((==) '}')
                |> getChompedString
                |> andThen (\s ->
                    if String.length s < 1
                        then problem PUnicodeEscapeEmpty
                    else if String.length s > 6
                        then problem PUnicodeEscapeTooLong
                        else case parseRadix 16 (String.toLower s) of
                            Just n ->
                                let
                                    codepoint = BigInt.toString n |> String.toInt |> withDefault 0
                                    char = Char.fromCode codepoint
                                    string = String.fromChar char
                                in if unicodeScalarValue char
                                    then succeed string
                                    else problem PIllegalUnicodeCodepoint
                            Nothing -> problem PUnicodeEscapeInvalidCharacters
                )
            )
        |. symbol (Token "}" PUnicodeEscapeNotClosed)
    _ -> if isAnyWhitespace c
        then chompWhile isAnyWhitespace |> Parser.map (k "")
        else  problem (PUnrecognizedEscapeCode c)

parseEscapeSequence : Parser Context PProblem StrLexeme
parseEscapeSequence =
    succeed (mkStrLexeme STEscaped)
    |= getPosition
    |. symbol (Token "\\" <| PExpecting "backslash")
    |= (
        chompIf (k True) PUnclosedString
        |> getChompedString
        |> Parser.andThen (String.uncons >> withDefault ('_', "") >> Tuple.first >> parseEscapeCode)
    )
    |= getPosition
    |> inContext WithinStrEscape

parseQuotedStringSegment : Parser Context PProblem StrLexeme
parseQuotedStringSegment =
    let
        plainStringChar = not << (orf (flip member ['"', '\\']) unicodeNewline)
    in
    oneOf
        [
            succeed (mkStrLexeme STNormal)
            |= getPosition
            |= (
                chompIf plainStringChar (PExpecting "string character")
                |. chompWhile plainStringChar
                |> getChompedString
            )
            |= getPosition
        , parseEscapeSequence
        ]

processWhitespaceEscapes : List (List StrLexeme) -> List (List StrLexeme)
processWhitespaceEscapes = (List.map << List.filter) (\l -> case l of
        StrLexeme STEscaped _ "" -> False
        _ -> True
    )

endOfLexemeLine : List StrLexeme -> Maybe Position
endOfLexemeLine line = line
    |> List.drop (List.length line - 1)
    |> List.head
    |> Maybe.map (\(StrLexeme _ (_, end) _) -> end)

processMultilineString : List (List StrLexeme) -> Result (SourceRange -> Problem) (List (List StrLexeme))
processMultilineString lines =
    case lines of
        [] -> Ok []
        [singleLine] -> Ok [singleLine]
        [] :: firstLine :: otherLines ->
            let
                lastLine =
                    List.drop (List.length otherLines - 1) otherLines
                    |> List.head
                    |> withDefault firstLine
                firstNonwhitespace whitespaceSeen s = flip Maybe.andThen (String.uncons s) (\(c, rest) -> 
                        if unicodeSpace c
                            then firstNonwhitespace (whitespaceSeen + 1) rest
                            else Just whitespaceSeen
                    )
                endOfLastLine = endOfLexemeLine lastLine
                whitespacePrefixOrFirstGarbageChar = case lastLine of
                    [] -> Ok ""
                    [StrLexeme STNormal ((lrow, lcol), _) val] -> case firstNonwhitespace 0 val of
                        Nothing -> Ok val
                        Just garbageStart -> Err (lrow, lcol + garbageStart)
                    StrLexeme STNormal ((lrow, lcol), _) val :: _ -> case firstNonwhitespace 0 val of
                        Nothing -> Err (lrow, lcol + String.length val)
                        Just garbageStart -> Err (lrow, lcol + garbageStart)
                    StrLexeme STEscaped (garbageStart, _) _ :: _ -> Err garbageStart
                whitespacePrefixOrErr = flip Result.mapError
                    whitespacePrefixOrFirstGarbageChar
                    (\firstGarbageChar strLoc ->
                        MultilineStringTrailingCharacters {strLoc = strLoc, trailingChars = (firstGarbageChar, withDefault firstGarbageChar endOfLastLine)}
                    )
                isAllWhitespace l = case l of
                    [] -> True
                    StrLexeme STEscaped _ _ :: _ -> False
                    StrLexeme STNormal _ contents :: rest -> case firstNonwhitespace 0 contents of
                        Nothing -> isAllWhitespace rest
                        Just _ -> False
                dedentLine prefix line =
                    if isAllWhitespace line
                        then Ok []
                    else case String.uncons prefix of
                        Nothing -> Ok line
                        Just (firstPrefixChar, remainingPrefix) -> case (line) of
                            [] -> Ok []
                            StrLexeme STEscaped ((gsrow, gscol) as garbageStart, _) _ :: _ -> Err (\strLoc ->
                                MultilineStringLineLacksPrefix {strLoc = strLoc, violatingChars = (garbageStart, (gsrow, gscol + (String.length prefix) - 1))})
                            StrLexeme STNormal ((row, lCol) as lexemeStart, lexemeEnd) content :: rest -> case String.uncons content of
                                Nothing -> dedentLine prefix rest
                                Just (firstLineChar, remainingContent) ->
                                    if firstPrefixChar == firstLineChar
                                        then dedentLine remainingPrefix (StrLexeme STNormal ((row, lCol + 1), lexemeEnd) remainingContent :: rest)
                                    else if unicodeSpace firstPrefixChar && unicodeSpace firstLineChar
                                        then Err (\strLoc ->
                                            MultilineStringMismatchedPrefixSpace {strLoc = strLoc, offendingChar = lexemeStart, expected = firstPrefixChar, got = firstLineChar}
                                        )
                                        else Err (\strLoc ->
                                            MultilineStringLineLacksPrefix {strLoc = strLoc, violatingChars = (lexemeStart, (row, lCol + (String.length remainingPrefix)))}
                                        )
                dedentAllLines = dedentLine >> traverseListResult
            in Result.andThen (flip dedentAllLines (firstLine :: otherLines)) whitespacePrefixOrErr
        nonEmptyFirstLine :: _ :: _ ->
            let
                endOfFirstLine = endOfLexemeLine nonEmptyFirstLine |> withDefault (0, 0)
            in Err (\strLoc -> NewlineInQuotedString {strLoc=strLoc, newlineLoc=endOfFirstLine})

lexemeToString : StrLexeme -> String
lexemeToString l = case l of
    StrLexeme _ _ c -> c

lexemesToString : List (List StrLexeme) -> String
lexemesToString = List.map (List.map lexemeToString >> String.concat) >> unlines

parseQuotedStringInnards : Parser Context PProblem (List (List StrLexeme))
parseQuotedStringInnards =
    let
        parseLine = starL parseQuotedStringSegment
    in succeed (::)
        |= parseLine
        |= starL (succeed identity |. newline |= parseLine)

parseQuotedString : Parser Context PProblem String
parseQuotedString =
    let
        f : Position -> List (List StrLexeme) -> Position -> Parser Context PProblem String
        f openPos contents (closeRow, closeCol) =
            contents
            |> processWhitespaceEscapes
            |> processMultilineString
            |> Result.map lexemesToString
            |> Result.mapError ((|>) (openPos, (closeRow, closeCol - 1)) >> PPrelocated)
            |> result problem succeed
    in
        succeed f
        |= getPosition
        |. symbol (Token "\"" <| PExpecting "opening quote")
        |= parseQuotedStringInnards
        |. symbol (Token "\"" PUnclosedString)
        |= getPosition
        |> Parser.andThen identity
        |> inContext WithinQuotedString

parseRawString : Parser Context PProblem String
parseRawString =
    plus k () (chompIf ((==) '#') (PExpecting "raw string"))
    |. symbol (Token "\"" <| PMalformedRawStringOpening)
    |> getChompedString 
    |> andThen (\openingToken ->
        let
            closeToken = Token (String.reverse openingToken) PUnclosedString
        in
            chompUntil closeToken
            |> getChompedString
            |> flip (|.) (symbol closeToken)
    )
    |> inContext WithinRawString

identifyKeyword : String -> Maybe ValueContents
identifyKeyword keyword = case keyword of
    "true" -> Just <| BoolVal True
    "false" -> Just <| BoolVal False
    "null" -> Just NullVal
    "inf" -> Just (NumberVal PositiveInfinity)
    "-inf" -> Just (NumberVal NegativeInfinity)
    "nan" -> Just (NumberVal NaN)
    _ -> Nothing

parseKeyword : Parser Context PProblem ValueContents
parseKeyword =
    succeed identifyKeyword
    |. backtrackable (chompIf ((==) '#') (PExpecting "keyword"))
    |= variable
        { start = identifierCharacter
        , inner = identifierCharacter
        , reserved = Set.empty
        , expecting = PExpecting "keyword"
        }
    |> Parser.andThen (maybe (problem PUnrecognizedKeyword) succeed)
    |> inContext WithinKeyword

parseStringVal : Parser Context PProblem ValueContents
parseStringVal = parseString [] |> Parser.map StringVal

parseDigits : (Char -> Bool) -> Parser c PProblem String
parseDigits isDigitValid =
    let
        parseDigitSeries =
            (
                chompIf isDigitValid PMalformedNumber
                |. chompWhile isDigitValid
                |> getChompedString
            )
            |. (symbol (Token "_" <| PExpecting "underscore") |> optional ())
    in plus (++) "" parseDigitSeries

parseUnsignedRadixNumber : Parser Context PProblem BigInt
parseUnsignedRadixNumber =
    let
        numberStyles =
            [ (Token "0b" (PExpecting "binary number"), "01", 2)
            , (Token "0x" (PExpecting "hexadecimal number"), "0123456789abcdefABCDEF", 16)
            , (Token "0o" (PExpecting "octal number"), "01234567", 8)
            ]
        parseNumberStyle (prelude, digits, radix) =
            succeed identity
                |. symbol prelude
                |= parseDigits (flip member <| String.toList digits)
                |> Parser.map (String.toLower >> parseRadix radix)
                |> Parser.andThen (maybe (problem <| PExpecting "bad digit???") succeed)
                |> Parser.inContext (WithinRadixNumber radix)
    in oneOf (List.map parseNumberStyle numberStyles)

parseSign : Parser c PProblem number
parseSign = oneOf
    [ succeed 1 |. symbol (Token "+" <| PExpecting "plus")
    , succeed -1 |. symbol (Token "-" <| PExpecting "minus")
    ]

parseUnsignedDecimalNumber : Parser Context PProblem BigRational
parseUnsignedDecimalNumber =
    let
        parseUnsignedDecimalInteger =
            parseDigits isDigit
            |> Parser.andThen
                ( String.toInt
                >> maybe (problem <| PExpecting "bad digit?") succeed
                )
        parseSignedDecimalInteger = succeed (*) |= (parseSign |> optional 1) |= parseUnsignedDecimalInteger
        parseFractionalPart =
            succeed identity
            |. symbol (Token "." <| PExpecting "decimal part")
            |= (
                parseDigits isDigit
                |> Parser.andThen (\s ->
                    let
                        n = String.toInt s
                    in
                        case n of
                            Nothing -> problem <| PExpecting "bad digit?!"
                            Just n_ -> succeed <|
                                BigRational.div
                                    (BigRational.fromInt n_)
                                    (BigRational.pow (String.length s) (BigRational.fromInt 10))
                )
            )
            |> optional (BigRational.fromInt 0)
            |> inContext WithinFractional
        parseScientificPart =
            succeed identity
            |. oneOf
                [ symbol (Token "e" <| PExpecting "scientific part")
                , symbol (Token "E" <| PExpecting "scientific part!")
                ]
            |= parseSignedDecimalInteger
            |> Parser.map (\e ->
                if e > 0
                    then BigRational.pow e (BigRational.fromInts 10 1)
                    else BigRational.pow (-e) (BigRational.fromInts 1 10)
            )
            |> optional (BigRational.fromInt 1)
            |> inContext WithinExponent
    in 
        (
            succeed (BigRational.fromInt >> BigRational.add)
            |= parseUnsignedDecimalInteger
            |= parseFractionalPart
            |> Parser.map BigRational.mul
        )
        |= parseScientificPart

parseNumber : Parser Context PProblem BigRational
parseNumber = 
    let
        parseBody = oneOf
            [ parseUnsignedRadixNumber |> Parser.map BigRational.fromBigInt
            , parseUnsignedDecimalNumber
            ]
    in oneOf
        [ succeed (BigRational.fromInt >> BigRational.mul)
            |= parseSign
            |= oneOf
                [ parseBody
                , succeed identity
                    |. lookAhead1 False PMalformedNumber (identifierCharacter)
                    |. commit ()
                    |= problem PMalformedNumber 
                ]
        , parseBody
        ]
        |. lookAhead1 True PMalformedNumber (not << identifierCharacter)
    |> inContext WithinNumber

parseNumberVal : Parser Context PProblem ValueContents
parseNumberVal = Parser.map NumberVal (parseNumber |> Parser.map Rational)

mkLocVal : Position -> Maybe String -> ValueContents -> Position -> LocatedValue
mkLocVal s t v e = Value (s, e) t v

parseValue : Parser Context PProblem LocatedValue
parseValue =
    succeed mkLocVal
    |= getPosition
    |= optional Nothing (Parser.map Just parseType)
    |= oneOf
    [ parseKeyword
    , parseStringVal
    , parseNumberVal
    ]
    |= getPosition
    |> inContext WithinValue

signChar : Char -> Bool
signChar = flip member ['+', '-']

parseBareString : List Char -> Parser c PProblem String
parseBareString possibleFollowingCharacters = oneOf
    [ variable
        { start = andf (not << signChar) initialCharacter
        , inner = identifierCharacter
        , reserved = Set.empty
        , expecting = PExpecting "variable"
        }
    , chompIf signChar (PExpecting "-variable")
        |. (
            variable
                { start = initialCharacter
                , inner = identifierCharacter
                , reserved = Set.empty
                , expecting = PExpecting "variable"
                }
                |> optional ""
        )
        |> getChompedString
    ]
    |. lookAhead1 True PInvalidIdentifier
        (
            flip member possibleFollowingCharacters
            |> orf unicodeNewline
            |> orf unicodeSpace
        )
    |> Parser.andThen (\s ->
        case identifyKeyword s of
            Nothing -> succeed s
            Just _ -> problem PForbiddenBareString
    )

parseString : List Char -> Parser Context PProblem String
parseString possibleFollowingCharacters =
    oneOf
        [ parseRawString
        , parseBareString possibleFollowingCharacters
        , parseQuotedString
        ]
        |> inContext WithinString

parsePropOrArg : Parser Context PProblem PropOrArg
parsePropOrArg = oneOf
    [ succeed (\a b c -> (a, b, c))
        |= getPosition
        |= (backtrackable <| parseString ['='])
        |= getPosition
        |. star k () nodespace
        |. symbol (Token "=" <| PExpecting "equals")
        |. star k () nodespace
        |> Parser.andThen (\(startPos, keyName, (endRow, endCol)) ->
            succeed (Prop keyName)
            |= parseValue
            |> orProblem PMissingValue
            |> inContext (WithinPropValue (startPos, (endRow, endCol - 1)))
        )
        |> inContext WithinProp
    , succeed Arg
        |= parseValue
    ]

parseType : Parser Context PProblem String
parseType =
     succeed identity
        |. symbol (Token "(" <| PExpecting "type")
        |. star k () nodespace
        |= oneOf
            [ parseString [')']
            , commit identity |= problem PInvalidIdentifier
            ]
        |. oneOf
            [ star k () nodespace
                |. symbol (Token ")" <| PUnclosedType)
            , commit identity |= problem PUnclosedType
            ]
        |. star k () nodespace
    |> inContext WithinType
        
parseMultilineComment : Parser Context PProblem ()
parseMultilineComment =
    let
        specialChars = (flip member ['/', '*'])
        commentGuts_ () = oneOf
            [ symbol (Token "*/" PUnclosedMultilineComment)
            , oneOf
                [ lazy (\() -> parseMultilineComment)
                , chompIf (k True) (PExpecting "") |. chompWhile (not << specialChars)
                ]
                |. lazy commentGuts_
            , commit identity |= problem PUnclosedMultilineComment
            ]
        commentGuts = commentGuts_ ()
    in
        symbol (Token "/*" (PExpecting "multicomment"))
        |. commentGuts
        |> inContext WithinComment

parseLineComment : Parser Context PProblem ()
parseLineComment =
    symbol (Token "//" <| PExpecting "linecomment")
    |. chompUntilEndOr "\n"
    |. (optional () <| chompIf ((==) '\n') (PExpecting "linecomment newline"))
    |> inContext WithinComment

ws : Parser Context PProblem ()
ws =
     oneOf
        [ chompIf unicodeSpace (PExpecting "whitespace") |. chompWhile unicodeSpace |> backtrackable
        , parseMultilineComment
        ]

newline : Parser c PProblem ()
newline = oneOf
    [ symbol (Token "\u{000D}\u{000A}" <| PExpecting "CRLF")
    , chompIf unicodeNewline <| PExpecting "newline"
    ]

nodespace : Parser Context PProblem ()
nodespace = oneOf
    [ ws
    , chompIf ((==) '\\') (PExpecting "escline") 
        |. star k () ws
        |. oneOf
            [ parseLineComment
            , newline
            , end (PExpecting "endtimes")
            , commit identity |= problem PUnfinishedEscline
            ]
        |> inContext WithinEscline
    ]

linespace : Parser Context PProblem ()
linespace = oneOf
    [ newline
    , ws
    , parseLineComment
    ]

parseAstComment : Parser Context PProblem (a -> Maybe a)
parseAstComment =
    symbol (Token "/-" (PExpecting <| "node comment"))
    |. star k () linespace
    |> Parser.map (k (k Nothing))
    |> optional Just
    |> backtrackable

mkNode
    : (Int, Int)
    -> (Maybe String {- type -}, String {- name -})
    -> List (Maybe PropOrArg) {- props & args -}
    -> List LocatedNode {- children -}
    -> (Int, Int)
    -> LocatedNode
mkNode startLoc (typ, name) propsAndArgsM children endLoc =
    let
        propsAndArgs = List.filterMap identity propsAndArgsM
        props = Dict.fromList <| List.filterMap
            (\pov -> 
                case pov of
                    Prop k v -> Just (k, v)
                    _ -> Nothing
            ) propsAndArgs
        args = List.filterMap (\pov ->
            case pov of
                Arg v -> Just v
                _ -> Nothing
            ) propsAndArgs
    in Node name typ args props children (startLoc, endLoc)

parseNode : Parser Context PProblem (Maybe LocatedNode)
parseNode =
    let
        nodeTerminator : Parser Context PProblem ()
        nodeTerminator = oneOf
            [ parseLineComment 
            , newline
            , symbol (Token ";" <| PExpecting "semicolon")
            , lookAhead1 True (PExpecting "eof or }") ((==) '}') |> Parser.map (k ())
            , commit identity |= problem PMalformedNodeComponent
            ]
        children : Parser Context PProblem (List (LocatedNode))
        children = parseAstComment |= (
                succeed identity
                |. symbol (Token "{" <| PExpecting "children")
                |= lazy (\() -> parseNodes)
                |. oneOf
                    [ symbol (Token "}" PUnclosedChildBlock)
                    , commit () |. end PInvalidIdentifier |. problem PUnclosedChildBlock
                    ]
            )
            |> Parser.map (Maybe.withDefault [])
            |> inContext WithinChildBlock
    in
        parseAstComment
        |= (
            succeed mkNode
            |= getPosition
            |= oneOf
                [ succeed Tuple.pair
                    |= (parseType |> Parser.map Just)
                    |= oneOf
                        [ parseString [';', '/', '}']
                        , commit identity |= problem PInvalidIdentifier
                        ]
                , succeed (Tuple.pair Nothing)
                    |= parseString [';', '/', '}']
                ]
            |= starL (
                succeed identity
                |. (plus k () nodespace |> backtrackable)
                |= parseAstComment
                |= parsePropOrArg
            )
            |= ((succeed identity |. (star k () nodespace |> backtrackable) |= children) |> optional [])
            |. star k () nodespace
            |. nodeTerminator
            |= getPosition
        )
        |> inContext WithinNode

parseNodes : Parser Context PProblem (List LocatedNode)
parseNodes =
    succeed identity
    |. star k () linespace
    |= starL (parseNode |. star k () linespace)
    |> Parser.map (List.filterMap identity)

parseDocument : Parser Context PProblem (List LocatedNode)
parseDocument = parseNodes |. end PInvalidIdentifier

findInvalidChars : String -> Maybe Position
findInvalidChars =
    Parser.run
        (
            oneOf
                [ chompIf (orf legalCharacter ((==) bom)) ()
                , succeed ()
                ]
            |. chompWhile legalCharacter
            |. end ()
        )
    >> result
        ( List.head
            >> Maybe.map (\{row, col} -> (row, col))
        )
        ( k Nothing )

type Problem
    = Unexpected String
    | UnrecognizedEscapeCode {strLoc: Position, escLoc: SourceRange, char: Char}
    | UnicodeEscapeNotOpened {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeEmpty {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeInvalidCharacters {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeTooLong {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeNotClosed {strLoc: SourceRange, escLoc: Position}
    | UnicodeEscapeBadCodepoint {strLoc: Position, escLoc: SourceRange}
    | UnclosedString {strType: StringType, strStart: Position}
    | UnclosedType {typeOf: TypeableThing, typeStart: Position, gaveUpAt: Position}
    | MalformedRawStringOpening {stringStart: SourceRange, nonQuotationCharacter: Position}
    | MalformedNumber {numberStart: Position, malformedAt: Position}
    | UnclosedMultilineComment {commentStart: Position}
    | UnclosedChildBlock {blockStart: Position}
    | UnterminatedNode {blockStart: Position, nodeStart: Position, blockEnd: Position} 
    | MissingValue {property: SourceRange, absentValue: Position}
    | InvalidIdentifier {identifierStart: Position, confusedAt: Position}
    | UnfinishedEscline {backslashLoc: Position, nextCharOrEof: Position}
    | MalformedNodeComponent {nodeStart: Position, badElement: Position}
    | ForbiddenCharacter {charPos: Position}
    | ForbiddenBareString {strLoc: SourceRange}
    | UnrecognizedKeyword {keywordLoc: SourceRange}
    | NewlineInQuotedString {strLoc: SourceRange, newlineLoc: Position}
    | MultilineStringTrailingCharacters {strLoc: SourceRange, trailingChars: SourceRange}
    | MultilineStringLineLacksPrefix {strLoc: SourceRange, violatingChars: SourceRange}
    | MultilineStringMismatchedPrefixSpace {strLoc: SourceRange, offendingChar: Position, expected: Char, got: Char}

type alias DefaultContextFrame = {row: Int, col: Int, context: Context}
type MyContextFrame = ContextFrame Context Position

makeContextMatchable : DefaultContextFrame -> MyContextFrame
makeContextMatchable {row, col, context} = ContextFrame context (row, col)

parse : String -> Result Problem (List (LocatedNode))
parse s =
    case findInvalidChars s of
        Nothing ->
            Parser.run parseDocument s
            |> Result.mapError (
                List.head
                >> Maybe.map translateDeadEnd
                >> Maybe.withDefault (Unexpected "No dead ends returned in parse")
            )
        Just pos -> Err <| ForbiddenCharacter {charPos = pos}

translateDeadEnd : DeadEnd Context PProblem -> Problem
translateDeadEnd {row, col, contextStack, problem} =
    let
        finalPosition = (row, col)
        myContextStack = List.map makeContextMatchable contextStack
    in
        case problem of
            PExpecting s -> Unexpected ("Unexpected problem which resulted in attempting to interpret a character as a " ++ s)
            PPrelocated p -> p
            PUnrecognizedEscapeCode c -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnrecognizedEscapeCode {strLoc=strLoc, escLoc=(escStart, (row, col - 1)), char=c}
                _ -> Unexpected "Encountered a PUnrecognizedEscapeCode error while not parsing an escape code in a string"
            PUnicodeEscapeNotOpened -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnicodeEscapeNotOpened {strLoc=strLoc, escLoc=(escStart, (row, col - 1))}
                _ -> Unexpected "Encountered a PUnicodeEscapeNotOpened error while not parsing an escape code a string"
            PUnicodeEscapeEmpty -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnicodeEscapeEmpty {strLoc=strLoc, escLoc=(escStart, finalPosition)}
                _ -> Unexpected "Encountered a PUnicodeEscapeEmpty error while not parsing an escape sequence in a string"
            PUnicodeEscapeInvalidCharacters -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnicodeEscapeInvalidCharacters {strLoc=strLoc, escLoc=(escStart, finalPosition)}
                _ -> Unexpected "Encountered a PUnicodeInvalidCharacters error while not parsing an escape sequence in a string"
            PIllegalUnicodeCodepoint -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnicodeEscapeBadCodepoint {strLoc=strLoc, escLoc=(escStart, (row, col - 1))}
                _ -> Unexpected "Encountered a PIllegalUnicodeCodepoint error while not parsing an escape sequence in a string"
            PUnicodeEscapeTooLong -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    UnicodeEscapeTooLong {strLoc=strLoc, escLoc=(escStart, finalPosition)}
                _ -> Unexpected "Encountered a PUnicodeEscapeTooLong error while not parsing an escape sequence in a string"
            PUnicodeEscapeNotClosed -> case myContextStack of
                ContextFrame WithinStrEscape escLoc :: ContextFrame WithinQuotedString strStart :: _ ->
                    UnicodeEscapeNotClosed {strLoc=(strStart, finalPosition), escLoc=escLoc}
                _ -> Unexpected "Encountered a PUnicodeEscapeNotClosed error while not parsing an escape sequence in a string"
            PUnclosedString -> case myContextStack of
                ContextFrame WithinQuotedString strStart :: _ ->
                    UnclosedString {strType=Quoted, strStart=strStart}
                ContextFrame WithinRawString strStart :: _ ->
                    UnclosedString {strType=Raw, strStart=strStart}
                _ -> Unexpected "Encountered a PUnclosedString error while not parsing a string"
            PUnclosedType -> case myContextStack of
                ContextFrame WithinType typeStart
                    :: ContextFrame WithinNode _
                    :: _ ->
                    UnclosedType {typeOf=TNode,typeStart=typeStart,gaveUpAt=finalPosition}
                ContextFrame WithinType typeStart
                    :: ContextFrame WithinValue _
                    :: ContextFrame WithinNode _
                    :: _ ->
                    UnclosedType {typeOf=TArgument,typeStart=typeStart,gaveUpAt=finalPosition}
                ContextFrame WithinType typeStart
                    :: ContextFrame WithinValue _
                    :: ContextFrame (WithinPropValue propLoc) _
                    :: _ ->
                    UnclosedType {typeOf=TProperty propLoc,typeStart=typeStart,gaveUpAt=finalPosition}
                _ -> Unexpected "Encountered a PUnclosedType error while not parsing a value, property, or node's type"
            PMalformedRawStringOpening -> case myContextStack of
                ContextFrame WithinRawString strStart :: _ ->
                    MalformedRawStringOpening {stringStart=(strStart, (row, col - 1)), nonQuotationCharacter=finalPosition}
                _ -> Unexpected "Encountered a PMalformedRawStringOpening error while not parsing a raw string"
            PMalformedNumber -> case myContextStack of
                ContextFrame (WithinRadixNumber _) numberStart :: _ ->
                    MalformedNumber {numberStart = numberStart, malformedAt = finalPosition}
                ContextFrame WithinNumber numberStart :: _ ->
                    MalformedNumber {numberStart = numberStart, malformedAt = finalPosition}
                ContextFrame _ _
                    :: ContextFrame WithinNumber numberStart
                    :: _ ->
                    MalformedNumber {numberStart = numberStart, malformedAt = finalPosition}
                _ -> Unexpected "Encountered a PMalformedNumber error while not parsing a number"
            PUnclosedMultilineComment -> case myContextStack of
                ContextFrame WithinComment commentStart :: _ ->
                    UnclosedMultilineComment {commentStart = commentStart}
                _ -> Unexpected "Encountered a PUnclosedMultilineComment while not parsing a comment"
            PUnclosedChildBlock -> case myContextStack of
                ContextFrame WithinChildBlock blockStart :: _ ->
                    UnclosedChildBlock {blockStart = blockStart}
                _ -> Unexpected "Encountered a PUnclosedChildBlock while not parsing a block of children"
            PMissingValue -> case myContextStack of
                ContextFrame (WithinPropValue propLoc) _ :: _ ->
                    MissingValue {property = propLoc, absentValue = finalPosition}
                _ -> Unexpected "Encountered a PMissingValue while not parsing a property"
            PInvalidIdentifier -> case myContextStack of
                ContextFrame WithinString identStart :: _ ->
                    InvalidIdentifier {identifierStart = identStart, confusedAt = finalPosition}
                ContextFrame WithinType _ :: _ ->
                    InvalidIdentifier {identifierStart = finalPosition, confusedAt = finalPosition}
                ContextFrame WithinNode _ :: _ ->
                    InvalidIdentifier {identifierStart = finalPosition, confusedAt = finalPosition}
                ContextFrame WithinChildBlock _ :: _ ->
                    InvalidIdentifier {identifierStart = finalPosition, confusedAt = finalPosition}
                [] ->
                    InvalidIdentifier {identifierStart = finalPosition, confusedAt = finalPosition}
                _ -> Unexpected "Encountered a PInvalidIdentifier while not parsing an identifier"
            PUnfinishedEscline -> case myContextStack of
                ContextFrame WithinEscline backslashLoc :: _ ->
                    UnfinishedEscline {backslashLoc = backslashLoc, nextCharOrEof = finalPosition}
                _ -> Unexpected "Encountered a PUnfinishedEscline while not parsing an escline"
            PMalformedNodeComponent -> case myContextStack of
                ContextFrame WithinNode nodeStart :: _ ->
                    MalformedNodeComponent {nodeStart = nodeStart, badElement = finalPosition}
                _ -> Unexpected "Encountered a PMalformedNodeComponent while not parsing a node"
            PUnrecognizedKeyword -> case myContextStack of
                ContextFrame WithinKeyword keywordStart :: _ ->
                    UnrecognizedKeyword {keywordLoc = (keywordStart, (row, col - 1))}
                _ -> Unexpected "Encountered a PUnrecognizedKeyword while not parsing a keyword"
            PForbiddenBareString -> case myContextStack of
                ContextFrame WithinString identStart :: _ ->
                    ForbiddenBareString {strLoc = (identStart, (row, col - 1))}
                _ -> Unexpected "Encountered a PForbiddenBareString while not parsing a string"

type MessageComponent
    = PlainText String
    | Emphasized String
    | LineExerpt {lineNo: Int, highlightStart: Int, highlightEnd: Int, exerpt: String}
    | Link String

type alias Message = List MessageComponent

uppercaseEscapeCodes : List Char
uppercaseEscapeCodes = ['N', 'R', 'T', 'B', 'F', 'U']

type alias Exerpt =  {lineNo: Int, highlightStart: Int, highlightEnd: Int, exerpt: String}

exerptCode : String -> SourceRange -> Exerpt
exerptCode source ((startRow, startCol) as start, (endRow, endCol) as end) =
    let
        bufferSize = if start == end then 17 else 10
        sourceLines = String.lines source
        interestingLine = sourceLines
            |> List.drop (startRow - 1)
            |> List.head
            |> withDefault ""
        lineLength = String.length interestingLine
        trueEndCol = if endRow > startRow then lineLength else endCol
        clipStart = startCol > bufferSize + 3
        clipEnd = trueEndCol + bufferSize + 3 < lineLength
        exerptStart = if clipStart then startCol - bufferSize else 0
        dropCharsLeft = max 0 (exerptStart - 3)
        exerptEnd = if clipEnd then trueEndCol + bufferSize else lineLength
        newStartCol = startCol - dropCharsLeft - 1
        newEndCol = trueEndCol - dropCharsLeft
        clippedString = (if clipStart then "..." else "") ++ (String.slice exerptStart exerptEnd interestingLine) ++ (if clipEnd then "..." else "")
    in {lineNo = startRow, highlightStart = newStartCol, highlightEnd = newEndCol, exerpt = clippedString}

pointAtCode : String -> Position -> Exerpt
pointAtCode source pos = exerptCode source (pos, pos)

getTextUnderRange : SourceRange -> String -> String
getTextUnderRange ((startRow, startCol), (endRow, endCol)) source =
    let
        sourceLines = String.lines source
        relevantLines = List.drop (startRow - 1) sourceLines |> List.take (endRow - startRow + 1)
        mapLast f l = case l of
            [] -> []
            [last] -> [f last]
            h :: t -> h :: mapLast f t
    in case relevantLines of
        [] -> ""
        [line] -> String.slice (startCol - 1) endCol line
        h :: t -> String.dropLeft (startCol - 1) h :: (mapLast (String.left endCol) t)
            |> String.join "\n"

multilineStringGuidance : Message
multilineStringGuidance =
    [ PlainText "The multi-line string syntax can take a little bit of getting used to, but if you want to read up on it, you can check out the official specification here:\n"
    , Link "https://github.com/kdl-org/kdl/blob/76a1de5/SPEC.md#multi-line-strings"
    ]

getErrorMessage : String -> Problem -> Message
getErrorMessage source p = case p of
    Unexpected msg ->
        [ PlainText "I encountered an unexpected problem while parsing the input!  Merely seeing this message while not having tampered with the library is cause for a bug report, if it behooves you.  The only other information I have is this message:\n"
        , Emphasized msg
        ]    
    UnrecognizedEscapeCode            {{-strLoc,-} escLoc, char}               ->
        [ PlainText <| "While parsing a string, I found an escape code I didn't know what to do with.  Specifically, you used the escape code '\\" ++ String.fromChar char ++ "' here:"
        , LineExerpt (exerptCode source escLoc)
        , PlainText <| "But I don't recognize that sequence!  "
        ] ++  if member char uppercaseEscapeCodes
            then [PlainText <| "That said, there is a valid escape code with a lowercase letter " ++ String.fromChar (Char.toLower char) ++ ".  Maybe that's what you meant to use?"]
            else [PlainText "For a list of supported escape codes, see this link:\n", Link "https://github.com/kdl-org/kdl/blob/270c60c/SPEC.md#quoted-string"]
    UnicodeEscapeNotOpened            {{-strLoc,-} escLoc}                     ->
        [ PlainText "While parsing a string, I saw a unicode escape code here:"
        , LineExerpt <| exerptCode source escLoc
        , PlainText "But I didn't see any unicode codepoint after!  To use the \\u escape code, make sure you include a unicode codepoint, so that it looks something like "
        , Emphasized "\\u{4a}"
        , PlainText " (which renders as \"J\") or "
        , Emphasized "\\u{1F990}"
        , PlainText " (which renders as \"🦐\")"
        ]
    UnicodeEscapeEmpty                {{-strLoc,-} escLoc}                     ->
        [ PlainText "While parsing a string, I saw a unicode escape code here:"
        , LineExerpt <| exerptCode source escLoc
        , PlainText "But I didn't see any hexadecimal digits in the curly parenthesis!  To use the \\u escape code, make sure you include a unicode codepoint, so that it looks something like "
        , Emphasized "\\u{4a}"
        , PlainText " (which renders as \"J\") or "
        , Emphasized "\\u{1F990}"
        , PlainText " (which renders as \"🦐\")"
        ]
    UnicodeEscapeInvalidCharacters    {{-strLoc,-} escLoc}                     ->
        [ PlainText "While parsing a string, I saw a unicode escape code here:"
        , LineExerpt <| exerptCode source escLoc
        , PlainText "But some of the characters in the curly brackets weren't hexadecimal digits!  Make sure you only use the numbers 0-9 and letters a-f when entering hexadecimal codepoints.  For example, "
        , Emphasized "\\u{4a}"
        , PlainText " (which renders as \"J\") or "
        , Emphasized "\\u{1F990}"
        , PlainText " (which renders as \"🦐\")"
        ]
    UnicodeEscapeTooLong              {{-strLoc,-} escLoc}                     ->
        [ PlainText "While parsing a string, I saw a unicode escape code here:"
        , LineExerpt <| exerptCode source escLoc
        , PlainText "But there were way too many characters in the curly parenthesis!  Unicode escapes can have at most six digits, which should be all you need for any codepoint.  If you want multiple codepoints one after another, you can use multiple unicode escapes, like this:  "
        , Emphasized "\\u{1f3f3}\\u{fe0f}\\u{200d}\\u{26a7}\\u{fe0f}"
        , PlainText " (which renders as \"🏳️‍⚧️\")"
        ]
    UnicodeEscapeNotClosed            {{-strLoc,-} escLoc}                     ->
        [ PlainText "While parsing a string, I saw a unicode escape code here:"
        , LineExerpt <| pointAtCode source escLoc
        , PlainText "And while I do see the opening curly bracket, I don't see any closing curly bracket!"
        ]
    UnicodeEscapeBadCodepoint         {{-strLoc,-} escLoc}                     ->
        [ PlainText "The unicode codepoint being escaped here doesn't seem like a valid unicode codepoint:"
        , LineExerpt <| exerptCode source escLoc
        , PlainText "Double check to make sure that the hex code here corresponds to a real unicode character, and that you didn't make any typos."
        ]
    UnclosedString                    {strType, strStart}                  ->
        [ PlainText <| "I see the start of a " ++ stringTypeToString strType ++ " string here:"
        , LineExerpt <| pointAtCode source strStart
        , PlainText "But I never see where it gets closed!"
        ]
    UnclosedType                      {typeOf, typeStart, gaveUpAt}        ->
        let
            firstLine =
                case typeOf of
                    TNode -> "It looks like you're trying to start a type annotation for a node here:"
                    TProperty prop -> "It looks like you're trying to start a type annotation for the " ++ getTextUnderRange prop source ++ " property here:"
                    TArgument -> "It looks like you're trying to start a type annotation for a value here:"
        in
            [ PlainText <| firstLine
            , LineExerpt <| pointAtCode source typeStart
            , PlainText "But it seems like you never close it!  I got as far as this point:"
            , LineExerpt <| pointAtCode source gaveUpAt
            , PlainText "But I stopped looking because it didn't look like I was parsing a type annotation anymore.  Make sure you've included a closing parenthesis, and that your type annotation is a valid identifier.  You can always use a string as an identifier if you need to include spaces or other things like that."
            ]
    MalformedRawStringOpening         {stringStart{-, nonQuotationCharacter-}} ->
        [ PlainText "It looks like you're trying to open a raw string, but forgot the quotation mark (\"):"
        , LineExerpt <| exerptCode source stringStart
        ]
    MalformedNumber                   {numberStart, malformedAt}           ->
        [ PlainText "I saw what looks like it might be a number here:"
        , LineExerpt <| pointAtCode source numberStart
        , PlainText "But as I tried to parse it, I encountered a part I didn't understand, here:"
        , LineExerpt <| pointAtCode source malformedAt
        , PlainText "A number should generally have a form like "
        , Emphasized "15"
        , PlainText " or "
        , Emphasized "1.320_001e-5"
        , PlainText " or "
        , Emphasized "0xACAB"
        , PlainText " or "
        , Emphasized "6_942"
        , PlainText "\n\nIf you didn't mean to have a number here, maybe try surrounding it with quotes (\") to make it a string or identifier."
        ]
    UnclosedMultilineComment          {commentStart}                       ->
        [ PlainText "I see the start of a multi-line comment here:"
        , LineExerpt <| pointAtCode source commentStart
        , PlainText "But I don't see where it closes!  Make sure you add a \""
        , Emphasized "*/"
        , PlainText "\" somewhere to close it off."
        ]
    UnclosedChildBlock                {blockStart}                         ->
        [ PlainText "It looks like you open a block to contain child nodes here:"
        , LineExerpt <| pointAtCode source blockStart
        , PlainText "But I got all the way to the end of the input and never saw a closing brace ("
        , Emphasized "}"
        , PlainText ").  Make sure you add one in somewhere after you're done declaring all the children!"
        ]
    UnterminatedNode                  {blockStart, nodeStart, blockEnd}    ->
        [ PlainText "It looks like you may have forgotten to include a semicolon or newline right here:"
        , LineExerpt <| pointAtCode source blockEnd
        , PlainText "It seems like you open a block of nodes here:"
        , LineExerpt <| pointAtCode source blockStart
        , PlainText "and within that block, you declare your last node here:"
        , LineExerpt <| pointAtCode source nodeStart
        , PlainText "But then I think you try to close the block here, before I see a semicolon or a newline to close off that node:"
        , LineExerpt <| pointAtCode source blockEnd
        ]
    MissingValue                      {property{-, absentValue-}}              ->
        [ PlainText "It looks like you defined a property called "
        , PlainText <| getTextUnderRange property source
        , PlainText " here:"
        , LineExerpt <| exerptCode source property
        , PlainText "So after the equals sign, I was expecting to see a value of some kind, like a number or a string, but I don't see anything I could understand.  Maybe you meant to wrap the value in quotes to make it a string?"
        ]
    InvalidIdentifier                 {identifierStart, confusedAt}        ->
        [ PlainText "I think I'm trying to parse a (bare) identifier that starts here:"
        , LineExerpt <| pointAtCode source identifierStart
        , PlainText "But part of the way through, I start seeing characters that can't be part of an identifier unless it's wrapped in quotes (\")"
        , LineExerpt <| pointAtCode source confusedAt
        , PlainText "Did you mean for this to be an identifier?  If so, maybe try wrapping it in quotation marks, or removing the bad characters."
        ]
    UnfinishedEscline                 {backslashLoc, nextCharOrEof}        ->
        [ PlainText "I see a backslash here:"
        , LineExerpt <| pointAtCode source backslashLoc
        , PlainText "Which normally means that I'm about to parse an escaped newline, but instead, I see:"
        , LineExerpt <| pointAtCode source nextCharOrEof
        , PlainText "Maybe you forgot to include a double slash (//) to start a comment?"
        ]
    MalformedNodeComponent            {{-nodeStart,-} badElement}              ->
        [ PlainText "I was trying to parse a node, and was expecting to see either a property (like \"key\"=\"value\"), an argument (like a string or a number), or a block containing child nodes, but instead I encountered something I couldn't understand:"
        , LineExerpt <| pointAtCode source badElement
        , PlainText "I'm not quite sure what you meant to put here.  If it was meant to be a string, maybe try wrapping it in quotation marks?"
        ]
    ForbiddenCharacter                {charPos}                            ->
        let
            char = getTextUnderRange (charPos, charPos) source
                |> String.toList
                |> List.head
                |> withDefault '\u{202e}'
            charHex = toHex <| toCode char
            escapedChar = String.concat ["\\u{", charHex, "}"]
            sampleString = "myNode \"key\"=\"Hello " ++ escapedChar ++ " world!\"..."
            highlightStart = 20
            highlightEnd = highlightStart + String.length escapedChar
            newExerpt = {lineNo = 0, highlightStart = highlightStart, highlightEnd = highlightEnd, exerpt = sampleString}
        in
            [ PlainText "While trying to parse this document, I encountered a character ("
            , Emphasized ("U+" ++ charHex)
            , PlainText ") that the KDL specification has banned from being used literally in any part of a KDL document.  The character shows up somewhere near here:"
            , LineExerpt <| pointAtCode source charPos
            , PlainText "Unfortunately, many characters, such as characters for right-to-left text or a few unicode control characters, aren't allowed here.  You can still use them in a string, if you'd like, but you'll have to use unicode escapes, like this:"
            , LineExerpt newExerpt
            ]
    UnrecognizedKeyword               {keywordLoc}                         ->
        let
            keywordText = getTextUnderRange keywordLoc source
            lowerKeywordText = String.toLower keywordText
        in
            [ PlainText "I seem to have encountered a keyword I don't recognize!"
            , LineExerpt <| exerptCode source keywordLoc
            , PlainText "I was expecting to see a keyword like "
            , Emphasized "#true"
            , PlainText " or "
            , Emphasized "#nan"
            , PlainText " or "
            , Emphasized "#null"
            , PlainText ", but instead I saw "
            , Emphasized (getTextUnderRange keywordLoc source)
            , PlainText "\n\nIf you meant to represent this as a string, make sure you surround it with double-quotes (\") so that I don't get confused.  If instead you meant to make a raw string (e.g. "
            , Emphasized "#\"She said \"hello!\" to her friend\"#"
            , PlainText ") don't forget that double-quote after your pound sign (#).  Otherwise, check to make sure you didn't make a typo!"
            ] ++ (if maybe False (k True) <| identifyKeyword lowerKeywordText
                then
                    [ PlainText "\n\nP.S. It looks like "
                    , Emphasized ("#" ++ lowerKeywordText)
                    , PlainText " (the lowercase version of what you typed) is a valid keyword.  Is that what you meant?"
                    ]
                else [])
    ForbiddenBareString               {strLoc}                            ->
        [ PlainText "Something you were trying to write here looks a little bit ambiguous to me:"
        , LineExerpt <| exerptCode source strLoc
        , PlainText "I suspect that you were actually trying to use the special value "
        , Emphasized ("#" ++ getTextUnderRange strLoc source)
        , PlainText " and accidently just wrote "
        , Emphasized (getTextUnderRange strLoc source)
        , PlainText ", but as you've written it, this actually means the bare string "
        , Emphasized <| "\"" ++ (getTextUnderRange strLoc source) ++ "\""
        , PlainText ".  To prevent confusion, the spec says that this specific word can't be used in a bare identifier, so this is an error.\n\nTo fix this problem, you just need to decide whether you want the special value "
        , Emphasized ("#" ++ getTextUnderRange strLoc source)
        , PlainText ", or the string "
        , Emphasized <| "\"" ++ (getTextUnderRange strLoc source) ++ "\""
        , PlainText ", then use the unambiguous version."
        ]
    NewlineInQuotedString                   {strLoc{-, newlineLoc-}}       ->
        let
            ((startRow, _), (endRow, _) as strEnd) = strLoc
            stringTotalLines = endRow - startRow + 1
        in
            [ PlainText "I was trying to parse a string here:"
            , LineExerpt <| exerptCode source strLoc
            , PlainText "But the only closing double-quote (\") I see is "
            , PlainText <| String.fromInt (stringTotalLines - 1)
            , PlainText " lines down, here:"
            , LineExerpt <| pointAtCode source strEnd
            , PlainText "Did you mean for this to be a multiline string?  If so, recall that the opening quote of a multi-line string must be immediately followed by a newline, and the string itself starts on the next line."
            , PlainText "\n\nOtherwise, maybe you just forgot to close this string?\n\n"
            ] ++ multilineStringGuidance
    MultilineStringTrailingCharacters {{-strLoc, -}trailingChars}         ->
        [ PlainText "I was trying to parse a multi-line string, but it looks like you've got some trailing characters on the last line:"
        , LineExerpt <| exerptCode source trailingChars
        , PlainText "To declare a multi-line string, make sure you leave the last line (the one with the closing double-quote) as just whitespace.  I use that whitespace to help calibrate how much whitespace to trim off of the prior lines, so there can't be any non-whitespace characters there or I'll get confused.\n\n"
        ] ++ multilineStringGuidance
    MultilineStringLineLacksPrefix    {strLoc, violatingChars}            ->
        let
            (_, (strEndRow, _) as strEnd) = strLoc
        in
            [ PlainText "I was trying to parse a multi-line string, but one of the lines doesn't have enough leading whitespace:"
            , LineExerpt <| exerptCode source violatingChars
            , PlainText "When parsing a multi-line string, every non-empty line has to start with at least as much whitespace as precedes the closing double-quote, which gets stripped off as part of parsing.\n\n"
            , PlainText "For reference, your closing double-quote has this much whitespace:"
            , LineExerpt <| exerptCode source ((strEndRow, 1), strEnd)
            ] ++ multilineStringGuidance
    MultilineStringMismatchedPrefixSpace {strLoc, offendingChar, expected, got} ->
        let
            (_, (strEndRow, _)) = strLoc
            (offendingRow, offset) = offendingChar
            expectedName = nameWhitespace expected
            gotName = nameWhitespace got
        in
            [ PlainText "I was trying to parse a multi-line string, but the leading whitespace on some of your lines is mismatched.  In particular, there's a "
            , PlainText gotName
            , PlainText " at this point on line "
            , PlainText <| String.fromInt offendingRow
            , PlainText ":"
            , LineExerpt <| pointAtCode source offendingChar
            , PlainText "But at the same point in the whitespace before the closing double quote, there's a "
            , PlainText expectedName
            , PlainText ":"
            , LineExerpt <| pointAtCode source (strEndRow, offset)
            , PlainText "When you're writing a multi-line string, it's important that the whitespace preceding each line is exactly the same.\n\n"
            ] ++ multilineStringGuidance

messageToString : Int -> Message -> String
messageToString =
    let
        countTabs = String.toList >> List.filter ((==) '\t') >> List.length
        showMessageComponent tabWidth c = case c of
            PlainText t -> t
            Emphasized t -> t
            Link l -> l
            LineExerpt {lineNo, highlightStart, highlightEnd, exerpt} ->
                let
                    lineNoStr = String.fromInt lineNo
                    highlightLength = highlightEnd - highlightStart
                    linePrefix = if lineNo > 0
                        then String.repeat (5 - String.length lineNoStr) " " ++ lineNoStr ++ " | "
                        else String.repeat 8 " "
                    tabsBeforeHighlight = countTabs (String.left highlightStart exerpt)
                    tabsInHighlight = countTabs (String.dropLeft highlightStart exerpt |> String.left highlightLength)
                in String.concat
                    [ "\n\n"
                    , linePrefix
                    , exerpt
                    , "\n"
                    , String.repeat (8 + highlightStart + (tabWidth - 1) * tabsBeforeHighlight) " "
                    , String.repeat (highlightLength + (tabWidth - 1) * tabsInHighlight) "^"
                    , "\n\n"
                    ]
    in showMessageComponent >> List.map >> ((<<) String.concat)
