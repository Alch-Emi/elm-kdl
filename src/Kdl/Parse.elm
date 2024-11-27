module Kdl.Parse exposing (parse, Problem(..), Message, MessageComponent(..), getErrorMessage, messageToString)

import Kdl exposing (Node(..), Value, ValueContents(..), LocatedNode, LocatedValue, Position, SourceRange)
import Kdl.Shared exposing (identifierCharacter, initialCharacter, unicodeNewline, unicodeSpace)
import Kdl.Util exposing (andf, flip, k, maybe, orf, parseRadix)

import BigInt exposing (BigInt)
import BigRational exposing (BigRational)

import Char exposing (isDigit)
import Dict
import List exposing (member)
import Maybe exposing (withDefault)
import Parser.Advanced as Parser exposing (DeadEnd, Parser, Nestable(..), Token(..), (|.), (|=), andThen, backtrackable, chompIf, chompUntil, chompWhile, commit, end, getChompedString, getOffset, getSource, getPosition, inContext, lazy, problem, keyword, mapChompedString, oneOf, succeed, symbol, variable)
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
    | PUnclosedString
    | PUnclosedType
    | PMalformedRawStringOpening
    | PUnclosedMultilineComment
    | PUnclosedChildBlock
    | PUnterminatedNode
    | PMissingValue
    | PMalformedNodeComponent
    | PInvalidIdentifier
    | PUnfinishedEscline
    | PMalformedNumber

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
    | WithinIdentifier
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

parseNullVal : Parser c PProblem ValueContents
parseNullVal =
    succeed NullVal
    |. keyword (Token "null" <| PExpecting "null")

parseEscapeCode : Char -> Parser c PProblem String
parseEscapeCode c = case c of
    'n' -> succeed "\n"
    'r' -> succeed "\r"
    't' -> succeed "\t"
    '\\' -> succeed "\\"
    '/' -> succeed "/"
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
                            Just n -> succeed (String.fromList [Char.fromCode <| withDefault 0 <| String.toInt <| BigInt.toString n])
                            Nothing -> problem PUnicodeEscapeInvalidCharacters
                )
            )
        |. symbol (Token "}" PUnicodeEscapeNotClosed)
    _ -> problem (PUnrecognizedEscapeCode c)

parseEscapeSequence : Parser Context PProblem String
parseEscapeSequence =
    succeed parseEscapeCode
    |. symbol (Token "\\" <| PExpecting "backslash")
    |= (
        chompIf (k True) PUnclosedString
        |> getChompedString
        |> Parser.map (String.uncons >> withDefault ('_', "") >> Tuple.first)
    )
    |> andThen identity
    |> inContext WithinStrEscape

parseQuotedStringSegment : Parser Context PProblem String
parseQuotedStringSegment =
    let
        plainStringChar = not << flip member ['"', '\\']
    in
    oneOf
        [
            chompIf plainStringChar (PExpecting "string character")
            |. chompWhile plainStringChar
            |> getChompedString
        , parseEscapeSequence
        ]

parseQuotedStringInnards : Parser Context PProblem String
parseQuotedStringInnards = star (++) "" parseQuotedStringSegment

parseQuotedString : Parser Context PProblem String
parseQuotedString =
    succeed identity
    |. symbol (Token "\"" <| PExpecting "opening quote")
    |= parseQuotedStringInnards
    |. symbol (Token "\"" PUnclosedString)
    |> inContext WithinQuotedString

parseRawString : Parser Context PProblem String
parseRawString =
    (
        symbol (Token "r" <| PExpecting "opening raw quote")
        |> backtrackable -- Note that r#hello_world is a valid identifier
    )
    |. chompWhile ((==) '#')
    |. symbol (Token "\"" <| PMalformedRawStringOpening)
    |> getChompedString 
    |> andThen (\openingToken ->
        let
            closeToken = Token (String.dropLeft 1 openingToken |> String.reverse) PUnclosedString
        in
            chompUntil closeToken
            |> getChompedString
            |> flip (|.) (symbol closeToken)
    )
    |> inContext WithinRawString

parseString : Parser Context PProblem String
parseString = oneOf
    [ parseQuotedString
    , parseRawString
    ]

parseStringVal : Parser Context PProblem ValueContents
parseStringVal = parseString |> Parser.map StringVal

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
parseNumberVal = Parser.map NumberVal parseNumber

parseBool : Parser c PProblem Bool
parseBool = oneOf
    [ succeed True |. symbol (Token "true" <| PExpecting "true")
    , succeed False |. symbol (Token "false" <| PExpecting "false")
    ]

parseBoolVal : Parser c PProblem ValueContents
parseBoolVal = Parser.map BoolVal parseBool

mkLocVal : Position -> Maybe String -> ValueContents -> Position -> LocatedValue
mkLocVal s t v e = Value (s, e) t v

parseValue : Parser Context PProblem LocatedValue
parseValue =
    succeed mkLocVal
    |= getPosition
    |= optional Nothing (Parser.map Just parseType)
    |= oneOf
    [ parseStringVal
    , parseNumberVal
    , parseBoolVal
    , parseNullVal
    ]
    |= getPosition
    |> inContext WithinValue

signChar : Char -> Bool
signChar = flip member ['+', '-']

parseBareIdentifier : List Char -> Parser c PProblem String
parseBareIdentifier possibleFollowingCharacters = oneOf
    [ variable
        { start = andf (not << signChar) initialCharacter
        , inner = identifierCharacter
        , reserved = Set.fromList ["true", "false", "null"]
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

parseIdentifier : List Char -> Parser Context PProblem String
parseIdentifier possibleFollowingCharacters =
    oneOf [parseRawString, parseBareIdentifier possibleFollowingCharacters, parseString]
        |> inContext WithinIdentifier

parsePropOrArg : Parser Context PProblem PropOrArg
parsePropOrArg = oneOf
    [ succeed Prop
        |= (backtrackable <| parseIdentifier ['='])
        |. symbol (Token "=" <| PExpecting "equals")
        |= (parseValue |> orProblem PMissingValue)
        |> inContext WithinProp
    , succeed Arg
        |= parseValue
    ]

parseType : Parser Context PProblem String
parseType =
     succeed identity
        |. symbol (Token "(" <| PExpecting "type")
        |= oneOf
            [ parseIdentifier [')']
            , commit identity |= problem PInvalidIdentifier
            ]
        |. symbol (Token ")" <| PUnclosedType)
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
    |. star k () nodespace
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
            , end (PExpecting "eof") 
            , commit () |. lookAhead1 True PMalformedNodeComponent ((==) '}') |. problem PUnterminatedNode
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
                        [ parseIdentifier [';', '/', '}']
                        , commit identity |= problem PInvalidIdentifier
                        ]
                , succeed (Tuple.pair Nothing)
                    |= parseIdentifier [';', '/', '}']
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

type Problem
    = Unexpected String
    | UnrecognizedEscapeCode {strLoc: Position, escLoc: SourceRange, char: Char}
    | AttemptingToEscapeNewlineInString {strLoc: Position, backslashLoc: Position}
    | UnicodeEscapeNotOpened {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeEmpty {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeInvalidCharacters {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeTooLong {strLoc: Position, escLoc: SourceRange}
    | UnicodeEscapeNotClosed {strLoc: SourceRange, escLoc: Position}
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

type alias DefaultContextFrame = {row: Int, col: Int, context: Context}
type MyContextFrame = ContextFrame Context Position

makeContextMatchable : DefaultContextFrame -> MyContextFrame
makeContextMatchable {row, col, context} = ContextFrame context (row, col)

parse : String -> Result Problem (List (LocatedNode))
parse =
    Parser.run parseDocument
    >> Result.mapError (
        List.head
        >> Maybe.map translateDeadEnd
        >> Maybe.withDefault (Unexpected "No dead ends returned in parse")
    )

translateDeadEnd : DeadEnd Context PProblem -> Problem
translateDeadEnd {row, col, contextStack, problem} =
    let
        finalPosition = (row, col)
        myContextStack = List.map makeContextMatchable contextStack
    in
        case problem of
            PExpecting s -> Unexpected ("Unexpected problem which resulted in attempting to interpret a character as a " ++ s)
            PUnrecognizedEscapeCode c -> case myContextStack of
                ContextFrame WithinStrEscape escStart :: ContextFrame WithinQuotedString strLoc :: _ ->
                    if c == '\n'
                        then AttemptingToEscapeNewlineInString {strLoc=strLoc, backslashLoc=escStart}
                        else UnrecognizedEscapeCode {strLoc=strLoc, escLoc=(escStart, (row, col - 1)), char=c}
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
                    :: ContextFrame WithinValue (valStartRow, valStartCol)
                    :: ContextFrame WithinProp propStart
                    :: _ ->
                    UnclosedType {typeOf=TProperty (propStart, (valStartRow, valStartCol - 2)),typeStart=typeStart,gaveUpAt=finalPosition}
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
            PUnterminatedNode -> case myContextStack of
                ContextFrame WithinNode nodeStart
                    :: ContextFrame WithinChildBlock blockStart
                    :: _ ->
                    UnterminatedNode {blockStart = blockStart, nodeStart = nodeStart, blockEnd = finalPosition}
                _ -> Unexpected "Encountered a PUnterminatedNode while not parsing a node within a child block"
            PMissingValue -> case myContextStack of
                ContextFrame WithinProp propStart :: _ ->
                    MissingValue {property = (propStart, (row, col - 2)), absentValue = finalPosition}
                _ -> Unexpected "Encountered a PMissingValue while not parsing a property"
            PInvalidIdentifier -> case myContextStack of
                ContextFrame WithinIdentifier identStart :: _ ->
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
    AttemptingToEscapeNewlineInString {{-strLoc,-} backslashLoc}               ->
        [ PlainText "While parsing a string, I saw an backslash followed by a newline, right here:"
        , LineExerpt <| pointAtCode source backslashLoc
        , PlainText "If you meant to escape the newline so that it'll show up in the string, you don't actually need to include the backslash!  In KDL, all strings are by default multi-line strings.  If you want a literal backslash instead, make sure you use two backslashes, so that the first escapes the second."
        ]
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
                    TProperty prop -> "It looks like you're trying to start a type annotation for the" ++ getTextUnderRange prop source ++ " property here:"
                    TArgument -> "It looks like you're trying to start a type annotation for a value here:"
        in
            [ PlainText <| firstLine
            , LineExerpt <| pointAtCode source typeStart
            , PlainText "But it seems like you never close it!  I got as far as this point:"
            , LineExerpt <| pointAtCode source gaveUpAt
            , PlainText "But I stopped looking, because it didn't look like I was parsing a type annotation anymore.  Make sure you've included a closing parenthesis, and that your type annotation is a valid identifier.  You can always use a string as an identifier if you need to include characters like spaces."
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

messageToString : Message -> String
messageToString =
    let
        showMessageComponent c = case c of
            PlainText t -> t
            Emphasized t -> t
            Link l -> l
            LineExerpt {lineNo, highlightStart, highlightEnd, exerpt} ->
                let
                    lineNoStr = String.fromInt lineNo
                    linePrefix = String.repeat (5 - String.length lineNoStr) " " ++ lineNoStr ++ " | "
                in String.concat
                    [ "\n\n"
                    , linePrefix
                    , exerpt
                    , "\n"
                    , String.repeat (8 + highlightStart) " "
                    , String.repeat (highlightEnd - highlightStart) "^"
                    , "\n\n"
                    ]
    in List.map showMessageComponent >> String.concat
