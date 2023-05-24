module Regex.Term
  ( RegexTerm (..),
    SpecialCharacter (..),
    SpecialEscCharacter (..),
    Range,
    SetItem,
    metaCharacters,
    specialEscapedCharacterMap,
    specialEscapedCharacter,
    specialUnescapedCharacterMap,
    specialEscCharToChar,
    specialCharToChar,
  )
where

import RIO

-- Anchors
-- Character classes
-- Assertions
-- Quantifiers
-- Special characters
-- String replacement
-- Ranges
-- Metacharacters

type Positive = Bool

type Range = (Char, Char)

type SetItem = Either Char Range

data RegexTerm
  = Literal Text
  | SpecialCharacter SpecialCharacter
  | SpecialEscCharacter SpecialEscCharacter
  | Sequence [RegexTerm]
  | Repeat (Int, Maybe Int) RegexTerm
  | Choice [RegexTerm]
  | Set Positive [SetItem]
  | PositiveLookahead RegexTerm
  | NegativeLookahead RegexTerm
  | PositiveLookbehind RegexTerm
  | NegativeLookbehind RegexTerm
  deriving (Eq, Show)

data SpecialCharacter
  = AnyCharacter
  | TheStartOfALine
  | TheEndOfALine
  deriving (Eq, Show, Enum, Bounded)

data SpecialEscCharacter
  = NewLineCharacter
  | TabCharacter
  | AnyWhiteSpaceCharacter
  | AnyNonWhiteSpaceCharacter
  | AnyWordCharacter
  | AnyNonWordCharacter
  | AnyDigitCharacter
  | AnyNonDigitCharacter
  | WordBoundary
  | NonWordBoundary
  deriving (Eq, Show, Enum, Bounded)

-- | Characters that must be escaped
metaCharacters :: [Char]
metaCharacters = "^$()<>|\\{}[].*+?"

specialEscCharToChar :: SpecialEscCharacter -> Char
specialEscCharToChar = \case
  NewLineCharacter -> 'n'
  TabCharacter -> 't'
  AnyWhiteSpaceCharacter -> 's'
  AnyNonWhiteSpaceCharacter -> 'S'
  AnyWordCharacter -> 'w'
  AnyNonWordCharacter -> 'W'
  AnyDigitCharacter -> 'd'
  AnyNonDigitCharacter -> 'D'
  WordBoundary -> 'b'
  NonWordBoundary -> 'B'

specialEscapedCharacterMap :: [(Char, SpecialEscCharacter)]
specialEscapedCharacterMap = map f [minBound @SpecialEscCharacter ..]
  where
    f char = (specialEscCharToChar char, char)

specialEscapedCharacter :: [Char]
specialEscapedCharacter = fst <$> specialEscapedCharacterMap

specialCharToChar :: SpecialCharacter -> Char
specialCharToChar = \case
  AnyCharacter -> '.'
  TheStartOfALine -> '^'
  TheEndOfALine -> '$'

specialUnescapedCharacterMap :: [(Char, SpecialCharacter)]
specialUnescapedCharacterMap = map f [minBound @SpecialCharacter ..]
  where
    f char = (specialCharToChar char, char)
