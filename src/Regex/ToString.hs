{-# LANGUAGE QuasiQuotes #-}

module Regex.ToString
  ( regexToString,
    testRTS,
    test1,
  )
where

import Data.String.QQ
import Data.Text (intercalate)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import RIO
import Regex.Parser (parseRegex)
import Regex.Term
import System.IO (print)
import Text.Show.Pretty (pPrint)
import Text.Utils

escapeMeta :: Text -> Text
escapeMeta = escape (T.pack metaCharacters)

class ToString a where
  toString :: a -> Text

instance ToString Int where
  toString = T.pack . show

-- | TODO: omit parentheses wherever possible
instance ToString RegexTerm where
  toString = \case
    Literal str -> escapeMeta str
    SpecialCharacter special -> toString special
    SpecialEscCharacter special -> toString special
    Sequence terms -> "(?:" <> mconcat (toString <$> terms) <> ")"
    Repeat (0, Nothing) trm -> toString trm <> "*"
    Repeat (1, Nothing) trm -> toString trm <> "+"
    Repeat (m, Nothing) trm -> toString trm <> "{" <> toString m <> ",}"
    Repeat (0, Just 1) trm -> toString trm <> "?"
    Repeat (m, Just n) trm ->
      toString trm <> "{" <> toString m <> "," <> toString n <> "}"
    Choice terms -> "(?:" <> intercalate "|" (toString <$> terms) <> ")"
    PositiveLookahead trm -> "(?=" <> toString trm <> ")"
    NegativeLookahead trm -> "(?!" <> toString trm <> ")"
    PositiveLookbehind trm -> "(?<=" <> toString trm <> ")"
    NegativeLookbehind trm -> "(?<!" <> toString trm <> ")"
    Set True items -> "[" <> mconcat (toString <$> items) <> "]"
    Set False items -> "[^" <> mconcat (toString <$> items) <> "]"

instance ToString SpecialCharacter where
  toString = T.singleton . specialCharToChar

instance ToString SpecialEscCharacter where
  toString = ("\\" <>) . T.singleton . specialEscCharToChar

instance ToString SetItem where
  toString = \case
    Left c -> escapeMeta $ T.singleton c
    Right (c1, c2) ->
      escapeMeta (T.singleton c1) <> "-" <> escapeMeta (T.singleton c2)

regexToString :: RegexTerm -> Text
regexToString = toString

testRTS :: Text -> Either String Text
testRTS = fmap regexToString . parseRegex

test1 :: IO ()
test1 = do
  let regStr =
        [s|(?:% of women(?!\.)\W+(?:\w+(?!\.)\W+){0,5}board of directors|gender balance)(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:20[0-9]{2})|(?:20[0-9]{2})(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:% of women(?!\.)\W+(?:\w+(?!\.)\W+){0,5}board of directors|gender balance)|(?:% of women(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:group executive committee|(?:executive|senior) directors|key positions|directors))|(?:20[0-9]{2})(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:group executive committee|(?:executive|senior) directors|key positions|directors)|(?:female representation)(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:20[0-9]{2})|(?:20[0-9]{2})(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:female representation)|(?:gender pay gap)(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:20[0-9]{2})|(?:20[0-9]{2})(?!\.)\W+(?:\w+(?!\.)\W+){0,5}(?:gender pay gap)|]
  -- let regStr = [s|(?:partnerships?|foundations?)(?!\.)\W+(?:\w+(?!\.)\W+){0,30}gender|]
  let eRegEx = parseRegex regStr
  case eRegEx of
    Left err -> print err
    Right regEx -> do
      pPrint regEx
      let regStrNew = regexToString regEx
      putStrLn regStrNew
