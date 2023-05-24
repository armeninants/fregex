-- TODO:
--
-- REFERENCES:
--    * http://www.pixelsham.com/wp-content/uploads/2020/05/REGEX.png

module Regex.Parser
  ( regexParser,
    parseRegex,
    regexParserDummy,
  )
where

import Data.Char (isSpace, ord)
import qualified Data.Text as T
import RIO hiding
  ( many,
    sequence,
    try,
    (<|>),
  )
import RIO.Partial (read)
import Regex.Term
import Text.Parsec hiding (choice, space)
import Text.Parsec.Expr

type ParserM m = ParsecT Text () m

regexParserDummy :: Monad m => ParserM m Text
regexParserDummy = do
  T.pack <$> manyTill anyChar (try (void $ string "\n") <|> eof)

------------------------------------------------------------

specialCharParser :: Monad m => ParserM m SpecialCharacter
specialCharParser = msum special
  where
    special = map (\(c, t) -> char c $> t) specialUnescapedCharacterMap

specialEscCharParser :: Monad m => ParserM m SpecialEscCharacter
specialEscCharParser = char '\\' >> msum escaped
  where
    escaped = map (\(c, t) -> char c $> t) specialEscapedCharacterMap

decimal :: Monad m => ParserM m Int
decimal = read <$> many1 digit

literalParser :: Monad m => ParserM m Char
literalParser =
  noneOf metaCharacters <|> try (char '\\' >> noneOf specialEscapedCharacter)

setItemParser :: Monad m => ParserM m SetItem
setItemParser = do
  c1 <- literalParser
  dash <- isJust <$> optionMaybe (char '-')
  if dash
    then do
      c2 <- literalParser
      return $ Right (c1, c2)
    else return $ Left c1

setParser :: Monad m => ParserM m RegexTerm
setParser = do
  _ <- char '['
  positive <- isNothing <$> optionMaybe (char '^')
  setItems <- many1 setItemParser
  _ <- char ']'
  return $ Set positive setItems

-- |
-- {M} --> (M, Just M)
-- {M,} --> (M, Nothing)
-- {M,N} --> (M, Just N)
quantifierParser :: Monad m => ParserM m (Int, Maybe Int)
quantifierParser = between (char '{') (char '}') $ do
  fromNum <- spaces1 >> decimal
  spaces1
  hasComma <- optionMaybe $ char ',' >> spaces1
  case hasComma of
    Nothing -> return (fromNum, Just fromNum)
    Just _ -> do
      toNum <- optionMaybe decimal
      spaces1
      return (fromNum, toNum)

regexParser :: Monad m => ParserM m RegexTerm
regexParser = buildExpressionParser ops atom
  where
    ops =
      [ [ Postfix (Repeat (0, Nothing) <$ char '*'),
          Postfix (Repeat (1, Nothing) <$ char '+'),
          Postfix (Repeat (0, Just 1) <$ char '?'),
          Postfix (Repeat <$> quantifierParser)
        ],
        [Infix (return sequence) AssocRight],
        [Infix (choice <$ char '|') AssocRight]
      ]

    atom =
      msum
        [ SpecialCharacter <$> specialCharParser,
          SpecialEscCharacter <$> try specialEscCharParser,
          Literal . T.pack <$> many1 literalParser,
          parensAll,
          setParser
        ]

    parensAll = do
      _ <- char '('
      m1 <- optionMaybe $ char '?'
      t <- case m1 of
        Nothing -> regexParser
        Just _ -> do
          m2 <- optionMaybe $ char '<'
          case m2 of
            Nothing ->
              (char ':' >> regexParser)
                <|> (char '=' >> PositiveLookahead <$> regexParser)
                <|> (char '!' >> NegativeLookahead <$> regexParser)
            Just _ ->
              (char '=' >> PositiveLookbehind <$> regexParser)
                <|> (char '!' >> NegativeLookbehind <$> regexParser)
      _ <- char ')'
      return t

    sequence a b = Sequence $ seqTerms a ++ seqTerms b
    choice a b = Choice $ choiceTerms a ++ choiceTerms b

    seqTerms (Sequence ts) = ts
    seqTerms t = [t]

    choiceTerms (Choice ts) = ts
    choiceTerms t = [t]

parseRegex :: Text -> Either String RegexTerm
parseRegex regStr = first show $ parse regexParser "" regStr

spaces1 :: (Stream s m Char) => ParsecT s u m ()
{-# INLINEABLE spaces1 #-}
spaces1 = skipMany space1 <?> "white space"

space1 :: (Stream s m Char) => ParsecT s u m Char
{-# INLINEABLE space1 #-}
space1 = satisfy isSpace1 <?> "space"

-- | Returns 'True' for any character that satisfies 'isSpace',
-- except characters @\\n@, @\\r@, @\\f@, @\\v@.
isSpace1 :: Char -> Bool
isSpace1 c =
  isSpace c && not (uc >= 10 && uc <= 13)
  where
    uc = fromIntegral (ord c) :: Word
