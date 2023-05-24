module Fregex.Parser (runFregexParser, fregexToRegex) where

import Fregex.Serialize
import Fregex.Term
import RIO hiding
  ( many,
    sequence,
    try,
    (<|>),
  )
import Regex.Parser
import Text.Parsec hiding (choice)
import qualified Text.Parsec.Indent as I

-- import Text.Parsec.Expr
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import Fregex.Serialize
-- import Regex.Term
-- import qualified System.IO
-- import qualified Text.Parsec.Indent.Explicit as EI

-- type Parser = Parsec Text ()

withinSentence :: Monad m => I.IndentParserT Text () m FregexTerm
withinSentence = I.withPos $ do
  void $ string "<within sentence>" >> spaces >> I.indented
  r2 <- I.block $ do
    arg <- fregexParser
    void spaces
    return arg
  return $ WithinSentence r2

choiceParser :: Monad m => I.IndentParserT Text () m FregexTerm
choiceParser = I.withPos $ do
  void $ string "<choice>" >> spaces >> I.indented
  r2 <- I.block $ do
    arg <- fregexParser
    void spaces
    return arg
  return $ Choice r2

fregexParser :: Monad m => ParsecT Text () (I.IndentT m) FregexTerm
fregexParser =
  try withinSentence
    <|> try choiceParser
    <|> try (YearFuture <$ string "<year future>")
    <|> try (Year21Cent <$ string "<year 21cent>")
    <|> try (Year2020s <$ string "<year 2020s>")
    <|> try (Year <$ string "<year>")
    <|> Regex <$> regexParserDummy

runFregexParser :: Monad m => Text -> m (Either ParseError FregexTerm)
runFregexParser = I.runIndentParserT p () ""
  where
    p = do
      res <- fregexParser
      eof
      return res

fregexToRegex :: Text -> Either ParseError Text
fregexToRegex fregexStr = runIdentity $ fmap toString <$> runFregexParser fregexStr
