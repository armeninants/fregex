module Fregex.Serialize (toString) where

import Data.List (permutations)
import qualified Data.Text as T
import Fregex.Term
import RIO hiding
  ( many,
    sequence,
    try,
    (<|>),
  )

toString :: FregexTerm -> Text
toString (Regex txt) = txt
toString Year = "19[0-9]{2}|20[0-9]{2}"
toString Year2020s = "202[0-9]"
toString YearFuture = "202[3-9]"
toString Year21Cent = "20[0-9]{2}"
toString (WithinSentence trms) =
  permutations trms & fmap f & T.intercalate "|" & wrap "\\b" "\\b"
  where
    f = T.intercalate "(?:[^\\w.]+\\w+){0,20}?[^\\w.]+" . fmap (wrap "(?:" ")" . toString)
toString (Choice trms) =
  trms & fmap f & T.intercalate "|"
  where
    f = wrap "(?:" ")" . toString

wrap :: Text -> Text -> Text -> Text
wrap x y t = x <> t <> y
