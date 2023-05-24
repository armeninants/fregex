module Text.Utils
  ( escape
  ) where

import qualified Data.Text                     as T
import           RIO


escape :: Text -> Text -> Text
escape chars = T.foldr
  (\b acc ->
    if b `T.elem` chars then '\\' `T.cons` b `T.cons` acc else b `T.cons` acc
  )
  ""
