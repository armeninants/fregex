-- |
-- Module      : Fregex.Term
-- Description : Fregex Language Definition
-- Copyright   : (c) Armen Inants, 2023
-- License     : MIT
-- Maintainer  : armen@inants.com
-- Stability   : experimental
--
-- Fregex Language Definition
module Fregex.Term
  ( FregexTerm (..),
  )
where

import RIO

-- import Regex.Term

data FregexTerm
  = -- TODO: replace Text by RegexTerm
    Regex Text
  | -- Years
    Year
  | Year2020s
  | YearFuture
  | Year21Cent
  | -- Within Sentence
    WithinSentence [FregexTerm]
  | Choice [FregexTerm]
  deriving (Eq, Show)
