{-# LANGUAGE TemplateHaskell #-}

module FregexApp
  ( main,
  )
where

import qualified Data.Text.IO as T
import Fregex.Parser
import Options.Applicative.Simple
import qualified Paths_fregex
import RIO

newtype Options = Options
  { optFregex :: Text
  }

optFregexL :: Lens' Options Text
optFregexL = lens optFregex (\o f -> o {optFregex = f})

type AppM = RIO Options

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_fregex.version)
      "Fregex to RegEx converter"
      "Fregex to RegEx converter"
      ( Options
          <$> strArgument
            ( help "Fregex expression"
            )
      )
      empty
  runRIO options runApp

runApp :: AppM ()
runApp = do
  fregexStr <- view optFregexL
  -- liftIO $ T.putStrLn fregexStr
  liftIO $ either (fail . show) T.putStrLn $ fregexToRegex fregexStr
