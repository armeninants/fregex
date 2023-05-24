module Fregex.EndToEndSpec
  ( spec,
  )
where

import qualified Data.Vector as V
import Data.Yaml
import Fregex.Parser
import RIO
import RIO.Directory
  ( getCurrentDirectory,
  )
import RIO.FilePath
  ( (</>),
  )
import Test.Hspec

spec :: Spec
spec = do
  projectDir <- runIO getCurrentDirectory
  let specPath = projectDir </> "test/Fregex/end-to-end.yaml"
  mVal <- runIO ((rightToMaybe <$> decodeFileEither specPath) :: IO (Maybe EndToEnd))
  fromMaybe (EndToEnd V.empty V.empty) mVal & testCases

testCases :: EndToEnd -> Spec
testCases (EndToEnd succs fails) = do
  describe "Success cases" $ do
    forM_ succs $ \(fregexStr, regexStr) -> do
      it "passes success test" $
        fregexToRegex fregexStr
          `shouldBe` Right regexStr
  describe "Failure cases" $ do
    forM_ fails $ \fregexStr -> do
      it "passes failure test" $
        fregexToRegex fregexStr
          `shouldSatisfy` isLeft

data EndToEnd = EndToEnd (Vector (Text, Text)) (Vector Text)

instance FromJSON EndToEnd where
  parseJSON =
    withObject "Success and Failure cases" $ \v -> do
      successArr <- v .: "success"
      successCases <-
        withArray
          "Success cases"
          ( mapM $
              withObject "Success case" $ \v2 -> do
                fregex <- v2 .: "fregex"
                regex <- v2 .: "regex"
                return (fregex, regex)
          )
          successArr
      failArr <- v .: "fail"
      failCases <-
        withArray
          "Success cases"
          ( mapM $
              withObject "Success case" $ \v2 -> do
                v2 .: "fregex"
          )
          failArr
      return $ EndToEnd successCases failCases

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right a) = Just a
