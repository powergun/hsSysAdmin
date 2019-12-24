module ExtendHspec.Demo (demo) where

import           Data.Bifunctor
import           Data.List
import           Test.Hspec
import           Text.Read      (readEither, readMaybe)

data WeaponSpec = WeaponSpec
  { name    :: String
  , cal     :: Float
  , magSize :: Int
  } deriving (Eq, Show)

strToSpec :: String -> Maybe WeaponSpec
strToSpec s =
  let segments = words s
  in case segments of
       [n, c, m] -> WeaponSpec <$> Just n
                               <*> (readMaybe c :: Maybe Float)
                               <*> (readMaybe m :: Maybe Int)
       _ -> Nothing

-- this also works:
-- NOTE: readMaybe <a string> :: Maybe String will always return Nothing !!

-- strToSpec s =
--   case length $ words s of
--     3 -> segmentsToSpec $ words s
--     _ -> Nothing
--   where
--     segmentsToSpec [n,c,m] = do
--       c' <- readMaybe c :: Maybe Float
--       m' <- readMaybe m :: Maybe Int
--       return $ WeaponSpec n c' m'

data Error = InvalidFormat String
           | InvalidCalibre
           | InvalidMagSize
           deriving (Eq, Show)

strToSpecEither :: String -> Either Error WeaponSpec
strToSpecEither s =
  case words s of
    [n,c,m] -> WeaponSpec <$> Right n
                          <*> first (\_ -> InvalidCalibre) (readEither c :: Either String Float)
                          <*> first (\_ -> InvalidMagSize) (readEither m :: Either String Int)
    _       -> Left $ InvalidFormat "Must be \"<string> <float> <int>\""

demo :: IO ()
demo = hspec $ do
  describe "Convert String to WeaponSpec" $ do
    it "expect failed conversion" $ do
      strToSpec "12" `shouldBe` Nothing

    it "expect successful conversion" $ do
      strToSpec "m14 7.62 20" `shouldBe` Just (WeaponSpec "m14" 7.62 20)

    it "expect failed conversion (calibre)" $ do
      strToSpec "BFG-9000mk2 20mm 3" `shouldBe` Nothing

    it "expect failed conversion with error detail (calibre)" $ do
      strToSpecEither "BFG-9000mk2 20mm 3" `shouldBe` Left InvalidCalibre
