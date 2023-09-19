{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Config.Suckless.AesonSpec (spec) where

import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Function
import Data.Scientific

import GHC.Generics hiding (C)
import Text.InterpolatedString.Perl6 (qc,q)
import Data.Aeson
import Data.Maybe
import Test.Hspec
import Prettyprinter


readConfig :: String -> IO [Syntax C]
readConfig s = do
  pure $ parseTop s & either mempty id
  -- print $ pretty f
  -- pure f

data SomeData =
  SomeData
  { someDataKey1 :: Int
  , someDataKey2 :: String
  , someDataKey3 :: [Scientific]
  }
  deriving stock (Generic,Show,Eq)

instance ToJSON SomeData
instance FromJSON SomeData

spec :: Spec
spec = do
  describe "toJSON" $ do

    it "reads int" $ do
      c <- readConfig [qc|1|] <&> toJSON
      c `shouldBe` toJSON [1::Int]

    it "reads scientific" $ do
      c <- readConfig [qc|1.00|] <&> toJSON
      c `shouldBe` toJSON [1.00 :: Scientific]

    it "reads bool" $ do
      t <- readConfig [qc|#t|]  <&> toJSON . head
      t `shouldBe` toJSON [Bool True]
      f <- readConfig [qc|#f|] <&> toJSON . head
      f `shouldBe` toJSON [Bool False]

    it "reads string" $ do
      s <- readConfig [qc|"somestring"|] <&> toJSON
      s `shouldBe` toJSON ["somestring" :: String]

    it "reads array" $ do
      s <- readConfig [qc|(1 2 3 4)|] <&> toJSON . head
      print s
      s `shouldBe` toJSON [1::Int,2,3,4]

    it "reads simple object" $ do
      s <- readConfig [qc|
        (object
            (key1 : 22)
            (key2 : #f)
            (key3 : [1 2 3 4])
            (key4 : (object (o1 : "bebe")) )
            ("fafa" : "fifa")
            (none : #nil)
        )
      |] <&> toJSON . head

      let s1 = decode @Value [q|
        {
            "key1": 22,
            "key2": false,
            "key3": [1, 2, 3, 4],
            "key4": {
                "o1": "bebe"
            },
            "fafa" : "fifa",
            "none" : null
        }

      |]

      print s
      print s1
      Just s `shouldBe` s1


    it "serializes object to syntax"  $ do
      let some = SomeData 1 "some-data" [1, 2, 3, 4, 5, 10]

      let someSyn = case fromJSON @(Syntax ()) (toJSON some) of
                      Success syn -> Just syn
                      _           -> Nothing

      print $ pretty someSyn

      let json = fromJust $ someSyn <&> toJSON

      let someObject = fromJSON @SomeData json

      print someObject
      someObject `shouldBe` Success some


