{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Config.Suckless.AesonSpec (spec) where

import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Function
import Data.Scientific
-- import Data.Set (Set)
-- import qualified Data.Set as Set
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc,q)
import Data.Aeson
import Test.Hspec


readConfig :: String -> IO [Syntax C]
readConfig s = do
  pure $ parseTop s & either mempty id
  -- print $ pretty f
  -- pure f

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





