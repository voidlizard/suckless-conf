{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Config.Suckless.KeyValueSpec (spec) where

import Control.Monad.IO.Class
import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Scientific
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import Data.Aeson
import Text.InterpolatedString.Perl6 (qc,q)
import Test.Hspec

data FirstKey

data SecondKey

data ThirdKey

data Int1
data Int2
data Int3
data Int4
data Int5
data Int6

data Sci1
data Sci2
data Sci3
data Sci4
data Sci5

data O1
data O2

instance MonadIO m => HasCfgKey FirstKey (Maybe String) m where
  key = "foo"

instance MonadIO m => HasCfgKey SecondKey (Set String) m where
  key = "bar"

instance MonadIO m => HasCfgKey ThirdKey (Maybe String) m where
  key = "baz"

instance MonadIO m => HasCfgKey Int1 b m where
  key = "int1"

instance MonadIO m => HasCfgKey Int2 b m where
  key = "int2"

instance MonadIO m => HasCfgKey Int3 b m where
  key = "int3"

instance MonadIO m => HasCfgKey Int4 b m where
  key = "int4"

instance MonadIO m => HasCfgKey Int5 b m where
  key = "int5"

instance MonadIO m => HasCfgKey Int6 b m where
  key = "int6"

instance MonadIO m => HasCfgKey Sci1 b m where
  key = "sci1"

instance MonadIO m => HasCfgKey Sci2 b m where
  key = "sci2"

instance MonadIO m => HasCfgKey Sci3 b m where
  key = "sci3"

instance MonadIO m => HasCfgKey Sci4 b m where
  key = "sci4"

instance MonadIO m => HasCfgKey Sci5 b m where
  key = "sci5"

instance MonadIO m => HasCfgKey O1 b m where
  key = "some-object"

instance MonadIO m => HasCfgKey O2 b m where
  key = "another-object"

instance (Monad m, MonadIO m) => HasConf m where
  getConf = liftIO readConfig

readConfig :: IO [Syntax C]
readConfig = do
  let configFilePath = "t/key-value-test-config"
  f <- readFile configFilePath <&> parseTop <&> either mempty id
  print $ pretty f
  pure f

spec :: Spec
spec = do
  describe "config parsing" $ do

    it "reads string" $ do
      firstValue <- cfgValue @FirstKey @(Maybe String)
      firstValue `shouldBe` Just "a"

    it "reads a set of strings" $ do
      secondValue <- cfgValue @SecondKey @(Set String)
      secondValue `shouldBe` Set.fromList ["a", "b"]

    it "reads nothing" $ do
      thridValue <- cfgValue @ThirdKey @(Maybe String)
      thridValue `shouldBe` Nothing

    it "reads ints" $ do
      x1 <- cfgValue @Int1 @(Maybe Integer)
      x1 `shouldBe` Just 122

      x2 <- cfgValue @Int2
      x2 `shouldBe` Just (0 :: Integer)

      x3 <- cfgValue @Int3
      x3 `shouldBe` Just (-22 :: Integer)

      x4 <- cfgValue @Int4 @(Maybe Integer)
      x4 `shouldBe` Just 0xFAFA

      x5 <- cfgValue @Int5 @(Maybe Integer)
      x5 `shouldBe` Just 255

      x6 <- cfgValue @Int6 @(Maybe Integer)
      x6 `shouldBe` Just (-0xFAFA)

    it "reads scientifics" $ do
      x1 <- cfgValue @Sci1 @(Maybe Scientific)
      x1 `shouldBe` Just 1e9

      x2 <- cfgValue @Sci2 @(Maybe Scientific)
      x2 `shouldBe` Just 0.003

      -- x3 <- cfgValue @Sci3 @(Maybe Scientific)
      -- x3 `shouldBe` Just (-0.001)

      x4 <- cfgValue @Sci4 @(Maybe Scientific)
      x4 `shouldBe` Just (-2e11)

      x5 <- cfgValue @Sci5 @(Maybe Scientific)
      x5 `shouldBe` Just (-2e-3)

    it "reads objects" $ do
      o1 <- cfgValue @O1 @(Maybe Value)
      let wtf1 = [q|{ "key" : 42 }|]
      o1 `shouldBe` decode wtf1
      let wtf2 = [q|
        {   "key1" : 42
          , "key2" : false
          , "key3" : [ 1, 2, 3, 4]
        }
      |]
      o2 <- cfgValue @O2 @(Maybe Value)
      o2 `shouldBe` decode wtf2


