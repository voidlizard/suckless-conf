{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Config.Suckless.KeyValueSpec (spec) where

import Control.Monad.IO.Class
import Data.Config.Suckless.KeyValue
import Data.Config.Suckless.Parse
import Data.Config.Suckless.Syntax
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec

data FirstKey

data SecondKey

data ThirdKey

instance MonadIO m => HasCfgKey FirstKey (Maybe String) m where
  key = "foo"

instance MonadIO m => HasCfgKey SecondKey (Set String) m where
  key = "bar"

instance MonadIO m => HasCfgKey ThirdKey (Maybe String) m where
  key = "baz"

instance (Monad m, MonadIO m) => HasConf m where
  getConf = liftIO readConfig

readConfig :: IO [Syntax C]
readConfig = do
  let configFilePath = "t/key-value-test-config"
  readFile configFilePath <&> parseTop <&> either mempty id

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