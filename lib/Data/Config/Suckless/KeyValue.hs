{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Data.Config.Suckless.KeyValue where

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Data.String (IsString(..))
import Data.Set qualified as Set
import Data.Set (Set)
import Prettyprinter
import Safe

type C = MegaParsec

class Monad m => HasCfgKey a b m where
  -- type family CfgValue a :: Type
  key :: Id

class (Monad m, HasCfgKey a b m) => HasCfgValue a b m where
  cfgValue :: m b

class Monad m => HasConf m where
  getConf :: m [Syntax C]

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Maybe b) m) => HasCfgValue a (Maybe b) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Set b) m) => HasCfgValue a (Set b) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b) @m
                ]