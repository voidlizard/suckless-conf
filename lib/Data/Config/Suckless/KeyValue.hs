{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Data.Config.Suckless.KeyValue where

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Parse

import Data.String (IsString(..))
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Maybe
import Data.Scientific
import Data.Aeson
import Prettyprinter
import Control.Monad.Reader
import Safe

type C = MegaParsec

class Monad m => HasCfgKey a b m where
  -- type family CfgValue a :: Type
  key :: Id

class Monad m => HasCfgValue a b m where
  cfgValue :: m b

class Monad m => HasConf m where
  getConf :: m [Syntax C]

pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns


instance {-# OVERLAPPABLE #-} Monad m => HasConf (ReaderT [Syntax C] m) where
  getConf = ask

instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Maybe Integer) m) => HasCfgValue a (Maybe Integer) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ e
                | ListVal (Key s [LitIntVal e]) <- syn, s == key @a @(Maybe Integer) @m
                ]


instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Maybe Int) m) => HasCfgValue a (Maybe Int) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ fromIntegral e
                | ListVal (Key s [LitIntVal e]) <- syn, s == key @a @(Maybe Int) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Maybe Scientific) m) => HasCfgValue a (Maybe Scientific) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ e
                | ListVal (Key s [LitScientificVal e]) <- syn, s == key @a @(Maybe Scientific) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Maybe Bool) m) => HasCfgValue a (Maybe Bool) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ e
                | ListVal (Key s [LitBoolVal e]) <- syn, s == key @a @(Maybe Bool) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Maybe Value) m) => HasCfgValue a (Maybe Value) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ toJSON v
                | ListVal (Key s [v@ListVal{}]) <- syn, s == key @a @(Maybe Value) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, IsString b, HasCfgKey a (Maybe b) m) => HasCfgValue a (Maybe b) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b) @m
                ]


instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Set Integer) m) => HasCfgValue a (Set Integer) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ e
                | ListVal (Key s [LitIntVal e]) <- syn, s == key @a @(Set Integer) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Set Scientific) m) => HasCfgValue a (Set Scientific) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ e
                | ListVal (Key s [LitScientificVal e]) <- syn, s == key @a @(Set Scientific) @m
                ]


instance {-# OVERLAPPABLE #-} (HasConf m, HasCfgKey a (Set Value) m) => HasCfgValue a (Set Value) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ toJSON v
                | ListVal (Key s [v@ListVal{}]) <- syn, s == key @a @(Set Value) @m
                ]


instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Set b) m) => HasCfgValue a (Set b) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal (Key s [LitStrVal e]) <- syn, s == key @a @(Set b) @m
                ]


