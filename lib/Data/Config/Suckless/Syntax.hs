{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Config.Suckless.Syntax
  ( Syntax(..)
  , Id(..)
  , Literal(..)
  , Context
  , HasContext(..)
  , IsContext(..)
  , IsLiteral(..)
  , pattern SymbolVal
  , pattern ListVal
  , pattern LitIntVal
  , pattern LitStrVal
  , pattern LitBoolVal
  )
  where

import Data.Data
import Data.Kind
import Data.String
import Data.Text (Text)
import GHC.Generics

import Prettyprinter

pattern SymbolVal :: Id -> Syntax c
pattern SymbolVal v <- Symbol _ v

-- pattern LitVal :: forall {c}. Id -> Li
pattern LitIntVal :: forall {c}. Integer -> Syntax c
pattern LitIntVal v <- Literal _ (LitInt v)

pattern LitStrVal :: forall {c}. Text -> Syntax c
pattern LitStrVal v <- Literal _ (LitStr v)

pattern LitBoolVal :: forall {c}. Bool -> Syntax c
pattern LitBoolVal v <- Literal _ (LitBool v)

pattern ListVal :: forall {c}. [Syntax c] -> Syntax c
pattern ListVal v <- List _ v


data family Context c :: Type

class IsContext c where
  noContext :: Context c

class HasContext c a where
  setContext :: Context c -> a -> a
  getContext :: a -> Context c

class IsLiteral a where
  mkLit :: a -> Literal

newtype Id =
  Id Text
  deriving newtype (IsString,Pretty)
  deriving stock (Data,Generic,Show,Eq,Ord)

data Literal =
    LitStr   Text
  | LitInt   Integer
  | LitBool  Bool
  deriving stock (Eq,Ord,Data,Generic,Show)

instance IsLiteral Text where
  mkLit = LitStr

instance IsLiteral Bool where
  mkLit = LitBool

instance IsLiteral Integer where
  mkLit = LitInt

data Syntax c
  = List    (Context c) [Syntax c]
  | Symbol  (Context c) Id
  | Literal (Context c) Literal
  deriving stock (Generic)



instance HasContext c (Syntax c) where
  setContext c1 = \case
    List _ v    -> List c1 v
    Symbol _ v  -> Symbol c1 v
    Literal _ v -> Literal c1 v

  getContext = \case
    List x _ -> x
    Symbol x _ -> x
    Literal x _ -> x

instance Pretty (Syntax c) where
  pretty (Literal _ ast) = pretty ast
  pretty (Symbol _ s)    = pretty s
  pretty (List _ (x:xs)) = parens $ align $ sep ( fmap pretty (x:xs) )
  pretty (List _ [])     = parens mempty

instance Pretty Literal where
  pretty = \case
    LitStr s  -> dquotes (pretty s)
    LitInt i  -> pretty i

    LitBool b | b          -> "#t"
              | otherwise  -> "#f"


deriving instance ( Data c
                  , Data (Context c)
                  , Typeable c
                  ) => Data (Syntax c)


