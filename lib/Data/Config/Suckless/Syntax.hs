{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Data.Config.Suckless.Syntax
  ( Syntax(..)
  , Context(..)
  , Id(..)
  , Literal(..)
  , HasContext(..)
  , pattern SymbolVal
  )
  where

import GHC.Generics
import Data.Text (Text)
import Data.Data
import Data.String
import Data.Kind

import Prettyprinter

pattern SymbolVal :: Id -> Syntax c
pattern SymbolVal v <- Symbol _ v

data family Context c ::  Type

class HasContext c a where
  setContext :: Context c -> a -> a
  getContext :: a -> Context c

newtype Id =
  Id Text
  deriving newtype (IsString,Pretty)
  deriving stock (Data,Generic,Show,Eq,Ord)

data Literal =
    LitStr   Text
  | LitInt   Integer
  | LitBool  Bool
  deriving stock (Eq,Ord,Data,Generic,Show)

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


