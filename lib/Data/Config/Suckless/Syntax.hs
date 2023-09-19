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
  , pattern LitScientificVal
  )
  where

import Data.Data
import Data.Kind
import Data.String
import Data.Text (Text)
import Data.Scientific
import GHC.Generics
import Data.Maybe
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as Aeson
import Data.Vector qualified as V
import Data.Traversable (forM)

import Prettyprinter

pattern SymbolVal :: Id -> Syntax c
pattern SymbolVal v <- Symbol _ v

-- pattern LitVal :: forall {c}. Id -> Li
pattern LitIntVal :: forall {c}. Integer -> Syntax c
pattern LitIntVal v <- Literal _ (LitInt v)

pattern LitScientificVal :: forall {c}. Scientific -> Syntax c
pattern LitScientificVal v <- Literal _ (LitScientific v)

pattern LitStrVal :: forall {c}. Text -> Syntax c
pattern LitStrVal v <- Literal _ (LitStr v)

pattern LitBoolVal :: forall {c}. Bool -> Syntax c
pattern LitBoolVal v <- Literal _ (LitBool v)

pattern ListVal :: forall {c}. [Syntax c] -> Syntax c
pattern ListVal v <- List _ v


data family Context c :: Type

class IsContext c where
  noContext :: Context c

data instance Context () = EmptyContext

instance IsContext () where
  noContext = EmptyContext

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
  | LitScientific Scientific
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
    LitScientific v  -> viaShow v

    LitBool b | b          -> "#t"
              | otherwise  -> "#f"


deriving instance ( Data c
                  , Data (Context c)
                  , Typeable c
                  ) => Data (Syntax c)



instance ToJSON Literal where
    toJSON (LitStr s)       = String s
    toJSON (LitInt i)       = Number (fromInteger i)
    toJSON (LitScientific s) = Number s
    toJSON (LitBool b)      = Bool b

instance ToJSON (Syntax c) where
    toJSON (Symbol _ (Id "#nil")) = Null
    toJSON (Symbol _ (Id s)) = String s
    toJSON (Literal _ l) = toJSON l
    toJSON (List _ items) =
        case items of
            (Symbol _ "object" : rest) ->
                object $ mapMaybe pairToKeyValue rest
            _ -> Array . V.fromList $ fmap toJSON items

      where
        pairToKeyValue :: Syntax c -> Maybe (Key, Value)
        pairToKeyValue (List _ [SymbolVal (Id k), SymbolVal ":", v]) = Just (fromText k .= toJSON v)
        pairToKeyValue (List _ [LitStrVal k, SymbolVal ":", v]) = Just (fromText k .= toJSON v)
        pairToKeyValue _ = Nothing



instance FromJSON (Syntax ()) where
    parseJSON (String t) = pure $ Literal noContext (LitStr t)
    parseJSON (Number n)
        | isInteger n = pure $ Literal noContext (LitInt (floor n))
        | otherwise   = pure $ Literal noContext (LitScientific n)
    parseJSON (Bool b)  = pure $ Literal noContext (LitBool b)
    parseJSON (Array a) = List noContext <$> mapM parseJSON (V.toList a)
    parseJSON (Object o) = do
        pairs <- forM (Aeson.toList o) $ \(key, value) -> do
            valueSyntax <- parseJSON value
            pure $ List noContext [ Symbol noContext (Id (toText key))
                                  , Symbol noContext ":"
                                  , valueSyntax
                                  ]
        pure $ List noContext (Symbol noContext (Id "object") : pairs)
    parseJSON _ = fail "Cannot parse JSON to Syntax"


