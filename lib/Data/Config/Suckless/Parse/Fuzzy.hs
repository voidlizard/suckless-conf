module Data.Config.Suckless.Parse.Fuzzy
  ( parseTop
  , parseSyntax
  -- , C
  ) where

import Data.Config.Suckless.Syntax
import Data.Text.Fuzzy.SExp qualified as P
import Data.Text.Fuzzy.SExp (C0(..),SExpParseError,ForMicroSexp(..))

import Data.Functor
import Data.Text
import Control.Monad.Except
import Control.Monad.Identity
import Data.Coerce


parseTop :: Text -> Either SExpParseError [Syntax C]
parseTop what = runIdentity (runExceptT (P.parseTop  what)) <&> fmap toSyntax

parseSyntax :: Text -> Either SExpParseError (Syntax C)
parseSyntax txt = runIdentity (runExceptT (P.parseSexp txt)) <&> toSyntax


toSyntax :: P.MicroSexp C0 -> Syntax C
toSyntax = \case
 P.List_    co  a -> List (toContext co) (fmap toSyntax a)
 P.Symbol_  co  a -> Symbol (toContext co) (Id a)
 P.String_  co  a -> Literal (toContext co) (LitStr a)
 P.Boolean_ co  a -> Literal (toContext co) (LitBool a)
 P.Number_  co  v -> case v of
  P.NumInteger n  -> Literal (toContext co) (LitInt n)
  P.NumDouble n   -> Literal (toContext co) (LitScientific (realToFrac n))

toContext :: C0 -> Context C
toContext (C0 what) = SimpleContext (fromIntegral <$> what)



