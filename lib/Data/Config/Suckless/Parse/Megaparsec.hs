{-# Language ConstraintKinds #-}
{-# Language UndecidableInstances #-}
module Data.Config.Suckless.Parse.Megaparsec
  ( parseSyntax
  , parseTop
  , MegaParsec
  , MegaContext
  )
  where

import Data.Config.Suckless.Syntax

import Control.Applicative()

import Data.Text qualified as Text

import Control.Monad
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1,space1,char,letterChar,digitChar,eol,string)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char.Lexer ( signed )
import Data.String(IsString(..))
import GHC.Generics
import Data.Data
import Safe
import Data.Scientific

data MegaParsec =
  MegaParsec
  deriving (Data,Typeable,Generic)

newtype instance Context MegaParsec =
  MegaContext (Maybe Int)
  deriving (Eq,Ord,Generic)

instance IsContext MegaParsec where
  noContext = MegaContext Nothing

type MegaContext = Context MegaParsec

type MegaConstraints c = ( c ~ MegaParsec, IsContext c )

type Parser r = Parsec () [Char] r

type ParseFail = ParseErrorBundle [Char] ()

deriving instance Eq (Context MegaParsec) => Eq (Syntax MegaParsec)
deriving instance Ord (Context MegaParsec) => Ord (Syntax MegaParsec)

sc :: Parser ()
sc = do
 L.space space1 lineComment empty
 where
   lineComment = L.skipLineComment ";"

scTop :: Parser ()
scTop = do
 L.space hspace1 lineComment empty
 where
   lineComment = L.skipLineComment ";"

dquot :: Parser Char
dquot = char '"'

-- FIXME: position!
stringLit :: forall c . MegaConstraints c
          => Parser () -> Parser (Syntax c)

stringLit sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset
  Literal co . LitStr <$> str
  where
    str = do
      s <- dquot >> manyTill L.charLiteral dquot
      pure $ Text.pack s -- (mconcat [ showLitChar c "" | c <- s ])

numLit :: forall c . MegaConstraints c
       => Parser () -> Parser (Syntax c)

numLit sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset

  s <- try (char '-' >> pure True) <|> pure False

  base <- choice [ string "0x" >> pure 16
                 , string "0o" >> pure 8
                 , string "0b" >> pure 2
                 , pure (10 :: Int)
                 ]

  val <- case base of
          16 -> LitInt . sign s <$> L.hexadecimal
          8  -> LitInt . sign s <$> L.octal
          2  -> LitInt . sign s <$> L.binary
          10 -> do
                  ns <- many (digitChar <|> oneOf ['.', 'e', '-'])
                  let v =     (LitInt . sign s <$> readMay @Integer ns)
                          <|> (LitScientific . sign s <$> readMay @Scientific ns)
                  case v of
                    Just x  -> pure x
                    Nothing -> fail "not a numeric literal"

          _ -> fail "not a numeric literal"

  pure $ Literal co val

  where
    sign :: forall a . Num a => Bool -> a -> a
    sign x = if x then negate else id

symbolChars :: [Char]
symbolChars = "-!$%&|*+/:<=>?@^_~#.'"

symbolChar :: Parser Char
symbolChar = oneOf symbolChars

symbolCharNoMinus :: Parser Char
symbolCharNoMinus = oneOf symbolChars'
  where
    symbolChars' = dropWhile (`elem` "-") symbolChars

-- FIXME: position!
symbol :: forall c . MegaConstraints c
       => Parser () -> Parser (Syntax c)
symbol sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset
  h <- letterChar <|> symbolCharNoMinus
  -- FIXME: dont-start-symbol-with-minus
  t <- many (letterChar <|> digitChar <|> symbolChar)
  case h:t of
    "#t"  -> pure $ Literal co (mkLit True)
    "#f"  -> pure $ Literal co (mkLit False)
    other -> pure $ Symbol co (fromString other)


skipChar :: Char -> Parser ()
skipChar c = void (char c)

oParen :: Parser ()
oParen = skipChar '('

cParen :: Parser ()
cParen = skipChar ')'

oBrace :: Parser ()
oBrace = skipChar '{'

cBrace :: Parser ()
cBrace = skipChar '}'

oBracket :: Parser ()
oBracket= skipChar '['

cBracket :: Parser ()
cBracket = skipChar ']'

someEnclosedBy :: Parser ()
               -> Parser a
               -> Parser ()
               -> Parser [a]

someEnclosedBy o i c = do
  between o c (many (sc >> i)) -- <|> parseError (error (show ("WTF!", ctx))) -- (ParseError (ListParseError ctx))

list :: forall c . MegaConstraints c
     => Parser() -> Parser (Syntax c)

list sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset
  List co <$> choice [ someEnclosedBy oParen   (syntax sp) cParen
                     , someEnclosedBy oBrace   (syntax sp) cBrace
                     , someEnclosedBy oBracket (syntax sp) cBracket
                     ]

syntax :: forall c . MegaConstraints c
      => Parser () -> Parser (Syntax c)

syntax sp = choice [ symbol sp
                   , numLit sp
                   , stringLit sp
                   , list sp
                   ]

merely :: Parser a -> Parser a
merely f = do
  sc
  r <- f
  sc
  eof
  pure r

parseSyntax :: forall c . MegaConstraints c
            => String -> Either ParseFail (Syntax c)

parseSyntax = parse (merely (syntax sc)) "input"

top :: forall c . MegaConstraints c => Parser [Syntax c]
top = do
  sc
  many $ do
    t <- topStmt
    sc
    pure t

topTerm :: forall c . MegaConstraints c => Parser (Syntax c)
topTerm = do
    co <- MegaContext . Just <$> getOffset
    s0 <- symbol scTop
    ss <- many (syntax scTop)

    void eol <|> eof
    pure $ List co (s0:ss)

topStmt :: forall c . MegaConstraints c => Parser (Syntax c)
topStmt = topTerm <|> syntax sc

parseTop :: forall c . MegaConstraints c
         => String -> Either ParseFail [Syntax c]

parseTop = parse top "input"

deriving instance Data (Context MegaParsec)



