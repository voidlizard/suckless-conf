{-# Language ConstraintKinds #-}
module Data.Config.Suckless.Parse.Megaparsec
  ( parseSyntax
  , parseTop
  , MegaParsec
  , MegaContext
  )
  where

import Data.Config.Suckless.Syntax

import Control.Applicative()

import Data.Char (showLitChar)
import Data.Text qualified as Text

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1,space1,char,letterChar,digitChar,eol)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char.Lexer ( signed )
import Data.String(IsString(..))
import GHC.Generics
import Data.Data

data MegaParsec =
  MegaParsec
  deriving (Data,Typeable,Generic)

newtype instance Context MegaParsec = MegaContext (Maybe Int)

instance IsContext MegaParsec where
  noContext = MegaContext Nothing

type MegaContext = Context MegaParsec

type MegaConstraints c = ( c ~ MegaParsec, IsContext c )

type Parser r = Parsec () [Char] r

type ParseFail = ParseErrorBundle [Char] ()

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
      pure $ Text.pack (mconcat [ showLitChar c "" | c <- s ])

-- FIXME: position!
intLit :: forall c . MegaConstraints c
       => Parser () -> Parser (Syntax c)

intLit sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset
  Literal co . LitInt <$> choice [hex, oct, bin, dec, dec']
  where
    hex = L.symbol sc "0x" >> L.hexadecimal
    oct = L.symbol sc "0o" >> L.octal
    bin = L.symbol sc "0b" >> L.binary
    dec = L.decimal
    dec'= signed sc L.decimal

symbolChars :: [Char]
symbolChars = "!$%&|*+-/:<=>?@^_~#.'"

symbolChar :: Parser Char
symbolChar = oneOf symbolChars

-- FIXME: position!
symbol :: forall c . MegaConstraints c
       => Parser () -> Parser (Syntax c)
symbol sp = L.lexeme sp $ do
  co <- MegaContext . Just <$> getOffset
  h <- letterChar <|> symbolChar
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
                   , stringLit sp
                   , intLit sp
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
top = sc >> many (topStmt <|> syntax scTop)

topStmt :: forall c . MegaConstraints c => Parser (Syntax c)
topStmt = do
  scTop

  co <- MegaContext . Just <$> getOffset

  s0 <- symbol scTop
  ss <- many (syntax scTop)

  void eol <|> eof
  pure $ List co (s0:ss)

parseTop :: forall c . MegaConstraints c
         => String -> Either ParseFail [Syntax c]

parseTop = parse top "input"

