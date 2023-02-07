module Data.Config.Suckless.Parse.Megaparsec
  where

import Data.Config.Suckless.Syntax

import Control.Applicative()

import Data.Char (showLitChar)
import Data.Text qualified as Text

import Text.Megaparsec
import Text.Megaparsec.Char (space1,char,letterChar,digitChar)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char.Lexer ( signed )
import Data.String(IsString(..))

import Debug.Trace

type Parser r = Parsec () [Char] r

type ParseFail = ParseErrorBundle [Char] ()

sc :: Parser ()
sc = do
 L.space space1 lineComment empty
 where
   lineComment = L.skipLineComment ";"

dquot :: Parser Char
dquot = char '"'

-- FIXME: position!
stringLit :: Monoid (Context c) => Parser (Syntax c)
stringLit  = L.lexeme sc $
  Literal mempty . LitStr <$> str
  where
    str = do
      s <- dquot >> manyTill L.charLiteral dquot
      pure $ Text.pack (mconcat [ showLitChar c "" | c <- s ])

{-

-- FIXME: position!
intLit :: Parser Syntax
intLit = L.lexeme sc $
  Literal . LitInt <$> choice [hex, oct, bin, dec, dec']
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
symbol :: Parser Syntax
symbol = L.lexeme sc $ do
  co <- Context . Just <$> getOffset
  h <- letterChar <|> symbolChar
  t <- many (letterChar <|> digitChar <|> symbolChar)
  case Symbol (fromString (h:t)) of
    SymbolVal "#t" -> pure $ Literal (mkLit True)
    SymbolVal "#f" -> pure $ Literal (mkLit False)
    SymbolVal other       -> pure (Symbol_ co other)

skipChar :: Char -> Parser ()
skipChar c = void $ char c

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

list :: Parser Syntax
list = L.lexeme sc $ do
  co <- Context . Just <$> getOffset
  -- traceShowM ("List", co)
  List_ co <$> choice [ someEnclosedBy oParen syntax cParen
                      , someEnclosedBy oBrace syntax cBrace
                      , someEnclosedBy oBracket syntax cBracket
                      ]

syntax :: Parser Syntax
syntax = choice [ symbol
                , stringLit
                , intLit
                , list
                ]

-- top :: Parser (AST (Core S))
-- top = sc >> many syntaxWithPos >>= toAST
--   where
--     -- FIXME: push real position here
--     syntaxWithPos = do
--       pos <- getOffset
--       node <- (pos,) <$> syntax
--       pure node

--     toAST xs = go xs

--     go :: [(Int, Syntax)] -> Parser (AST (Core S))
--     go ( (p, syn) : rest ) = do
--       let c = Context (Just p)
--       mkExpr c . Seq syn <$> go rest

--     go [] = pure $ mkLeaf0 ()

-- merely :: Parser a -> Parser a
-- merely f = do
--   sc
--   r <- f
--   sc
--   eof
--   pure r

-- parseSyntax :: String -> Either ParseFail Syntax
-- parseSyntax = parse (merely syntax) "input"

-- parseTop :: String -> Either ParseFail (AST (Core S))
-- parseTop = parse top "input"
-}
