{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : RCore.Parser
-}
module RCore.Parser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Control.Applicative ((<$), (<$>), (<*>), (<*), (*>))
import RCore.Val
import RCore.Ast

rcoreDef :: P.LanguageDef ()
rcoreDef = P.LanguageDef 
            { P.commentStart    = "(*"
            , P.commentEnd      = "*)"
            , P.commentLine     = ""
            , P.nestedComments  = False
            , P.identStart      = letter <|> char '_'
            , P.identLetter     = alphaNum <|> char '\''
            , P.opStart         = oneOf "=^<."
            , P.opLetter        = oneOf "?="
            , P.reservedOpNames = ["=?", "^=", "<=", "."]
            , P.reservedNames   = ["read", "write", "nil", "cons", "hd", "tl", "from", "loop", "until"]
            , P.caseSensitive   = True
            }

P.TokenParser { P.parens = parens :: forall a. Parser a -> Parser a
              , P.braces = braces :: forall a. Parser a -> Parser a
              , P.identifier = identifier :: Parser String
              , P.reservedOp = reservedOp :: String -> Parser ()
              , P.reserved = reserved :: String -> Parser ()
              , P.whiteSpace = whiteSpace :: Parser ()
              , P.symbol = symbol :: String -> Parser String
              , P.semi = semi :: Parser String
              -- , P.semiSep = semiSep
              , P.commaSep = commaSep :: forall a. Parser a -> Parser [a]
              , P.semiSep1 = semiSep1 :: forall a. Parser a -> Parser [a]
              , P.dot = dot } = P.makeTokenParser rcoreDef


type Parser = Parsec String ()

pProgram :: Parser Program
pProgram = do x1 <- reserved "read" *> varIdentifier <* semi
              cs <- pCmd `endBy` semi
              x2 <- reserved "write" *> varIdentifier
              return $ PDefs (Ident x1) cs (Ident x2)
           <?> "program"

pCmd :: Parser Cmd
pCmd = pAss <|> pLoop -- e.g. X ^= E and X <= Y overlap
       <?> "command"

pAss :: Parser Cmd
pAss = SAss . Ident <$> varIdentifier <* reservedOp "^=" <*> pExp
       <?> "assignment"

varIdentifier :: Parser String
varIdentifier = (\x y -> x : y : []) <$> char 'X' <*> digit <* whiteSpace

pIdent :: Parser Ident
pIdent = Ident <$> varIdentifier

pVar :: Parser Exp
pVar = AVar <$> pIdent
   <?> "variable"

pVal :: Parser Val
pVal = Nil  <$  reserved "nil"
   <|> parens (Cons <$> pVal <* dot <*> pVal)
   <|> Atom <$> (symbol "'" *> many alphaNum <* spaces)
   <?> "value"

pLoop :: Parser Cmd
pLoop = do x1 <- reserved "from"  *> varIdentifier
           cs <- reserved "loop"  *> semiSep1 pCmd <|> return []
           x2 <- reserved "until" *> varIdentifier
           return (SLoop (Ident x1) cs (Ident x2))
        <?> "loop"

pExp :: Parser Exp
pExp = AVal Nil <$ reserved "nil"
   <|> pVar
   <|> (reserved "cons" >> ACons <$> pIdent <*> pIdent)
   <|> (reserved "hd"   >> AHd   <$> pIdent)
   <|> (reserved "tl"   >> ATl   <$> pIdent)
   <|> (reservedOp "=?" >> AEq   <$> pIdent <*> pIdent)
   <?> "expression"

pEnv :: Parser Env
pEnv = braces (Env <$> commaSep ((,) <$> (Ident <$> varIdentifier <* reservedOp ":=") <*> pVal))

parseProgram :: String -> Either ParseError Program
parseProgram = parse (pProgram <* eof) "(unknown)"

parseExp :: String -> Either ParseError Exp
parseExp = parse (pExp <* eof) "(unknown)"

parseCmd :: String -> Either ParseError Cmd
parseCmd = parse (pCmd <* eof) "(unknown)"

parseVal :: String -> Either ParseError Val
parseVal = parse (pVal <* eof) "(unknown)"

parseIdent :: String -> Either ParseError String
parseIdent = parse (identifier <* eof) "(unknown)"

parseEnv :: String -> Either ParseError Env
parseEnv = parse (pEnv <* eof) "(unknown)"
