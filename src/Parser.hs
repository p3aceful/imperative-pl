module Parser (program, parse) where 

import Syntax
import System.IO
import Control.Monad

import Text.Parsec

import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = words "true false var if while fun ref return try catch reset shift spawn detach join do"
           , Token.reservedOpNames = words "+ - * / % == != < > <= >= && || ! ="
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses a natural number
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses one of the ops
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses a comma separated list
stringlit = Token.stringLiteral lexer 
top = parse program

program = do
  whiteSpace
  ss <- many statement
  eof
  return $ SBlock $ foldr SSeq SSkip ss

statement =
  empty <|>
  ifStmt <|>
  whileStmt <|>
  doWhileStmt <|>
  block <|>
  returnStmt <|>
  tryStmt <|>
  throwStmt <|>

  varDeclStmt <|>
  assignStmt <|>
  exprStmt

empty = semi >> return SSkip
ifStmt = do
  reserved "if"
  e <- parens expr
  s1 <- statement
  reserved "else"
  s2 <- statement
  return $ SIf e s1 s2
whileStmt = do
  reserved "while"
  e <- parens expr
  s <- statement
  return $ SWhile e s
doWhileStmt = do
    reserved "do"
    s <- statement
    reserved "while"
    e <- parens expr
    return $ SDoWhile e s

block = do
  ss <- braces (many statement)
  return $ SBlock $ foldr SSeq SSkip ss
assignStmt = do
  i <- try ( identifier >>= \j -> reservedOp "=" >> return j )
  e <- expr
  semi
  return $ SAssign i e
varDeclStmt = do
  reserved "var"
  i <- identifier
  reservedOp "="
  e <- expr
  semi
  return $ SVarDecl i e
returnStmt = do
  reserved "return"
  e <- expr
  return $ SReturn e
tryStmt = do
    reserved "try"
    s1 <- statement
    reserved "catch"
    evar <- parens identifier
    s2 <- statement
    return $ STry s1 evar s2
throwStmt = do
    reserved "throw"
    str <- stringLiteral
    semi
    return $ SThrow str
exprStmt = do
  e <- expr
  semi
  return $ SExpr e

expr = conjunction `chainl1` binOp "||"
conjunction = relation `chainl1` binOp "&&"
relation = do
  l <- summation
  (anyBinOp (words "== != < <= > >=") >>= \o -> summation >>= \r -> return $ o l r) <|> return l
summation = term `chainl1` anyBinOp (words "+ -")
term = factor `chainl1` anyBinOp (words "* / %")
factor = literal <|> fun <|> atomicOrCall <|> ref 
literal = intLiteral <|> boolLiteral "false" False <|> boolLiteral "true" True <|> stringLiteral

intLiteral = natural >>= \i -> return $ EVal (VInt (fromInteger i))
boolLiteral s v = reserved s >> (return $ EVal (VBool v))
stringLiteral = stringlit >>= \s -> return $ EVal (VString s)

ref = reserved "ref" >> factor >>= \e -> return $ ERef e
deref = reservedOp "*" >> atomic >>= \e -> return $ EDeref e

binOp s = reservedOp s >> (return $ (\a b -> ECall (EVar ("__b" ++ s)) [a, b] []))

anyBinOp ops = foldl1 (<|>) (map binOp ops)

fun = do
  reserved "fun"
  pars <- parens (commaSep identifier)
  body <- block
  return $ EFun pars body

variable = identifier >>= return . EVar
resetExpr = do
    reserved "reset"
    e <- parens expr
    return $ EReset e
shiftExpr = do 
    reserved "shift"
    f <- parens fun
    return $ EShift f
spawnExpr = do 
    reserved "spawn"
    f <- parens fun
    return (ESpawn f)
detachExpr = do
    reserved "detach"
    e <- parens expr
    return (EDetach e)
joinExpr = do
    reserved "join"
    e <- parens expr
    return (EJoin e)

atomic = joinExpr <|> detachExpr <|> spawnExpr <|> resetExpr <|> shiftExpr <|> variable <|> parens expr <|> deref

atomicOrCall = do 
  a <- atomic
  argss <- many (parens (commaSep expr))
  return $ foldl (\a arg -> ECall a arg []) a argss
