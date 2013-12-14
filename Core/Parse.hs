{-# LANGUAGE NoMonomorphismRestriction #-}
module Parse where
    
import Types
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language(emptyDef)


def = emptyDef{ commentLine = "--"
              , nestedComments = False
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "+*-/<>=!ano"
              , opLetter = oneOf "+*-/<>"
              , reservedOpNames = ["+", "*", "-", "/", ">=", "<=", ">", "<", "==", "!=", "and", "or", "not", "\\", "->", "."]
              , reservedNames = ["true", "false",
                                 "if", "then", "else"]
              }

TokenParser{ parens = m_parens
         , brackets = m_brackets
         , braces = m_braces
         , commaSep = m_commaSep
         , commaSep1 = m_commaSep1
         , identifier = m_identifier
         , reservedOp = m_reservedOp
         , reserved = m_reserved
         , semiSep1 = m_semiSep1
         , colon = m_colon
         , dot = m_dot
         , whiteSpace = m_whiteSpace
         , integer = m_integer
         , float = m_float } = makeTokenParser def

simple :: Parser Char
simple  = letter

sign = (char '-' >> return negate)
    <|> (char '+' >> return id)
    <|> return id

exprparser :: Parser Expr
exprparser = buildExpressionParser table fielded_term <?> "expression"
table = [
          [Prefix (m_reservedOp "-"   >> return NegateE),
           Prefix (m_reservedOp "+"   >> return id)]
        , [Infix  (m_reservedOp "/"   >> return (ArithmE OpDivide)) AssocLeft
        ,  Infix  (m_reservedOp "*"   >> return (ArithmE OpMult)) AssocLeft]
        , [Infix  (m_reservedOp "-"   >> return (ArithmE OpMinus)) AssocLeft
        ,  Infix  (m_reservedOp "+"   >> return (ArithmE OpPlus)) AssocLeft]
        , [Infix  (m_reservedOp ">="  >> return (CompareE OpGeq)) AssocNone
        ,  Infix  (m_reservedOp "<="  >> return (CompareE OpLeq)) AssocNone
        ,  Infix  (m_reservedOp ">"   >> return (CompareE OpGreater)) AssocNone
        ,  Infix  (m_reservedOp "<"   >> return (CompareE OpLess)) AssocNone
        ,  Infix  (m_reservedOp "=="  >> return (CompareE OpEq)) AssocNone
        ,  Infix  (m_reservedOp "!="  >> return (CompareE OpNeq)) AssocNone]
        , [Prefix (m_reservedOp "not" >> return NotE)]
        , [Infix  (m_reservedOp "and" >> return (LogicalE OpAnd)) AssocLeft
        ,  Infix  (m_reservedOp "or"  >> return (LogicalE OpOr)) AssocLeft]
        ]

fielded_term = do {
    obj <- term;
    fielded <- many m_field;
    return $ makeFieldAccess obj fielded;
}
term = (try $ m_parens (do {
        firstT <- exprparser;
        restTs <- many1 exprparser;
        return $ makeCall firstT restTs;
    }))
    <|> try (m_parens exprparser)
    <|> m_number
    <|> m_bool
    <|> m_ifthenelse
    <|> m_lambda
    <|> fmap makeVar m_identifier
    <|> m_vector
    <|> m_record
    <|> m_tuple

m_number = try (do {
        v <- m_float; return (makeDouble v)
    })
    <|> do {
        v <- m_integer; return (makeInt v)
    }

m_bool =  (m_reserved "true"  >> return (makeBool True ))
      <|> (m_reserved "false" >> return (makeBool False))

m_ifthenelse = do {
    m_reserved "if"; e_if <- exprparser;
    m_reserved "then"; e_then <- exprparser;
    m_reserved "else"; e_else <- exprparser;
    return (makeIf e_if e_then e_else)
}

m_lambda = do {
    m_reservedOp "\\";
    ids <- many1 m_identifier;
    m_reservedOp "->";
    body <- exprparser;
    return $ makeLambda ids body;
}

m_vector = m_brackets $ fmap makeVector (m_commaSep exprparser)
m_field = m_dot >> m_identifier
m_record = m_braces $ fmap makeRecord $ m_commaSep m_fieldterm
m_tuple = try $ m_parens $ fmap makeTuple $ m_commaSep1 exprparser
m_fieldterm = do {
    s <- m_identifier;
    m_colon;
    v <- exprparser;
    return (s, v);
}

parseExpr = parse exprparser ""
