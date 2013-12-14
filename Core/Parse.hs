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
              , reservedOpNames = ["+", "*", "-", "/", ">=", "<=", ">", "<", "==", "!=", "and", "or", "not", "\\", "->", ".", "="]
              , reservedNames = ["true", "false",
                                 "if", "then", "else",
                                 "let", "in"]
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

exprparser :: Parser TExpr
exprparser = buildExpressionParser table fielded_term <?> "expression"
table = [
          [Prefix (m_reservedOp "-"   >> return makeNegate),
           Prefix (m_reservedOp "+"   >> return id)]
        , [Infix  (m_reservedOp "/"   >> return (makeArithmOp  OpDivide)) AssocLeft
        ,  Infix  (m_reservedOp "*"   >> return (makeArithmOp  OpMult)) AssocLeft]
        , [Infix  (m_reservedOp "-"   >> return (makeArithmOp  OpMinus)) AssocLeft
        ,  Infix  (m_reservedOp "+"   >> return (makeArithmOp  OpPlus)) AssocLeft]
        , [Infix  (m_reservedOp ">="  >> return (makeCompareOp OpGeq)) AssocNone
        ,  Infix  (m_reservedOp "<="  >> return (makeCompareOp OpLeq)) AssocNone
        ,  Infix  (m_reservedOp ">"   >> return (makeCompareOp OpGreater)) AssocNone
        ,  Infix  (m_reservedOp "<"   >> return (makeCompareOp OpLess)) AssocNone
        ,  Infix  (m_reservedOp "=="  >> return (makeCompareOp OpEq)) AssocNone
        ,  Infix  (m_reservedOp "!="  >> return (makeCompareOp OpNeq)) AssocNone]
        , [Prefix (m_reservedOp "not" >> return makeNot)]
        , [Infix  (m_reservedOp "and" >> return (makeLogicalOp OpAnd)) AssocLeft
        ,  Infix  (m_reservedOp "or"  >> return (makeLogicalOp OpOr)) AssocLeft]
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
    <|> m_let

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
m_let = do {
    m_reserved "let";
    bindings <- many1 let_pair;
    m_reserved "in";
    expr <- exprparser;
    return $ makeLet bindings expr;
}
let_pair = do {
    p <- patternparser;
    m_reservedOp "=";
    val <- exprparser;
    return (p, val);
}
patternparser = p_symbol <|> p_tuple <|> p_record <|> p_wildcard <?> "pattern"
p_symbol = fmap makeVarPat $ m_identifier
p_tuple = m_parens $ fmap makeTuplePat $ m_commaSep1 patternparser
p_record = m_braces $ fmap makeRecPat $ m_commaSep1 p_field
p_field = do {
    s <- m_identifier;
    m_reservedOp ":";
    p <- patternparser;
    return (s, p);
}
p_wildcard = m_reservedOp "_" >> return WildP

parseExpr = parse exprparser ""
