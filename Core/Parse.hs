{-# LANGUAGE NoMonomorphismRestriction #-}
module Parse where
    
import Syntax
import MaybeE(ok, err)
import Control.Applicative((<*), (<*>))
import Text.Parsec
import Text.Parsec.Error
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

-- exprparser :: Parser MExpr
exprparser = buildExpressionParser table fielded_term <?> "expression"
table = [
          [Prefix (m_reservedOp "-"   >> sourcefy UndefT ( return makeNegate)),
           Prefix (m_reservedOp "+"   >> return id)]
        , [Infix  (m_reservedOp "/"   >> sourcefy UndefT (return(makeArithmOp  OpDivide))) AssocLeft
        ,  Infix  (m_reservedOp "*"   >> sourcefy UndefT (return(makeArithmOp  OpMult))) AssocLeft]
        , [Infix  (m_reservedOp "-"   >> sourcefy UndefT (return(makeArithmOp  OpMinus))) AssocLeft
        ,  Infix  (m_reservedOp "+"   >> sourcefy UndefT (return(makeArithmOp  OpPlus))) AssocLeft]
        , [Infix  (m_reservedOp ">="  >> sourcefy UndefT (return(makeCompareOp OpGeq))) AssocNone
        ,  Infix  (m_reservedOp "<="  >> sourcefy UndefT (return(makeCompareOp OpLeq))) AssocNone
        ,  Infix  (m_reservedOp ">"   >> sourcefy UndefT (return(makeCompareOp OpGreater))) AssocNone
        ,  Infix  (m_reservedOp "<"   >> sourcefy UndefT (return(makeCompareOp OpLess))) AssocNone
        ,  Infix  (m_reservedOp "=="  >> sourcefy UndefT (return(makeCompareOp OpEq))) AssocNone
        ,  Infix  (m_reservedOp "!="  >> sourcefy UndefT (return(makeCompareOp OpNeq))) AssocNone]
        , [Prefix (m_reservedOp "not" >> sourcefy UndefT (return makeNot))]
        , [Infix  (m_reservedOp "and" >> sourcefy UndefT (return(makeLogicalOp OpAnd))) AssocLeft
        ,  Infix  (m_reservedOp "or"  >> sourcefy UndefT (return(makeLogicalOp OpOr))) AssocLeft]
        ]

fielded_term = do {
    obj <- term;
    fielded <- many m_field;
    return (makeFieldAccess obj fielded);
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
    <|> sourcefy UndefT (fmap makeVar m_identifier)
    <|> m_vector
    <|> m_record
    <|> m_tuple
    <|> m_let

m_number = try (do {
        p <- getPosition;
        v <- m_float;
        return $ makeDouble (Info (Just p, DblT)) v;
    })
    <|> do {
        p <- getPosition;
        v <- m_integer;
        return $ makeInt (Info (Just p, IntT)) v;
    }

m_bool =  (m_reserved "true"  >> (sourcefy BoolT $ return makeBool) <*> return True)
      <|> (m_reserved "false" >> (sourcefy BoolT $ return makeBool) <*> return False)

m_ifthenelse = do {
    p <- getPosition;
    m_reserved "if"; e_if <- exprparser;
    m_reserved "then"; e_then <- exprparser;
    m_reserved "else"; e_else <- exprparser;
    return $ makeIf (Info (Just p, UndefT)) e_if e_then e_else;
}

m_lambda = do {
    m_reservedOp "\\";
    ids <- many1 m_identifier;
    m_reservedOp "->";
    body <- exprparser;
    return $ makeLambda ids body;
}

m_vector = m_brackets $ (sourcefy UndefT $ return makeVector) <*> (m_commaSep exprparser)
m_field = m_dot >> m_identifier
m_record = m_braces $ (sourcefy UndefT $ return makeRecord) <*> (m_commaSep m_fieldterm)
m_tuple = try $ m_parens $ (sourcefy UndefT $ return makeTuple) <*> (m_commaSep1 exprparser)
m_fieldterm = do {
    s <- m_identifier;
    m_colon;
    v <- exprparser;
    return(s, v);
}
m_let = do {
    m_reserved "let";
    bindings <- many1 let_pair;
    m_reserved "in";
    expr <- exprparser;
    return $ makeLet bindings expr
}
let_pair = do {
    p <- patternparser;
    m_reservedOp "=";
    val <- exprparser;
    return(p, val);
}
patternparser = p_symbol <|> p_tuple <|> p_record <|> p_wildcard <?> "pattern"
p_symbol = (sourcefy UndefT $ return makeVarPat) <*> m_identifier
p_tuple = m_parens $ (sourcefy UndefT $ return makeTuplePat) <*> (m_commaSep1 patternparser)
p_record = m_braces $ (sourcefy UndefT $ return makeRecPat) <*> (m_commaSep1 p_field)
p_field = do {
    s <- m_identifier;
    m_reservedOp ":";
    p <- patternparser;
    return(s, p);
}
p_wildcard = m_reservedOp "_" >> (sourcefy UndefT $ return WildP)

parseExpr :: String -> SrcExpr
parseExpr = maybefy . (parse exprparser "")

maybefy :: Either ParseError (Expr Info) -> SrcExpr
maybefy (Left e) = err (Just $ errorPos e) (map messageString $ errorMessages e)
maybefy (Right e) = ok e

sourcefy ty f = f <*> (fmap (\p -> Info (Just p, ty)) getPosition)
