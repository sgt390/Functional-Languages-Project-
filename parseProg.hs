module ParseProg where
import Control.Applicative
import Parse

----------------------------------------------------------
-------------- Core Language Syntax definition -----------
----------------------------------------------------------
----------------------------------------------------------
type Name = String
data Expr a
       =   EVar Name            -- Variables
       | ENum Int               -- Numbers
       | EConstr Int Int        -- Constructor tag arity
       | EAp (Expr a) (Expr a)  -- Applications
       | ELet                   -- Let for recursive expressions
              IsRec               --  boolean True = NonRecursive Recursive
              [Def a]             --  Definitions
              (Expr a)            --  Body of let (rec)
       | ECase                  -- Case Expressions
              (Expr a)            --  Expression to scrutinise
              [Alter a]           --  Alternatives
       | ELam [a] (Expr a)      -- Lambda abstractions
       deriving Show

type Def a = (a, Expr a)          -- let and letrec  
type Alter a = (Int, [a], Expr a) -- case
data IsRec = NonRecursive | Recursive
       deriving Show

type Program a = [ScDef a]
type CoreProgram = Program Name

type ScDef a = (Name, [a], Expr a)
type CoreScDefN = ScDef Name

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

parseScDef :: Parser (ScDef Name)
parseScDef = do v <- identifier
                pf <- many identifier
                character "="
                body <- parseExpr
                return (v, pf, body)

parseVar :: Parser (Expr Name)
parseVar = do i <- identifier
              if not (elem i coreKeywords)
                     then return (EVar i)
                     else empty

character :: Name -> Parser Name
character xs = symbol xs


parseExpr :: Parser (Expr Name)
parseExpr = do
              brec <- parseBrec --let/letrec
              defs <- parseDefs
              character "in"
              expr <- parseExpr
              return (ELet brec defs expr)
       <|> do character "case"
              expr <- parseExpr
              character "of"
              alt <- parseAlts
              return (ECase expr alt)
       <|> do character "\\"
              v <- identifier 
              vs <- many identifier 
              symbol "."
              expr <- parseExpr
              return (ELam (v:vs) expr)
       <|> do expr1 <- parseExpr1 
              return expr1


parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- parseVar
                return v
               <|> do n <- parseNum
                      return n
               <|> do character "Pack"
                      character "{"
                      n0 <- integer 
                      character ","
                      n1 <- integer
                      character "}" 
                      return (EConstr n0 n1)
               <|> do character "("
                      expr <- parseExpr
                      character ")"
                      return expr

parseDefs :: Parser ([Def Name])
parseDefs = do d <- parseDef
               do character ";"
                  ds <- parseDefs
                  return (d:ds)
                  <|> return ([d])

parseDef :: Parser (Def Name)          
parseDef = do v <- identifier
              character "="
              expr <- parseExpr
              return (v, expr)


parseAlts :: Parser ([Alter Name])
parseAlts = do a <- parseAlt
               do character ";"
                  as <- parseAlts
                  return (a:as)
                  <|> return [a]


parseAlt :: Parser (Alter Name)
parseAlt = do character "<"
              n <- natural
              character ">"
              vs <- many identifier
              character "->"
              expr <- parseExpr
              return (n, vs, expr)

parseBrec :: Parser IsRec
parseBrec = do character "letrec"
               return Recursive
               <|> do character "let"
                      return NonRecursive

parseNum :: Parser (Expr Name)
parseNum = do num <- integer
              return (ENum num)

parseExpr1 :: Parser (Expr Name)
parseExpr1 = do expr2 <- parseExpr2
                do character "|"
                   expr1 <- parseExpr1
                   return (buildReturn "|" expr2 expr1)
                 <|> return expr2

parseExpr2 :: Parser (Expr Name)
parseExpr2 = do expr3 <- parseExpr3
                do character "&"
                   expr2 <- parseExpr2
                   return (buildReturn "&" expr3 expr2)
                 <|> return expr3

parseExpr3 :: Parser (Expr Name)
parseExpr3 = do expr4a <- parseExpr4
                do op <- parseRelop
                   expr4b <- parseExpr4
                   return (EAp (EAp op expr4a) expr4b) 
                 <|> return expr4a

parseExpr4 :: Parser (Expr Name)
parseExpr4 = do expr5 <- parseExpr5
                do character "+"
                   expr4 <- parseExpr4
                   return (buildReturn "+" expr5 expr4)
                 <|> do 
                      character "-"
                      expr5b <- parseExpr5
                      return (buildReturn "-" expr5 expr5b)
                 <|> return expr5

parseExpr5 :: Parser (Expr Name)
parseExpr5 = do expr6 <- parseExpr6
                do character "*"
                   expr5 <- parseExpr5
                   return (buildReturn "*" expr6 expr5)
                 <|> do character "/"
                        expr6b <- parseExpr6
                        return (buildReturn "/" expr6 expr6b)
                 <|> return expr6

parseExpr6 :: Parser (Expr Name)
parseExpr6 = do aexprs <- some parseAExpr
                return (compAExprs aexprs)

coreKeywords = ["in", "of", "let", "where", "letrec", "case"]

buildReturn :: Name -> (Expr Name) -> (Expr Name) -> (Expr Name)
buildReturn op e1 e2 = EAp (EAp (EVar op) e1) e2

compAExprs :: [Expr Name] -> (Expr Name)
compAExprs (e:[]) = e
compAExprs es = EAp (compAExprs (remlast es)) (last es)

remlast xs = reverse (tail (reverse xs))

parseRelop :: Parser (Expr Name)
parseRelop = do rel <- opIdentifier
                return (EVar rel)
