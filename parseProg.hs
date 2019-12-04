
module ParseProg where
import Parse
import Control.Applicative

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
       | Elam [a] (Expr a)      -- Lambda abstractions
       deriving Show

type Def a = (a, Expr a)          -- let and letrec  
type Alter a = (Int, [a], Expr a) -- case
data IsRec = NonRecursive | Recursive
       deriving Show

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefN = ScDefn Name

parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                character "="
                body <- parseExpr -- call to parseExpr
                return (v, pf, body)

{--TODO
/character
/parseVar
/parseExpr  - let letrec case lambda aexpr
parseDef -  Def (let and letrec)
parseAlt -  Alter (case)
/parseAExpr - Aexpr
--}


parseVar :: Parser (Expr Name)
parseVar = do i <- identifier
              return (EVar i)

character :: Name -> Parser Name
character xs = symbol xs


parseExpr :: Parser (Expr Name)
parseExpr = do 
              appl <- parseAppl
              return (appl)
           do brec <- parseBrec
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
              v <- parseVar
              vs <- many parseVar -- ..one or more
              symbol "."
              expr <- parseExpr
              return (Elam (v:vs) expr)
       <|> do expr <- parseAExpr
              return expr


parseAExpr :: Parser (Expr Name)
parseAExpr = do v <- parseVar -- var
                return (EVar v)
               <|> do n <- parseNum
                      return n
               <|> do character "Pack {"
                      n0 <- parseNum
                      character ","
                      n1 <- parseNum
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
parseDef = do v <- parseVar
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
              vs <- many parseVar
              character "->"
              expr <- parseExpr
              return (n, vs, expr)

parseBrec :: Parser IsRec
parseBrec = do character "letrec"
               return Recursive
               <|> do character "let"
                      return NonRecursive

parseNum :: Parser (Expr ENum)
parseNum = do num <- integer
              return (ENum num)

parseAppl :: Parser (Expr TODO)