{- |
Description: Parser for A&B protocol specifications.

This module contains the complete A&B to 'ParseTree' parser. 
-}

module Parser.ProtocolParser (
    anbParser, 
    parseAnB, 
    parseAnBFile, 
    msg
) where  

import Parser.ParseTree
import Parser.Basic
import Parser.Message

import Text.Parsec hiding (label)
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language  
import Control.Applicative((<*))
import Data.List (union, (\\), isPrefixOf)
import Data.Char (toUpper)
import qualified Data.MultiSet as MultiSet
  
{------------------------------------------------------------------------------}
{- SETTING UP PARSEC -}

{- Instructions to the parser. See documentation of Parsec for details. -}
def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , commentLine = "//"
              , nestedComments = True
              , caseSensitive = False
              , reservedOpNames = ["->", ":", "*", "^", ",", "'", ".", "<-", 
                                   ";", "/", "{", "}", ":=", ")", "(", "<", 
                                   ">"]
              , reservedNames = -- reserved words in A&B: 
                                ["injectively", "non-injectively", "agrees", 
                                 "with", 
                                 "secret", "of", "on", "protocol", "end", 
                                 "private", "public", "declarations", "goals", 
                                 "actions", "knowledge", "aenc", "senc",
                                 "h", "k", "sk", "pk", "init"] ++
                                -- the following words must not be used as
                                -- identifiers in Tamarin and are therfore
                                -- not legal in A&B, too
                                ["let", "in", "rule"] ++
                                -- the following are either functions that
                                -- are introduced by built-in theories of 
                                -- Tamarin or collide with functions that are
                                -- used internally in tamarin. 
                                [ "inv", "verify", "sign", 
                                  "adec", "sdec", "true", 
                                  "fst", "snd", "pair", "pmult", "em"]
              }
              
{- Building the token parser so we don't have to work at the lexer level -}          
TokenParser{ parens = m_parens
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace 
           , stringLiteral = m_stringLiteral
           , brackets = m_brackets
           , angles = m_angles
           , integer = m_integer
           , symbol = m_symbol
           , braces = m_braces } = makeTokenParser def

{------------------------------------------------------------------------------}
{- EXPORTED FUNCTIONS -}

-- |Parser for A&B protocol specifications. 
anbParser :: Parser ParseTree 
anbParser = m_whiteSpace >> protocol <* eof   

-- | Parses an A&B protocol specification string
parseAnB :: String -> Either ParseError ParseTree
parseAnB = parse anbParser "" 
        
-- |Parses an A&B protocol specification from a file and returns
--  the corresponding 'ParseTree'. 
parseAnBFile :: String -> IO (Either ParseError ParseTree)
parseAnBFile file = do { spec <- readFile file
                       ; return $ parse anbParser "" spec
                       }
  
{------------------------------------------------------------------------------}
{- PARSING MESSAGES -}     
  
-- | Parser for a single message. 
msg :: Parser Message
msg = comT <?> "message"

{-
The following four rules ensure the operator precedence '^' -> '*' -> '.':
comT = mulT '.' comT | mulT    -- A complete message
mulT = xorT '*' mulT | xorT   
xorT = bscT '^' xorT | bscT 
bscM = '(' comT ')' | '<' comT '>' | builtinFun | generalFun | str | var
-}

-- |Parser for a message with a '.', '*', '^' (or no operator) at the top level.
comT :: Parser Message
comT = do { m1 <- mulT
          ; r <- concat m1 <|> return m1
          ; return r
          }
    where concat m1 =   do { m_symbol "."
                           ; m2 <- comT
                           ; return (conc m1 m2)
                           }
      
-- |Parser for a message with a '*', '^' or no operator at the top level.
mulT :: Parser Message
mulT = do { m1 <- expT
          ; r <- mul m1 <|> return m1
          ; return r
          }
    where isMult (Mul _) = True
          isMult otherwise = False
          mul m1 = do { m_symbol "*"
                      ; m2 <- mulT  
                      ; return $ multiply m1 m2
                      }
    
    
-- |Parser for a message with '*' or no operator at the top level
expT :: Parser Message
expT = do { m1 <- bscM
          ; r <- expo m1 <|> return m1
          ; return r
          }
    where expo m1 = do { m_symbol "^"
                       ; m2 <- expT
                       ; return $ exponentiate m1 m2
                       }
    
-- |Parser for a message with no operator ('.', '*', '^') at the top level.
bscM :: Parser Message
bscM =   (m_parens comT)
     <|> (m_angles comT)
     <|> builtInFun
     <|> try generalFun
     <|> str
     <|> var
     <?> "message"
    
     
-- |Parser for a string in single quotes.  
str :: Parser Message
str = do { m_whiteSpace
         ; char '\''
         ; s <- ident
         ; char '\''
         ; m_whiteSpace
         ; return $ Str s 
         }    
     <?> "string"  

-- |Parser for an identifier or variable. 
var :: Parser Message
var = fmap Var ident <?> "identifier"  


-- |Parser for one of the built-in functions (aenc, adec, ...). 
builtInFun :: Parser Message
builtInFun =   ((try $ m_reserved "aenc") >> (crypto msg aencKey aenc))
           <|> ((try $ m_reserved "senc") >> (crypto msg bscM senc))
           <|> ((try $ m_reserved "h") >> (fun1 msg Hash))
           <|> ((try $ m_reserved "sk") >> (fun1 ident Sk))
           <|> ((try $ m_reserved "pk") >> (fun1 ident Pk))
           <|> parseSymKey
    where aencKey =   ((try $ m_reserved "pk") >> (fun1 ident Pk))
                  <|> ((try $ m_reserved "sk") >> (fun1 ident Sk))
          parseSymKey :: Parser Message
          parseSymKey = do 
              try $ m_reserved "k"
              -- Always list the role name in alphabetical order, 
              -- that is, the result is always 'K(A, B)' not matter if
              -- the user types 'K(A, B)' or 'K(B, A)'. 
              (K r1 r2) <- fun2 ident ident K 
              if r1 > r2 then return (K r2 r1) 
              else return (K r1 r2)
                   

                         
-- |Parser for a general (i.e. non-built-in) function.                              
generalFun :: Parser Message
generalFun = do { i <- ident
                ; ms <- m_parens $ sepBy msg (m_symbol ",")
                ; return (Fun i ms)
                }
       
{- |Parser for something of the form '(m)'. The parser for m is passed as 
    argument and the parsed m is applied to a function. -}
fun1 :: Parser a -> (a -> b) -> Parser b 
fun1 p f =  fmap f $ m_parens p

{- |Parser for something of the form '(m1, m2)'. The parsers for m1 and m2
    are given as arguments and the parsed m1 and m2 are applied to a 
    function. -} 
fun2 :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
fun2 p1 p2 f = m_parens ( do { m1 <- p1
                             ; m_symbol ","
                             ; m2 <- p2
                             ; return (f m1 m2)
                             })
                             
{- |Parser for something of the form '{m1}m2'. The parsers for m1 and m2
    are given as arguments and the parsed m1 and m2 are applied to a 
    function. -}
crypto :: Parser a -> Parser b -> (a -> b -> c) -> Parser c
crypto p1 p2 f = do { m1 <- m_braces $ p1
                    ; m2 <- p2
                    ; return (f m1 m2)
                    }

{------------------------------------------------------------------------------}
{- PARSING THE PROTOCOL AS A WHOLE -} 
protocol :: Parser ParseTree
-- |Parser for a protocol as a whole. 
protocol = do { m_reserved "Protocol"
              ; n <- ident
              ; m_symbol ":"
              ; f <- declarationBlock
              ; k <- knowledgeBlock
              ; a <- actionBlock
              ; g <- goalBlock
              ; m_reserved "end"
              ; return (ParseTree n f k a g)
              }

{------------------------------------------------------------------------------}
{- PARSING THE DECLARATION BLOCK -} 

-- |Parser for a function declaration. 
functionDecl :: Parser Declaration
functionDecl = do { m <- mod
                  ; name <- ident
                  ; m_symbol "/"
                  ; i <- m_integer
                  ; m_symbol ";"
                  ; return (Function m name i)
                  }
             <?> "function"
    where pri = do { m_reserved "private" 
                   ; return Private 
                   }
          pub = do { m_reserved "public" 
                   ; return Public 
                   }
          mod = try pri <|> try pub <|> (return Public) <?> "access modifier"
     
{- |Parser for the declaration block. 
    This parser returns an empty list if there is no such block 
    or if it is empty since it is not mandatory. -}         
declarationBlock :: Parser [Declaration]
declarationBlock = try (do { m_reserved "Declarations"
                           ; m_symbol ":"  
                           ; fs <- many $ try functionDecl
                           ; return fs
                           } )
                <|> return []
     
{------------------------------------------------------------------------------}
{- PARSING THE KNOWLEDGE BLOCK -} 

{- |Parser for the knowledge block. 
    This parser returns an empty list if there is no such block 
    or if it is empty since it is not mandatory. -}      
knowl :: Parser Knowledge
knowl = do { r <- ident
               ; m_symbol ":"
               ; ms <- sepBy msg (m_symbol ",")
               ; m_symbol ";"
               ; return (Knowledge r ms)
               }
          <?> "knowledge specifiaction"
    
-- |Parser for the knowledge block.            
knowledgeBlock :: Parser [Knowledge]
knowledgeBlock = try ( do { m_reserved "Knowledge"
                          ; m_symbol ":"
                          ; k <- many $ try knowl
                          ; return k
                          } )
                 <|> (return [])       
          
{------------------------------------------------------------------------------}
{- PARSING THE ACTION BLOCK -}

-- |Parser for an action specification. 
action :: Parser Action
action = do { l <- label
            ; r1 <- ident
            ; a <- actOp r1 (Action l)  
            ; f <- fresh
            ; m_symbol ":"
            ; m <- msg
            ; m_symbol ";"
            ; return (a f m)
            }
       <?> "action specification"
    where actOp r1 act =   do { m_symbol "->"
                              ; r2 <- ident
                              ; return (act r1 r2)
                              }
                       <|> do { m_symbol "<-"
                              ; r2 <- ident
                              ; return (act r2 r1)
                              }
          fresh = do { m_symbol "("
                     ; l <- sepBy ident $ m_symbol ","
                     ; m_symbol ")"
                     ; return l
                     }
               <|> return []
      
-- |Parser for the action block.   
actionBlock :: Parser [Action]     
actionBlock = try ( do { m_reserved "Actions"
                       ; m_symbol ":"
                       ; a <- many $ try action
                       ; return a
                       } ) 
            <|> (return [])

{------------------------------------------------------------------------------}
{- PARSING THE GOAL BLOCK -}

-- |Parser for a goal specification. 
goal :: Parser Goal 
goal = do { r <- try weakAuth <|> try strongAuth <|> try secret 
          ;  m_symbol ";"
          ; return r
          }
    
-- |Parser for the goal block. 
goalBlock :: Parser [Goal]
goalBlock = try ( do { m_reserved "Goals"
                     ; m_symbol ":"
                     ; g <- many $ try goal
                     ; return g
                     } )
          <|> (return [])
    
-- |Parser for the specification of a weak authentication security goal.     
weakAuth :: Parser Goal
weakAuth = do { l <- label
              ; r1 <- ident
              ; m_reserved "non-injectively"  
              ; m_reserved "agrees"
              ; m_reserved "with"
              ; r2 <- ident
              ; m_reserved "on"
              ; ms <- sepBy1 msg $ m_symbol ","
              ; return (WeakAuth l r1 r2 ms)
              }
              
-- |Parser for the specification of a strong authentication security goal.
strongAuth :: Parser Goal
strongAuth = do { l <- label
                ; r1 <- ident
                ; m_reserved "injectively"  
                ; m_reserved "agrees"
                ; m_reserved "with"
                ; r2 <- ident
                ; m_reserved "on"
                ; ms <- sepBy1 msg $ m_symbol ","
                ; return (StrongAuth l r1 r2 ms)
                }
              
-- |Parser for the specification of a secrecy security goal.
secret :: Parser Goal 
secret = do { l <- label
            ; m <- msg
            ; m_reserved "secret" >> m_reserved "of" 
            ; ls <- sepBy1 ident $ m_symbol ","
            ; return (Secret l m ls)
            }
    
{------------------------------------------------------------------------------}
{- HELPER FUNCTIONS -}       
        
-- |Parses a label.         
label :: Parser Label                 
label = m_brackets ident

-- |Parses an identifier. 
ident :: Parser Identifier
ident = let 
            {- | Checks if an identifier collides with a keyword -}
            isReserved :: Identifier -> Bool 
            isReserved name = 
                -- Convert all the strings to upper case before comparison. 
                map toUpper name `elem` (map (map toUpper) (reservedNames def))
        
            {- | Reads the beginning of an identifier -}
            init = do 
                -- a <- many (char '_') -- Arbitrarily many leading '_'
                b <- letter          -- Followed by a letter 
                return ({-a ++ -} [b])
        in do
            beg <- init
            fol <- many $ try (alphaNum <|> (char '_'))
            m_whiteSpace
            result <- return $ beg ++ fol 
            resultString <- return $ result
            if isReserved $ result then do 
                fail $ "unexpected keyword '" ++ beg ++ fol ++ "'"
            else if isPrefixOf "sk_" resultString then do 
                fail $ "identifier '" ++ beg ++ fol ++ "' starts with '" ++
                       (take 3 $ beg ++ fol) ++ "' which is not legal."
            else do 
                return result
        <?> "identifier"


    
{------------------------------------------------------------------------------}
