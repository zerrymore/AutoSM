{- |
Description: Definition of 'Parser.ParseTree.ParseTree' and related functions. 

This module contains the definition of the data type 'ParseTree'. 
A 'ParseTree' contains all the information that can be
parsed from an A&B file. Additionally, it contains some auxiliary
functions for working with parse trees. -}

module Parser.ParseTree 
( 
-- * The Data Structure
  ParseTree (ParseTree)
, Modifier (Public, Private)
, Declaration (Function)
, Knowledge (Knowledge)
, Action (Action) 
, Goal (Secret, WeakAuth, StrongAuth)
-- * Auxiliary Functions
, name
, declarations
, knowledge
, actions
, goals
, roleNames
, funDefs
, knowledgeOf
, functionDeclaration
, isFunctionPublic
) where

import Parser.Basic
import qualified Parser.Message as M

import Data.List

{- | The root data structure that contains all the information parsed from 
     the A&B input string.  -}
data ParseTree 
    {- | Specification of a protocol. Parameters: 
		
	     1. The name of the protocol, as given in the A&B file. 
		 
		 2. The declarations from the @Declarations@ block. 

		 3. The initial knowledge as declared in the @Knowledge@ block. 

		 4. The actions declared in the @Actions@ block. 

		 5. The security goals from the @Goals@ block. -}
    = ParseTree Label [Declaration] [Knowledge] [Action] [Goal]
    deriving Eq

-- | Access modifier for a function. 
data Modifier
    -- | The function can be applied by both the adversary and
	--   honest agents. 
    = Public 
	-- | The function can be applied only by honest agents. 
	--   The adversary cannot apply the function. 
    | Private 
    deriving Eq
    
-- | Declaration of a function (e.g. @private foo/2;@). 
data Declaration 
    {- | Declaration of function. Parameters: 

         1. Modifier. 

		 2. Name of the function. 

		 3. Arity of the function. -}
    = Function Modifier Identifier Integer
    deriving Eq

-- | Initial knowledge of a role (e.g. @A: pk(B), m;@)
data Knowledge 
    {- | Explicit initial Knowledge. Parameters: 
	    
		 1. The name of the role whose initial knowledge this is.

		 2. List of the messages in the initial knowledge. -}
    = Knowledge RoleName [M.Message] 
    deriving Eq

-- | An action (e.g. @[label] A -> B : message;@). 
data Action 
    {- | An action. Parameters: 
        
		 1. The label as given in the A&B specification. 

		 2. Role name of the sender. 

		 3. Role name of the receiver. 

		 4. Fresh names that are created by the sender. 

		 5. The message that is sent. -}
    = Action Label RoleName RoleName [Identifier] M.Message 
    deriving Eq

-- | A security goal (e.g. @msg secret of A, B;@). 
data Goal 
    {- | A secret. Parameters: 
	
	     1. The label as given in the A&B specification.
		 
		 2. The message that should be secret. 
		 
		 3. The roles involved. -}
    = Secret Label M.Message [RoleName]
    {- | Non-injective agreement. Parameters: 
		 
		 1. The label as given in the A&B specification. 
		 
		 2. The role that claims authenticity at the end of 
		    its execution. 
			
	     3. The other role involved.  
		 
		 4. The messages on which agreement should be achieved. -}
    | WeakAuth Label RoleName RoleName [M.Message]
    {- | Injective agreement. Analogous to non-injective agreement
	     ('WeakAuth'). -}
    | StrongAuth Label RoleName RoleName [M.Message]
    deriving Eq
       

{------------------------------------------------------------------------------}
{-- PRETTY PRINTING --}

instance Show Action where
    show (Action l p1 p2 [] m) = 
        labelShow l ++ p1 ++ " -> " ++ p2 ++ " : " ++ show m     
    show (Action l p1 p2 n m) = 
        labelShow l ++ p1 ++ " -> " ++ p2 ++ " (" ++ 
        (intercalate ", " . map show) n ++ ") : " ++ show m 
                  
instance Show Modifier where
    show Private  = "private"
    show Public   = "public"                    
                                
instance Show Declaration where    
    show (Function m n a) = 
        show m ++ " " ++ n ++ "/" ++ show a
        
instance Show Knowledge where
    show (Knowledge r m) = 
        r ++ ": " ++ (intercalate ", " . map show) m 
    
instance Show Goal where
    show (Secret l m ps) = 
        labelShow l ++ show m ++ " secret of " ++ intercalate ", " ps
    show (WeakAuth l r1 r2 msgs) = 
        labelShow l ++ show r1 ++ " non-injectively agree with " ++ 
        show r2 ++ " on " ++ (intercalate ", " . map show) msgs 
    show (StrongAuth l r1 r2 msgs) = 
        labelShow l ++ show r1 ++ " injectively agree with " ++ show r2 ++
        " on " ++ (intercalate ", " . map show) msgs  
   
   
   
instance Show ParseTree where
    show (ParseTree name functions knowledge actions goals) = 
        "Protocol " ++ name ++ "\n" ++
        "Declarations:\n" ++ linesShow functions ++
        "Knowledge:\n" ++ linesShow knowledge ++
        "Actions:\n" ++ linesShow actions ++ 
        "Goals:\n" ++ linesShow goals ++
        "end\n"
        where 
  
labelShow :: String -> String
labelShow l 
    | l == "" = ""
    | otherwise = "[" ++ l ++  "] " 

linesShow :: (Show a) => [a] -> String
linesShow []     = ""
linesShow [a]    = "\t" ++ show a ++ "\n"  
linesShow (a:as) = "\t" ++ show a ++ "\n" ++ linesShow as

{------------------------------------------------------------------------------}
{-- ACCESS --}


-- | Returns the name (label) of a protocol.
name :: ParseTree -> Label
name (ParseTree l _ _ _ _) = l

-- | Returns all declarations of a protocol. 
declarations :: ParseTree -> [Declaration]
declarations (ParseTree _ d _ _ _) = d

-- | Returns all knowledge declarations of a protocol. 
knowledge :: ParseTree -> [Knowledge]
knowledge (ParseTree _ _ k _ _) = k

{- | Returns the knowledge of a role that was explicitly stated in the 
     Knowledge block, that is, the implicit knowledge (own public and 
	 secret key, own name) is not contained. 
	 Returns the empty list if no knowledge was stated or the role 
	 does not exist. -}
knowledgeOf :: ParseTree -> RoleName -> [M.Message]
knowledgeOf p r = lookup $ knowledge p
    where lookup []                     = []
          lookup ((Knowledge rn kw):ls) = 
              if rn == r then kw `union` lookup ls else lookup ls
    
-- | Returns all actions of a protocol. 
actions :: ParseTree -> [Action]
actions (ParseTree _ _ _ a _) = a

-- | Returns all security goals of a protocol. 
goals :: ParseTree -> [Goal]
goals (ParseTree _ _ _ _ g) = g 

-- | Returns role names that appear as sender or receiver in a protocol. 
roleNames :: ParseTree -> [RoleName]
roleNames p = nub $ acs $ actions p
    where acs []     = []
          acs [x]    = roles x
          acs (x:xs) = (roles x) ++ (acs xs)
          roles (Action _ r1 r2 _ _) = [r1, r2]
         
-- | Returns the function declarations of a protocol. 
funDefs :: ParseTree -> [Declaration]
funDefs p = foldr (++) [] $ Prelude.map fundef $ declarations p
    where fundef (f@(Function _ _ _)) = [f]
          
-- | Gets the declaration of the function with the given name (label).
--   it is an error if there is not function with the given name. 
functionDeclaration :: ParseTree -> Identifier -> Declaration
functionDeclaration (ParseTree _ d _ _ _) name = 
    let 
        traverse :: [Declaration] -> Declaration
        traverse [] = 
            error $ "Function with name '" ++ name ++ "' does not exist."
        traverse ((Function mod n p):rs) = 
            if n == name then (Function mod n p) else traverse rs
     in 
        traverse d

-- | Looks up a function and returns if it is public or not. 
--   It is an error if there is not function with the given name. 
isFunctionPublic :: ParseTree -> Identifier -> Bool 
isFunctionPublic p id = case functionDeclaration p id of
    (Function mod _ _) -> mod == Public

