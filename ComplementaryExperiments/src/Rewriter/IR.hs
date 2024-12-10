{- |
Description: Definition of 'Rewriter.IR.IR' and related functions. 

This module contains the data structure for the intermediate
representation (IR) of A&B protocol specifications and related
functions. 
-}
module Rewriter.IR ( 
-- * Definition of data structures
  Protocol (Protocol)
, Function
, Role (Role) 
, Action (Prepare, Send, Receive)
, Goal (Secret, WeakAuth, StrongAuth)
, Knowledge
-- * Functions
-- ** Working with a protocol
, roleByName
, protocolName
, roles
, functions
, actions
, goals
, roleNames
, allMessages
-- ** Working with roles
, roleName
, actionsOfRole
, getInitialKnowledge
-- ** Working with actions
, getKnowledge
, getMessage
, getLabel
, isPrepareAction
, isSendAction
, isReceiveAction
, isActionExecutable
-- ** Working with functions
, functionName
, functionArity
, findFunction
-- ** Working with goals
, isSecrecyGoal
, isWeakAuthGoal
, isStrongAuthGoal
, isGoalOf
, labelOfGoal
, isAgreementPartner
, authMessages
, secretsOf
) where

import Parser.Basic
import Parser.Message

import Control.Monad.State 
import Data.List (intercalate, find, nub) 
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

{--------------------------------------}
{- INTERMEDIATE REPRESENTATION FORMAT -}
{--------------------------------------}

    
{- | Describes a complete protocol. 

     1. The name of the protocol. 

     2. The functions that appear in the protocol. 

     3. The roles of the protocol.

     4. The security goals that should be achieved by the protocol. -}
data Protocol 
    = Protocol Identifier [Function] [Role] [Goal]
    deriving Eq
   
{- | Information about a declared function.

     1. Name of the function. 

     2. Aritiy of the function

     3. Is the function public? -}      
type Function = (Identifier, Integer, Bool) 

{-|
  Describes a role of the protocol, that is, all actions that
  are executed by a principal executing the role. 
-} 
data Role 
    {- | 1. Name of the role. 
	    
		 2. The actions that are executed by the protocol. 
      
         The first action always is a 'Prepare' action which gives the 
         initial knowledge of the role. This action is then followed by
         a sequence of 'Send' and 'Receive' actions. -}
    = Role RoleName [Action] 
    deriving Eq
       
{-| 
  Describes a single action of a role. 
-}
data Action 
    {-| 
      The 'Prepare' action is the first action of every role. Its parameter
      describes the initial knowledge of the role. -}
    = Prepare Knowledge
    {-| 
      A 'Send' action describes the sending of a messages: 

      1. Is this action executable? 

      2. Label of the action as given in the A&B specification. 

      3. Name of the role receiving the message

      4. List of fresh variables

      5. Message being sent (constructive form)

      6. Knowledge after the sending action. 
    -}
    | Send Bool Label RoleName [Identifier] Message Knowledge
    {-|
      A 'Receive' action describes the receiving of a message: 

      1. Label of the action as given in the A&B specification. 

      2. Name of the role from which the message is received. 

      3. Message being sent (constructive form AFTER receiving)

      4. Knowledge after the sending action. 
    -}
    | Receive Label RoleName Message Knowledge
    deriving Eq
    

{- |
   Describes a security goal of the protocol. 
-}
data Goal 
    {- | Secrecy goal. 

         1. Label of the goal as given in the A&B specification. 

         2. The message.

         3. The roles.
    -}
    = Secret Label Message [RoleName]
    {- | Non-injective agreement goal. 
	
	     1. Label of the goals as given in the A&B specification. 
		 
		 2. The first role ('A agrees ...')
		 
		 3. The second role ('... with B ...')
		 
		 4. The messages on which to agree. -}
    | WeakAuth Label RoleName RoleName [Message]
    {- | Injective agreement. Analogous to non-injective agreement 
	     ('WeakAuth'). -}
    | StrongAuth Label RoleName RoleName [Message]
    deriving Eq 
 
{- | The knowledge of a role. -}
type Knowledge = Set.Set Message

instance Show Protocol where
    show (Protocol name functions roles goals) = 
        "[Protocol " ++ name ++ ". Functions={" ++
        intercalate ","
        (map (\(n, a, p) -> 
            (if p then "public " else "private ") ++
            (show a) ++ "/" ++ n) functions) ++ 
        "},Roles={" ++ intercalate "," (map show roles) ++ 
        "},Goals={" ++ intercalate "," (map show goals) ++ "}]"

instance Show Role where
    show (Role rolename actions) =  
        "[Role. Name=\"" ++ rolename ++ "\",Actions={" ++
        intercalate "," (map show actions) ++ "}]"
        
instance Show Action where
    show (Prepare k) = 
        "[Prepare Action. Knowledge={" ++ 
        intercalate "," (map show (Set.toList k)) ++ "}]"
    show (Receive l rn m k) = 
        "[Receive Action. Label=\"" ++ l ++ "\",Sender=\"" ++ 
        rn ++ "\",Message=\"" ++ show m ++
        "\",Knowledge={" ++ intercalate "," (map show (Set.toList k)) ++
        "}]"
        where 
            ghostShow ((n, m)) = n ++ "=" ++ show m 
    show (Send ex l rn fresh m k) = 
        "[Send Action. Executable=" ++ show ex ++ ",Label=\"" ++ l ++ 
        "\",Receiver=\"" ++ rn ++ "\",Fresh={" ++ 
        intercalate "," (map show fresh) ++ "},Message=\"" ++ show m ++
        "\",Knowledge={" ++ intercalate "," (map show (Set.toList k)) ++
        "}]"
        
       
instance Show Goal where
    show (Secret label message roles) = 
        "[Secrecy Goal. Label=\"" ++ label ++ "\",Message=\"" ++ show message ++ 
        "\",Roles={" ++ intercalate "," (map show roles) ++ "}]"
    show (WeakAuth label role1 role2 messages) = 
        "[Non-injective Agreement Goal. Label=\"" ++ label ++ "\",Role1=\"" ++ 
        show role1 ++
        "\",Role2=\"" ++ show role2 ++
        "\",Message={" ++ 
        intercalate "," (map show messages) ++ "}]"
    show (StrongAuth label role1 role2 messages) = 
        "[Injective Agreement Goal. Label=\"" ++ label ++ "\",Role1=\"" ++ 
        show role1 ++
        "\",Role2=\"" ++ show role2 ++
        "\",Message={" ++ 
        intercalate "," (map show messages) ++ "}]"
        
{------------------------------------------------------------------------------}
{- Functions for Protocols -}
    
{- | Returns the role with the given name from a protocol. 
     Do not call this function on a role name that has no 
     actions in the protocol. -}  
roleByName :: Protocol -> RoleName -> Role
roleByName (Protocol _ _ roles _) name = 
    getRole roles
    where getRole [] = 
              error "There is no role with the given name."
          getRole (r@(Role name2 _):rs) = 
              if name == name2 then r else getRole rs
      
{- | Returns the name of a protocol. -}      
protocolName :: Protocol -> RoleName 
protocolName (Protocol name _ _ _) = name

{- | Returns all roles of a protocol. -}
roles :: Protocol -> [Role]
roles (Protocol _ _ rs _) = rs

{- | Returns all declared function declarations of a protocol. -}
functions :: Protocol -> [Function]
functions (Protocol _ funs _ _) = funs

{- | Returns all actions of a protocol. -}
actions :: Protocol -> [Action]
actions protocol = 
    let 
        rs = roles protocol 
        as = foldr (++) [] $ map actionsOfRole rs
    in 
        as

{- | Returns all goals of a protocol. -}
goals :: Protocol -> [Goal]
goals (Protocol _ _ _ goals) = goals

{- | Returns all role names that appear in a protocol -}
roleNames :: Protocol -> [Identifier]
roleNames protocol = 
    let 
        rls = roles protocol 
        getNames [] = []
        getNames ((Role name _):rs) = name:(getNames rs)
    in 
        getNames rls
        

{------------------------------------------------------------------------------}
{- Functions for Roles -}

{- | Returns the name of a role. -}
roleName :: Role -> RoleName       
roleName (Role name _) = name

{- | Returns all actions of a role. -}
actionsOfRole :: Role -> [Action]
actionsOfRole (Role _ actions) = actions 

{- | Returns the initial knowledge of a role. -}
getInitialKnowledge :: Role -> Knowledge
getInitialKnowledge role = getKnowledge $ head $ actionsOfRole role

{------------------------------------------------------------------------------}
{- Functions for Actions -}

{- | Returns the knowledge the executing role has at the end of an action. -}
getKnowledge :: Action -> Knowledge
getKnowledge (Prepare k) = k 
getKnowledge (Receive _ _ _ k) = k 
getKnowledge (Send _ _ _ _ _ k) = k

{- | Returns the message being sent (if a sending action) or the action
     begin received (if a receiving action). Do not call on Prepare
     actions. -}
getMessage :: Action -> Message
getMessage (Send _ _ _ _ msg _) = msg
getMessage (Receive _ _ msg _) = msg
getMessage (Prepare _) = error "getMessage: Prepare actions have no message"

{- | Returns the label that was assigned to an action in the
     A&B specification. -}
getLabel :: Action -> Identifier
getLabel (Prepare _) = "init" -- Do not change this!
getLabel (Receive l _ _ _) = l 
getLabel (Send _ l _ _ _ _) = l 

{- | Returns if an action is a 'Prepare' action. -}
isPrepareAction :: Action -> Bool 
isPrepareAction (Prepare _) = True
isPrepareAction _ = False

{- | Returns if an action is a 'Send' action. -}
isSendAction :: Action -> Bool 
isSendAction (Send _ _ _ _ _ _) = True   
isSendAction _ = False       
       
{- | Returns if an action is a 'Receive' action. -}
isReceiveAction :: Action -> Bool 
isReceiveAction (Receive _ _ _ _) = True    
isReceiveAction _ = False   
       
{- | Returns if an action is executable. -}
isActionExecutable :: Action -> Bool 
isActionExecutable (Send ex _ _ _ _ _) = ex
isActionExecutable _ = True  
    
{------------------------------------------------------------------------------}
{- Functions for Functions -}

{- | Returns the name of a function. -}
functionName :: Function -> Identifier
functionName (name, _, _) = name
    
{- | Returns the arity of a function. -}
functionArity :: Function -> Integer
functionArity (_, arity, _) = arity
    
{- | Returns 'Just' the function declaration with the given name 
     or returns 'Nothing' if there is no function with the given name
     in the protocol. -}
findFunction :: Identifier -> Protocol -> Maybe Function
findFunction name protocol = 
    let 
        funs = functions protocol
        
        eq :: Function -> Bool 
        eq (fName, _, _) = name == fName
    in 
        find eq funs
     
{------------------------------------------------------------------------------}

{- | Returns the set of all messages that appear in a protocol at
     any position. -}
allMessages :: Protocol -> Set.Set Message
allMessages protocol = 
    let
        {- All actions -}
        acts = actions protocol 
        {- All actions except for the 'Prepare' actions -}
        actsWOPrep = filter (not . isPrepareAction) acts
        {- All messages that appear in messages (except 'Prepare' actions) -}
        msgsActsWOPrep = nub $ map getMessage actsWOPrep
        
        {- All roles -}
        rols = roles protocol 
        {- Initial knowledge of all roles -}
        msgsInitKw = foldr Set.union Set.empty 
            $ map getInitialKnowledge rols
        
        {- Returns all messages that appear in a goal -}
        goalMessages :: Goal -> [Message]
        goalMessages (Secret _ m _) = [m]
        goalMessages (WeakAuth _ _ _ ms) = ms
        goalMessages (StrongAuth _ _ _ ms) = ms
        
        {-- All messages that appear in a goal -}
        msgsFromGoals = 
            foldr Set.insert Set.empty $ concatMap goalMessages $ goals protocol 
    in 
        (Set.fromList msgsActsWOPrep) `Set.union` 
        msgsInitKw `Set.union`
        msgsFromGoals

   
{------------------------------------------------------------------------------}
{------------------------}
{-- Working with goals --}
{------------------------}

{- | Returns if a goal is a secrecy ('Secret') goal. -}
isSecrecyGoal :: Goal -> Bool 
isSecrecyGoal (Secret _ _ _) = True
isSecrecyGoal otherwise = False 

{- | Returns if a goal is a non-injective agreement ('WeakAuth') goal -}
isWeakAuthGoal :: Goal -> Bool 
isWeakAuthGoal (WeakAuth _ _ _ _) = True
isWeakAuthGoal otherwise = False

{- | Returns if a goal is an injective agreement ('StrongAuth') goal -}
isStrongAuthGoal :: Goal -> Bool 
isStrongAuthGoal (StrongAuth _ _ _ _) = True
isStrongAuthGoal otherwise = False

{- | Returns if a goal is a goal of a role. In particular: 
   
     * If a secrecy goal ('Secret'), then returns if the role claims
	   secrecy in its last action. 
	   
	 * If a non-injective ('WeakAuth') or injective ('StrongAuth') agreement
	   goal, then returns if this role claims authenticity
	   (A in 'A agrees with ...'). -}
isGoalOf :: RoleName -> Goal -> Bool 
isGoalOf role (Secret _ _ rs) = 
    role `elem` rs
isGoalOf role (WeakAuth _ r _ _) = 
    r == role
isGoalOf role (StrongAuth _ r _ _) = 
    r == role
    
{- | Returns the label of a goal. -}
labelOfGoal :: Goal -> Label
labelOfGoal (Secret l _ _) = l 
labelOfGoal (WeakAuth l _ _ _) = l
labelOfGoal (StrongAuth l _ _ _) = l 
    
{- | Returns if the given role is the second role 
     (B in '... agrees with B ...) in a non-injective 
     ('WeakAuth') or injective ('StrongAuth') agreement goal. 
	 Returns 'False' if it is not an agreement goal. -}
isAgreementPartner :: RoleName -> Goal -> Bool 
isAgreementPartner role (WeakAuth _ _ r _ ) = 
    r == role
isAgreementPartner role (StrongAuth _ _ r _ ) = 
    r == role
isAgreementPartner _ _ = 
    False
    
{- | Only call this on non-injective ('WeakAuth') or injective 
     ('StrongAuth') agreement goals (error otherwise). Returns
	 the messages on which the roles should agree. -}
authMessages :: Goal -> [Message] 
authMessages (WeakAuth _ _ _ msgs ) = 
    msgs
authMessages (StrongAuth _ _ _ msgs ) = 
    msgs
authMessages _ = 
    error "Not an authentication goal."
    

{-| Returns the secrecy goals of a role -}
secretsOf :: Protocol -> RoleName -> [Goal]
secretsOf protocol rolename = 
    let
        allSecs = filter isSecrecyGoal $ goals protocol 
        secs = filter (\(Secret _ _ rs) -> rolename `elem` rs) allSecs
    in 
        secs
    
