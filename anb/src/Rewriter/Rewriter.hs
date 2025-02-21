{- | 
Description: Functions for translating from parse tree ('Parser.ParseTree.ParseTree') to intermediate representation ('Rewriter.IR.Protocol'). 

This module provides the functionality for translating from 
a parse tree ('Parser.ParseTree.ParseTree') to the corresponding 
protocol in intermediate representation ('Parser.Rewriter.IR'). -}
module Rewriter.Rewriter (
    translateToIR
) where

import Parser.Basic
import Parser.Message
import Rewriter.State

import qualified Parser.ParseTree as PT
import qualified Rewriter.IR as IR
import Rewriter.MessageModel 
import Rewriter.Functions 

import Control.Monad.State 
import Data.List
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import Data.Maybe (isJust)


{------------------------------------------------------------------------------}
{- | Takes a parse tree and translates it to the corresponding
     protocol in intermediate representation format. -}
translateToIR :: PT.ParseTree -> IR.Protocol 
translateToIR p = 
    let 
        label = PT.name p 
        funs = Prelude.map translateFunction $ PT.funDefs p 
        roles = Prelude.map (translateRole p) $  PT.roleNames p 
        goals = Prelude.map translateGoal $ PT.goals p 
    in 
        IR.Protocol label funs roles goals
     
{------------------------------------------------------------------------------}
{- TRANSLATING ROLES -}

{- | Takes a protocol in its parse tree form and a role name that 
     occurs in the protocol. Returns the IR form of the role -}
translateRole :: PT.ParseTree -> RoleName -> IR.Role 
translateRole p rn =  
    let 
        {- Initial state for extraction of role from parse tree and
           translation to IR form. -}
        roleState = (rn, p, Set.empty, symbolStore, Set.empty)
    
        {- Traverses the actions in a list. The actions that 
           are performed by the current role are translated to 
           IR form and added to the role -}
        traverseActions :: [PT.Action] -> RoleState [IR.Action]
        traverseActions [] = return []
        traverseActions (pa:pas) = 
            do action <- translateAction pa 
               rest <- traverseActions pas
               case action of 
                   (Just a) -> return $ a:rest
                   Nothing -> return rest
      
        {- Extracts the role in its IR form. -}
        translRole :: RoleState IR.Role 
        translRole =   
             do roleName <- getRoleName
                putInitialKnowledge roleName
                initChi <- getChi
                protocol <- getProtocol
                pactions <- return $ PT.actions protocol
                actions <- traverseActions pactions
                return $ IR.Role roleName ((IR.Prepare initChi):actions)
    in
        fst $ runState translRole roleState
        
{------------------------------------------------------------------------------}
{- TRANSLATING ACTIONS -}

{- | Translates an action from its parse tree form into its IR form. 
     If the current role is the sender, the corresponding send action is 
     returned. If the current role is the receiver, the corresponding
     receive action is returned. Otherwise, 'Nothing' is returned -}
translateAction :: PT.Action -> RoleState (Maybe IR.Action) 
translateAction action@(PT.Action label sender receiver fresh message) =  do 
    roleName <- getRoleName
    if roleName == sender then do 
        result <- translateSendAction action
        return $ Just result
    else if roleName == receiver then do
        result <- translateReceiveAction action
        return $ Just result
    else do 
        return Nothing   
          
{- | Translates an action from the parse tree form to 
     the corresponding sending action in IR form. -}
translateSendAction :: PT.Action -> RoleState IR.Action 
translateSendAction (PT.Action label me receiver fresh message) = do 
    -- Create fresh values and add them to the knowledge. 
    addToChi $ Set.fromList $ Prelude.map Var fresh
    -- Get the constructive form of the message. 
    cf_m <- cf message
    -- Check if the message is constructible (otherwise the action is not
    -- executable by the sender). 
    executable <- inSynth cf_m
    -- Get the knowledge after the sending action (i.e. with the
    -- freshly generated values)
    chi <- getChi 
    return $ IR.Send executable label receiver fresh cf_m chi 
 
{- | Translates an action from the parse-tree form to 
     the corresponding receive action in IR form. -}
translateReceiveAction :: PT.Action -> RoleState IR.Action 
translateReceiveAction (PT.Action label sender me _ message) = do 
    -- Remember the current knowledge, it is going to be changed. 
    oldchi <- getChi
    -- Analyze the message and obtain new knowledge from it.  
    -- Add this new knowledge to the current knowledge. Remove any
    -- messages that have become synthesizable. 
    addKnowledge message
    -- View that the receiver has of the arriving message after
    -- analyzing it. This is the constructive form, i.e. the form 
    -- the receiver has of synthesizing the message. Certain messages may
    -- be not synthesizable but still analyzable. In order not to lose
    -- any information, 'newAna' contains information about the sub-messages
    -- that are analyzable but not synthesizable. 
    view <- cf message
    -- Knowledge after the receiving action. 
    chi <- getChi
    return $ IR.Receive label sender view chi   

{------------------------------------------------------------------------------}
{- HANDLING FUNCTION DECLARATOINS -}

{- | Translates a function declaration from parse tree form to 
     IR form -}
translateFunction :: PT.Declaration -> IR.Function
translateFunction (PT.Function mod id arity) = 
    (id, arity, mod == PT.Public)
         
{------------------------------------------------------------------------------}
{- EXTRACTING INITIAL KNOWLEDGE -}  
     
{- | Adds the implicit knowledge (own name, public and private key) of 
     the role given as a parameter to the implicit knowledge. -}
addImplicitKnowledge :: RoleName -> RoleState ()
addImplicitKnowledge rn =
    do msg1 <- gamma $ Var rn 
       addToChi $ Set.singleton msg1
       msg2 <- gamma $ Pk rn 
       addToChi $ Set.singleton msg2
       msg3 <- gamma $ Sk rn 
       addToChi $ Set.singleton msg3
       
{- | Extracts the knowledge stated in the 'Knowledge' block from the 
     parse tree and sets it as the current knowledge. Additionally, 
     the implicit knowledge of the role given as a parameter (own 
     name, public and private key) is added. -}
putInitialKnowledge :: RoleName -> RoleState ()
putInitialKnowledge rn = 
    do p <- getProtocol
       addImplicitKnowledge rn
       kw <- return $ PT.knowledgeOf p rn 
       ga <- mapM gamma kw
       addToChi $ Set.fromList $ ga
  
    
{------------------------------------------------------------------------------}
{- TRANSLATING GOALS -}

{- | Translates a parse tree goal into an IR goal -}
translateGoal :: PT.Goal -> IR.Goal
translateGoal (PT.Secret label message roles) = 
    IR.Secret label message roles
translateGoal (PT.WeakAuth label role1 role2 messages) = 
    IR.WeakAuth label role1 role2 messages
translateGoal (PT.StrongAuth label role1 role2 messages) = 
    IR.StrongAuth label role1 role2 messages
    
{------------------------------------------------------------------------------}
{- Synthesizing and Analyzing: handling new knowledge -}

{- | Applies the analysis rules to the current knowledge. 
     Note that this function may destroy the basic set property
     of the knowledge! -}
analyzeKnowledge :: RoleState () 
analyzeKnowledge = 
    let 
        analyzeSet :: [Message] -> RoleState ()
        analyzeSet [] = return ()
        analyzeSet (c:cs) = do
            chi <- getChi 
            ana <- analyzeOnce c
            newchi <- return $ Set.delete c chi 
            putChi $ Set.union newchi ana
            analyzeSet cs
    in do 
        oldchi <- getChi 
        analyzeSet (Set.toList oldchi)
        newchi <- getChi 
        if newchi == oldchi then 
            return () 
        else 
            analyzeKnowledge
               
{- | Removes all messages from the knowledge that are synthesizable 
     from others messages in the knowledge. 
     Guarantees that the new knowledge is a basic set and that
     exactly the same messages can be synthesized from the set as before, 
     provided that the knowledge was already fully analyzed before (i.e. 
     it contained every message that could be analyzed from other messages). 
     -}
removeSynth :: RoleState () 
removeSynth = 
    let 
        removeSynthesizable :: [Message] -> RoleState ()
        removeSynthesizable [] = return ()
        removeSynthesizable (m:ms) = 
           do oldchi <- getChi
              putChi $ Set.delete m oldchi 
              b <- inSynth m
              if b then 
                  return () -- keep m removed
              else
                  putChi oldchi -- restore the old knowledge. 
              removeSynthesizable ms 
    in 
        do chi <- getChi
           removeSynthesizable (Set.toList chi)
       
{- | Adds a new message to the current knowledge and restores the 
     basic set property. -}
addKnowledge :: Message -> RoleState () 
addKnowledge m = 
    do ana1 <- analyzeOnce m
       addToChi ana1
       analyzeKnowledge
       removeSynth
 
    
    
    
    
    
    
    
    
