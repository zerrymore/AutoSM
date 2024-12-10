{-|
Description: Definition of monadic 'Translator.State.TranslationState' and functions for working with it.

For the translation for intermediate representation to Tamarin, a lot
of information has to be remembered. In this module, 
'Translator.State.TranslationState' is defined and functions for 
working with it are provided. -}

module Translator.State where

import Parser.Basic
import Parser.Message
import Rewriter.IR

import Control.Monad.State

{- | The translation is stateful (State Monad). The state is represented as 
     follows: 
	 
	 1. The protocol that should be translated to Tamarin. 
	 
	 2. Information about the role that is currently being translated.
	 
	 3. Output and accompanying state. -}  
type TranslationState a = 
    State (Protocol, RoleState, OutputState) a   

{- | State regarding the translation of a role. 
     The following information is stored: 

     1. The role name. 

     2. The remaining actions (that is, the actions that have not yet
        been written to the output). 

     3. The variables that are fresh in the current role ('~' in Tamarin).

     4. The variables that are public in the current role ('$' in Tamarin).
-}
type RoleState =   
    (RoleName, [Action], [Identifier], [Identifier])

{- | State regarding the output. 
     The following information is stored: 

     1. The characters that have been written so far.

     2. Current indentation depth. When a line break occurs, the
        next line will be filled with spaces up to indentation depth. 

     3. Current position of the cursor on a line. A newline is inserted
        when the maximum line length is reached. 
-}
type OutputState = 
    (String, Int, Int)
    
{- | A predicate in Tamarin. -}
type Predicate = (Label, [Message])
{- | An equation (for the @let@ block of the rule in Tamarin). -}
type Equation = (Identifier, Message) 

{------------------------------------------------------------------------------}
{- Functions for accessing and changing the state -}
  
{- | Returns the current 'RoleState'. -}
getRoleState :: TranslationState RoleState
getRoleState = 
    state $ \s@(p, r, o) -> (r, s)
    
{- | Returns the current 'OutputState'. -}
getOutputState :: TranslationState OutputState
getOutputState = 
    state $ \s@(p, r, o) -> (o, s)
    
{- | Set the role state to the given value. -}
putRoleState :: RoleState -> TranslationState ()
putRoleState r = 
    state $ \(p, _, o) -> ((), (p, r, o))

{- | Set the output state to the given value. -}
putOutputState :: OutputState -> TranslationState ()
putOutputState o = 
    state $ \(p, r, _) -> ((), (p, r, o))
   
{- | Returns the protocol that is being translated -}
getProtocol :: TranslationState Protocol
getProtocol = 
    state $ \s@(p, r, o) -> (p, s)   
 
{- | Puts the names that are public. 
     When writing messages to output, the variables with those names   
     will be prefixed with a '$'. -}
putPublicNames :: [Identifier] -> TranslationState ()
putPublicNames public = 
    do (rn, ac, fr, _) <- getRoleState
       putRoleState (rn, ac, fr, public)
       
{- | Returns a list with the names that are currently public. -}
getPublicNames :: TranslationState [Identifier]
getPublicNames = 
    do (_, _, _, public) <- getRoleState
       return public
     
{- | Puts the names that are fresh. 
     When writing messages to output, the variables with those names   
     will be prefixed with a '~'. -}
putFreshNames :: [Identifier] -> TranslationState ()
putFreshNames fr = 
    do (rn, ac, _, pu) <- getRoleState
       putRoleState (rn, ac, fr, pu)

{- | Returns a list with the names that are currently fresh. -}
getFreshNames :: TranslationState [Identifier]
getFreshNames = 
    do (_, _, fresh, _) <- getRoleState
       return fresh 

{- | Returns the name of the role that is currently being translated. -}
getRoleName :: TranslationState RoleName 
getRoleName = 
    do (rolename, _, _, _) <- getRoleState
       return rolename

{- | Returns the remaining actions of the role that is currently 
     being translated. -}
getActions :: TranslationState [Action] 
getActions = 
    do (_, actions, _, _) <- getRoleState
       return actions
       
{- | Puts the actions of the role that is currently being translated. -}
putActions :: [Action] -> TranslationState () 
putActions actions = 
    do (rn, _, fr, pu) <- getRoleState
       putRoleState (rn, actions, fr, pu)

{- | Get the current indentation. -}
getIndentation :: TranslationState Int
getIndentation = 
    do (_, r, _) <- getOutputState
       return r
       
{- | Set the current indentation. -}
putIndentation :: Int -> TranslationState ()
putIndentation i = 
    do (s, _, o) <- getOutputState
       putOutputState (s, i, o)       
       
{- | Add the value given as a parameter to the indentation. -}
addToIndentation :: Int -> TranslationState () 
addToIndentation i = 
    do indent <- getIndentation
       putIndentation $ indent + i       
       
{- | Get the current offset of the cursor (from the left-hand side). -}
getOffset :: TranslationState Int
getOffset = 
    do (_, _, r) <- getOutputState
       return r
      
{- | Put the current offset of the cursor. This will not actually change
     the position of the cursor on the output, i.e. this method is only
     for keeping track of where the cursor is -}
putOffset :: Int -> TranslationState ()
putOffset offset = 
    do (s, i, _) <- getOutputState
       putOutputState (s, i, offset)       
    
{- | Adds the value given as a parameter to the current offset of the cursor. 
     Like 'putOffset', this function does not actually affect the cursor 
     position on the output -}  
addToOffset :: Int -> TranslationState () 
addToOffset i = 
    do offset <- getOffset
       putOffset $ offset + i
      
{- | Returns the output that has been written so far. -} 
getOutput :: TranslationState String
getOutput = 
    do (s, _, _) <- getOutputState
       return s       
    
{- | Changes the output. -}  
putOutput :: String -> TranslationState () 
putOutput s = 
    do (_, i, o) <- getOutputState
       putOutputState (s, i, o)
 
{- | Appends a string to the output. -}      
addToOutput :: String -> TranslationState () 
addToOutput str = 
    do oldStr <- getOutput
       putOutput $ oldStr ++ str
       
