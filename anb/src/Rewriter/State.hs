{-| 
Description: Definition of State Monad 'RoleState' that is used for rewriting plus functions for working with it. 

During rewriting, a lot of information has to be remembered. To this end, 
the State Monad 'RoleState' is defined. It stores information while a role 
is rewritten. Additionally, this module defines
functions for working with 'RoleState'. -}
module Rewriter.State where

import Parser.Basic
import Parser.Message
import qualified Rewriter.IR as IR
import qualified Parser.ParseTree as PT

import Control.Monad.State 
import qualified Data.Set as Set


{- | Used to keep state when analyzing a role. 
     'RoleState' is used when rewriting a role (a new one is set up 
     when another role is rewritten). 
     The information kept is: 

     1. The role name (fixed), 

     2. The parse tree (fixed), 

     3. The current knowledge (changes during rewriting), and 

     4. an (infinite) list of (yet unused) symbols for naming ghost messages. 

     5. a list of ghost messages. This is to ensure that the same message
         always gets the same ghost symbol (when putting a message under
		 ghost abstraction, we can check here if the same message was already
		 put under a ghost abstraction earlier on; that way, a message @m@ is 
		 always represented by for example @alpha@ and never by @beta@). 
-}
type RoleState a 
    = State ( RoleName
            , PT.ParseTree
            , IR.Knowledge
            , [Identifier]
            , (Set.Set Message)) a 
                       
{- | Extracts the name of the role from the state. Does not alter state. -}
getRoleName :: RoleState RoleName
getRoleName = state $ \s@(rn, _, _, _, _) -> (rn, s)

{- | Returns the current knowledge from the state. Does not alter state. -}
getChi :: RoleState IR.Knowledge
getChi = state $ \s@(_, _, chi, _, _) -> (chi, s)

{- | Changes the current knowledge to the knowledge given as a 
     parameter.  -}
putChi :: IR.Knowledge -> RoleState ()
putChi newchi = state $ \(p, rn, _, sym, ga) -> ((), (p, rn, newchi, sym, ga))

{- | Adds the knowledge given as a parameter to the current knowledge. 
     Does not insert duplicates, but messages are inserted even if they
     are synthesizable. -}
addToChi :: IR.Knowledge -> RoleState () 
addToChi ms =
    do oldchi <- getChi
       putChi (ms `Set.union` oldchi)
       
{- | Returns the parse tree. Does not alter state. -}
getProtocol :: RoleState PT.ParseTree
getProtocol = state $ \s@(_, p, _, _, _) -> (p, s)

{- | Returns the list of remaining symbols. Does not alter state. -}
getSym :: RoleState [Identifier]
getSym = state $ \s@(_, _, _, sym, _) -> (sym, s)
     
{- | Returns if a message is in the current knowledge. Does not check 
     if a message is synthesizable from the current knowledge, only
     checks for simple equality. Check the 'Eq' instance of 'Message' for 
     details on which messages are equal. -}
inChi :: Message -> RoleState Bool  
inChi m = state $ \s@(_, _, chi, _, _) -> (m `Set.member` chi, s)     
    
{- | Returns the head of the current list of symbols and removes
     that symbol from the list of symbols. -}
nextSymbol :: RoleState Identifier
nextSymbol = state $ \(rn, p, chi, (s:sym), ga) -> (s, (rn, p, chi, sym, ga))

{- | Adds the identifier given as a parameter at the head of the list
     of symbols. Does not check if the element is already in the list, 
     i.e. may cause duplicates if not used carefully (should not happen)! -}
addSymbol :: Identifier -> RoleState ()
addSymbol s = state $ \(rn, p, chi, sym, ga) -> ((), (rn, p, chi, s:sym, ga))

{- | Adds a new ghost message to the list of ghost messages. -}
addGhostMessage :: Message -> RoleState () 
addGhostMessage g = 
    state $ \(rn, p, chi, sym, gh) -> ((), (rn, p, chi, sym, Set.insert g gh))
    
{- | Returns the set of ghost messages. -}
getGhostMessages :: RoleState (Set.Set Message)
getGhostMessages = state $ \s@(_, _, _, _, gh) -> (gh, s)
