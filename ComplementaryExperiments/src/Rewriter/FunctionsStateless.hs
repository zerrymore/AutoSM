{-|
Description: Functions for working with messages based on a knowledge. 

This module contains functions for working with messages based on 
knowledge. In contrast to the functions in "Rewriter.Functions", 
the functions in this module do not depend on 'Rewriter.State.RoleState'. 

Note that we always assume that the knowledge is a basic set. The
functions may return false values otherwise. 
-}
module Rewriter.FunctionsStateless where 

import Parser.Basic
import Parser.Message
import Rewriter.IR    
import Rewriter.MessageModel
import qualified Parser.ParseTree as PT

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Data.List (union)
import Control.Monad.State

{- | Takes knowledge @chi@ and messages @a@ and @b@. Returns
     if there is a message @c@ such that @a = Mul {b, c}@. 
	 Parameters: 

     1. Knowledge @chi@

     2. Message to be constructed (@a@).

     3. Message to be used in construction (@b@). -}
constrableMul :: Knowledge -> Message -> Message -> Bool 
constrableMul chi msg submsg =    
    msg `divisible` submsg && 
    inSynth chi (msg `divide` submsg)
        
{- | Takes knowledge @chi@ and messages @a@ and @b@. Returns
     if there is a message @c@ such that @a = Exp b c@. 	
	 Parameters: 

     1. Knowledge @chi@

     2. Message to be constructed (@a@).

     3. Message to be used in construction (@b@). -}
constrableExpl :: Knowledge -> Message -> Message -> Bool 
constrableExpl chi msg submsg = 
    msg `reduciblel` submsg &&
    inSynth chi (msg `reducel` submsg)

{- | Takes knowledge @chi@ and messages @a@ and @b@. Returns
     if there is a message @c@ such that @a = Exp c b@. 	
	 Parameters: 

     1. Knowledge @chi@

     2. Message to be constructed (@a@).

     3. Message to be used in construction (@b@). -}
constrableExpr :: Knowledge -> Message -> Message -> Bool 
constrableExpr chi msg submsg = 
    msg `reducibler` submsg && 
    inSynth chi (msg `reducer` submsg)


{- | Returns if a message (second parameter) can be synthesized from 
     certain knowledge (first parameter). -}
inSynth :: Knowledge -> Message -> Bool 
inSynth chi (Gamma _ m) = 
    inSynth chi m 
inSynth _ (Str _) = 
    -- Strings are always synthesizable. 
    True
inSynth chi msg@(K _ _) = 
    -- We can construct this message iff it is in 'chi'.  
    msg `Set.member` chi 
inSynth chi msg@(Pk _) = 
    -- We can construct this message iff it is in 'chi'. 
    msg `Set.member` chi 
inSynth chi msg@(Sk _) =
    -- We can construct this message iff it is in 'chi'. 
    msg `Set.member` chi 
inSynth chi msg@(Var _) = 
    -- We can construct this message iff it is in 'chi'. 
    msg `Set.member` chi
inSynth _ One = 
    True
inSynth chi (Concat m1 m2) =
    -- A concatenation '(Concat m1 m2)' is synthesizable if both 
    -- 'm1' and 'm2' are synthesizable. A basic set never contains messages
    -- that are concatenations at the top level since concatenation 
    -- can always be analyzed, therefore we do not need to check 
    -- 'msg `Set.member` chi'. 
    inSynth chi m1 && inSynth chi m2
inSynth chi msg@(Senc m k) =
    -- We can construct 'msg' if we know both 'm' and 'k' or
    -- if 'msg'  is in 'chi'.  
    (inSynth chi m && inSynth chi k) || 
    msg `Set.member` chi
inSynth chi msg@(Aenc m k) =
    -- Analogous to symmetric encryption. 
    (inSynth chi m && inSynth chi k) || 
    msg `Set.member` chi
inSynth chi msg@(Hash h) =
    -- We can construct 'msg' if we know 'h' or
    -- if 'msg' is in 'chi'. 
    inSynth chi h || msg `Set.member` chi 
inSynth chi msg@(Fun n ms) =
    -- We can construct 'msg' if we know all arguments or
    -- if 'msg' is in 'chi'. 
    all (inSynth chi) ms || msg `Set.member` chi
inSynth chi msg@(Exp m1 m2) = 
    inSynthExpTerm chi msg
inSynth chi msg@(Mul ms) = 
    inSynthMulTerm chi msg 
    
    
    
{- | Checks if a message (second parameter) can be constructed
     from certain knowledge (first parameter) using
	 'Rewriter.MessageModel.divide'. 
     Also returns @True@ if the message is in the knowledge
	 directly (i.e. does not have to be constructed). 
	 Only call this on multiplicative messages -}
inSynthMulTerm :: Knowledge -> Message -> Bool
inSynthMulTerm chi msg@(Mul facs) =
    let
        messages = Set.toList chi
        factors = MultiSet.toList facs
    in 
        msg `Set.member` chi ||
        any (constrableMul chi msg) messages ||
        any (\f -> inSynth chi f && 
                   constrableMul chi msg f) factors
        
{- | Checks if a message (second parameter) can be constructed
     from certain knowledge (first parameter) using
	 'Rewriter.MessageModel.reducel' or 'Rewriter.MessageModel.reducer'. 
     Also returns @True@ if the message is in the knowledge
	 directly (i.e. does not have to be constructed). 
	 Only call this on exponentiation messages. -}
inSynthExpTerm :: Knowledge -> Message -> Bool 
inSynthExpTerm chi msg@(Exp m1 m2) = 
    let 
        messages = Set.toList chi
    in 
        msg `Set.member` chi ||
        any (constrableExpl chi msg) messages ||
        any (constrableExpr chi msg) messages ||
        (inSynth chi m1 && inSynth chi m2) 
          
        
        
{- | Takes a set of messages (first parameter) 
     and a message (second parameter). Looks up the ghost message
	 in the knowledge that represents the message and returns it. 
	 The purpose of this function is to ensure that the same message
	 is always represented by the same ghost symbol.
	 This requires that the corresponding  message is already in 
	 the knowledge, otherwise calling 'gamma' would cause an error.-}
gamma :: Knowledge -> Message -> Message
gamma kw m = 
    head $ filter (==m) (Set.toList kw)

{- | A 'Ghost' is a term that can not be synthesized but is 
        still analyzable. See 'Rewriter.Function.partial' for details. -}
type Ghost = (Identifier, Message) 
        
{- | When a principal receives a new message, other messages that were
     already in the knowledge may become analyzable, in which case they
     usually also become synthesizable. 
     However, asymmetric encryption is a special case since different 
     keys are used for en- and decryption. 
     If we have a message that was encrypted with the secret key 
     then we can encrypt the message if we have the
     public key, but cannot we construct the message unless we also 
     possess the secret key.
   
     'partial' returns the (sub-)messages that are analyzable but not
     synthesizable. The messages are returned as pairs (name, message)
     where name is the name of the ghost symbol (since the message is not
     synthesizable) and message is the actual message (an asymmetric 
     encryption where the message under the encryption is in its 
	 constructive form). 
   
     In Tamarin, these messages are stated in the 'let' block of rules. 
     For example, if we have the ghost term alpha[Aenc m (Sk A)] in our
     knowledge and now receive (Pk A), m becomes analyzable (but (Aenc
     m (Sk A)) remains non-constructible). In this case we have to 
     tell Tamarin what the form of alpha is, that is, we write
     let alpha = aenc{m}sk_A in [...] --> [ State_out (..., m, ...) ], 
     so Tamarin knows how to extract m from alpha and can add it
     to the state. 
   -}
partial :: Knowledge -> Message -> [Ghost]
partial chi (Gamma i m) = 
    -- We just look at the message under the gamma term. 
    partial chi m 
partial chi (Concat m1 m2) =
    -- Concatenation can always be analyzed, so we just look 
    -- at the messages one level deeper: 
    partial chi m1 `union` partial chi m2 
partial chi msg@(Aenc m k@(Pk r)) = 
    let
        cf_msg = cf chi msg
        cf_m = cf chi m
    in  
        if (Sk r) `Set.member` chi && not ((Pk r) `Set.member` chi) then 
            -- We have the secret key but not he public key. That is, 
            -- we can look under the encryption but we cannot synthesize
            -- our message. In this case, we add this message to our list
            -- and also analyze the message under the encryption. 
            -- The constructive form of msg is a ghost message
            -- since it is not synthesizable, therefore we can get its
            -- symbol. 
            partial chi m `union` [(getSymbol cf_msg, (Aenc cf_m (Pk r)))]
        else
            -- We can either not look under the encryption or the message
            -- is both synthesizable and analyzable. 
            []
partial chi msg@(Aenc m k@(Sk r)) = 
    let
        cf_msg = cf chi msg
        cf_m = cf chi m
    in  
        -- Analogous to Aenc m (Pk r). 
        if (Pk r) `Set.member` chi && not ((Sk r) `Set.member` chi) then 
            partial chi m `union` [(getSymbol cf_msg, (Aenc cf_m (Sk r)))]
        else
            []
partial _ _ = 
    -- All other messages can never be analyzed. 
    -- Therefore we return the empty list. 
    []
        
 
{- | The knowledge of a principal should always be a basic set. When he
     receives a new message, some of the messages in the knowledge may
     become synthesizable and should therefore be removed. 
     
     The function 'newSynthesizable' takes the old knowledge (first parameter)
     and the new knowledge (second parameter) after all messages that
     can now be analyzed have been added. The function then returns a list
     of all the messages that must be removed to restore the basic set 
     property. 
     
     This function returns only ghost messages since atomic messages can 
     never be removed (they cannot be synthesized from sub-messages). -}
newSynthesizable :: Knowledge -> Knowledge -> [Ghost]
newSynthesizable oldChi newChi = 
    let 
        -- A message is synthesizable if: 
        -- a) it is a gamma term (atomic messages are never forgotten)
        -- b) it is synthesizable
        -- c) it is not in chi (otherwise it would be trivially synthesizable)
        -- Function 'removable' returns true if a message is analyzable. 
        removable :: Message -> Bool 
        removable msg = 
            isGamma msg && inSynth newChi msg && not (msg `Set.member` newChi)

        rem = filter removable (Set.toList oldChi)
    in
        map (\(Gamma n m) -> (n, m)) rem
       
        
{- | Returns the /constructive form/ of a message under the given 
     knowledge. Does not alter state. See the thesis for details
	 about the /constructive form/. 
	 
	 There is another version of 'cf' in "Rewriter.Functions", 
	 'Rewriter.Functions.cf' which works with 
	 'Rewriter.State.RoleState'. 
     The two functions do the same, however, this version here
	 cannot create new ghost messages (to that end, the state
	 would be needed). This means that you should only call 
	 this version of 'cf' if all needed ghost messages are already
	 in the knowledge. -}
cf :: Knowledge -> Message -> Message
cf _ msg@(Str _) = 
    msg
cf _ msg@(K _ _) =
    msg
cf _ msg@(Pk _) =
    msg
cf _ msg@(Sk _) =
    msg
cf _ msg@(Var _) =
    msg
cf _ One =  
    One
cf chi msg@(Concat m1 m2) = 
    conc (cf chi m1) (cf chi m2) 
cf chi msg@(Senc m k) =  
    if inSynth chi m && inSynth chi k then 
        senc (cf chi m) (cf chi k)
    else
        gamma chi msg
cf chi msg@(Aenc m k) = 
    if inSynth chi m && inSynth chi k then 
        aenc (cf chi m) (cf chi k)
    else
        gamma chi msg
cf chi msg@(Hash h) =  
    if inSynth chi h then 
        Hash (cf chi h)
    else
        gamma chi msg
cf chi msg@(Fun n ms) =  
    if all (inSynth chi) ms then
        Fun n (Prelude.map (cf chi) ms)
    else
        gamma chi msg 
cf chi msg@(Exp m1 m2) = do 
    cfExpTerm chi msg
cf chi msg@(Mul ms) =  
    cfMulTerm chi msg 
cf _ msg@(Gamma _ _) =  
    msg
       
{- | Takes knowledge @chi@ (first parameter) and a multiplicative 
     message @m@. Tries to construct @m@ from the knowledge
     with the help of 'Rewriter.MessageModel.divide'. 
     If this is not possible, then @m@ is put under a gamma 
     abstraction. Only call this function if @inSynthTerm chi m = True@. -}
cfMulTerm ::  Knowledge -> Message -> Message
cfMulTerm chi msg@(Mul facs) = 
    let   
        messages = Set.toList chi 
        cmul = any (constrableMul chi msg) messages
        constrMul chi submsg = 
            multiply submsg (cf chi (divide msg submsg))
            
        factors = MultiSet.toList facs
        cmulf = any (constrableMul chi msg) factors
        
                
    in 
        if cmul then 
            constrMul chi $ head $ filter (constrableMul chi msg) messages
        else if cmulf then 
            constrMul chi $ head $ filter (constrableMul chi msg) factors
        else 
		    -- The message is constructible (since @inSynthTerm chi m = True@)
			-- We cannot construct it by multiplication, 
			-- therefore we must have a corresponding ghost message in 
			-- our knowledge and it is safe to call 'gamma'. 
            gamma chi msg  
     
     
       
{- | Takes knowledge @chi@ (first parameter) and a exponential
     message @m@. Tries to construct @m@ from the knowledge
     with the help of 'Rewriter.MessageModel.reducel' or
     'Rewriter.MessageModel.reducer'. If this is not possible, 
     then @m@ is put under a gamma abstraction. Only call this function 
     if @inSynthTerm chi m = True@. -} 
cfExpTerm ::  Knowledge -> Message -> Message
cfExpTerm chi msg@(Exp m1 m2) = 
    let   
        messages = Set.toList chi 
        cexpl = any (constrableExpl chi msg) messages
        cexpr = any (constrableExpr chi msg) messages
        
        constrExpl chi submsg = 
            exponentiate submsg (cf chi (reducel msg submsg))
            
        constrExpr chi submsg = 
            exponentiate (cf chi (reducer msg submsg)) submsg
                  
        synm1 = inSynth chi m1  
        synm2 = inSynth chi m2          
    in 
        if cexpl then 
            constrExpl chi $ head $ filter (constrableExpl chi msg) messages
        else if cexpr then 
            constrExpr chi $ head $ filter (constrableExpr chi msg) messages
        else if synm1 && synm2 then 
            exponentiate (cf chi m1) (cf chi m2) 
        else 
		    -- The message is constructible (since @inSynthTerm chi m = True@)
			-- We cannot construct it by exponentiation, 
			-- therefore we must have a corresponding ghost message in 
			-- our knowledge and it is safe to call 'gamma'. 
            gamma chi msg  
      
          

