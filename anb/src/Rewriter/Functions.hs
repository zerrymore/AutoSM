{- | 
Description: Functions for working with messages based on knowledge and 'Rewriter.State.RoleState

This module contains functions for working with messages based on 
knowledge. The functions in this module depend on 
'Rewriter.State.RoleState'. They are at the core of the rewriting 
step. 
-}

module Rewriter.Functions 
( inSynth
, cf
, analyzeOnce
, gamma
, symbolStore ) where 

import Parser.Basic
import Parser.Message
import Rewriter.IR    
import Rewriter.MessageModel
import Rewriter.State
import qualified Parser.ParseTree as PT
import qualified Rewriter.FunctionsStateless as Stateless

import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Data.List (union)
import Control.Monad.State

    
{- | Returns if a message is synthesizable from the current knowledge. 
     See 'Rewriter.FunctionsStateless.inSynth' for details. -}
inSynth :: Message -> RoleState Bool 
inSynth msg = do 
    chi <- getChi 
    return $ Stateless.inSynth chi msg   

{- | Returns the constructive form of a message under the current 
     knowledge. Does not alter state.
     See the thesis for more information on the /constructive form/. 
   
     There is another version of this function at
     'Rewriter.FunctionsStateless.cf', which does exactly the same 
     thing, however, without using state. Since this version here
     works with state, it can also cope with unconstructable
     messages (since it can create new ghost messages). -}
cf :: Message -> RoleState Message
cf msg@(Str _) = do 
    return msg
cf msg@(K _ _) = do 
    return msg
cf msg@(Pk _) = do 
    return msg
cf msg@(Sk _) = do 
    return msg
cf msg@(Var _) = do 
    return msg
cf msg@(Concat m1 m2) = do 
    cf_m1 <- cf m1
    cf_m2 <- cf m2
    return $ conc cf_m1 cf_m2
cf msg@(Senc m k) = do 
    has_m <- inSynth m
    has_k <- inSynth k
    if has_m && has_k then do 
        cf_m <- cf m 
        cf_k <- cf k 
        return $ senc cf_m cf_k
    else do 
        gamma msg 
cf msg@(Aenc m k) = do
    has_m <- inSynth m
    has_k <- inChi k
    if has_m && has_k then do 
        cf_m <- cf m 
        cf_k <- cf k 
        return $ aenc cf_m cf_k
    else do 
       gamma msg 
cf msg@(Hash h) = do 
    has_h <- inSynth h 
    if has_h then do 
        cf_h <- cf h 
        return $ Hash cf_h 
    else do 
        gamma msg
cf msg@(Fun n ms) = do 
    inChi_msg <- inChi msg
    syn_ms <- mapM inSynth ms
    has_ms <- return $ and syn_ms
    p <- getProtocol
    if inChi_msg then 
        gamma msg
    else if has_ms then do
        cf_ms <- mapM cf ms
        return $ Fun n cf_ms
    else do 
        gamma msg
cf msg@(Exp m1 m2) = do 
    cfExpTerm msg
cf msg@(Mul ms) = do 
    cfMulTerm msg
cf One = do 
    return One
cf (Gamma _ msg) = do 
    cf msg       
        
{- | See 'Rewriter.FunctionsStateless.cfMulTerm' for details. This function
     does exactly the same thing, only working with state, which 
	 means that it can create new gamma messages. -}
cfMulTerm :: Message -> RoleState Message
cfMulTerm msg@(Mul facs) = 
    let
        {- Takes a message 'submsg' and returns the constructive form 
           of 'msg' as 'submsg * (msg `divide` submsg)'. 
           Only call this if msg is divisible by submsg and 
           'msg `divide` submsg' is constructible. -}
        constrMul :: Message -> RoleState Message
        constrMul submsg = do 
            div <- return $ divide msg submsg
            cf_div <- cf div
            return $ multiply submsg cf_div
            
        factors = MultiSet.toList facs
    in do 
        chi <- getChi 
        messages <- return $ Set.toList chi 
        cmul <- return $ any (Stateless.constrableMul chi msg) messages
        cmulf <- return $ any (\f -> Stateless.inSynth chi f && 
                               Stateless.constrableMul chi msg f) factors
        if cmul then do 
            h <- return $ head $ 
                filter (Stateless.constrableMul chi msg) messages
            constrMul h 
        else if cmulf then do 
            h <- return $ head $ 
                filter (\f -> Stateless.inSynth chi f && 
                        Stateless.constrableMul chi msg f) factors
            constrMul h
        else 
            gamma msg
        
{- | See 'Rewriter.FunctionsStateless.cfExpTerm' for details. This function
     does exactly the same thing, only working with state, which 
	 means that it can create new gamma messages. -}    
cfExpTerm :: Message -> RoleState Message
cfExpTerm msg@(Exp m1 m2) = 
    let  
        {- Takes a message 'submsg' and returns the constructive form 
           of 'msg' as 'submsg ^ (msg `divide` submsg)'. 
           Only call this if msg is left reducible by submsg and 
           'msg `reducel` submsg' is constructible. -}
        constrExpl :: Message -> RoleState Message
        constrExpl submsg = do 
            red <- return $ reducel msg submsg
            cf_red <- cf red
            return $ exponentiate submsg cf_red
                
        {- Takes a message 'submsg' and returns the constructive form 
           of 'msg' as '(msg `divide` submsg) ^ submsg'. 
           Only call this if msg is right reducible by submsg and 
           'msg `reducer` submsg' is constructible. -}
        constrExpr :: Message -> RoleState Message
        constrExpr submsg = do 
            red <- return $ reducer msg submsg
            cf_red <- cf red
            return $ exponentiate cf_red submsg
    in do 
        chi <- getChi 
        messages <- return $ Set.toList chi 
        cexpl <- return $ any (Stateless.constrableExpl chi msg) messages
        cexpr <- return $ any (Stateless.constrableExpr chi msg) messages
        synm1 <- inSynth m1
        synm2 <- inSynth m2
        if cexpl then do 
            h <- return $ head $ 
                filter (Stateless.constrableExpl chi msg) messages
            constrExpl h 
        else if synm1 && synm2 then do
            cfm1 <- cf m1
            cfm2 <- cf m2
            return $ exponentiate cfm1 cfm2
        else if cexpr then do 
            h <- return $ head $ 
                filter (Stateless.constrableExpr chi msg) messages
            constrExpr h 
        else 
            gamma msg
        
{- | Applies the analysis rules to a message under the current knowledge and
     returns a list of (sub-)messages that can be parsed from that message. 
     Check the thesis for details on /analysis/. 
     A message is only parsed as far as possible under the
     current knowledge; knowledge that has been gained from the analysis
     is not used to further analyze the message. For example, in 
     @\<m1 . senc {m2}m1\>, m2@ is only extracted if @m1@ was already in the 
     knowledge since 'analyzeOnce' only returns the analyzed messages
     but does not add it to the knowledge.

     This function may alter the symbol table, 
	 the current knowledge, however, is left untouched. -}                   
analyzeOnce :: Message -> RoleState (Set.Set Message)
analyzeOnce msg@(Gamma _ m) = 
    -- When analyzing a gamma term we just analyze the message it
    -- represents. 
    analyzeOnce m
analyzeOnce msg@(Str _) = do 
    -- Strings are always constructible, even if they are not in the
    -- knowledge. Therefore we never store strings. 
    return Set.empty
analyzeOnce (Concat m1 m2) = do 
    -- Look at both sub-messages and return what can be analyzed from 
    -- them.  
    r1 <- analyzeOnce m1
    r2 <- analyzeOnce m2
    return $ r1 `Set.union` r2
analyzeOnce msg@(Senc m k) = do 
    hask <- inSynth k 
    hasm <- inSynth m
    if hask && hasm then do 
        -- We have both the key k and the message m to encrypt, that is, we
        -- can construct (Senc m k) from m and k. Therefore, there is no 
        -- need to store (Senc m k) as a whole. 
        return Set.empty
    else if hask then do 
        -- We can construct the key k. We can therefore extract m from 
        -- (Senc m k), i.e. we have m and k. Therefore we can construct 
        -- (Senc m k) as a whole. m may be new knowledge, therefore we
        -- analyze it. 
        analyzeOnce m
    else do 
        -- We know neither m nor k and therefore we can neither construct
        -- (Senc m k) nor can we look under the encryption and extract m. 
        -- In this case, we need to store the whole message. 
        ga <- gamma msg
        return $ Set.singleton ga
analyzeOnce msg@(Aenc m k@(Pk r)) = do 
    hasm <- inSynth m
    hask <- inChi k 
    hasinvk <- inChi (Sk r)
    if hask && hasm then do 
        -- We have both the key and the message and can therefore
        -- construct (Aenc m (Pk r)) from other messages. (Aenc m (Pk r))
        -- therefore contains no new knowledge. 
        return Set.empty
    else if hasinvk then do 
        -- We have the inverse key but not the key that was used for 
        -- encryption. Therefore, we cannot construct (Aenc m (Pk r))
        -- and therefore have to remember it. With the inverse key, we
        -- can also look under the encryption and extract m which may be 
        -- new knowledge, so we analyze it. 
        res1 <- analyzeOnce m
        res2 <- gamma msg
        return $ Set.insert res2 res1
    else do 
        -- We know neither key and therefore cannot extract any information
        -- from (Aenc m (Pk r)). We just remember (Aenc m (Pk r)). 
        ga <- gamma msg
        return $ Set.singleton ga
analyzeOnce msg@(Aenc m k@(Sk r)) = do 
    -- This case is analogous to (Aenc m (Pk r)). 
    hasm <- inSynth m
    hask <- inChi k 
    hasinvk <- inChi (Pk r)
    if hask && hasm then 
        return Set.empty
    else if hasinvk then do 
        res1 <- analyzeOnce m
        res2 <- gamma msg
        return $ Set.insert res2 res1
    else do 
        ga <- gamma msg
        return $ Set.singleton ga
analyzeOnce msg = do 
    -- All other messages cannot be analyzed. We just remember
    -- the message itself. 
    ga <- gamma msg
    return $ Set.singleton ga
   

     
        
 
{- | An infinite list of symbol names that are used 
     to represent ghost messages. Each symbol is unique within the list. -}
symbolStore :: [Identifier]
symbolStore = greekLetters ++ gammaInfinite
    where 
        greekLetters =  ["alpha", "beta", "delta", "epsilon", 
                         "zeta", "eta", "theta", "iota", "kappa", 
                         "lambda", "my", "ny", "xi", "omikron", "pi", 
                         "rho", "sigma", "tau", "ypsilon", "phi", 
                         "chi", "psi", "omega"]                   
        gammaInfinite :: [String]
        gammaInfinite = build 1
            where build :: Int -> [String]
                  build i = (map (\c -> c ++ (show i)) greekLetters) ++ (build (i+1))
   
{- | Returns the ghost (gamma abstraction) of a message. 
     If the message is atomic, the message itself will be returned 
     (atomic messages are never put under a gamma abstraction). 
     If the message is non-atomic, it will be put under a gamma 
     abstractions. Ensures that the same messages will always get
     the same symbol (as long as in the same 'RoleState'). -}
gamma :: Message -> RoleState Message
gamma m = 
    let 
        {- | Puts a message under a gamma abstraction if the message is 
             non-atomic. If it is atomic, the message itself is returned. -}
        newGamma :: Message -> RoleState Message
        newGamma msg 
            | isAtomic msg =
                  return msg
            | otherwise = do 
                  s <- nextSymbol 
                  message <- return (Gamma s msg)  
                  addGhostMessage message
                  return message 
    in do 
        ghosts <- getGhostMessages
        if m `Set.member` ghosts then do 
            return $ head $ filter (==m) (Set.toList ghosts)
        else do 
            newGamma m
  
