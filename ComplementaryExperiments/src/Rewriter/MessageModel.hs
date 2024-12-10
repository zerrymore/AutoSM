{- |
Description: Provides functions for deconstructing multiplicative messages and exponentiation messages. 

A&B can work with multiplication and exponentiation in a group (to a 
certain extent). 

Multiplicative messages and exponentiation messages are stored in 
a canonical form (see 'Parser.Message.multiply' for details). 

In order to send a message, a principal must be able to construct 
a message from his current knowledge. Depending on that knowledge, 
there may be many different ways of constructing a message. For 
example, the message @Mul {Exp m1 m2, m3}@ can be constructed directly
from @m1@, @m2@ and @m3@ if they are known. But it is also possible 
that only @m3@ is known. In this case, the message can still be 
constructed, if @Exp m1 m2@ is known as a ghost message, or if the 
whole message @Mul {Exp m1 m2, m3}@ is known as a ghost message. 

This module provides functions for taking multiplicative messages
and exponentiation messages apart into their sub-messages. They
are then used elsewhere for finding ways of constructing such 
messages. 
-}
module Rewriter.MessageModel where

import qualified Data.MultiSet as MultiSet

import Parser.Message


{- | Checks if a message is divisible by another message. 
     See 'divide' for details. -}
divisible :: Message -> Message -> Bool
divisible (Gamma _ m1) msg2 = 
    divisible m1 msg2
divisible msg1 (Gamma _ m2) = 
    divisible msg1 m2
divisible (Mul m1) (Mul m2) = 
    let
        Mul ms1 = gammaLess (Mul m1) 
        Mul ms2 = gammaLess (Mul m2) 
    in 
        MultiSet.isSubsetOf ms2 ms1
divisible (Mul m1) msg2 = 
    let
        Mul ms1 = gammaLess (Mul m1) 
        ms2 = gammaLess msg2
    in 
    MultiSet.member ms2 ms1
divisible (Exp b1 e1) (Exp b2 e2) = 
    divisible b1 b2 && e1 == e2
divisible m1 m2 
    | m1 == m2 = True
    | otherwise = False
      
{- | Divides one message by another. 
     Call this only when the messages are divisible.
	 
	 A message @a@ is divisible by message @b@ if there
	 is a message @c@ such that @Mul{b, c} = a@. 
	 If a principal can construct @b@ and @c@, he can 
	 therefore also construct @a@. 
	 
	 The function 'divide' takes such messages @a@ and @b@
	 and returns the corresponding @c@. -}
divide :: Message -> Message -> Message
divide (Gamma _ m1) msg2 = 
    -- We are interested in the message under the gamma abstraction
    divide m1 msg2
divide msg1 (Gamma _ m2) = 
    -- We are interested in the message under the gamma abstraction
    divide msg1 m2
divide msg1@(Mul m1) msg2@(Mul m2) = 
    let
        Mul ms1 = gammaLess msg1
        Mul ms2 = gammaLess msg2
    in 
    if msg1 == msg2 then 
        One
    else if ms2 `MultiSet.isSubsetOf` ms1 then 
        mul $ ms1 MultiSet.\\ ms2
    else
        error "Not divisible."
divide msg1@(Exp b1 e1) msg2@(Exp b2 e2)
    | msg1 == msg2 =
        One
    | e1 == e2 && b1 `divisible` b2 = 
        exponentiate (b1 `divide` b2) e1
    | otherwise = 
        error "Not divisisble."
divide (Mul m1) msg2 = 
    let
        Mul ms1 = gammaLess (Mul m1) 
        ms2 = gammaLess msg2
    in 
        if MultiSet.member ms2 ms1 then
            mul $ MultiSet.delete ms2 ms1
        else
            error "Not divisisble."
divide m1 m2 
    | m1 == m2 = 
        One
    | otherwise = 
        error "Not divisible."

{- | Returns if a message is left reducible by another message. 
     See 'reducel' for details. -}
reduciblel :: Message -> Message -> Bool 
reduciblel msg1 (Gamma _ m2) = reduciblel msg1 m2
reduciblel (Gamma _ m1) msg2 = reduciblel m1 msg2
reduciblel (Exp b1 e1) (Exp b2 e2) = b1 == b2 && e1 `divisible` e2
reduciblel (Exp b1 e1) m2 = b1 == m2
reduciblel _ _ = False
    
{- | Returns the left reduction of a message with another message.
     Only call this if the first message is left reducible by the second
     message.
	 
	 A message @a@ is left reducible by a message @b@ if there is 
	 a message @c@ such that @a = Exp b c@. If a principal can 
	 construct @b@ and @c@, he can therefore also construct @a@. 
	 
	 The function 'reducel' takes such @a@ and @b@ and returns the 
	 corresponding @c@.-}
reducel :: Message -> Message -> Message
reducel msg1 (Gamma _ m2) = reducel msg1 m2
reducel (Gamma _ m1) msg2 = reducel m1 msg2
reducel msg1@(Exp b1 e1) msg2@(Exp b2 e2) 
    | b1 == b2 && e1 `divisible` e2 = 
        (e1 `divide` e2)
    | otherwise = 
        error "Not left reducible."
reducel (Exp b1 e1) m2 
    | b1 == m2 = 
        e1
reducel _ _ = 
    error "Not left reducible."
    

{- | Returns if a message is right reducible by another message.
     See 'reducer' for details. -}
reducibler :: Message -> Message -> Bool 
reducibler msg1 (Gamma _ m2) = reducibler msg1 m2
reducibler (Gamma _ m1) msg2 = reducibler m1 msg2
reducibler (Exp b e) m2 = e `divisible` m2
reducibler _ _ = False
    
{- | Returns the right reduction of a message with another message.
     Only call this if the first message is right reducible by the 
     second message. 
	 
	 A message @a@ is right reducible by another message if there is
	 a message @c@ such that @a = Exp c b@. If a principal 
	 can construct @b@ and @c@, he can therefore also construct @a@. 
	 
	 The function 'reducer' takes such @a@ and @b@ and returns the 
	 corresponding @c@. --}    
reducer :: Message -> Message -> Message 
reducer msg1 (Gamma _ m2) = reducer msg1 m2
reducer (Gamma _ m1) msg2 = reducer m1 msg2

reducer (Exp b e) m2 
    | e `divisible` m2 = 
        exponentiate b (e `divide` m2)
    | otherwise =
        error "Not right reducible."
reducer _ _ = 
        error "Not right reducible."      
        
