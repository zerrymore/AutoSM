{-|
Description: Definition of 'Parser.Message.Message' and related functions.

This module contains the definition of 
the data type 'Message' and some helpful functions for 
working with messages. 
-}
module Parser.Message where

import Data.List (intercalate, union, sort, (\\)) 
import Parser.Basic
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set

{------------------------------------------------------------------------------}


{- | A message. Messages should always be in canonical form, otherwise
     some functions might return wrong results. Make sure to always use
     the custom construction functions instead of the type constructors
     directly to ensure that the created messages are in canonical form.
     See the thesis for more details on the canonical form (section 2.4.4). -}
data Message 
    {- | A ghost message. 
	
	     * The 1st parameter is the representative of the ghost, 
	       for example \'gamma\'. 

		 * The 2nd parameter is the message that this ghost represents. -}
    = Gamma Identifier Message
    -- | A variable.
    | Var Identifier
    -- | A string (i.e., a constant number). 
    | Str String
    {- | Concatenation of two messages. Use the function 'conc' to create
         the concatenation of two messages to ensure that the result
         is in canonical form. Use 'Concat' for pattern matching only. -}   
    | Concat Message Message
    {- | Asymmetric encryption. 

	     * 1st parameter: Message that is encrypted. 

		 * 2nd parameter: Encryption key. 
		 
		 Use the function 'aenc' to construct asymmetrically encrypted messages
		 to ensure that the resulting message is in canonical form. 
		 Use 'Aenc' for pattern matching only.-} 
    | Aenc Message Message
    {- | Symmetric encryption. 

	     * 1st parameter: Message that is encrypted. 

		 * 2nd parameter: Encryption key. Use only key of the shape 
		   (Sk _) and (Pk _) as encryption keys. 
		 
		 Use the function 'senc' to construct symmetrically encrypted messages
		 to ensure that the resulting message is in canonical form. 
		 Use 'Senc' for pattern matching only. -}  
    | Senc Message Message
    -- | Hash of a message. 
    | Hash Message
    {- | Multiplication. Use the function 'multiply' for constructing
         the multiplication of two terms to ensure that the result is in 
         canonical form. Use 'Mul' for patern matching only. -}
    | Mul Factors
    {- | Exponentiation of the first with the second argument. 
         Use 'exponentiate' to generate the exponentiation of two messages
         to ensure that the result is in canonical form. Use 'Exp' for 
         pattern matching only. -} 
    | Exp Message Message
    -- | Public key of a participant. 
    | Pk RoleName
    -- | Secret key of a participant. 
    | Sk RoleName
    -- | Shared symmetric key of two participants. 
    | K RoleName RoleName
    {- | Function application. 

	     * 1st parameter: Name of the function (function names are unique). 

		 * 2nd parameter: Argument list. -}
    | Fun Identifier [Message]
    -- | The 1 message (neutral element of multiplication and exponentiation). 
	--   This message type is used internally only. 
    | One

{- | The factors of a multiplication in a message are represented by 
     a multiset. -}
type Factors = MultiSet.MultiSet Message

{- | We explained in the thesis that we can use equality by shape if
     the messages are in canonical form (Proposition 2.13) and for this
     reason we work with messages in canonical form here. 
     
     However, ghost messages are a special case. For example, 
     the three messages @Gamma \"alpha\" (Str \"hello\")@, 
	 @Gamma \"beta\" (Str \"hello\")@ and @Str \"hello\"@ 
	 are equal since they all represent the message @Str \"hello\"@.
	 In detail: 

	 * Two messages are equal if they have exactly the same shape. 

	 * A message @m@ is equal to the message @Gamma i m@ for all @i@. 

	 * A message @Gamma i1 m@ is equal to  the message
	   @Gamma i2 m@ for all @i1@ and @i2@.

	 * All other messages are not equal. -}
instance Eq Message where
    (Gamma _ m1) == m2 = 
        m1 == m2
    m1 == (Gamma _ m2) =
        m1 == m2
    (Str s1) == (Str s2) = 
        s1 == s2
    (Var m1) == (Var m2) = 
        m1 == m2
    m1@(Concat m11 m12) == m2@(Concat m21 m22) = 
        m11 == m21 && m12 == m22
    (Aenc m1 k1) == (Aenc m2 k2) = 
        m1 == m2 && k1 == k2
    (Senc m1 k1) == (Senc m2 k2) =
        m1 == m2 && k1 == k2
    (Hash m1) == (Hash m2) = 
        m1 == m2
    m1@(Mul _) == m2@(Mul _) = 
        let 
            Mul ms1 = gammaLess m1
            Mul ms2 = gammaLess m2
        in 
            ms1 == ms2
    (Exp m11 m12) == (Exp m21 m22) = 
        m11 == m21 && m12 == m22
    (Pk r1) == (Pk r2) = 
        r1 == r2
    (Sk r1) == (Sk r2) = 
        r1 == r2
    (K r11 r12) == (K r21 r22) = 
        r11 == r21 && r12 == r22
    (Fun n1 m1) == (Fun n2 m2) = 
        n1 == n2 && m1 == m2 
    One == One = 
        True
    _ == _ = 
        False
   
{- | Removes all ghost abstraction from a message. -}
gammaLess :: Message -> Message 
gammaLess (Gamma _ m) = gammaLess m 
gammaLess (Concat m1 m2) = 
    Concat (gammaLess m1) (gammaLess m2) 
gammaLess (Aenc m1 m2) = 
    aenc (gammaLess m1) (gammaLess m2) 
gammaLess (Senc m1 m2) = 
    senc (gammaLess m1) (gammaLess m2) 
gammaLess (Exp m1 m2) = 
    exponentiate (gammaLess m1) (gammaLess m2) 
gammaLess (Fun n ms) = 
    Fun n $ map gammaLess ms
gammaLess (Hash m) = 
    Hash $ gammaLess m
gammaLess (Mul ms) = 
    let
        msl = MultiSet.toList ms
        (a:as) = map gammaLess msl 
    in
        foldr multiply a as
gammaLess m =
    m      
   
{- | 'Ord' is defined in order to allow storage of messages in ordered 
     sets ('Data.Set.Set'). 

     'Ord' is consistent with 'Eq', that is, if @m1 == m2@ then 
     @m1 \<= m2@ and @m1 >= m2@ but not  @m1 \< m2@ and not  @m1 > m2@
	 for any two messages @m1@ and @m2@.
	 
	 The messages types are ordered as follows (from low to high):

	 1. Strings ('Str')
	 	
	 2. Variables ('Var')

	 3. Secret keys ('Sk')

	 4. Private keys ('Pk')

	 5. Symmetric keys ('K')

	 6. Concatenation ('Concat')

	 7. Symmetric encryption ('Senc')

	 8. Asymmetric encryption ('Aenc')

	 9. Hashed messages ('Hash')
	 
	 10. Function application ('Fun')

	 11. Multiplication ('Mul')

	 12. Exponentiation ('Exp')

	 13. The message 1 ('One')
	 
	 For ghost messages, the value represented by the ghost is used for 
	 comparison. For example, @Gammma \"alpha\" (Str \"a\") < Str \"b\"@
	 since @Str \"a\" < Str \"b\"@.

	 For comparison of two equal message types, the parameters
	 are compared (first parameter, second parameter, ...). 
	 -} 
instance Ord Message where
    (Gamma _ m1) <= m2 = 
        m1 <= m2
    m1 <= (Gamma _ m2) = 
        m1 <= m2
    (Str s1) <= (Str s2) = 
        s1 <= s2
    (Var m1) <= (Var m2) = 
        m1 <= m2
    (Concat m11 m12) <= (Concat m21 m22) = 
        [m11, m12] <= [m21, m22]
    (Aenc m1 k1) <= (Aenc m2 k2) = 
        [m1, k1] <= [m2, k2] 
    (Senc m1 k1) <= (Senc m2 k2) = 
        [m1, k1] <= [m2, k2] 
    (Hash m1) <= (Hash m2) = 
        m1 <= m2 
    m1@(Mul _) <= m2@(Mul _) = 
        let 
            Mul ms1 = gammaLess m1
            Mul ms2 = gammaLess m2
        in 
            ms1 <= ms2
    (Exp m11 m12) <= (Exp m21 m22) =
        [m11, m12] <= [m21, m22]
    (Pk r1) <= (Pk r2) = 
        r1 <= r2
    (Sk r1) <= (Sk r2) = 
        r1 <= r2
    (K r11 r12) <= (K r21 r22) = 
        [r11, r12] <= [r21, r22]
    (Fun n1 m1) <= (Fun n2 m2) = 
        if n1 == n2 then m1 <= m2 else n1 <= n2
    a <= b = ordnum a <= ordnum b
        where
            ordnum :: Message -> Integer
            ordnum (Gamma _ m) = ordnum m 
            ordnum (Str _) = 1
            ordnum (Var _) = 2
            ordnum (Sk _) = 3
            ordnum (Pk _) = 4
            ordnum (K _ _) = 5
            ordnum (Concat _ _) = 6
            ordnum (Senc _ _) = 7
            ordnum (Aenc _ _) = 8
            ordnum (Hash _) = 9
            ordnum (Fun _ _) = 10
            ordnum (Mul _) = 11
            ordnum (Exp _ _) = 12
            ordnum One = 13

instance Show Message where
    show (Gamma i m)     = "Ghost[" ++ i ++ ":" ++ 
                           show m ++ "]"
    show (Var m)         = m
    show (Str m)         = "'" ++ m ++ "'"
    --show (Concat m1 m2) = "<" ++ show m1 ++ " . "  ++ show m2 ++ ">"
    show (Concat m1 m2)  = "<" ++ subcon m1 ++ " . " ++ subcon m2 ++ ">"
        where subcon (Concat m1 m2) = subcon m1 ++ " . " ++ subcon m2
              subcon (m)            = show  m
    show (Aenc m1 m2)    = "aenc{" ++ show m1 ++ "}" ++ show m2
    show (Senc m1 m2)    = "senc{" ++ show m1 ++ "}" ++ show m2
    show (Hash m)        = "h(" ++ show m ++ ")"
    show (Mul ms)        = "(" ++ intercalate " * " 
                           (map show (MultiSet.toList ms)) ++ ")"
    show (Exp m1 m2)     = "(" ++ show m1 ++ "^" ++ show m2 ++ ")"
    show (Pk p)          = "pk(" ++ p ++ ")"
    show (Sk p)          = "sk(" ++ p ++ ")"
    show (K p1 p2)       = "k(" ++ p1 ++ ", " ++ p2 ++ ")"
    show (Fun n m)       = n ++ "(" ++ 
                           (intercalate ", " . map show) m ++ ")"
    show One             = "1"
   
   
{------------------------------------------------------------------------------}

{- | Returns if a message is atomic (Definition 2.2. of the thesis), 
     that is, if it is a variable ('Var'), a string ('Str'), a key ('Pk', 
	 'Sk' or 'K'), or 'One'.
	 
	 For ghost messages, this function returns if the value
	 represented by the ghost is atomic, i.e. 
	 
	 isAtomic (Gamma _ m) = isAtomic m. -} 
isAtomic :: Message -> Bool  
isAtomic (Gamma _ m) = isAtomic m    
isAtomic (Var m)     = True
isAtomic (Str m)     = True
isAtomic (Pk p)      = True
isAtomic (Sk p)      = True
isAtomic (K p1 p2)   = True
isAtomic One         = True
isAtomic otherwise   = False
    
{- | Returns if a message is a ghost message ('Gamma') -}    
isGamma :: Message -> Bool 
isGamma (Gamma _ _ ) = True
isGamma otherwise    = False    

{- | Returns if a message is a variable ('Var'). -}
isVar :: Message -> Bool 
isVar (Var _)   = True
isVar otherwise = False

{- | Returns if a message is a string ('Str'). -}
isStr :: Message -> Bool 
isStr (Str _)   = True
isStr otherwise = False

{- | Returns if a message is a public key ('Pk'). -}
isPk :: Message -> Bool 
isPk (Pk _)    = True
isPk otherwise = False
   
{- | Returns if a message is a secret key ('Sk'). -}
isSk :: Message -> Bool 
isSk (Sk _)    = True
isSk otherwise = False   
    
{- | Returns if a message is a symmetric key ('K'). -}
isK :: Message -> Bool 
isK (K _ _)   = True
isK otherwise = False    

{- | Returns if a message is the hash of a message ('Hash') -}
isHash :: Message -> Bool 
isHash (Hash _)  = True
isHash otherwise = False

{- | Returns if a message is the application of a function ('Fun'). -}
isFun :: Message -> Bool 
isFun (Fun _ _) = True
isFun otherwise = False

{- | Returns if a message is a symmetric encryption message ('Senc'). -}
isSenc :: Message -> Bool 
isSenc (Senc _ _) = True
isSenc otherwise  = False

{- | Returns if a message is an asymmetric encryption message ('Aenc'). -}
isAenc :: Message -> Bool 
isAenc (Aenc _ _) = True
isAenc otherwise  = False

{- | Returns if a message is a multiplication ('Mul'). -}
isMul :: Message -> Bool 
isMul (Mul _)   = True
isMul otherwise = False

{- | Returns if a message is the exponentiation of two messages ('Exp'). -}
isExp :: Message -> Bool
isExp (Exp _ _) = True
isExp otherwise = False

{- | Returns if a message is the message 1 ('One').  

     Note that this function does not take into 
     account any algebraic properties of the message, it works
     simply by pattern matching. 
     For example, @aenc{aenc{One}pk(A)}sk(A)@ would return 'False'
     even though it evaluates to 'One'. -}
isOne :: Message -> Bool 
isOne One       = True
isOne otherwise = False
    
{- | Returns if a message is a concatenation ('Concat') of two messages -}
isConcat :: Message -> Bool 
isConcat (Concat _ _) = True
isConcat otherwise    = False

{- | Returns the representative of a ghost symbol. -}
getSymbol :: Message -> String
getSymbol (Gamma s _) = s
getSymbol otherwise   = error "Not a ghost message."

{------------------------------------------------------------------------------}

{- | Returns the set of the atomic sub-messages of a message
     (see 'isAtomic' for a definition of /atomic/). The sub-messages are
	 evaluated recursively. 
	 
	 The sub-messages of a non-atomic message or a ghost message
	 are defined as follows: 

	 * The sub-message of @Gamma i m@ is @m@. 
	 
	 * The sub-messages of @Concat m1 m2@ are @m1@ and @m2@.
	 
	 * The sub-messages of @Aenc m k@ are @m@ and @k@.
	 
	 * The sub-messages of @Senc m k@ are @m@ and @k@. 
	 
	 * The sub-message of @Hash m@ is @m@. 
	 
	 * The sub-messages of @Mul ms@ are all the messages
	   in the set @ms@. 
	 
	 * The sub-messages of @Exp m1 m2@ are @m1@ and @m2@. 
	 
	 * The sub-messages of @Fun _ ms@ are all the messages 
	   in the list @ms@. 
	   
	 * All other messages are atomic and do not have sub-messages. -}  
sub :: Message -> (Set.Set Message)
sub (Gamma i m) = sub m 
sub msg@(Var m) = Set.singleton msg
sub msg@(Str m) = Set.singleton msg
sub msg@(Pk p) = Set.singleton msg
sub msg@(Sk p) = Set.singleton msg
sub One = Set.singleton One
sub msg@(K p1 p2) = Set.singleton msg
sub (Concat m1 m2) = (sub m1) `Set.union` (sub m2)
sub (Aenc m k) = (sub m) `Set.union` (sub k)
sub (Senc m k) = (sub m) `Set.union` (sub k)
sub (Hash m) = sub m 
sub (Mul ms) = 
    let
        ms_set = MultiSet.toSet ms
        ms_sub = Set.map sub ms_set
    in 
        Set.fold Set.union Set.empty ms_sub
sub (Exp m1 m2) = (sub m1) `Set.union` (sub m2)
sub (Fun n m) = 
    let
        m_set = Set.fromList m
        m_sub = Set.map sub m_set
    in 
        Set.fold Set.union Set.empty m_sub


{- | Returns the set of all sub-messages of a message (including
     the message itself). For a definition of /sub-message/, see
	 'sub'.-}
allSubs :: Message -> (Set.Set Message)
allSubs msg@(Gamma i m) = 
    Set.insert msg $ allSubs m 
allSubs msg@(Var m) = 
    Set.singleton msg
allSubs msg@(Str m) = 
    Set.singleton msg
allSubs msg@(Pk p) =
    Set.singleton msg
allSubs msg@(Sk p) = 
    Set.singleton msg
allSubs One = 
    Set.singleton One
allSubs msg@(K p1 p2) = 
    Set.singleton msg
allSubs msg@(Concat m1 m2) = 
    Set.insert msg $ (allSubs m1) `Set.union` (allSubs m2)
allSubs msg@(Aenc m k) = 
    Set.insert msg $ (allSubs m) `Set.union` (allSubs k)
allSubs msg@(Senc m k) = 
    Set.insert msg $ (allSubs m) `Set.union` (allSubs k)
allSubs msg@(Hash m) = 
    Set.insert msg $ allSubs m 
allSubs msg@(Mul ms) = 
    let
        ms_set = MultiSet.toSet ms
        ms_sub = Set.map allSubs ms_set
    in 
        Set.insert msg $ Set.fold Set.union Set.empty ms_sub
allSubs msg@(Exp m1 m2) = 
    Set.insert msg $ (allSubs m1) `Set.union` (allSubs m2)
allSubs msg@(Fun n m) = 
    let
        m_set = Set.fromList m
        m_sub = Set.map allSubs m_set
    in 
        Set.insert msg $ Set.fold Set.union Set.empty m_sub



{------------------------------------------------------------------------------}
{- FUNCTIONS FOR COMBINING MULTIPLICATION AND EXPONENTIATION -}    
{- | Takes two messages in canonical form and returns their 
     multiplication in canonical form. 

	 A message is in its canonical form if: 

	 * The factors are not nested, that is, we have
	   @Mul {m1, m2,  m3}@ rather than Mul{m1, Mul{m2, m3}}. 

     * Messages with the same exponent are grouped, that is, 
	   we have @Exp (Mul {m1, m2}) m3@ rather than
	   @Mul {Exp m1 m3, Exp m2 m3}@. Note that this is no requirement
	   of the canonical form that was defined in the thesis. 

	 * There are no multiplications with only one factor, 
	   that is, we have @m@ rather than @Mul {m}@. 
	   
	 * There is no @One@ factor. 
	  
	 See Definition 2.10 in the thesis for details. 

	 If the input messages are not in canonical form, the
	 result may be wrong. 

 -}
multiply :: Message -> Message -> Message
multiply One m2 = m2
multiply m1 One = m1
multiply (Mul m1) (Mul m2) = mul $ merge $ m1 `MultiSet.union` m2
multiply m1 (Mul m2) = mul $ merge $ MultiSet.insert m1 m2
multiply (Mul m1) m2 = mul $ merge $ MultiSet.insert m2 m1
multiply m1 m2 = mul $ merge $ MultiSet.insert m1 $ MultiSet.singleton m2
  
{- | Takes a multiset of factors  and turns them into a 
     multiplicative message. If there is only one factor 
	 in the multiset, no multiplication will be applied: 
	 
	 prop> mul {m1, m2, m2} = Mul {m1, m2, m2.}
	 prop> mul {m} = m.-}    
mul :: Factors -> Message
mul m 
    | MultiSet.size m == 1 = MultiSet.findMin m 
    | otherwise            = Mul m 
    
{- | Concatenate two messages observing the rules of the 
     canonical form. -}
conc :: Message -> Message -> Message
conc (Concat m11 m12) m2 = conc m11 (conc m12 m2)
conc m1 m2 = Concat m1 m2
        
{- | Aymmetrically encrypt a message observing the rules of the
     canonical form. -}
aenc :: Message -> Message -> Message
aenc enc@(Aenc m (Pk p1)) k@(Sk p2) = 
    if p1 == p2 then m 
    else Aenc enc k
aenc enc@(Aenc m (Sk p1)) k@(Pk p2) = 
    if p1 == p2 then m
    else Aenc enc k
aenc m k = 
    Aenc m k
    
{- | Symmetrically encrypt a message observing the rules of the 
     canonical form. -}
senc :: Message -> Message -> Message
senc enc@(Senc m ksub) k = 
    if k == ksub then m 
    else Senc enc k
senc m k = 
    Senc m k 
        
{- | Takes a multiset of factors and groups the factors 
     that have the same exponent under one exponentiation
	 ('Exp'). For example:  
	 @merge {Exp a a, Exp b a, Exp c d} = {Exp (Mul {a, b}) a, Exp c  d}@ -}
merge :: Factors -> Factors 
merge factors = 
    let 
        expMatches exp (Exp _ e) = exp == e
        expMatches _ _           = False
        
        purgeList :: [Message] -> [Message] 
        purgeList (msg@(Exp b e):fs) = 
            let
                matchin = filter (expMatches e) fs
                
                rest = purgeList (fs \\ matchin)
                
                merge' :: [Message] -> Message
                merge' (e:[]) = e
                merge' ((Exp b0 _):es) = 
                   let
                       (Exp b1 e) = merge' es
                   in 
                       Exp (multiply b0 b1) e
                        
            in 
                if matchin == [] then 
                    msg:rest
                else
                    (merge' (msg:matchin)) : rest
        purgeList (f:fs) = 
            f : (purgeList fs) 
        purgeList [] = []
    in 
        MultiSet.fromList $ purgeList $ MultiSet.toList factors 
    
{- | Takes two messages in canonical form and 
     returns their exponentiation in canonical 
	 form. See Definition 2.10 in the thesis for
	 details of the canonical form. -}
exponentiate :: Message -> Message -> Message
exponentiate One _ = One
exponentiate m One = m
exponentiate (Exp m11 m12) m2 = 
    Exp m11 (multiply m12 m2) 
exponentiate m1 m2 = 
    Exp m1 m2  
    
    
{- | 'mlist ls' is a shorthand for @Mul (MultiSet.fromList ls)@ -}
mlist :: [Message] -> Message
mlist msgs = Mul $ MultiSet.fromList msgs


