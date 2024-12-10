{- | 
Description: Checks to ensure that there are no identifiers that differ only in case. 

One should not use identifiers in Tamarin that differ only in case, 
for example \'message\' and \'Message\'. This module provides a check 
to ensure that there are no two such identifiers in a protocol. -}
module Checker.CaseInsensitivity 
(checkCasesConsistent)
where

import Data.List (intercalate, find, union)

import qualified Rewriter.IR as IR
import Parser.Message
import Parser.Basic

import Data.Monoid
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Checker.CheckResult
import Data.Char (toUpper)
import Data.Foldable (foldrM)

import Control.Monad.State 
   
{- | Tamarin on the one hand is case-sensitive, on the other hand it 
     runs into 
     problems when to identifiers differ in case only. For example, problems
     may occur if both "message" and "Message" are used in the same protocol. 
     The following check tests if there are such conflicts in a protocol -}
checkCasesConsistent :: IR.Protocol -> CheckResult 
checkCasesConsistent protocol = 
    fst $ runState (traverseProtocol protocol) Set.empty   
   
{------------------------------------------------------------------------------}
{- The whole protocol is traversed and all identifiers that were discovered 
   are remembered (using a state monad). -}
   
{- | An occurrence of an identifier. It consists of the identifier itself and 
     a label giving information about the location of the identifier. 
     'Occurrence' is in both the 'Eq' and the 'Ord' type classes. Two 
     occurrences are equal iff the identifiers are equal in a case-insensitive
     sense. An occurrence is smaller or equal than another occurrence iff 
     the same holds for the identifiers in a case-insensitive sense. -}
data Occurrence 
    = Occurrence Identifier Label
    deriving Show
    
instance Eq Occurrence where
    (Occurrence i1 _) == (Occurrence i2 _) = map toUpper i1 == map toUpper i2
    
instance Ord Occurrence where
    (Occurrence i1 _) <= (Occurrence i2 _) = map toUpper i1 <= map toUpper i2
   
{- | The state -}
type CheckState a 
    = State (Set.Set Occurrence) a
    
{- | Checks if two occurrences collide, i.e. if the identifiers differ only in 
     case -}
collides :: Occurrence -> Occurrence -> Bool 
collides (Occurrence i1 _) (Occurrence i2 _) = 
     i1 /= i2 && (map toUpper i1) == (map toUpper i2)
    
{- | Returns an earlier occurrence of the same identifier (only the first 
     occurrence is stored). Identifiers are compared in a case-insensitive 
     sense. -}
occurrence :: Occurrence -> CheckState Occurrence
occurrence occ = do 
    store <- get
    return $ Set.findMin $ Set.filter (==occ) store
    
{- | Adds an occurrence of an identifier to the state if this is the first
     occurrence of that identifier. The return value contains all conflicting
     identifiers (or is 'Success'). -}
add :: Occurrence -> CheckState CheckResult
add occnew = do 
    -- Get the current set of occurrences. 
    store <- get
    if occnew `Set.member` store then do 
        occold <- occurrence occnew
        if collides occold occnew then do 
            -- Already in store (do not insert), but collision. 
            Occurrence identold labelold <- return occold
            Occurrence identnew labelnew <- return occnew
            return $ Failure [InconsistentCases
                (labelold, identold, labelnew, identnew)]
        else 
            -- Already in store, no need to insert. 
            return Success
    else do 
        -- Not yet in store, insert. 
        put $ Set.insert occnew store
        return Success
         
{- | Traverses the protocol and reports all conflicting identifiers. -}       
traverseProtocol :: IR.Protocol -> CheckState CheckResult
traverseProtocol (IR.Protocol name funs roles goals) =
    let
    
    in do  
        resultName <- add (Occurrence name "at definition of protocol name")
        resultFuns <- traverseFuns funs
        resultRoles <- traverseRoles roles
        resultGoals <- traverseGoals goals
        return $ resultName `mappend` resultFuns `mappend` resultRoles `mappend`
                 resultGoals      
        
{- | Traverses the functions given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseFuns :: [IR.Function] -> CheckState CheckResult
traverseFuns [] = do 
    return Success
traverseFuns ((name, _, _):rest) = do 
    result <- add $ Occurrence name "in declaration block"
    resultrest <- traverseFuns rest
    return $ result `mappend` resultrest
    
{- | Traverses the roles given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseRoles :: [IR.Role] -> CheckState CheckResult
traverseRoles [] = do 
    return Success
traverseRoles ((IR.Role rolename actions):rest) = do 
    result <- add $ Occurrence rolename "name of a role"
    resultRest <- traverseRoles rest
    resultActions <- traverseActions actions
    return $ result `mappend` resultRest `mappend` resultActions

{- | Traverses the goals given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseGoals :: [IR.Goal] -> CheckState CheckResult
traverseGoals [] = do 
    return Success
traverseGoals ((IR.Secret label message rolen):rest) = do 
    resultRest <- traverseGoals rest
    resultLabel <- add $ Occurrence label "name of goal"
    resultMessage <- traverseMessage ("in action '" ++ label ++ "'") message
    resultRolen <- traverseRoleNames ("in goal '" ++ label ++ "'") rolen
    return $ resultLabel `mappend` resultMessage `mappend` resultRolen `mappend`
             resultRest
traverseGoals ((IR.WeakAuth label rolen1 rolen2 messages):rest) = do 
    resultRest <- traverseGoals rest 
    resultLabel <- add $ Occurrence label "name of goal"
    resultRolen1 <- add $ Occurrence rolen1 $ 
        "rolename in goal '" ++ label ++ "'"
    resultRolen2 <- add $ Occurrence rolen2 $ 
        "rolename in goal '" ++ label ++ "'"
    imessages <- mapM (traverseMessage ("in goal " ++ label)) messages
    resultMessages <- return $ foldr mappend mempty imessages
    return $ resultLabel `mappend` resultRolen1 `mappend` resultRolen2 `mappend`
             resultMessages `mappend` resultRest
traverseGoals ((IR.StrongAuth label rolen1 rolen2 messages):rest) = do 
    resultRest <- traverseGoals rest 
    resultLabel <- add $ Occurrence label "name of goal"
    resultRolen1 <- add $ Occurrence rolen1 $ 
        "rolename in goal '" ++ label ++ "'"
    resultRolen2 <- add $ Occurrence rolen2 $ 
        "rolename in goal '" ++ label ++ "'"
    imessages <- mapM (traverseMessage ("in goal " ++ label)) messages
    resultMessages <- return $ foldr mappend mempty imessages
    return $ resultLabel `mappend` resultRolen1 `mappend` resultRolen2 `mappend`
             resultMessages `mappend` resultRest
    
{- | Traverses the actions given as a parameter and returns all conflicting 
     identifiers that are encountered. -}
traverseActions :: [IR.Action] -> CheckState CheckResult 
traverseActions [] = do 
    return Success
traverseActions ((IR.Prepare kw):rest) = do 
    rs <- mapM (traverseMessage "in knowledge block") (Set.toList kw)
    resultRest <- traverseActions rest 
    return $ (foldr mappend mempty rs) `mappend` resultRest
traverseActions ((IR.Send _ label rolen fresh msg _):rest) = do
    resultLabel <- add $ Occurrence label "name of action"
    resultRoleN <- add $ Occurrence rolen $ 
        "rolename of receiver in action '" ++ label ++ "'"
    resultFresh <- traverseFresh ("in action '" ++ label ++ "'") fresh 
    resultMessage <- traverseMessage ("in action '" ++ label ++ "'") msg
    resultRest <- traverseActions rest 
    return $ resultLabel `mappend` resultRoleN `mappend` resultFresh `mappend`
             resultMessage `mappend` resultRest
traverseActions ((IR.Receive label rolen msg _):rest) = do 
    resultRoleN <- add $ Occurrence rolen $ 
        "rolename of sender in action '" ++ label ++ "'"
    resultRest <- traverseActions rest 
    return $ resultRoleN `mappend` resultRest

{- | Traverses the fresh names given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseFresh :: Label -> [Identifier] -> CheckState CheckResult
traverseFresh _ [] = do 
    return Success
traverseFresh l (i:is) = do 
    result <- add $ Occurrence i $ "fresh name (" ++ l ++ ")"
    restResult <- traverseFresh l is
    return $ result `mappend` restResult
    
{- | Traverses the role names given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseRoleNames :: Label -> [RoleName] -> CheckState CheckResult
traverseRoleNames _ [] = do 
    return Success
traverseRoleNames l (r:rs) = do 
    result <- add $ Occurrence r $ "name of role (" ++ r ++ ")"
    restResult <- traverseRoleNames l rs
    return $ result `mappend` restResult
    
{- | Traverses the message given as a parameter and returns all conflicting
     identifiers that are encountered. -}
traverseMessage :: Label -> Message -> CheckState CheckResult
traverseMessage l (Gamma _ m) = do 
    traverseMessage l m 
traverseMessage l (Var v) = do 
    add $ Occurrence v l 
traverseMessage l (Concat c1 c2) = do 
    t1 <- traverseMessage l c1
    t2 <- traverseMessage l c2
    return $ t1 `mappend` t2
traverseMessage l (Aenc m k) = do 
    tm <- traverseMessage l m
    tk <- traverseMessage l k 
    return $ tm `mappend` tk 
traverseMessage l (Senc m k) = do 
    tm <- traverseMessage l m
    tk <- traverseMessage l k 
    return $ tm `mappend` tk 
traverseMessage l (Hash h) = do 
    traverseMessage l h 
traverseMessage l (Mul facs) = do 
    tfacs <- mapM (traverseMessage l) (MultiSet.toList facs) 
    return $ foldr mappend mempty tfacs
traverseMessage l (Exp m1 m2) = do 
    t1 <- traverseMessage l m1
    t2 <- traverseMessage l m2
    return $ t1 `mappend` t2
traverseMessage l (Pk rn) = do 
    add $ Occurrence rn l 
traverseMessage l (Sk rn) = do 
    add $ Occurrence rn l 
traverseMessage l (K r1 r2) = do 
    t1 <- add $ Occurrence r1 l 
    t2 <- add $ Occurrence r2 l 
    return $ t1 `mappend` t2
traverseMessage l (Fun n ms) = do 
    tn <- add $ Occurrence n l 
    lms <- mapM (traverseMessage l) ms
    tms <- return $ foldr mappend mempty lms
    return $ tms `mappend` tn 
traverseMessage l _ = do 
    return Success

{------------------------------------------------------------------------------}
