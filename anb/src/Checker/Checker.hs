{- | 
Description: Functions for checking the well-formedness of a protocol. 

This module provides functions for checking the well-formedness of 
a protocol. -}
module Checker.Checker where

import Data.List (intercalate, find, union, last)

import qualified Rewriter.IR as IR
import Parser.Message
import Parser.Basic
import Rewriter.FunctionsStateless

import Data.Monoid
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import Checker.CheckResult
import Checker.CaseInsensitivity


{- | Applies all well-formedness checks to a protocol.
     Some checks only make sense if some other checks are Successful
     (for example, checking if a protocol is executable does not make
     sense if there are problems with the function declarations). 
     Therefore, certain checks are only executed if all previous checks
     have succeeded. -}
doAllChecks :: IR.Protocol -> CheckResult
doAllChecks protocol =  
    let
        {- Shorthands for the checks -}
        funUnique = functionsUnique protocol
        funArities = aritiesPositive protocol
        funUsage = functionUsageCorrect protocol 
        labels = labelsUnique protocol
        fresh = freshUnique protocol 
        senderReceiver = senderReceiverDifferent protocol 
        exec = executable protocol 
        cases = checkCasesConsistent protocol
 
        goalsexec = goalsExecutable protocol 
        roleshaveac = rolesInGoalsHaveActions protocol 
        
        checks1 = 
            funUnique <> funArities <> labels <> cases
        checks2 = 
            roleshaveac 
        checks3 = 
            goalsexec <> funUsage <> fresh 
           
    in 
        if isFailure checks1 then 
            {- This first checks makes sure that the overall structure 
               of the protocol is well-formed. -}
            checks1
        else if isFailure senderReceiver then 
            {- The check if there are actions where sender and receiver are
               equal should be performed after the check if there are 
               multiple labels with the same name. -}
            senderReceiver
        else if isFailure checks2 then 
            checks2 
        else if isFailure checks3 then 
            checks3
        else
           {- Performing the executability check makes sense only 
              if all previous tests have succeeded. -}
            exec


{------------------------------------------------------------------------------}
{- Functions checking well-formedness of function declarations. -}

{- | Checks if a function is declared twice -}
functionsUnique :: IR.Protocol -> CheckResult
functionsUnique protocol = 
    let
        functions = IR.functions protocol
        
        {- Returns if two functions have the same name -}
        sameName :: IR.Function -> IR.Function -> Bool 
        sameName f1 f2 = 
            IR.functionName f1 == IR.functionName f2
        
        {- Returns the names of functions that are declared multiple
           times (one entry per name that is defined multiple twice) -}
        dups :: [IR.Function] -> [Identifier]
        dups [] = []
        dups (l:ls) = 
            let
                -- is there (at least one) a duplicate of l in ls?
                hasDuplicate = find (sameName l) ls /= Nothing
                -- ls with all duplicates of l removed (if any)
                clearedList = filter (\x -> not (sameName l x)) ls
            in 
                if hasDuplicate then 
                    (IR.functionName l) : (dups clearedList) 
                else 
                    dups ls
                  
        {- List of all function names that are declared more than once -}
        duplicates = dups $ IR.functions protocol 
        
        {- Are there functions that are declared more than once? -}
        hasDuplicates = duplicates /= []
    in 
        if hasDuplicates then 
            Failure $ Prelude.map DuplicateFunctionDec duplicates 
        else 
            Success
        
{- | Checks if functions are declared with negative arities -}
aritiesPositive :: IR.Protocol -> CheckResult
aritiesPositive protocol = 
    let 
        {- Returns a list of the functions with negative arities from 
           a list -}
        fails :: [IR.Function] -> [Identifier]
        fails [] = []
        fails (l:ls) = 
            if IR.functionArity l < 0 then 
                union [IR.functionName l] $ fails ls
            else
                fails ls
                
        -- List of all function names that are declared with negative 
        -- arities
        failures = fails (IR.functions protocol)
    in  
        if failures == [] then 
            Success
        else 
           Failure $ Prelude.map NegativeArity failures 
   


{------------------------------------------------------------------------------}
{- Functions checking if functions are used correctly -}

{- | Checks if functions are used correctly (arity, only declared 
     functions, no variables with same name as declared functions). -}
functionUsageCorrect :: IR.Protocol -> CheckResult   
functionUsageCorrect protocol = 
    let
        {- Check if functions are used correctly in an action. 
           No fresh variable must have the same name as a function
           and the message sent or received must be correct. -}
        checkAction :: IR.Action -> CheckResult
        checkAction a@(IR.Prepare initK) = 
            foldr mappend mempty $ 
                Prelude.map (functionInMessage protocol (IR.getLabel a))
                (Set.toList initK)
        checkAction a@(IR.Send _ _ _ fresh msgs _) = 
            functionInMessage protocol (IR.getLabel a) msgs `mappend`
            (foldr mappend mempty 
             (Prelude.map (functionInMessage protocol (IR.getLabel a)) 
             (Prelude.map Var fresh)) )
        checkAction a@(IR.Receive _ _ msgs _) = 
            {- Every message is both sent and received. We only need to 
               check the sending side -}
            Success   
            
        checkGoal :: IR.Goal -> CheckResult
        checkGoal (IR.Secret l m _) = 
            functionInMessage protocol l m 
        checkGoal (IR.WeakAuth l _ _ ms) = 
            foldr mappend mempty $ map (functionInMessage protocol l) ms
        checkGoal (IR.StrongAuth l _ _ ms) = 
            foldr mappend mempty $ map (functionInMessage protocol l) ms
        
        {- All actions in the protocol -}
        actions = IR.actions protocol
        
        {- All goals in the protocol -}
        goals = IR.goals protocol 
        
        checkResults = map checkAction actions `mappend` map checkGoal goals
    in 
        foldr mappend mempty $ checkResults     
       
{- | Checks if functions are used correctly (arity, only declared
     functions, no variables with same name as functions) in a message. -}
functionInMessage :: IR.Protocol -> Label -> Message -> CheckResult
functionInMessage p a (Var v) =
    -- Make sure that the variable does not have the same name as a 
    -- function.  
    let 
        fun = IR.findFunction v p 
        exists = fun /= Nothing 
    in 
        if exists then 
            Failure [UsedFunAsVar (v, a)]
        else 
            Success
functionInMessage p l (Concat m1 m2) = 
    functionInMessage p l m1 `mappend` functionInMessage p l m2 
functionInMessage p l (Aenc m1 m2) = 
    functionInMessage p l m1 `mappend` functionInMessage p l m2
functionInMessage p l (Senc m1 m2) = 
    functionInMessage p l m1 `mappend` functionInMessage p l m2
functionInMessage p l (Gamma _ m) = 
    functionInMessage p l m 
functionInMessage p l (Hash m) =
    functionInMessage p l m 
functionInMessage p l (Mul m) = 
    foldr mappend mempty (Prelude.map (functionInMessage p l)
                                      (MultiSet.toList m))
functionInMessage p l (Exp m1 m2) = 
    functionInMessage p l m1 `mappend` functionInMessage p l m2
functionInMessage p l (Fun name args) = 
    -- Make sure the function was declared and has the correct arity. 
    let
        fun = IR.findFunction name p 
        exists = fun /= Nothing
        (Just res) = fun 
        arityCorrect = IR.functionArity res == toInteger (length args)
    in 
        (if exists && arityCorrect then 
            Success
        else if exists && not arityCorrect then
            Failure [WrongArity (name, l)]
        else
            Failure [UnknownFunction (name, l)]
        ) `mappend` foldr mappend mempty 
                    (Prelude.map (functionInMessage p l) args)
functionInMessage _ _ _ = Success
            

{------------------------------------------------------------------------------}

{- | Checks if there are declared fresh variables that collide with 
     a fresh name declared earlier or with a variable in the
     initial knowledge -}
freshUnique :: IR.Protocol -> CheckResult
freshUnique protocol = 
    let
        {- All the roles in the protocol -}
        roles = IR.roles protocol
    
        {- Set of all messages that are initial knowledge of 
           any principal. -}
        initK = foldr Set.union Set.empty $ 
                Prelude.map IR.getInitialKnowledge roles
        
        {- All actions in the protocol -}
        actions = IR.actions protocol 
        
        {- Traverse all actions in the protocol. Returns a list of all
           fresh names that collide with a name that was declared earlier.
        -}
        collision :: [IR.Action] -> IR.Knowledge -> [(Identifier, Label)]
        collision [] _ = []
        collision (a:as) kw = 
            let
                -- Get list of fresh names in the action (if a send action)
                IR.Send _ _ _ fresh _ _ = a

                -- List of fresh variables
                freshVars = Prelude.map Var fresh

                -- List of all the collisions
                collisions = filter (`Set.member` kw) freshVars 
                
                hasCollision = not $ null collisions
                
                newKnowledge = kw `Set.union` (Set.fromList freshVars)
                
                pairs = Prelude.map (\(Var f) -> (f, IR.getLabel a)) collisions
            in 
                if IR.isSendAction a then 
                    if hasCollision then 
                        union pairs $ collision as newKnowledge
                    else
                        collision as newKnowledge
                else 
                    collision as kw
                
        {- List of all the collisions -}
        collisions = collision actions initK 
        
        hasCollisions = not $ null collisions
                
    in 
        if hasCollisions then
            Failure $ Prelude.map NameCollision collisions 
        else 
            Success
    
{- | Checks if there are any actions where sender and receiver are
     equal. -}
senderReceiverDifferent :: IR.Protocol -> CheckResult
senderReceiverDifferent protocol = 
    let
        
        {- Check if sender and receiver of an action are equal -}
        seeq ::  Identifier -> IR.Action -> Bool 
        seeq _ (IR.Prepare _)  = False
        seeq actor (IR.Send _ _ name _ _ _) = name == actor
        seeq actor (IR.Receive _ name _ _) = name == actor
        
        {- Returns a list of all the actions of a role where the receiver
           is equal to the sender -}
        equalsInRole :: IR.Role -> [IR.Action]
        equalsInRole role = 
            filter (seeq $ IR.roleName role) (IR.actionsOfRole role)
             
        {- Returns a list of all the actions in a list of roles
           where sender and receiver are equal -}
        equalsInRoles :: [IR.Role] -> [IR.Action]
        equalsInRoles roles = 
            foldr union [] $ Prelude.map equalsInRole roles
            
        {- Returns a list of all the actions where sender and receiver are
           equal -}
        equals = equalsInRoles $ IR.roles protocol 
        
        labels = map IR.getLabel equals
        
        hasEquals = not $ null equals 
    in 
        if hasEquals then 
            Failure $ Prelude.map EqualSenderReceiver labels
        else 
            Success
        
{- | Checks if the protocol is executable. -}
executable :: IR.Protocol -> CheckResult
executable protocol = 
    let 
        {- All actions in the protocol -}
        actions = IR.actions protocol
        
        {- List of actions that are not executable -}
        notExecutables = filter (not . IR.isActionExecutable) actions
        
        {- Is the protocol executable -}
        executable = null notExecutables
    in 
        if executable then 
            Success
        else
            Failure $ Prelude.map NotExecutable notExecutables
    

{- | Roles that appear in goals should have at least one action. 
     This function checks if this is the case. -}
rolesInGoalsHaveActions :: IR.Protocol -> CheckResult
rolesInGoalsHaveActions protocol = 
    let
        goals = IR.goals protocol 
    
        checkRole :: Label -> RoleName -> CheckResult
        checkRole label rn = 
            if not (rn `elem` IR.roleNames protocol) then 
                Failure [RoleInGoalWithNoAction (label, rn)]
            else if 
                length (IR.actionsOfRole $ IR.roleByName protocol rn) > 0 then 
                Success
            else
                Failure [RoleInGoalWithNoAction (label, rn)]
    
        checkGoal :: IR.Goal -> CheckResult
        checkGoal (IR.Secret label msg rns) = 
            foldr mappend mempty (map (checkRole label) rns)
        checkGoal (IR.WeakAuth label rn1 rn2 msgs) = 
            checkRole label rn1 <> checkRole label rn2
        checkGoal (IR.StrongAuth label rn1 rn2 msgs) = 
            checkRole label rn1 <> checkRole label rn2
    in 
        foldr mappend mempty (map checkGoal goals)


{- | Checks if the messages in the goals can be constructed:

     A goal of the form 
     @msg secret of A, B;@ 
     only makes sense if both @A@ and @B@ 
     can actually construct @msg@ after their last action. This function
     checks if this is the case. 

     A goal 
     @A (non-)injectively agrees with B on msg1, msg2@ 
     only makes sense, if all messages (@msg1@ and @msg2@ in this case)
     can actually be constructed by both A and B the latest in their 
     last action. This function checks if this is the case. 
     
     This function should only be called if all roles that appear in 
     goals have at least one action. -}
goalsExecutable :: IR.Protocol -> CheckResult
goalsExecutable protocol =
    let 
        goals = IR.goals protocol 
        
        -- returns the knowledge after the last action of a role
        kw :: RoleName -> IR.Knowledge
        kw = (IR.getKnowledge) . last . (IR.actionsOfRole) . 
              (IR.roleByName protocol)
              
        -- Checks if a message is synthesizable in the last action of a role.
        -- If so, 'Success' is returned, otherwise 'GoalNotExecutable' 
        -- with the label given. 
        check :: Label -> Message -> RoleName -> CheckResult
        check label msg rn = 
            if inSynth (kw rn) msg then 
                Success
            else
                Failure [GoalNotExecutable (label, msg, rn)] 

        -- Checks a goal. 
        checkGoal :: IR.Goal -> CheckResult
        checkGoal (IR.Secret label msg rns) = 
            foldr mappend mempty (map (check label msg) rns)
        checkGoal (IR.WeakAuth label rn1 rn2 msgs) = 
            foldr mappend mempty (map ((flip (check label)) rn1) msgs)
            <> foldr mappend mempty (map ((flip (check label)) rn2) msgs)
        checkGoal (IR.StrongAuth label rn1 rn2 msgs) = 
            foldr mappend mempty (map ((flip (check label)) rn1) msgs)
            <> foldr mappend mempty (map ((flip (check label)) rn2) msgs)
    in 
        foldr mappend mempty (map checkGoal goals) 

{- | Checks if the labels are unique. -}           
labelsUnique :: IR.Protocol -> CheckResult
labelsUnique protocol = 
    let
        {- All actions in the protocol -}
        actions = IR.actions protocol 
        {- All goals of the protocol -}
        goals = IR.goals protocol 
        
        {- Retrieves labels of actions, ignores 'Prepare' actions since
           they do not have a user-defined label. Ignores 'Receive' 
           actions since they are the counterpart of a 'Send' action 
           (with the same label). -}
        getActionLabels :: [IR.Action] -> [Label]
        getActionLabels [] = []
        getActionLabels (a:as) = 
            if IR.isReceiveAction a || IR.isPrepareAction a then 
                getActionLabels as
            else
                IR.getLabel a : getActionLabels as
        
        
        
        {- All labels that occur in the protocol (including duplicates) -}
        labels = getActionLabels actions ++ map IR.labelOfGoal goals
        
        {- Traverses a list of labels and returns the set of the labels
           that occur at least twice. The second parameter is used as a
           store of all the labels that have been encountered so far. -}
        traverse :: [Label] -> 
                    Set.Set Label -> 
                    [Label]
        traverse [] _ = []
        traverse (l:ls) store = 
            let
                isDuplicate = Set.member l store
                newLabels = Set.insert l store
            in 
                if isDuplicate then 
                    union [l] $ traverse ls newLabels
                else 
                    traverse ls newLabels
                    
        duplicates = traverse labels Set.empty
        
        hasDuplicates = not $ null duplicates
    in 
        if hasDuplicates then 
            Failure $ Prelude.map DuplicateLabels duplicates
        else 
            Success
        
{------------------------------------------------------------------------------}
        
