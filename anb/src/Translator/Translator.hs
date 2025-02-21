{- |
Description: Functions for translating IR to Tamarin. 

This module provides the functionality for translating from 
the intermediate representation language to Tamarin. -}
module Translator.Translator (
    translateProtocol
)where

import Parser.Basic
import Parser.Message
import Rewriter.IR
import Rewriter.Rewriter
import Rewriter.FunctionsStateless (inSynth, cf, newSynthesizable, partial)
import Translator.State
import Translator.Printer
import Translator.Auxiliary

import Debug.Trace

import Data.List
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet

{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}

{- | Takes a protocol in its IR representation and returns Tamarin code. -}
translateProtocol :: Protocol -> String 
translateProtocol p@(Protocol name funs roles goals) = 
    let 
        protocolstate = p 
        rolestate = ("", [], [], [])
        outputstate = ("", 0, 0)
        translationstate = (protocolstate,   
                            rolestate,  
                            outputstate) 
    in 
        fst $ runState printProtocol translationstate

{------------------------------------------------------------------------------}

{- | Translates the protocol that is currently in the state
     and writes the result to the output. Returns this output -}
printProtocol :: TranslationState String
printProtocol = 
    do p@(Protocol name funs roles goals) <- getProtocol
       printString $ "theory " ++ name
       printNewline >> printString "begin"
       printNewline >> printNewline >> printFunctionDeclarations
       printBuiltins 
       printNewline >> printNewline >> printEquations
       printNewline >> printNewline >> printRuleAsymmetricSetup 
       printNewline >> printNewline >> printRulePublishPK 
       when (usesSenc p) $ do
           printNewline >> printNewline >> printRuleSymmetricSetup     
       printNewline >> printNewline >> printKnowledgeSetup
       printNewline >> printNewline >> printRoles roles
       
       printList (\l -> printSecrecyLemma l >> printNewline >> printNewline)
                 (printString "")
                 (filter isSecrecyGoal goals)
       printList  (\l -> printWeakAuthLemma l >> printNewline >> printNewline)
                 (printString "")
                 (filter isWeakAuthGoal goals)
       printList  (\l -> printStrongAuthLemma l >> printNewline >> printNewline)
                 (printString "")
                 (filter isStrongAuthGoal goals)
                 
       printString "end"
       printNewline
       getOutput
{------------------------------------------------------------------------------}

{- | Writes the builtins that Tamarin should use to the output.
     Only the builtins that are actually needed are printed. -}
printBuiltins :: TranslationState () 
printBuiltins = do 
    p <- getProtocol
    bi <- return []
    bi <- return $ if usesHash p then "hashing" : bi else bi
    bi <- return $ if usesSenc p then "symmetric-encryption" : bi else bi
    bi <- return $ if usesDH p then "diffie-hellman" : bi else bi
    if bi /= [] then do 
        printNewline >> printNewline
        printString "builtins:" >> printSpace
        printList printString (printString "," >> printSpace) bi
    else do 
        return () 
   
{------------------------------------------------------------------------------}
{- PRINTING FIXED-FORM FUNCTIONS -}
{- | Writes the rule for asymmetric key setup to the output. -}
printRuleAsymmetricSetup :: TranslationState () 
printRuleAsymmetricSetup = 
    do printString "rule Asymmetric_key_setup:"
       addToIndentation 4
       printString 
           "\n[ Fr(~f) ] --> [ !Sk($A, sk(~f)), !Pk($A, pk(~f)) ]"
       addToIndentation (-4)
    
{- | Writes the rule for publishing public keys to the output. -}
printRulePublishPK =  
    do printString "rule Publish_public_keys:"
       addToIndentation 4
       printString "\n[ !Pk(A, pkA) ] --> [ Out(pkA) ]" 
       addToIndentation (-4)
       
{- | Writes the rule for symmetric key setup to the output. -}
printRuleSymmetricSetup :: TranslationState ()  
printRuleSymmetricSetup = 
    do printString "rule Symmetric_key_setup:"
       addToIndentation 4
       printString "\n[ Fr(~symK) ] --> [ !Key($A, $B, ~symK) ]" 
       addToIndentation (-4)
       
{------------------------------------------------------------------------------}

{- | Writes the rules representing the actions of the roles 
     (given as a parameter) to the output. -}
printRoles :: [Role] -> TranslationState () 
printRoles [] = return () 
printRoles (r:rs) = 
    let 
        rolename = roleName r
        actions = actionsOfRole r
    in
    do printString $ "// ROLE " ++ rolename
       printNewline
       putRoleState (rolename, actions, [], [])
       printRole 
       printRoles rs
       
{- | Writes the rules representing the actions of the 
     role given as a parameter to the output. -}
printRole :: TranslationState () 
printRole = 
    do rolename <- getRoleName
       actions <- getActions
       case actions of
        [] -> return ()
        (_:[]) -> return ()
        (a1:a2:as) -> do 
            printAction a1 a2
            putActions (a2:as)
            printNewline >> printNewline >> printRole 

{- | Writes the function declarations to the output. -}
printFunctionDeclarations :: TranslationState () 
printFunctionDeclarations = 
    do p <- getProtocol 
       funDecs <- return $ functions p
       printString "functions:"
       printSpace 
       if funDecs == [] then do 
           printString "pk/1, sk/1, aenc/2, adec/2"
       else do 
           printString "pk/1, sk/1, aenc/2, adec/2, "
           printList printFunDec (printString "," >> printSpace) funDecs
       return ()      
       
{- | Writes a function declaration to the output. -}    
printFunDec :: Function -> TranslationState () 
printFunDec (identifier, arity, public) = 
    do printString $ identifier
       printString "/"
       printString $ show arity
       unless public $ printSpace >> (printString "[private]")
       
       
{- | Writes the rule "Init_knowledge" to the output. -}
printKnowledgeSetup :: TranslationState () 
printKnowledgeSetup = 
    let
        -- Get a set of all atomic messages that occur in the
        -- initial knowledges. 
        getAtomic :: [Role] -> Set.Set Message
        getAtomic [] = Set.empty
        getAtomic (r:rs) = 
            let 
                prepare = head $ actionsOfRole r
                chi = getKnowledge prepare
                subs = Set.fold Set.union Set.empty $ Set.map sub chi 
            in 
                subs `Set.union` (getAtomic rs) 
  
        -- Get predicates for initial knowledge. 
        getStPreds :: [Role] -> [Predicate]
        getStPreds [] = []
        getStPreds (r:rs) = 
            let 
                prepare = head $ actionsOfRole r
                rolename = roleName r
                predname = "St_init_" ++ rolename
                chi = (Set.toList (getKnowledge prepare))
            in 
                (predname, chi):(getStPreds rs)         
        
        getSymKeys :: [Message] -> [Message]
        getSymKeys m = nub $ filter isK m 
        
        getPubKeys :: [Message] -> [Message]
        getPubKeys m = nub $ filter isPk m 
        
        getSecKeys :: [Message] -> [Message]
        getSecKeys m = nub $ filter isSk m 
        
        getVars :: [Message] -> [Message]
        getVars m = nub $ filter isVar m 
        
        getStrings :: [Message] -> [Message]
        getStrings m = nub $ filter isStr m 
        
        getKPredicate :: Message -> TranslationState Predicate
        getKPredicate key@(K a b) = 
            return ("!Key", [(Var a), (Var b), key])
            
        getPkPredicate :: Message -> TranslationState Predicate
        getPkPredicate key@(Pk a) =
            return ("!Pk", [(Var a), key])
            
        getSkPredicate :: Message -> TranslationState Predicate
        getSkPredicate key@(Sk a) = 
            return ("!Sk", [(Var a), key])
            
                
    in
    do protocol@(Protocol _ _ roles _) <- getProtocol 
       -- Get a list of all atomic messages that are in the 
       -- initial knowledge of a role. 
       atomics <- return $ (Set.toList (getAtomic roles))
       
       -- List of the role names 
       rolenamevars <- return $ roleNames protocol
       
       -- List of all variables that are in the initial knowledge of a role
       -- with the variables representing role names removed. 
       vars <- return $ (getVars atomics) \\ (map Var rolenamevars)
       
       -- List of all symmetric keys that are in the initial knowledge of a role.
       symkeys <- return $ getSymKeys atomics
       
       -- List of all public keys that are in the initial knowledge of a role.
       pubkeys <- return $ getPubKeys atomics
       
       -- List of all secret keys that are in the initial knowledge of a role.
       seckeys <- return $ getSecKeys atomics
       
       -- Create a predicate for each symmetric, public or secret key. 
       predsk <- mapM getKPredicate symkeys
       predspk <- mapM getPkPredicate pubkeys
       predssk <- mapM getSkPredicate seckeys
       
       -- Create a predicate for each fresh value
       predsfresh <- return $ map (\v -> ("Fr", [v])) vars
       
       -- Predicates in the precondition block
       preconds <- return $ predsk ++ predspk ++ predssk ++ predsfresh
       
       -- Predicates in the results block 
       results <- return $ getStPreds roles
       
       -- The role names are public values (with a $)
       putPublicNames rolenamevars
       -- All other variables are fresh (with a ~)
       putFreshNames $ map (\(Var v) -> v) vars
       
       -- Print the rule 
       printRule "Init_Knowledge" [] preconds [] results printFullMessage
       
       -- Reset public and fresh values
       putPublicNames []
       putFreshNames []
       
       return ()     
       
{- | Writes an action to the output. 
     The first parameter is the previous action, the second
     parameter the action that is to be written to the output. 
     The first action is needed to determine the input state. -}             
printAction :: Action -> Action -> TranslationState () 
printAction a1 a2@(Send _ lab rn fr msg chi) =
    let 
        -- List of fresh variables
        freshList = map (\f -> ("Fr", [Var f])) fr
        outPred = ("Out", [msg]) 
    in do
        -- Name of the role executing this action
        rolename <- getRoleName
        
        protocol <- getProtocol 
        
        goalPredicates <- getGoalPredicates 
        
        -- Get the predicate for input state
        state1 <- getStatePredicate a1 rolename
        -- Get the predicate for output state
        state2 <- getStatePredicate a2 rolename
        -- Construct the name of the rule
        rulename <- return $ lab ++ "_" ++ rolename
        
        -- Store which names are fresh (so the printer knows that
        -- a '~' must be prepended)
        putFreshNames fr 
         
        -- Print the rule to the output. 
        printRule rulename 
                  [] 
                  (state1:freshList) 
                  (goalPredicates)
                  (outPred:[state2])
                  printMessage
                
        putFreshNames []
       
printAction a1 a2@(Receive lab rn msg chi) =
    let     
        inPred = ("In", [msg]) 
        -- Check if there are messages in the knowledge that are 
        -- analyzable but not synthesizable.
        part = Set.fold union [] 
               (Set.map (partial (getKnowledge a2)) (getKnowledge a2))
        newSyn = newSynthesizable (getKnowledge a1) (getKnowledge a2) 
    in do
        rolename <- getRoleName
        
        protocol <- getProtocol 
        
        goalPredicates <- getGoalPredicates 
        
        
        state1 <- getStatePredicate a1 rolename
        state2 <- getStatePredicate a2 rolename
        
        rulename <- return $ lab ++ "_" ++ rolename
         
        printRule rulename 
                  (part ++ newSyn)
                  (state1:[inPred]) 
                  (goalPredicates)
                  ([state2])
                  printMessage
        return ()
         

getStatePredicate :: Action -> RoleName -> TranslationState Predicate 
getStatePredicate action rn = 
    let
        knowledge =  (Set.toAscList (getKnowledge action))
        label = "St_" ++ getLabel action ++ 
                "_" ++ rn 
    in 
        return (label, knowledge)
   
getGoalPredicates :: TranslationState [Predicate]
getGoalPredicates = 
    let 

        allSynthesizable :: Knowledge -> Goal -> Bool 
        allSynthesizable chi goal = 
            let 
                msgs = authMessages goal 
            in 
                all (inSynth chi) msgs
           

        freshlySynthesizable :: Action -> Action -> Goal -> Bool 
        freshlySynthesizable a1 a2 goal = 
            let
                syn1 = allSynthesizable (getKnowledge a1) goal 
                syn2 = allSynthesizable (getKnowledge a2) goal 
            in
                (not syn1 || isPrepareAction a1) && syn2
    in do 
        protocol <- getProtocol 
        myName <- getRoleName

        let mygoals = filter (isGoalOf myName) $ goals protocol
            secrecyGoals = filter isSecrecyGoal mygoals
            wauthGoals = filter isWeakAuthGoal mygoals
            sauthGoals = filter isStrongAuthGoal mygoals

        actions <- getActions
        case actions of
            a1:a2:_ -> do
                let partnerGoals = filter (isAgreementPartner myName) (goals protocol)
                    runningGoals = filter (freshlySynthesizable a1 a2) partnerGoals
                    runnings = map (\g -> (("Running_" ++ labelOfGoal g),
                                        map (cf (getKnowledge a2)) (authMessages g))) runningGoals

                last <- isLastAction
                commits <- if last then do
                            let secP = map (\(Secret l msg _) -> (("Secret_" ++ l ++ "_" ++ myName),
                                                                [cf (getKnowledge a2) msg])) secrecyGoals
                                wauP = map (\(WeakAuth l _ _ msgs) -> (("Commit_" ++ l),
                                                                    map (cf (getKnowledge a2)) msgs)) wauthGoals
                                sauP = map (\(StrongAuth l _ _ msgs) -> (("Commit_" ++ l),
                                                                        map (cf (getKnowledge a2)) msgs)) sauthGoals
                            return $ secP ++ wauP ++ sauP
                        else return []
                return $ commits ++ runnings
            _ -> error "Expected at least two actions"

printSecrecyLemma :: Goal -> TranslationState () 
printSecrecyLemma (Secret label message roleNames) = 
    let 
        numRoles = length roleNames
        -- Creates a time variable for each role, for example
        -- ["#i1", "#i2", "#i3"] if there are three roles. 
        vars = map (\i -> "#i" ++ show i) [1 .. numRoles]
        -- Creates a stirng with time variables for each role, for example
        -- "#i1 #i2 #i3" if there are three roles. 
        varString = intercalate " " vars
        
        -- Creates a predicate "Secret_label_roleName(msg)" for each role name. 
        predicates = 
            map (\rn -> "Secret_" ++ label ++ "_" ++ rn ++ "(msg)") roleNames
        -- Appends a time variable to each predicate, for example
        -- "Secret_label_roleName(msg) @ #i1". 
        conds = 
            map (\(a, b) -> a ++ b) 
                (zip predicates (map (\v -> " @ " ++ v ++ " &") vars))
    in do
        printString $ "lemma " ++ label ++ ":"
        addToIndentation 4 >> printNewline
        printString $ "\" not("
        addToIndentation 4 >> printNewline
        printString $ "Ex msg " ++ varString ++ " #j ."
        addToIndentation 4 >> printNewline
        printList printString printNewline conds
        printNewline >> printString "K(msg) @ #j"
        addToIndentation (-8) >> printNewline
        printString ")\""
        addToIndentation (-4)

printWeakAuthLemma :: Goal -> TranslationState () 
printWeakAuthLemma (WeakAuth label role1 role2 messages) = 
    let messageList :: Int -> [String]
    	messageList 1 = ["m1"]
    	messageList i = (messageList (i-1)) ++ ["m" ++ (show i)]
    in do 
		printString $ "lemma " ++ label ++  ":"
		addToIndentation 4 >> printNewline
		printString $ "\" (All " ++
		              intercalate " " (messageList (length messages)) ++
			          " #i ." 
		addToIndentation 4 >> printNewline
		printString $ "Commit_" ++ label ++  "(" ++ 
		              intercalate ", " (messageList (length messages)) ++
		              ")@ #i"
		printNewline 
		printString "==>" >> printNewline
		printString $ "(Ex #j . Running_" ++ label ++ "(" ++
		              intercalate ", " (messageList (length messages)) ++ 
		              ") @ #j & #j < #i)" 
		addToIndentation (-4) >> printNewline
		printString ")\"" 
		addToIndentation (-4) >> printNewline
    
printEquations :: TranslationState() 
printEquations = do 
    printString $ "equations:"
    addToIndentation 4
    printNewline
    printString "adec(aenc(x.1, sk(x.2)), pk(x.2)) = x.1,\n"
    printString "adec(aenc(x.1, pk(x.2)), sk(x.2)) = x.1"
    addToIndentation (-4)

printStrongAuthLemma :: Goal -> TranslationState () 
printStrongAuthLemma (StrongAuth label role1 role2 messages) = 
    let 
    	messageList :: Int -> [String]
    	messageList 1 = ["m1"]
    	messageList i = (messageList (i-1)) ++ ["m" ++ (show i)]
    in do 
		printString $ "lemma " ++ label ++  ":"
		addToIndentation 4 >> printNewline
		printString $ "\" (All " ++
		              intercalate " " (messageList (length messages)) ++
			          " #i ." 
		addToIndentation 4 >> printNewline
		printString $ "Commit_" ++ label ++  "(" ++ 
		              intercalate ", " (messageList (length messages)) ++
		              ")@ #i"
		printNewline 
		printString "==>" >> printNewline
		printString $ "(Ex #j . Running_" ++ label ++ "(" ++
		              intercalate ", " (messageList (length messages)) ++ 
		              ") @ #j & #j < #i) &" 
		printString $ "\n(not (Ex #j . Commit_" ++ label ++  
		              "(" ++ intercalate ", " (messageList (length messages)) ++ 
		              ") @ #j & not (#i = #j)))"
		addToIndentation (-4) >> printNewline
		printString ")\"" 
		addToIndentation (-4) >> printNewline

       
{------------------------------------------------------------------------------}


allMessagesIncSubs protocol =
    let
        allMsgs = Set.toList $ allMessages protocol 
        result = Set.toList $ foldr Set.union Set.empty 
                 $ map allSubs allMsgs
    in 
        result

{- | Returns if hashing is used in the protocol -}
usesHash :: Protocol -> Bool 
usesHash protocol = 
    let 
        hash (Gamma _ m) = isHash m
        hash m           = isHash m
    in 
        find hash (allMessagesIncSubs protocol) /= Nothing
    
{- | Returns if Multiplication or Exponentiation is used in the protocol -}
usesDH :: Protocol -> Bool 
usesDH protocol = 
    let 
        dh (Gamma _ m) = isMul m || isExp m
        dh m           = isMul m || isExp m
    in 
        find dh (allMessagesIncSubs protocol) /= Nothing
    
{- | Returns if symmetric encryption is used in the protocol -}    
usesSenc :: Protocol -> Bool 
usesSenc protocol = 
    let 
        senc (Gamma _ m) = isSenc m 
        senc m           = isSenc m
    in 
        find senc (allMessagesIncSubs protocol) /= Nothing

{- | Returns if asymmetric encryption is used in the protocol -} 
usesAenc :: Protocol -> Bool 
usesAenc protocol = 
    let 
        aenc (Gamma _ m) = isAenc m
        aenc m           = isAenc m
    in 
        find aenc (allMessagesIncSubs protocol) /= Nothing
    
{- | Returns if the given action is the last action of the 
     current role. -}
isLastAction :: TranslationState Bool 
isLastAction = do  
    actions <- getActions
    -- This is two since the state also remembers the action that was
    -- last translated (we need information from that state, too) 
    return $ length actions == 2    

