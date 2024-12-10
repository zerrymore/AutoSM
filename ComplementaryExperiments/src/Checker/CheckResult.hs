{-|
Description: Definition of the 'Checker.CheckResult.CheckResult' data type and related functions. 

In this module, the data type 'Checker.CheckResult.CheckResult' is 
defined. It is used in the checker to report all errors. 
-}
module Checker.CheckResult where

import Data.List (intercalate)

import qualified Rewriter.IR as IR
import Parser.Message
import Parser.Basic

import Data.Monoid
import qualified Data.Set as Set

{------------------------------------------------------------------------------}
{- Data Structure for check results -}

{- | 'CheckResult' represents the result of a well-formedness check. 
     The check was either a 'Success', or it was a 'Failure', in which 
	 case a list of the errors is attached. -}
data CheckResult 
    -- | All checks have been successful. 
    = Success
    -- | At least one check has failed. The failed checks are 
    --   in the list. 
    | Failure [Fail]
    deriving Eq
    
{- | Information about a failed check. -}
data Fail
    -- | A label has been used for multiple actions or multiple goals. 
    = DuplicateLabels (Label)
    -- | A sending action is not executable . 
    | NotExecutable IR.Action
    -- | Sender and receiver are equal. 
    | EqualSenderReceiver Label
    -- | A fresh variable is declared and collides with a variable that
    --   was declared earlier on.
    | NameCollision (Identifier, Label)
    -- | There are two declarations for a function with the same name. 
    | DuplicateFunctionDec Identifier
    -- | Function declaration with negative arity. 
    | NegativeArity Identifier
    -- | Function use where the function is used with a
    --   wrong arity. 
    | WrongArity (Identifier, Label)
    -- | Function use of a function that was never declared. 
    | UnknownFunction (Identifier, Label) 
    -- | A variable with the same name as a function. 
    | UsedFunAsVar (Identifier, Label) 
    -- | There are messages which are equal in the case-insensitive sense 
    --   but have different capitalizations. First label and identifier: 
    --   first occurrence. Second label and identifier: second occurrence, 
    --   in conflict with first one. 
    | InconsistentCases (Label, Identifier, Label, Identifier)
	-- | A goal that contains a message that cannot be constructed
	--   by a role mentioned in the goal. 
	| GoalNotExecutable (Label, Message, RoleName) 
	-- | A role is used in a security goal even though this role name 
	--   is not assigned any actions in the protocol. 
    | RoleInGoalWithNoAction (Label, RoleName) 
    deriving Eq
    
instance Show CheckResult where
    show Success = "All well-formedness checks were successful."
    show (Failure fails) = 
        "The following well-formedness checks failed:\n\n" ++
        intercalate "\n\n" (Prelude.map show fails) ++
        "\n\n" ++
        (show $ length fails) ++ 
        if (length fails == 1) then " well-formedness check failed." 
        else " well-formedness checks failed."
       
instance Show Fail where
    show (DuplicateLabels label) = 
        "Label '" ++ label ++ "' is declared multiple times."
    show (NotExecutable (IR.Send _ label _ _ msg kw)) = 
        "The sending action at label '" ++ label ++
        "' is not executable: Sender cannot construct message " ++ 
        show msg ++ " with current knowledge {" ++ 
        intercalate ", " (Prelude.map show (Set.toList kw)) ++ "}."
    show (EqualSenderReceiver label) = 
        "Sender and receiver are equal in the action with label '" ++
        label ++ "'."
    show (NameCollision (name, label)) = 
        "Variable '" ++ name ++ "' is declared as fresh " ++
        "value in the action with label '" ++ 
        label ++ 
        "' even though it was already declared earlier."
    show (DuplicateFunctionDec funname) = 
        "There are multiple declarations for a function with name '"
        ++ funname ++ "'."
    show (NegativeArity funname) = 
        "The function with name '" ++ funname ++ "' is declared " ++
        "with negative arity."
    show (WrongArity (funname, label)) = 
        "The function with name '" ++ funname ++ 
        "' is used with" ++
        " a wrong number of arguments in the action with label '" ++
        label ++ "'."
    show (UnknownFunction (funname, label) ) = 
        "A function with name '" ++ funname ++ 
        "' is used in the action " ++
        "with label '" ++ label ++ "' even though no " ++
        "function with this name has been declared."
    show (UsedFunAsVar (funname, label) ) = 
        "A variable with name '" ++ funname ++ 
        "' is used in the action " ++
        "with label '" ++ label ++ 
        "'. This name collides with the function of the same name."
    show (InconsistentCases (label1, id1, label2, id2)) = 
        "Conflicting identifiers: " ++ 
        "'" ++ id1 ++ "' (at label '" ++ label1 ++ "') and " ++ 
        "'" ++ id2 ++ "' (at label '" ++ label2 ++ "')."
    show (GoalNotExecutable (label, message, role)) = 
		"Message '" ++ show message ++ "' is used in goal '" ++
		label ++ "' but cannot be constructed by role " ++ role ++ 
		" at any time."
    show (RoleInGoalWithNoAction (label, role)) = 
        "Role " ++ role ++ " is mentioned in security goal '" ++ label ++
        "' but has no actions."

{- | The well-formedness of a protocol is checked by applying several 
     checks, one after the other. The well-formedness check as a whole
     is successful only if all checks succeed. As soon as one check fails, 
     the well-formedness is disproved and the result is a failure. If
     two failures are appended, the corresponding errors are
     appended. -}
-- instance Monoid CheckResult where  
--     mempty = Success
--     mappend Success cr = cr
--     mappend cr Success = cr
--     mappend (Failure f1) (Failure f2) = Failure (f1 ++ f2)

instance Semigroup CheckResult where
    Success <> cr = cr
    cr <> Success = cr
    Failure f1 <> Failure f2 = Failure (f1 ++ f2)

instance Monoid CheckResult where
    mempty = Success
    mappend = (<>)
    

{- | Returns if 'Success'. -}
isSuccess :: CheckResult -> Bool 
isSuccess cr = cr == Success

{- | Returns if 'Failure'. -}
isFailure :: CheckResult -> Bool 
isFailure = not . isSuccess 

{------------------------------------------------------------------------------}
