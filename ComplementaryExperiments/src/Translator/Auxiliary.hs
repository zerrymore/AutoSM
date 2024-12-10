{- |
Description: Auxiliary functions for writing Tamarin code to output. 

This module contains auxiliary functions for printing Tamarin code. 
-}
module Translator.Auxiliary where

import Parser.Basic
import Parser.Message
import Rewriter.IR
import Translator.State
import Translator.Printer

import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet


{- | Writes a Tamarin rule to the output. 
     Parameters: 

     1. Name of the rule

     2. Equations for the let block 

     3. Preconditions (Predicates)

     4. Labels

     5. Output Predicates 

     6. Function used for printing of message-}
printRule :: String -> 
             [Equation] -> 
             [Predicate] -> 
             [Predicate] -> 
             [Predicate] ->
             (Message -> TranslationState ()) ->
             TranslationState String
printRule name letBlock precond labels output printer = 
    do printString $ "rule " ++ name ++ ":"
       addToIndentation 4
       if length letBlock /= 0 then 
           do printNewline
              printString "let"
              addToIndentation 4
              printNewline
              printList printEquation printNewline letBlock
              addToIndentation (-4)
              printNewline
              printString "in"
              addToIndentation 4
              printNewline
       else 
           do addToIndentation 4
              printNewline
       printString "[ "
       addToIndentation 2
       printList 
           (printPredicate printer) 
           (printString "," >> printNewline) 
           precond
       addToIndentation (-2)
       printNewline
       printString "]"
       printNewline 
       printString "--[ " 
       addToIndentation 4
       printList 
           (printPredicate printer) 
           (printString "," >> printNewline) 
           labels
       addToIndentation (-4)
       printSpace 
       printString "]->"
       printNewline
       printString "[ "
       addToIndentation 2
       printList 
           (printPredicate printer) 
           (printString "," >> printNewline) 
           output
       addToIndentation (-2)
       printNewline
       printString "]"
       addToIndentation (-4)
       addToIndentation (-4)
       getOutput
       
{- | Writes a predicate to the output. The first parameter
     is the function used for the printing of the messages. -}
printPredicate :: (Message -> TranslationState ()) ->
                  Predicate -> 
                  TranslationState () 
printPredicate printer (name, params) = 
    do printString $ name ++ "("
       printList printer (printString "," >> printSpace) params
       printString ")"
    
{- | Writes an equation to the output. -}
printEquation :: Equation -> TranslationState () 
printEquation (left, right) = 
    do printString $ left
       printSpace 
       printString "="
       printSpace 
       printMessage right 
       
{- | Writes a message to the output in Tamarin code. -}
printMessage :: Message -> TranslationState () 
printMessage (Gamma i m) = do 
    printString i
    -- printString "["
    -- printMessage m 
    -- printString "]"
printMessage (Var m) = do 
    freshNames <- getFreshNames
    pubNames <- getPublicNames
    if m `elem` freshNames then 
        printString $ '~' : m 
    else if m `elem` pubNames then 
        printString $ '$' : m
    else
        printString m 
printMessage (Str s) = do 
    printString $ "'" ++ s ++ "'"
printMessage (Concat m1 m2) = 
    let 
        subcon (Concat m1 m2) = do 
            subcon m1 >> printString ", " >> subcon m2
        subcon m = printMessage m 
    in do 
        printString "<"; subcon m1; printString ", "
        subcon m2; printString ">"
printMessage (Aenc m1 m2) = do 
    printString "aenc"; printString "{"; printMessage m1; 
    printString "}"; printMessage m2
printMessage (Senc m1 m2) = do     
    printString "senc"; printString "{"; printMessage m1; 
    printString "}"; printMessage m2
printMessage (Hash m) = 
    printString "h" >> printString "(" >> printMessage m >> printString ")"
printMessage (Mul ms) = do 
    printString "("
    printList printMessage 
       (printSpace >> printString "*" >> printSpace) 
       (MultiSet.toList ms)
    printString ")"
printMessage (Exp m1 m2) = 
    printString "(" >> printMessage m1 >> printString " ^ " >>
    printMessage m2 >> printString ")"
printMessage (Pk p) = 
    do printString $ "pk(k_" ++ p ++ ")"
printMessage (Sk p) = 
    do printString $ "sk(k_" ++ p ++ ")"
printMessage (K p1 p2) = 
    do printString $ "k_" ++ p1 ++ "_" ++ p2
printMessage (Fun n m) =
    do printString $ n 
       printString "(" 
       lprint m 
       printString ")" 
        where lprint [] = return () 
              lprint (m:[]) = printMessage m
              lprint (m:ms) = 
                  do printMessage m 
                     printString ","
                     printSpace
                     lprint ms
                     
{- | Writes a message to the output in Tamarin code. 
     Same as 'printMessage', however, 'printFullMessages' 
	 does not print the symbol when encountering a ghost symbol
	 but the message it represents. -}
printFullMessage :: Message -> TranslationState () 
printFullMessage (Gamma _ m) = printMessage m 
printFullMessage m = printMessage m   

