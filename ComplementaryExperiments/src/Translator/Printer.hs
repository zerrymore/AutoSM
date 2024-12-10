{- |
Description: Functions for writing the output. 

This module contains functions for printing the Tamarin output. 
Provideed automatic line breaks and indentation. -}
module Translator.Printer where 

import Translator.State
import Data.List (replicate)
import Data.List.Split (splitOn)
import Control.Monad.State (when)

{- | Maximum width of a line. 
     The compiler will try to produce no lines that are longer than this, 
     however, this is not guaranteed. Values below 50 do not produce nice
     output. -}
maxOffset = 80

{- | Formats a list and appends it to the output. 

     1. Printing function. This function is used to print 
        the elements of the list. 

     2. Seperator printing funciton. This function is used to print
        the seperators between the elements of the list. 

     3. List to be printed. -}
printList :: (a -> TranslationState ()) -> 
             (TranslationState ()) -> [a] -> 
             TranslationState () 
printList f i [] = return () 
printList f i [l] = do 
    -- print the (only) element of the list
    f l 
printList f i (l:ls) = do
    -- print the first element of the list
    f l 
    -- print the separator
    i 
    -- print the rest of the list
    printList f i ls
    
{- | Appends a string to the output. 
     Inserts a newline character before the string
     if the line would become too long otherwise. 
	 The string is treated as one chunk, i.e. the string will always 
	 be printed as it is (no line breaks will be inserted). 
     Newline characters in the string will be printed 
     (ignoring indentation rules). -}
printRawString :: String -> TranslationState () 
printRawString str = do 
    -- Get the current cursor position. 
    offset <- getOffset
    -- If the line would be too long after insertion of the string, 
    -- add a newline character (automatically updates cursor position). 
    when (offset + (length str) > maxOffset) printNewline
    -- Append the string to the output.
    addToOutput str 
    -- Update the cursor position. 
    addToOffset (length str)    
    
{- | Appends a string to the output. Other than printRawString, this 
     function will detect spaces and newline characters in the string. 
     Line breaks may be inserted at spaces in the string. 
     After a line break, indentation rules will be observed.  -}
printString :: String -> TranslationState () 
printString str =
    let
        chunks = splitOn "\n" str
        wordchunks = map (splitOn " ") chunks
        
        printChunk :: [String] -> TranslationState () 
        printChunk cs = do 
            printList printRawString printSpace cs
            return ()
    in do 
        printList printChunk printNewline wordchunks
        return () 
        
    
{- | Appends a space to the output, 
     or goes to a new line if the line is already full. -}
printSpace :: TranslationState ()
printSpace = do 
    -- Get the current cursor position
    offset <- getOffset 
    if offset + 1 > maxOffset then do 
        -- Line is full, print newline (automatically updates cursor position)
        printNewline
    else do 
        -- Line is not full, print space. 
        addToOutput " "
        -- And update the cursor position. 
        addToOffset 1
        
{- | Appends a newline character to the output and adds indentation spaces
     on the new line. -}
printNewline :: TranslationState () 
printNewline = do 
    -- Get current indentation
    indent <- getIndentation
    -- Set the current cursor position to the indentation value
    putOffset indent
    -- Append newline characer and spaces to output. 
    addToOutput $ "\n" ++ (replicate indent ' ') 
