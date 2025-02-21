{- |
Description: Contains the actual compiler. 

This module contains the actual compiler that takes the 
path to a .anb file and translates it into Tamarin code. 
-}
module Compiler where

import System.IO  
import System.Environment
import System.Directory (doesFileExist) 
import Data.List (isSuffixOf, isPrefixOf) 
import Control.Monad.State




import Parser.ProtocolParser
import Checker.Checker
import Checker.CheckResult
import Rewriter.Rewriter
import Translator.Translator
import Data.Either hiding (isLeft)

{------------------------------------------------------------------------------}
{-- ANALYZING COMMAND LINE ARGUMENTS --}
      
{- | This data structure is used when analyzing the command line 
     arguments. The arguments that have not yet been analyzed are stored
     in 'remaining', the other fields are for storing information that
     is collected during analysis. -}          
data Options = 
    Options { remaining :: [String]
            , inputFile :: String
            , outputFile :: String
            , verbose :: Bool 
            } 
            deriving Show 
   
-- | Returns the number of remaining arguments
numArgs :: State Options Int
numArgs = do 
    state <- get
    return $ length $ remaining state  
    
-- | Returns the next argument and removes it from the state. 
popArg :: State Options (Maybe String)
popArg = do 
    state <- get
    case remaining state of
        (a:as) -> do
            put $ state {remaining = as}
            return $ Just a
        [] -> return Nothing

    
{- | Iterates over all the arguments in the state and analyzes them. 
     Returns @Left <error message>@ if there is an error, and
     @Right <options>@ otherwise. -}
handleArgs :: State Options (Either String Options)
handleArgs = do
    state <- get
    len <- numArgs
    if len > 0 then do
        nextArg <- popArg
        case nextArg of
            Just arg -> processArg arg
            Nothing -> return $ Left "Expected an argument but none found."
    else do
        -- No more arguments to handle.
        if inputFile state == "" then
            return $ Left "No input file specified."
        else if outputFile state == "" then do
            -- Automatically set output file name based on input file
            let infile = inputFile state
                root = take (length infile - length ".anb") infile
            put $ state { outputFile = root ++ ".spthy" }
            return $ Right state
        else
            return $ Right state

processArg :: String -> State Options (Either String Options)
processArg arg = do
    state <- get
    if arg == "-o" then do
        len <- numArgs
        ofile <- popArg
        case ofile of
            Just file ->
                if len < 1 then
                    return $ Left "No output file specified after '-o'."
                else if outputFile state /= "" then
                    return $ Left "Multiple output files specified."
                else if not (isSuffixOf ".spthy" file) then
                    -- return $ Left "Wrong or no file extension in file '" ++ file ++ "' (expecting '.spthy')."
                    return $ Left ("Wrong or no file extension in file '" ++ file ++ "' (expecting '.spthy').")
                else do
                    put $ state { outputFile = file }
                    handleArgs
            Nothing -> return $ Left "No output file name after '-o'."
    else if arg == "-v" then do
        put $ state { verbose = True }
        handleArgs
    else if isPrefixOf "-" arg then
        return $ Left $ "Unknown option: '" ++ arg ++ "'."
    else
        if inputFile state /= "" then
            return $ Left "Multiple input files specified."
        else if not (isSuffixOf ".anb" arg) then
            return $ Left $ "Wrong or no file extension in file '" ++ arg ++ "' (expecting '.anb')."
        else do
            put $ state { inputFile = arg }
            handleArgs

  
{------------------------------------------------------------------------------}

{- | Information message. -}
info = 
    "Usage:\n" ++
    "    anb [input-file and options]\n" ++ 
    "\n" ++ 
    "Options:\n" ++
    "    -o <file>       Place the output into <file>.\n" ++ 
    "    -v              Verbose output.\n" ++
    "    -h              Display this information."
   
-- | Returns if the argument is @Left@. 
isLeft :: Either a b -> Bool 
isLeft = null . rights . return

-- | Returns if the argument is @Right@. 
isRight :: Either a b -> Bool 
isRight = null . lefts . return
   
   
{- | The compiler. -}  
run_anb :: IO () 
run_anb = do   
    args <- getArgs
    
    options <- return $ fst $ runState handleArgs 
               $ Options{remaining=args, inputFile="", outputFile="", 
                         verbose=False}
    if "-h" `elem` args || "--help" `elem` args then do 
        putStrLn info
    else if isLeft options then do 
        (Left errormsg) <- return options
        putStrLn $ "Error: " ++ errormsg
        
        return () 
    else do 
        (Right options) <- return options
        
        -- CHECKING IF INPUT FILE EXISTS
        inputExists <- doesFileExist $ inputFile options
        
        if inputExists then do 
        
            when (verbose options) $ putStrLn $ replicate 80 '*'
            -- PARSING -- 
            when (verbose options) $ putStrLn "Parsing ..."
            
            parseResult <- parseAnBFile $ inputFile options
            
            case parseResult of 
                Left e -> do 
                    putStrLn "Parse error:"
                    putStrLn $ show e
                Right parseTree -> do
                    when (verbose options) $ putStrLn $ "Parsed protocol:"
                    when (verbose options) $ putStrLn $ show parseTree
                    when (verbose options) $ putStrLn $ replicate 80 '*'
                    -- REWRITING -- 
                    when (verbose options) $ putStrLn "Rewriting ..."
                    
                    protocol <- return $ translateToIR parseTree
                    
                    when (verbose options) $ putStrLn $ 
                        "Intermediate representation:"
                    when (verbose options) $ putStrLn $ show protocol 
                    when (verbose options) $ putStrLn $ replicate 80 '*'
                    
                    -- EXECUTING CHECKS -- 
                    when (verbose options) $ putStrLn 
                        "Checking well-formedness ..."
                    
                    checkResult <- return $ doAllChecks protocol
           
                    if checkResult /= Success then do 
                        putStrLn $ show checkResult
                        
                    else do 
                        when (verbose options) $  
                            putStrLn "All well-formedness checks passed."
                            
                        when (verbose options) $ putStrLn $ replicate 80 '*'
                        when (verbose options) $  
                            -- putStrLn "Producing Tamarin code ..."
                            putStrLn $ translateProtocol protocol
                        outputText <- return $ translateProtocol protocol

                        writeFile "" outputText
                        
                        when (verbose options) $ putStrLn 
                            "Produced Tamarin code:\n"
                        when (verbose options) $ putStr outputText 
                        when (verbose options) $ putStrLn $ replicate 80 '*'
                        return () 
        else do 
            putStrLn $ "Error: Input file '" ++ inputFile options ++ 
                       "' does not exist."
            return () 
   
   
