module Test.Tests where

import qualified Test.Parser
import qualified Test.Message
import qualified Test.MessageModel
import Test.HUnit

import System.IO

main = do
    putStrLn "Testing Parser.msg:"
    res <- runTestTT Test.Parser.testCasesMessage
    putStrLn "Testing Parser.Message.multiply and Parser.Message.exponentiate:"
    runTestTT Test.Message.testCasesMulExp 
    putStrLn "Testing Rewriter.MessageModel.divide:"
    runTestTT Test.MessageModel.testDivision
    putStrLn "Testing Rewriter.MessageModel.reducel:"
    runTestTT Test.MessageModel.testLeftReduction
