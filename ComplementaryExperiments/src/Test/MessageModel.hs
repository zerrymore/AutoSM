module Test.MessageModel where

import Test.HUnit

import Parser.Message
import Rewriter.MessageModel

testDivision = 
    TestList $
        Prelude.map (\(a, b, c) -> 
             TestCase ( assertEqual
                        (show a ++ " `divide` " ++ show b) 
                        (a `divide` b) 
                        c)) testsDiv
          
testLeftReduction = 
    TestList $
       Prelude.map (\(a, b, c) -> 
             TestCase ( assertEqual
                        (show a ++ " `reducel` " ++ show b) 
                        (a `reducel` b) 
                        c)) testsLeftReduction         
                        
testsDiv = 
    [ ( Var $  "a"
      , Var $  "a" 
      , One) 
    , ( mlist [Var $  "a", Var $  "a"] 
      , Var $  "a"
      , Var $  "a" )
    , ( mlist [Var $  "b", Var $  "a"] 
      , Var $  "a"
      , Var $  "b" )
    , ( Exp (Var $  "a") (Var $  "b")
      , Exp (Var $  "a") (Var $  "b")
      , One ) 
    , ( Exp (mlist [(Var $  "b"), (Var $  "c")]) (Var $  "a")
      , Exp (Var $  "b") (Var $  "a")
      , Exp (Var $  "c") (Var $  "a") ) 
    , ( Exp (mlist [(Var $  "b"), (Var $  "c"), (Var $  "d")]) (Var $  "a")
      , Exp (mlist [(Var $  "b"), (Var $  "c")]) (Var $  "a")
      , Exp (Var $  "d") (Var $  "a") ) 
    , ( Exp (mlist [(Var $  "b"), (Var $  "c"), (Var $  "d")]) (Var $  "a")
      , Exp (Var $  "c") (Var $  "a") 
      , Exp (mlist [(Var $  "b"), (Var $  "d")]) (Var $  "a") )
    , (One, One, One) 
    ]
    
testsLeftReduction = 
    [ ( Exp (Var $  "a") (Var $  "b")
      , Var $  "a"
      , Var $  "b" )
    , ( Exp (Var $  "a") (Var $  "b")
      , Exp (Var $  "a") (Var $  "b")
      , One ) 
    , ( Exp (Var $  "a") (mlist [Var $  "b", Var $  "c"])
      , Exp (Var $  "a") (Var $  "b")
      , Var $  "c" ) 
    , ( Exp (Var $  "a") (mlist [Var $  "b", Var $  "c"])
      , Exp (Var $  "a") (Var $  "b")
      , Var $  "c" ) 
    , ( Exp (Var $  "a") (mlist [Var $  "b", Var $  "c", Var $  "d"])
      , Exp (Var $  "a") (Var $  "b")
      , mlist [Var $  "c", Var $  "d"] ) 
    , ( Exp (Var $  "a") (mlist [Var $  "b", Var $  "c", Var $  "d"])
      , Exp (Var $  "a") (mlist [Var $  "c", Var $  "d"])
      , Var $  "b" ) 
    ]
      
