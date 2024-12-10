module Test.Message where

import Test.HUnit

import Parser.Message

testCasesMulExp :: Test
testCasesMulExp = 
    let 
        translate :: 
                     (Message -> Message -> Message) -> 
                     (Message, Message, Message) -> 
                     Test
        translate fun (m1, m2, expected) = 
            let 
                label = "(" ++ show m1 ++ ") * (" ++ show m2 ++ ")"
                result = fun m1 m2
            in 
                TestCase (assertEqual label result expected)
    in 
        TestList
        [ (TestList $ Prelude.map (translate multiply) testsMultiply)
        , (TestList $ Prelude.map (translate exponentiate) testsExponentiate)]

checkMultiply :: Message -> Message -> Message -> Bool 
checkMultiply m1 m2 expected = 
    m1 `multiply` m2 == expected
    
testsMultiply :: [(Message, Message, Message)]
testsMultiply = 
    [ ( (Var $  "a"), (Var $  "b"), (mlist [(Var $  "a"), (Var $  "b")]) )
    , ( (Var $  "a")
      , (mlist [(Var $  "b"), (Var $  "c")])
      , (mlist [(Var $  "a"), (Var $  "b"), (Var $  "c")]) ) 
    , ( (mlist [(Var $  "a"), (Var $  "b")])
      , (Var $  "c")
      , (mlist [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ( (mlist [(Var $  "a"), (Var $  "b")]), 
        (mlist [(Var $  "a"), (Var $  "b")]), 
        (mlist [(Var $  "a"), (Var $  "a"), (Var $  "b"), (Var $  "b")]) ) 
    , ( (mlist [(Var $  "a"), (Var $  "b")])
      , (mlist [(Var $  "c"), (Var $  "d")])
      , (mlist [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d")]) ) 
    , ( (Exp (Var $  "a") (Var $  "b"))
      , (Exp (Var $  "b") (Var $  "a"))
      , (mlist [(Exp (Var $  "a") (Var $  "b")), (Exp (Var $  "b") (Var $  "a"))]) )
    , ( (Exp (Var $  "a") (Var $  "b"))
      , (Exp (Var $  "a") (Var $  "b")) 
      , (Exp (mlist [(Var $  "a"), (Var $  "a")]) (Var $  "b")) )
    , ( (Exp (Var $  "a") (Var $  "b"))
      , (Exp (Var $  "c") (Var $  "b")) 
      , (Exp (mlist [(Var $  "a"), (Var $  "c")]) (Var $  "b")) )
    , ( (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (Var $  "c") (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b"), (Var $  "c")]) (Var $  "e")) ) 
    , ( (Exp (Var $  "c") (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b"), (Var $  "c")]) (Var $  "e")) ) 
    , ( (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b"), (Var $  "a"), (Var $  "b")]) (Var $  "e")) )
    , ( (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "c"), (Var $  "d")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "c"), (Var $  "d"), (Var $  "b")]) (Var $  "e")) )  
    , ( One
      , One
      , One ) 
    , ( One
      , (Var $  "a")
      , (Var $  "a") ) 
    , ( (Var $  "a")
      , One
      , (Var $  "a") ) 
    , ( (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , One
      , (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e")) ) 
    , ( One
      , (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e"))
      , (Exp (mlist [(Var $  "a"), (Var $  "b")]) (Var $  "e")) ) 
    , ( (Gamma ( "alpha") (Var $  "a"))
      , (Var $  "b")
      , (mlist [(Gamma ( "alpha") (Var $  "a")), (Var $  "b")]) ) 
    , ( (Var $  "b")
      , (Gamma ( "alpha") (Var $  "a"))
      , (mlist [(Gamma ( "alpha") (Var $  "a")), (Var $  "b")]) ) 
    , ( (Gamma ( "alpha") (Var $  "a"))
      , (Gamma ( "beta") (Var $  "b"))
      , (mlist [(Gamma ( "alpha") (Var $  "a")), (Gamma ( "beta") (Var $  "b"))] ) )  
    ]
    
    
testsExponentiate :: [(Message, Message, Message)]
testsExponentiate = 
    [ ( (Var $  "a")
      , (Var $  "b")
      , (Exp (Var $  "a") (Var $  "b")) ) 
    , ( (Exp (Var $  "a") (Var $  "b"))
      , (Exp (Var $  "a") (Var $  "b"))
      , (Exp (Var $  "a") (mlist [(Var $  "b"), (Exp (Var $  "a") (Var $  "b"))])) ) 
    , ( (Exp (Var $  "a") (Var $  "b"))
      , (Var $  "a")
      , (Exp (Var $  "a") (mlist [(Var $  "b"), (Var $  "a")])) ) 
    , ( (Var $  "a")
      , (Exp (Var $  "b") (Var $  "c"))
      , (Exp (Var $  "a") (Exp (Var $  "b") (Var $  "c"))) ) 
    , ( (Exp (Var $  "a") (mlist [(Var $  "b"), (Var $  "c")]))
      , (mlist [(Var $  "d"), (Var $  "e")])
      , (Exp (Var $  "a") (mlist [Var $  "e", Var $  "b", Var $  "c", Var $  "d"])) ) 
    ]
    

