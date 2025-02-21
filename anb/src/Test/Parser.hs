module Test.Parser where

import Test.HUnit

import Text.Parsec.Prim (parse)
import qualified Data.MultiSet as MS
import qualified Parser.ProtocolParser as Parser

import Parser.Message

{- | Takes a string and a message. Parses the string as a message and 
     compares it to the message.  -}
checkMessage :: String -> Message -> Bool 
checkMessage inp msg =
    case parse Parser.msg "" inp of 
        Left e -> False
        Right r -> r == msg

testsMessage :: [(String, Message)]
testsMessage = 
    [ ("a", Var ( "a"))
    , ("(a)", Var ( "a"))
    , ("<a>", Var ( "a"))
    , ("( a )", Var ( "a"))
    , ("< a >", Var ( "a"))
    , ("((a))", Var ( "a"))
    , ("<<a>>", Var ( "a"))
    , ("(<a>)", Var ( "a"))
    , ("<(a)>", Var ( "a"))
    , ("( < (a) > )", Var ( "a"))
    , ("<<<(((<<<a>>>)))>>>", Var ( "a"))
    , ("a123", Var ( "a123"))
    , ("'a__12'", Str "a__12") 
    , ("pk(A)", Pk ( "A")) 
    , ("pk ( A ) ", Pk ( "A"))
    , ("PK(A)", Pk ( "A"))
    , ("Pk(A)", Pk ( "A"))
    , ("pK(A)", Pk ( "A")) 
    , ("sk(B)", Sk ( "B"))
    , ("sk ( A ) ", Sk ( "A"))
    , ("sk ( A123 )", Sk ( "A123"))
    , ("aenc{m}pk(A)", Aenc (Var ( "m")) (Pk ( "A")))
    , ("aenc{m1 . m2}pk(A)", Aenc (Concat (Var $  "m1") (Var $  "m2")) (Pk $  "A"))
    , ("aenc { m } pk ( A )", Aenc (Var $  "m") (Pk $  "A"))
    , ("aenc{ < m1 . m2 > }pk(A)", 
        Aenc (Concat (Var $  "m1") (Var $  "m2")) (Pk $  "A") )
    , ("senc{m}pk(A)", Senc (Var $  "m") (Pk $  "A"))
    , ("senc{m1 . m2}pk(A)", Senc (Concat (Var $ "m1") (Var $  "m2")) (Pk $  "A"))
    , ("senc { m } pk ( A )", Senc (Var $  "m") (Pk $  "A"))
    , ("senc{ < m1 . m2 > }pk(A)", 
        Senc (Concat (Var $  "m1") (Var $  "m2")) (Pk $  "A") ) 
    , ("senc{m}key", Senc (Var $  "m") (Var $  "key") )
    , ("senc{m1 . m2}key", Senc (Concat (Var $  "m1") (Var $  "m2")) (Var $  "key") )
    , ("senc { m } key", Senc (Var $  "m") (Var $  "key") )
    , ("senc{ < m1 . m2 > }key", 
        Senc (Concat (Var $  "m1") (Var $  "m2")) (Var $  "key") ) 
    , ("senc{ < m1 . m2 > } < m1 . m2 >", 
        Senc (Concat (Var $  "m1") (Var $  "m2")) (Concat (Var $  "m1") (Var $  "m2")) ) 
    , ("senc{ m1 . m2 } m1 . m2", 
        Concat (Senc (Concat (Var $  "m1") (Var $  "m2")) (Var $  "m1")) (Var $  "m2") ) 
    , ("h(m)", Hash (Var $ "m") ) 
    , ("h(m1 . m2)", Hash (Concat (Var $  "m1") (Var $  "m2")))
    , ("h(senc{m1.m2}m1.m2)", 
       Hash (
       Concat (Senc (Concat (Var $  "m1") (Var $  "m2")) (Var  $ "m1")) (Var $  "m2") 
       ) )
    , ("k(A, B)", K ( "A") ( "B"))
    , ("K ( A , B )", K ( "A") ( "B"))
    , ("senc{m1 . m2}k(A, B)", 
       Senc (Concat (Var  $ "m1") (Var  $ "m2")) (K ( "A") ( "B")) )
    , ("a . b . c", 
       Concat (Var $  "a") (Concat (Var $  "b") (Var $  "c")) )
    , ("a ^ b ^ c", 
       Exp (Var $  "a") (Exp (Var $  "b") (Var $  "c")) )
    , ("a . < b . c >", 
       Concat (Var $  "a") (Concat (Var $  "b") (Var $  "c")) )
    , ("a . <b . c>", 
       Concat (Var $  "a") (Concat (Var $  "b") (Var $  "c")) )
    , ("a ^ (b ^ c)", 
       Exp (Var $  "a") (Exp (Var $  "b") (Var $  "c")) )
    , ("a ^ ((b ^ c))", 
       Exp (Var $  "a") (Exp (Var $  "b") (Var $  "c")) )
    , ("a ^ ((((( (b) ^  (c) )))))", 
       Exp (Var $  "a") (Exp (Var $  "b") (Var $  "c")) )
    , ("(a . b) . c", 
       Concat (Var $  "a") (Concat (Var $  "b") (Var $  "c")) ) 
    , ("(a ^ b) ^ c", 
       Exp (Var $  "a") (Mul (MS.fromList [(Var $  "b"), (Var $  "c")])) ) 
    , ("(a ^ c) ^ b", 
       Exp (Var $  "a") (Mul (MS.fromList [(Var $  "b"), (Var $  "c")])) ) 
    , ("((a ^ c) ^ b) ^ d", 
       Exp (Var $  "a") (Mul (MS.fromList [(Var $  "b"), (Var $  "c"), (Var $  "d")])) )
    , ("(a ^ (c * b)) ^ d", 
       Exp (Var $  "a") (Mul (MS.fromList [(Var $  "b"), (Var $  "c"), (Var $  "d")])) )
    , ("(a ^ c) ^ (b * d)", 
       Exp (Var $  "a") (Mul (MS.fromList [(Var $  "b"), (Var $  "c"), (Var $  "d")])) ) 
    , ("a ^ (d ^ e) ^ f", 
       Exp (Var $  "a") 
       (Exp (Var $  "d") (Mul (MS.fromList [(Var $  "e"), (Var $  "f")])))  )
    , ("(a ^ (d ^ e) ^ f) ^ e", 
       Exp (Var $  "a") 
       (Mul ( MS.fromList 
       [(Exp (Var $  "d") (Mul (MS.fromList [(Var $  "e"), (Var $  "f")]))), 
        (Var $ "e") ] )) )
    , ("a * b", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b")]) )
    , ("a * b * c", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ("(a * b) * c", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ("a * (b * c)", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ("(((a * b))) * c", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ("((((((a * b))) * (((<<<c>>>)))))))", 
       Mul (MS.fromList [(Var $  "a"), (Var $  "b"), (Var $  "c")]) )
    , ("(a * b) * (c * d)", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d")]) )
    , ("((a * b) * (c * d))", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d")]) )
    , ("b * c * d * a", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d")]) )
    , ("a * b * c * d", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d")]) )
    , ("a * b * c * d * e * f", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d"), (Var $  "e"), (Var $  "f")]) )
    , ("(a * b) * (c * d * e) * f", 
       Mul (MS.fromList 
       [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d"), (Var $  "e"), (Var $  "f")]) )
    , ("senc{a}sk(A) * senc{a}sk(A)", 
       Mul (MS.fromList [(Senc (Var $  "a") (Sk $  "A")), (Senc (Var $  "a") (Sk $  "A"))])
       )
    , ("foo('param')", 
       Fun ( "foo") [(Str "param")] )
    , ("foo ()", 
       Fun ( "foo") [] ) 
    , ("foo (  )", 
       Fun ( "foo") [] ) 
    , ("foo123 (a, b, c, d, e)", 
       Fun ( "foo123") [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d"), (Var $  "e")] )
    , ("foo (a, b, <c>, d, e)", 
       Fun ( "foo") [(Var $  "a"), (Var $  "b"), (Var $  "c"), (Var $  "d"), (Var $  "e")] )
    , ("foo (a . b)", 
       Fun ( "foo") [(Concat (Var $  "a") (Var $  "b"))] )
    , ("foo (a . b, a . b )", 
       Fun ( "foo") [(Concat (Var $  "a") (Var $  "b")), 
                  (Concat (Var $  "a") (Var $  "b"))] )
    , ("foo (b . a , a . b )", 
       Fun ( "foo") [(Concat (Var $  "b") (Var $  "a")), 
                  (Concat (Var $  "a") (Var $  "b"))] )
    , ("foo (<b . a> , <a . b>)", 
       Fun ( "foo") [(Concat (Var $  "b") (Var $  "a")), 
                  (Concat (Var $  "a") (Var $  "b"))] )
    , ("K(A, B)", K "A" "B")
    , ("K(B, A)", K "A" "B")
    -- Checks for canonical form --
    , ("senc{senc{m}key}key", Var "m")
    , ("aenc{aenc{m}pk(A)}sk(A)", Var "m")
    , ("aenc{aenc{m}sk(A)}pk(A)", Var "m")
    , ("a . b . c . d", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("<a . b> . <c . d>", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("a . <b . c> . d", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("a . <b . c . d>", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("<a . b . c> . d", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("<<a . b . c>> . d", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("aenc{aenc{<<<a . b> . c>> . d}pk(S)}sk(S)", 
       Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))) 
    , ("h(<a . b . c> . d)", 
      Hash (Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d")))))
    , ("foo(h(<a . b . c> . d), aenc{aenc{m}sk(A)}pk(A))", 
      Fun "foo" [(Hash 
      (Concat (Var "a") (Concat (Var "b") (Concat (Var "c") (Var "d"))))), 
      Var "m"])
    , ("a * b * c * d", 
       Mul $ MS.fromList [Var "a", Var "b", Var "c", Var "d"])
    , ("a * (b * c * d)", 
       Mul $ MS.fromList [Var "a", Var "b", Var "c", Var "d"])
    , ("((a * b) * c) * d", 
       Mul $ MS.fromList [Var "a", Var "b", Var "c", Var "d"])
    , ("(a * b) * (c * d)", 
       Mul $ MS.fromList [Var "a", Var "b", Var "c", Var "d"])
    , ("a * (b * (c * d))", 
       Mul $ MS.fromList [Var "a", Var "b", Var "c", Var "d"])
    , ("((a ^ b) ^ c) ^ d", 
       Exp (Var "a") (Mul $ MS.fromList [Var "b", Var "c", Var "d"]))
    , ("(a ^ b) ^ (c * d)", 
       Exp (Var "a") (Mul $ MS.fromList [Var "b", Var "c", Var "d"]))
    , ("(a ^ (b * d)) ^ c", 
       Exp (Var "a") (Mul $ MS.fromList [Var "b", Var "c", Var "d"]))
    , ("(a ^ (b * c)) ^ d", 
       Exp (Var "a") (Mul $ MS.fromList [Var "b", Var "c", Var "d"]))
    , ("(a ^ b) ^ (d ^ c)", 
       Exp (Var "a") (Mul $ MS.fromList [Var "b", Exp (Var "d") (Var "c")]))
    ]

{- | Checks the message parser. -}
testCasesMessage = 
    TestList $ Prelude.map ( \(st, ms) -> 
    TestCase (assertBool st (checkMessage st ms)) ) testsMessage

