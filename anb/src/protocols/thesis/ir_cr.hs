{- Explicit implementation of the intermediate representation
   of the CR protocol from the thesis. -}
   
import Parser.Message
import Rewriter.IR
import qualified Data.Set as S

protocol = Protocol "CR" [] 
    [ Role "C" 
        [ Prepare (S.fromList [Sk "C", Pk "C", Var "C", Pk "R"])
        , Send True "cr_1" "R" ["n"] (Aenc (Var "n") (Pk "R")) 
            (S.fromList [Sk "C", Pk "C", Var "C", Pk "R", Var "n"])
        , Receive "cr_2" "R" (Hash (Var "n")) 
            (S.fromList [Sk "C", Pk "C", Var "C", Pk "R", Var "n"])
        ]
    , Role "R" 
        [ Prepare (S.fromList [Sk "R", Pk "R", Var "R"])
        , Receive "cr_1" "C" (Aenc (Var "n") (Pk "R")) 
            (S.fromList [Sk "R", Pk "R", Var "R", Var "n"])
        , Send True "cr_2" "C" [] (Hash (Var "n")) 
            (S.fromList [Sk "R", Pk "R", Var "R", Var "n"])
        ]
     ]
     [ Secret "n_secret" (Var "n") ["C", "R"]
     , WeakAuth "authNonInj" "C" "R" [(Var "n")]
     , StrongAuth "authNonInj" "C" "R" [(Var "n")]
     ]
