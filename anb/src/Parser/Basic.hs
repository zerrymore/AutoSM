{- |
Description: General type declarations. 

Type declarations that are used everywhere in the compiler. 
-}
module Parser.Basic where

-- | A label naming a protocol, an action or a security goal. 
type Label      = String; 
-- | The name of a role. 
type RoleName   = String; 
-- | An identifier is the name of a function or a variable. 
type Identifier = String; 


