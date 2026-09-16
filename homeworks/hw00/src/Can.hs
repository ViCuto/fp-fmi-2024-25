{-# LANGUAGE EmptyDataDeriving #-}

module Can where

import Binary (Binary (..), Bit (..))  

data LeadingOne
  = JustOne
  | AppendOne LeadingOne Bit
  deriving (Show)


canOne :: LeadingOne
canOne = JustOne 


data Can
  = TheZero              
  | WithOne LeadingOne 
  deriving (Show)


canZero :: Can
canZero = TheZero  


snoc :: Can -> Bit -> Can
snoc = undefined
-- snoc x Zero = x  
-- snoc TheZero One = WithOne TheOne  
-- snoc (WithOne TheOne) One = WithOne(TheOne One) 
-- snoc (WithOne (TheOne x)) One = oOo (TheOne leadingOne One)



forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne = undefined

forget :: Can -> Binary
forget = undefined

canonicalise :: Binary -> Can
canonicalise = undefined
