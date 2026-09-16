module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :.

succBinary :: Binary -> Binary
succBinary End = End :. One
succBinary (x :. Zero) = x :. One
succBinary (x :. One) = succBinary x :. Zero 

-- help function for integetToBinray
toBit :: Integer -> Bit
toBit n = if n `mod` 2 == 0
  then Zero
  else One

integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary n = integerToBinary (n `div` 2) :. toBit n

binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (x :. Zero) = binaryToInteger x * 2
binaryToInteger (x :. One) = 1 + binaryToInteger x * 2

hasLeadingZero :: Binary -> Bool
hasLeadingZero End = False
hasLeadingZero (End :. Zero) = True
hasLeadingZero (x :. _) = hasLeadingZero x

isEnd :: Binary -> Bool
isEnd End = True
isEnd _ = False 

canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise (x :. One) = canonicalise x :. One
canonicalise (x :. Zero) = addZero (canonicalise x)
  where
    addZero End = End
    addZero rest = rest :. Zero


addBinary :: Binary -> Binary -> Binary
addBinary a End = a 
addBinary End b = b 
addBinary (a1 :. One) (a2 :. One) = succBinary (addBinary a1 a2) :. Zero
addBinary (a1 :. One) (a2 :. Zero) = addBinary a1 a2 :. One 
addBinary (a1 :. Zero) (a2 :. One) = addBinary a1 a2 :. One 
addBinary (a1 :. Zero) (a2 :. Zero) = addBinary a1 a2 :. Zero 