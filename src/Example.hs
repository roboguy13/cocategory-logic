module Example where

import           Implies

tripleNot :: Proof (Not (Not (Not p)) `Implies` Not p)

