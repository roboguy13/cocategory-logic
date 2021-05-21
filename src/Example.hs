{-# LANGUAGE DataKinds #-}

module Example where

import           Implies

test :: Proof (T |- (T `And` T))
test x = verify (and_elimL x)

