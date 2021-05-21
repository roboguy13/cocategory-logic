{-# LANGUAGE DataKinds #-}

module Example where

import           Implies

test :: T |- (T `And` T)
test = Implies

testVerified :: ()
testVerified = verify (and_elimL test)

