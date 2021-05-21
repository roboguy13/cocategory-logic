{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}

module Example where

import           Implies
import           Cocategory

example1 :: Proof (p |- (p `And` p))
example1 x = verify (and_elimL x)

-- To test 'decompose':
example2 :: forall p. Proof (p |- ((p `And` p) `And` p))
example2 x = applyPair (decompose x) go
  where
    go :: (p |- (p `And` p)) %1 -> ((p `And` p) |- ((p `And` p) `And` p)) %1 -> ()
    go y z =
      example1 y <.> lemma z

    lemma :: Proof ((p `And` p) |- ((p `And` p) `And` p))
    lemma x = verify (and_elimL x)

