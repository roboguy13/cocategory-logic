{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Example where

import           Implies
import           Cocategory

example1 :: Proof (p |- (p && p))
example1 x = verify (and_elimL x)

-- To test 'decompose':
example2 :: forall p. Proof (p |- ((p && p) && p))
example2 x = applyPair (decompose x) go
  where
    go ::    (p |- (p && p)) %1
          -> ((p && p) |- ((p && p) && p)) %1
          -> ()
    go y z =
      example1 y <.> lemma z

    lemma :: Proof ((p && p) |- ((p && p) && p))
    lemma x = verify (and_elimL x)

