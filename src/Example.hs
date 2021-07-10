{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Example where

import           Implies
import           CoCategory

import           Prelude.Linear ((&))

example1 :: Proof (p |- (p && p))
example1 x = verify (and_elimL x)

-- To test 'decompose':
example2 :: forall p. Proof (p |- ((p && p) && p))
example2 x =
  decompose x & \case
    (y, z) -> example1 y <.> lemma z
  where
    lemma :: Proof ((p && p) |- ((p && p) && p))
    lemma x = verify (and_elimL x)

