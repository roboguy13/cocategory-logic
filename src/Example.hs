{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}

module Example where

import           Implies
import           Cocategory

test :: Proof (p |- (p `And` p))
test x = verify (and_elimL x)

test2 :: forall a b (c :: Prop). (a |- T) %1-> b
test2 x = applyLPair (decompose x) go2
  where
    -- go :: LPair (a |- c) (c |- T) %1-> b
    -- go (LPair y z) = go2 y z

    go2 :: (a |- c) %1 -> (c |- 'T) %1 -> b
    go2 = undefined
  -- case decompose x of
  --   LPair y z ->
  --     let () = verify y
  --         () = verify z in
  --       undefined

-- test2 :: forall p. Proof (p |- ((p `And` p) `And` p))
-- test2 x = decomposeCont x (\y z ->
--   let () = verify y
--       () = verify z
--   in undefined
--   ) --go (decompose x)
  -- case decompose x of
  --   _ -> verify x
  -- where
  --   go :: Proof _
  --   go (y, z) = case verify z of () -> verify y

  -- case decompose x of
  --   (y, z) ->
  --     case verify (weaken y) of
  --       () ->
  --         test z

  -- let (y, z) = decompose x
  --     -- () = _
  -- in _ --verify z

-- test' :: Proof ((p `And` p) |- p)
-- test' x = verify (and_elimL x)

-- test' :: Proof (T |- (T `And` T))
-- test' x =
--   let (y, z) = decompose x :: (T |- T, T |- (T `And` T))
--       (p, q) = decompose z :: (T |- T, T |- (T `And` T))
--   in
--   verify (and_intro y p)

