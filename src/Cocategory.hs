-- An attempt at an implementation of some of Chris Penner's ideas
-- described here: https://twitter.com/chrislpenner/status/1395242159410794496

{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}

module Cocategory where

data LPair a b where
  LPair :: a %1-> b %1-> LPair a b

applyLPair :: LPair a b %1-> (a %1-> b %1-> r) %1-> r
applyLPair (LPair x y) f = f x y

class Cocategory k where
  discharge :: k a a %1-> ()
  decompose :: k a c %1-> LPair (k a b) (k b c)

(<.>) :: () %1-> () %1-> ()
() <.> () = ()

