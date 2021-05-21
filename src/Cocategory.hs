-- An attempt at an implementation of some of Chris Penner's ideas
-- described here: https://twitter.com/chrislpenner/status/1395242159410794496

{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}

module Cocategory where

class Cocategory k where
  discharge :: k a a %1-> ()
  decompose :: k a c %1-> (k a b, k b c)  -- Should this be non-linear?

