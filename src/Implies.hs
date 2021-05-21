-- An attempt at an implementation of some of Chris Penner's ideas
-- described here: https://twitter.com/chrislpenner/status/1395242159410794496

{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Implies
  (Implies  -- NOTE: No data constructors are exported for 'Implies'
  ,type (|-)
  ,Proof
  ,verify
  -- ,decomposeCont

  ,Cocategory (..)

  ,Prop (..)

  ,t_intro
  ,f_elim
  ,and_intro
  ,and_elimL
  ,and_elimR
  ,or_introL
  ,or_introR
  ,or_elim
  ,entails_intro
  ,entails_elim
  ,not_intro
  ,not_elim
  ,exchange
  ,weaken
  ,contract
  ,reassoc
  )
  where

import           Cocategory

data Prop = T | F | Not Prop | And Prop Prop | Or Prop Prop | Entails Prop Prop

data Implies (p :: Prop) (q :: Prop) = Implies

instance Cocategory Implies where
  discharge Implies = ()

  decompose Implies = (Implies, Implies)


type (|-) = Implies
type Proof x = x %1-> ()

-- decomposeCont :: forall a b c r. (a |- c) %1-> ((a |- b) %1-> (b |- c) %1-> r) -> r
-- decomposeCont x k = go (decompose x)
--   where
--     go :: ((a |- b), (b |- c)) %1-> r
--     go (y, z) = k y z

verify :: Proof (p |- p)
verify = discharge

t_intro :: p |- T
t_intro = Implies

f_elim :: F |- q
f_elim = Implies

and_intro :: forall x p q. (x |- p) %1-> (x |- q) %1-> (x |- (p `And` q))
and_intro Implies Implies = Implies

and_elimL :: forall x p q. (x |- (p `And` q)) %1-> (x |- p)
and_elimL Implies = Implies

and_elimR :: forall x p q. (x |- (p `And` q)) -> (x |- q)
and_elimR Implies = Implies

or_introL :: forall x p q. (x |- p) -> (x |- (p `Or` q))
or_introL Implies = Implies

or_introR :: forall x p q. (x |- q) -> (x |- (p `Or` q))
or_introR Implies = Implies

or_elim :: forall x x' p q. (x |- (p `Or` q)) -> (p |- x') -> (q |- x') -> (x |- x')
or_elim Implies Implies Implies = Implies

entails_intro :: forall x p q. ((x `And` p) |- q) -> (x |- (p `Entails` q))
entails_intro Implies = Implies

entails_elim :: forall x p q. (x |- (p `Entails` q)) -> (x |- p) -> (x |- q)
entails_elim Implies Implies = Implies

not_intro :: forall x p q. (x |- (p `Entails` F)) -> (x |- Not p)
not_intro Implies = Implies

not_elim :: forall x p q. (x |- Not p) -> (x |- p) -> (x |- q)
not_elim Implies Implies = Implies


-- TODO: Are these structural rules necessary to be spelled out, or can
-- they be derived from the previous rules within this particular logic?
exchange :: forall x y p. ((x `And` y) |- p) -> ((y `And` x) |- p)
exchange Implies = Implies

weaken :: forall x y p. (x |- p) %1-> ((x `And` y) |- p)
weaken Implies = Implies

contract :: forall x y p. ((x `And` y `And` y) |- p) -> (x `And` y |- p)
contract Implies = Implies

reassoc :: forall x y z p. ((x `And` y) `And` z |- p) -> (x `And` (y `And` z) |- p)
reassoc Implies = Implies

