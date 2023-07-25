{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Barbies.Linear (FunctorB (..), TraversableB (..)) where

import qualified Data.Functor.Linear as Control
import Data.Kind (Constraint, Type)
import Prelude hiding (Foldable (..), Traversable (..), ($), (.))

type FunctorB :: ((k -> Type) -> Type) -> Constraint
class FunctorB b where
  bmap :: (forall a. f a %1 -> g a) -> b f -> b g

type TraversableB :: ((k -> Type) -> Type) -> Constraint
class TraversableB b where
  btraverse :: (Control.Applicative e) => (forall a. f a %1 -> e (g a)) -> b f %1 -> e (b g)
