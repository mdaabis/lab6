--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab 6: Functors                                                            --
--------------------------------------------------------------------------------

module Lab6 where

import Prelude hiding (Functor(..))

--------------------------------------------------------------------------------

-- | We are definig our own copy of the Functor type class here rather than
-- using the one from Prelude so that we can redefine instances for it that
-- already exist in Prelude.
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map

--------------------------------------------------------------------------------

-- | This type is a wrapper around values of some type.
data Identity a = Identity a
    deriving (Eq, Show)

-- | 'runIdentity' @action@ extracts the value from @action@.
runIdentity :: Identity a -> a
runIdentity (Identity a) = a

instance Functor Identity where
    fmap = undefined

--------------------------------------------------------------------------------

data Const v a = Const v
    deriving (Eq, Show)

-- | 'getConst' @action@ extracts the value from @action@.
getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
    fmap = undefined

--------------------------------------------------------------------------------

-- | Represents two-dimensional points.
data Point a = Point a a
    deriving (Eq, Show)

instance Functor Point where
    fmap = undefined

--------------------------------------------------------------------------------

-- | Represents rose trees.
data RoseTree a = Leaf a | Node [RoseTree a]
    deriving (Eq, Show)

instance Functor RoseTree where
    fmap = undefined

--------------------------------------------------------------------------------

instance Functor ((->) r) where
    fmap = undefined

--------------------------------------------------------------------------------
