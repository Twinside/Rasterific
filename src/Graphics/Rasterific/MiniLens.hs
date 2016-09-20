{-# LANGUAGE RankNTypes #-}
module Graphics.Rasterific.MiniLens
    ( -- * Types
      Lens
    , Lens'
    , Traversal
    , Traversal'
    , lens

      -- * Getter
    , (.^)
    , use

      -- * Setter
    , (.~)
    , (.=)
    , (%=)
    , (+=)
    ) where

import Control.Monad.Identity
import Control.Applicative
import Control.Monad.State        as State

infixl 8 .^
infixr 4 .~
infix  4 .=,%=,+=

-- | Does it look familiar? yes it's the official
-- Lens type.
type Lens s t a b =
    forall f. Functor f => (a -> f b) -> s -> f t

-- | Try to match the Lens' type alias.
type Lens' s a = Lens s s a a

-- | Traversal type, matched to the one of the lens
-- package.
type Traversal s t a b =
    forall f. Applicative f => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

-- | Create a full lens out of setter and getter
lens :: (s -> a)
     -> (s -> b -> t)
     -> Lens s t a b
{-# INLINE lens #-}
lens accessor setter = \f src ->
  fmap (setter src) $ f (accessor src)

(.^) :: s -> Lens s t a b -> a
{-# INLINE (.^) #-}
(.^) v l = getConst (l Const v)

(.~) :: s -> Lens' s a -> a -> s
{-# INLINE (.~) #-}
(.~) v l new = runIdentity $ l (\_ -> Identity new) v

(.=) :: MonadState s m => Lens' s a -> a -> m ()
{-# INLINE (.=) #-}
(.=) l v = State.modify $ \s -> (s .~ l) v

(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
{-# INLINE (%=) #-}
(%=) l f = State.modify $ \s -> (s .~ l) $ f (s .^ l)

(+=) :: (Num a, MonadState s m) => Lens' s a -> a -> m ()
{-# INLINE (+=) #-}
(+=) l n = l %= (+ n)

use :: MonadState s m => Lens s t a b -> m a
{-# INLINE use #-}
use l = State.gets (.^ l)

