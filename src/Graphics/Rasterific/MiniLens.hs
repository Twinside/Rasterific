{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
module Graphics.Rasterific.MiniLens
    ( -- * Types
      Lens
    , Lens'
    , Traversal
    , Traversal'
    , lens

      -- * Getter
    , (.^)
    , view
    , use

      -- * Setter
    , (.~)
    , (.=)
    , (%=)
    , (+=)
    , set

      -- * Helper
    , (&)
    ) where

import Control.Monad.Identity
import Control.Applicative
import Control.Monad.State        as State

#if MIN_VERSION_base(4,8,0)
import Data.Function( (&) )
#endif

infixl 8 .^
infixr 4 .~
infix  4 .=,%=,+=

#if !MIN_VERSION_base(4,8,0)
infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x
#endif

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

view :: s -> Lens s t a b -> a
{-# INLINE view #-}
view v l = getConst (l Const v)

(.^) :: s -> Lens s t a b -> a
{-# INLINE (.^) #-}
(.^) = view

set :: Lens' s a -> a -> s -> s
{-# INLINE set #-}
set l new v = runIdentity $ l (\_ -> Identity new) v

(.~) :: Lens' s a -> a -> s -> s
{-# INLINE (.~) #-}
(.~) = set

(.=) :: MonadState s m => Lens' s a -> a -> m ()
{-# INLINE (.=) #-}
(.=) l v = State.modify (l .~ v)

(%=) :: MonadState s m => Lens' s a -> (a -> a) -> m ()
{-# INLINE (%=) #-}
(%=) l f = State.modify $ \s -> s & l .~ f (s .^ l)

(+=) :: (Num a, MonadState s m) => Lens' s a -> a -> m ()
{-# INLINE (+=) #-}
(+=) l n = l %= (+ n)

use :: MonadState s m => Lens s t a b -> m a
{-# INLINE use #-}
use l = State.gets (.^ l)

