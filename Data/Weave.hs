{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Weave
Copyright   : (c) 2013 Craig Roche
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : stable
Portability : portable

-}

module Data.Weave (
    Weave,
    fromEithers,
    toEithers,
    weave,
    flatten,
    transcribe,
    transcribeA,
    ) where

import Control.Monad
import Control.Monad.Trans.Either

import Data.Bifunctor
import Control.Applicative
import Data.Monoid
import Data.Foldable    ( Foldable, foldMap )
import Data.Traversable ( Traversable, traverse )


-- | A list of interwoven @c@s and @a@s.
-- Intuitively, a @Weave c a@ is a container of @a@s with the values
-- interspersed within a context @c@.
-- Isomorphic to [Either c a].
newtype Weave c a = Weave { unWeave :: EitherT c [] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus,
              Foldable, Traversable)

toEithers :: Weave c a -> [Either c a]
toEithers = runEitherT . unWeave

fromEithers :: [Either c a] -> Weave c a
fromEithers = Weave . EitherT 


instance (Show c, Show a) => Show (Weave c a) where
    showsPrec d w = showParen (d > 10) $
        showString "fromEithers " . shows (toEithers w)

instance Bifunctor Weave where
    bimap l r = Weave . bimapEitherT l r . unWeave

instance Monoid (Weave c a) where
    mempty = fromEithers []
    a `mappend` b = fromEithers $ toEithers a ++ toEithers b


weave :: c -> Weave c a
weave = Weave . EitherT . (:[]) . Left


-- | Perform a monoidal fold over a 'Weave'.
foldWeave :: (Monoid w) => (c -> w) -> (a -> w) -> Weave c a -> w
foldWeave fc fa = foldMap (either fc fa) . toEithers

flatten :: Weave a a -> [a]
flatten = foldWeave (:[]) (:[])

transcribe :: (Monoid c) => (a -> c) -> Weave c a -> c
transcribe f = mconcat . map (either id f) . toEithers

transcribeA :: (Monoid c, Applicative f) => (a -> f c) -> Weave c a -> f c
transcribeA f = fmap mconcat . traverse (either pure f) . toEithers
