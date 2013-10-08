{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
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
    WeaveT,
    Weave,
    fromEithers,
    toEithers,

    bimapWeave,
    weave,
    flatten,
    transcribe,
    transcribeA,
)
where

import Pipes
import qualified Pipes.Prelude as P

import Data.Functor.Identity
import Control.Monad.Trans.Either as E

import Control.Applicative
import Data.Monoid
import Data.Traversable ( Traversable, traverse )


-- TODO define more instances, including `pipes` instances


newtype WeaveT c m a = WeaveT { unWeaveT :: EitherT c (ListT m) a }
    deriving (Functor, Applicative, Monad)

instance MonadTrans (WeaveT c) where
    lift = WeaveT . lift . lift

instance Monoid (WeaveT c Identity a) where
    mempty = fromEithers []
    a `mappend` b = fromEithers $ toEithers a ++ toEithers b


type Weave c = WeaveT c Identity

fromEithers :: [Either c a] -> Weave c a
fromEithers = WeaveT . EitherT . Select . each

toEithers :: Weave c a -> [Either c a]
toEithers = P.toList . enumerate . runEitherT . unWeaveT


weave :: (Monad m) => c -> WeaveT c m a
weave = WeaveT . E.left


bimapWeave :: (Monad m) => (c -> d) -> (a -> b) -> WeaveT c m a -> WeaveT d m b
bimapWeave f g = WeaveT . bimapEitherT f g . unWeaveT


-- | Perform a right fold over a 'Weave'.
foldWeave :: (c -> b -> b) -> (a -> b -> b) -> b -> Weave c a -> b
foldWeave fc fa x = foldr (either fc fa) x . toEithers

flatten :: Weave a a -> [a]
flatten = foldWeave (:) (:) []

transcribe :: (Monoid c) => (a -> c) -> Weave c a -> c
transcribe f = mconcat . map (either id f) . toEithers

transcribeA :: (Monoid c, Applicative f) => (a -> f c) -> Weave c a -> f c
transcribeA f = fmap mconcat . traverse (either pure f) . toEithers
