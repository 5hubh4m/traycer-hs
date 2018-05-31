module Traycer.Util
  ( maybeMin
  ) where

maybeMin :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maybeMin Nothing Nothing = Nothing
maybeMin x@Just{} Nothing = x
maybeMin Nothing x@Just{} = x
maybeMin x1@(Just a1) x2@(Just a2) = if a2 < a1 then x2 else x1
{-# INLINE maybeMin #-}
