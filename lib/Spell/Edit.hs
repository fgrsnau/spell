module Spell.Edit where

import           Control.Monad (guard)

import           Data.Maybe (fromJust, isJust, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V


data Penalties a p = Penalties
    { penaltyInsertion    :: Maybe a -> a -> p
    , penaltyDeletion     :: Maybe a -> a -> p
    , penaltySubstitution ::       a -> a -> p
    , penaltyReversal     ::       a -> a -> p
    }


defaultPenalties :: Eq a => Penalties a Int
defaultPenalties = Penalties
    { penaltyInsertion    = \_ _ -> 1
    , penaltyDeletion     = \_ _ -> 1
    , penaltySubstitution = \x y -> if x == y then 0 else 1
    , penaltyReversal     = \_ _ -> 1
    }

calculateEdit :: (Num p, Ord p, Unbox p)
                 => Penalties Char p -> Text -> [Char] -> [Vector p] -> Vector p
calculateEdit p r = f
  where
    f [] [] = V.scanl (+) 0 . V.map (penaltyInsertion p Nothing) . V.fromList $ T.unpack r
    f ks vs = V.constructN (T.length r + 1) (f' ks vs)

    f' (k:ks) (v:vs) v'
      | V.null v' = v V.! i + penaltyDeletion p (listToMaybe ks) k
      | isJust reversal = fromJust reversal
      | otherwise = minimum [ v  V.!   i   + penaltyDeletion p (listToMaybe ks) k
                            , v' V.! (i-1) + penaltyInsertion p (Just k) (r `T.index` (i-1))
                            , v  V.! (i-1) + penaltySubstitution p k (r `T.index` (i-1))
                            ]
      where
        i = V.length v'

        reversal = do
          let currentSuggestion = k
          lastSuggestion <- listToMaybe ks
          let currentChar = r `T.index` (i-1)
          lastChar <- do
            guard $ i - 2 >= 0
            return $ r `T.index` (i - 2)
          correctVector <- listToMaybe vs
          guard (currentSuggestion == lastChar)
          guard (lastSuggestion == currentChar)
          return $ correctVector V.! (i-2) + penaltyReversal p lastSuggestion currentSuggestion

    f' _ _ _ = error "This function should never get called."
