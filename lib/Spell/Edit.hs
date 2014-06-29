module Spell.Edit where

import           Control.Monad (guard)

import           Data.Maybe (fromJust, isJust, listToMaybe)
import           Data.Trie (TrieFunction)
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

calculateEdit :: (Eq k, Num p, Ord p, Unbox k, Unbox p) => Penalties k p -> [k] -> TrieFunction k (Vector p)
calculateEdit p reference = f
  where
    r = V.fromList reference

    f [] []  = V.fromList $ scanl1 (+) (0 : map (penaltyInsertion p Nothing) reference)
    f ks vs = V.constructN (V.length r + 1) (f' ks vs)

    f' (k:ks) (v:vs) v'
      | V.null v' = v V.! i + penaltyDeletion p (listToMaybe ks) k
      | isJust reversal = fromJust reversal
      | otherwise = minimum [ v  V.!   i   + penaltyDeletion p (listToMaybe ks) k
                            , v' V.! (i-1) + penaltyInsertion p (Just k) (r V.! (i-1))
                            , v  V.! (i-1) + penaltySubstitution p k (r V.! (i-1))
                            ]
      where
        i = V.length v'

        reversal = do
          let currentSuggestion = k
          lastSuggestion <- listToMaybe ks
          let currentChar = r V.! (i-1)
          lastChar <- r V.!? (i-2)
          correctVector <- listToMaybe vs
          guard (currentSuggestion == lastChar)
          guard (lastSuggestion == currentChar)
          return $ correctVector V.! (i-2) + penaltyReversal p lastSuggestion currentSuggestion

    f' _ _ _ = error "This function should never get called."
