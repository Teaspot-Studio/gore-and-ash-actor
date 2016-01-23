{-|
Module      : Game.GoreAndAsh.Actor.TypeRep
Description : Hashable type representation
Copyright   : (c) Anton Gushcha, 2015-2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : POSIX

-}
module Game.GoreAndAsh.Actor.TypeRep(
    HashableTypeRep
  , toHashableTypeRep
  , fromHashableTypeRep
  , hashableTypeRep
  ) where

import Control.DeepSeq
import Data.Hashable 
import Data.Typeable
import GHC.Generics 

-- | Wrapper around TypeRep that supports Hashable and Eq, that are performed over type name
-- Note: the implentation is choosen to support equality of actors between several applications
newtype HashableTypeRep = HashableTypeRep { unHashableTypeRep :: TypeRep }
  deriving (Generic, Typeable)

instance Eq HashableTypeRep where 
  (HashableTypeRep a) == (HashableTypeRep b) = show a == show b

instance Show HashableTypeRep where 
  showsPrec i = showsPrec i . unHashableTypeRep

instance Hashable HashableTypeRep where
  hashWithSalt salt (HashableTypeRep tr) = salt `hashWithSalt` show tr

-- | Helper to transform usual 'TypeRep' to hashable one.
toHashableTypeRep :: TypeRep -> HashableTypeRep
toHashableTypeRep = HashableTypeRep

-- | Helper to transform hashable type rep to usual 'TypeRep'.
fromHashableTypeRep :: HashableTypeRep -> TypeRep 
fromHashableTypeRep = unHashableTypeRep

-- | Helper to get hashable type rep from type
hashableTypeRep :: forall proxy a . Typeable a => proxy a -> HashableTypeRep
hashableTypeRep = HashableTypeRep . typeRep

instance NFData HashableTypeRep where
  rnf (HashableTypeRep tr) = rnfTypeRep tr