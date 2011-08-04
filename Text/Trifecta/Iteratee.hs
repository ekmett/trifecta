module Text.Trifecta.Iteratee where

import 

newtype Mark = Mark
  { markByte        :: {-# UNPACK #-} !Int 
  , markFileName    :: !FileName
  , markIncludePath :: !IncludePath
  , markLine        :: {-# UNPACK #-} !Int
  , markColumn      :: {-# UNPACK #-} !Int
  , markCursor      :: {-# UNPACK #-} !Cursor
  , mark
  }

newtype Cursor = Cursor 
  { cursorByteCount         :: {-# UNPACK #-} !Int
  , cursorLastNewline       :: {-# UNPACK #-} !Int
  , cursorLine              :: {-# UNPACK #-} !Int
  , cursorColumn            :: {-# UNPACK #-} !Int
  , cursorDescriptionBuffer :: !(Description Buffer)
  }

newtype Chunk = Chunk = Chunk { getChunk :: InternedByteString }
  deriving (Eq,Ord,Show,Read)

instance Measured Cursor Chunk where
  measure (Chunk xs) = Cursor (S.length xs) 

newtype Buffer = Buffer 
  { _bufferId  :: {-# UNPACK #-} Id
  , bufferTree :: FingerTree Cursor Chunk 
  }

instance Interned Buffer where
  type Uninterned Buffer = FingerTree Cursor Chunk
  newtype Description Buffer = DBuffer (Seq Id)
  describe = DBuffer . foldMap (Seq.singleton . identity)
  identify = Buffer
  identity (Buffer i _) = i
  cache = bufferCache

bufferCache :: Cache Buffer
bufferCache = mkCache
{-# NOINLINE bufferCache #-}
  

  

instance Monoid Cursor where
  {

  }
  
newtype Iteratee a = k
