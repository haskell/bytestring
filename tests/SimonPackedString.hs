{-# OPTIONS -cpp -fglasgow-exts -funbox-strict-fields #-}

-- define MODULENAME to the module name
-- define CHARBITS to 8, 16 or 32

#define CHARTYPE Word8
#define CHARBYTES (CHARBITS `quot` 8)
#define CHARBITS 8

#define STRICT1(f) f a b c | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined
#define STRICT6(f) f a b c d e f | a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` False = undefined

module SimonPackedString (
	-- * The @PackedString@ type
        PackedString,      -- abstract, instances: Eq, Ord, Show, Typeable

         -- * Converting between @String@ and @PackedString@
	pack,
	unpack,

	-- * I\/O with @PackedString@
	hPut, hGet, 
#if CHARBITS == 8
	hGetLine,
#endif

	-- * List-like manipulation functions
	nil,
	cons,
	head,
	tail,
	null,
	append,
	length,
	index,
	unsafeIndex,
	map,
	filter,
	reverse,
	concat,
	elem,
	substr,
	take,
	drop,
	splitAt,
	foldl,
	foldr,
	takeWhile,
	dropWhile,
	span,
	break,
	lines,
	unlines,
	words,
	unwords,
	split,
	splitWith,
	join,

	-- * Conversion between @PackedString@ and @ForeignPtr@
	fromForeignPtr,
	toForeignPtr,

	-- * Misc
	unpackList, -- eek, otherwise it gets thrown away by the simplifier

    ) where

import qualified Prelude
import Prelude hiding (
	head,
	tail,
	null,
	length,
	(!!),
	map,
	filter,
	reverse,
	concat,
	elem,
	take,
	drop,
	foldl,
	foldr,
	splitAt,
	takeWhile,
	dropWhile,
	span,
	break,
	lines,
	unlines,
	words,
	unwords
 )

import Text.Read (Read(..))
import GHC.Exts hiding (split)
import GHC.IOBase (IO(..))
import Foreign
import Data.Typeable
import Data.Char
import qualified Data.List
import System.IO hiding (hGetLine)
import GHC.Base (unsafeChr)

import GHC.Handle
import GHC.IOBase
import Foreign.C
import System.IO.Error

-- -----------------------------------------------------------------------------
-- PackedString type declaration

-- | A space-efficient representation of a 'String', which supports
-- various efficient operations.  A 'PackedString' contains Latin1
-- (8-bit) characters only.
data PackedString = PS {-#UNPACK#-}!Int {-#UNPACK#-}!Int 
		       {-#UNPACK#-}!(ForeignPtr CHARTYPE)
	-- this is a pretty efficient representation, and can be
	-- converted to/from a StorableArray.
	-- When the ForeignPtr is unpacked, we get the Addr# stored
	-- directly in the PS constructor.
  deriving Typeable

-- Perhaps making a slice should be conditional on the ratio of the
-- slice/string size to limit memory leaks.

instance Eq PackedString where
   a == b =  comparePS a b == EQ

instance Ord PackedString where
   compare = comparePS

offPS :: Ptr CHARTYPE -> Int -> Ptr CHARTYPE
offPS p i = p `plusPtr` (i * CHARBYTES)

comparePS (PS off1 len1 fp1) (PS off2 len2 fp2)
  = inlinePerformIO $
	withForeignPtr fp1 $ \p1 -> 
	withForeignPtr fp2 $ \p2 ->
	cmp (p1 `offPS` off1) 
	    (p2 `offPS` off2) 0 len1 len2

cmp :: Ptr CHARTYPE -> Ptr CHARTYPE -> Int -> Int -> Int-> IO Ordering
STRICT5(cmp)
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekElemOff p1 n
          b <- peekElemOff p2 n
          case a `compare` b of
		EQ -> cmp p1 p2 (n+1) len1 len2
		LT -> return LT
		GT -> return GT

instance Read PackedString where
    readPrec = do; s <- readPrec; return (pack s)

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpack ps) r

-- -----------------------------------------------------------------------------
-- Constructor functions

-- | The 'nil' value is the empty string.
nil :: PackedString
nil = inlinePerformIO $ do
		fp <- newForeignPtr_ nullPtr
		return (PS 0 0 fp)

-- | /O(n)/ The 'cons' function prepends the given character to the
-- given string.
cons :: Char -> PackedString -> PackedString
cons c cs = pack (c : (unpack cs)) -- ToDo:better

-- | Convert a 'String' into a 'PackedString'
packLen :: Int -> String -> PackedString
packLen len str = inlinePerformIO $ do 
  fp <- mallocForeignPtrBytes (len * CHARBYTES)
  withForeignPtr fp $ \p -> do 
	fill_it_in p 0 str
	return (PS 0 len fp)

fill_it_in p i [] = return ()
fill_it_in p i (c:cs) = do pokeElemOff p i (c2w c); fill_it_in p (i+1) cs

-- | /O(n)/ Converts a 'String' to a 'PackedString'
pack :: String -> PackedString
pack str = packLen (Prelude.length str) str

{-# INLINE w2c #-}
w2c :: CHARTYPE -> Char
w2c = unsafeChr . fromIntegral -- assume Chars are in range
{-# INLINE c2w #-}
c2w :: Char -> CHARTYPE
c2w = fromIntegral . ord

-- -----------------------------------------------------------------------------
-- List-mimicking functions for PackedStrings

-- | /O(1)/ The 'length' function returns the length of the input list.
-- Analogous to 'length'.
length :: PackedString -> Int
length (PS _ len _) = len

-- | /O(1)/ The 'index' function returns the character in the string at the
-- given position.
index :: PackedString -> Int -> Char
index ps i 
  | i >= 0 && i < len = unsafeIndex ps i
  | otherwise = error "Data.PackedString.Latin1.index: index out of range"
  where len = length ps

-- | /O(1)/ Like 'index', but without any bounds checking.
unsafeIndex :: PackedString -> Int -> Char
unsafeIndex (PS off len fp) i =
  withPackedString fp $ \p -> do
    w <- peekElemOff (p `offPS` off) i
    return $! unsafeChr (fromIntegral w)

-- | /O(1)/ The 'head' function returns the first element of a
-- 'PackedString' or throws an error if the string is empty.
head :: PackedString -> Char
head ps
  | len <= 0 = error "Data.PackedString.Latin1.head: head []"
  | otherwise = index ps 0
  where len = length ps

-- | /O(1)/ The 'tail' function returns the tail of a 'PackedString'
-- or throws an error if the string is empty.
tail :: PackedString -> PackedString
tail ps
  | len <= 0 = error "Data.PackedString.Latin1.tail: tail []"
  | len == 1 = nil
  | otherwise  = substr ps 1 (len - 1)
  where len = length ps

-- | /O(1)/ The 'null' function returns True iff the argument is null.
null :: PackedString -> Bool
null (PS _ l _) = l == 0

-- | /O(n)/ The 'append' function appends the second string onto the first.
append :: PackedString -> PackedString -> PackedString
append xs ys
  | null xs = ys
  | null ys = xs
  | otherwise  = concat [xs,ys]

-- | /O(n)/ The 'map' function applies a function to each character in
-- | the string.
map :: (Char -> Char) -> PackedString -> PackedString
-- Simple version: 
-- map f ps = pack (Prelude.map f (unpack ps))
-- Prelude.map fuses with unpack, but we can't fuse pack.map.
-- Using packLen directly helps a bit, but it's still hard to get pack
-- to fuse with anything.  Hence, hand-coded version:
map f (PS start len fp) =
  inlinePerformIO $ do 
     withForeignPtr fp $ \p -> do
       new_fp <- mallocForeignPtrBytes (len * CHARBYTES)
       withForeignPtr new_fp $ \new_p -> do 
	  map_ f (len-1) (p `offPS` start) new_p
  	  return (PS 0 len new_fp)

STRICT4(map_)
map_ f n p1 p2
   | n < 0 = return ()
   | otherwise = do
		x <- peekElemOff p1 n
		pokeElemOff p2 n (c2w (f (w2c x)))
		map_ f (n-1) p1 p2

-- | /O(n)/ The 'filter' function filters out the appropriate substring.
filter :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filter pred ps = pack $ Prelude.filter pred $ unpack ps

-- | /O(n)/ The 'foldl' function behaves like 'foldl' on 'PackedString's.
foldl :: (a -> Char -> a) -> a -> PackedString -> a
foldl f b ps = Prelude.foldl f b $ unpack ps

-- | /O(n)/ The 'foldr' function behaves like 'foldr' on 'PackedString's.
foldr :: (Char -> a -> a) -> a -> PackedString -> a
foldr f v ps = Prelude.foldr f v $ unpack ps -- no intermediate list, we hope

-- | /O(1)/ The 'take' function takes the first @n@ characters of a
-- 'PackedString'.
take :: Int -> PackedString -> PackedString
take n ps = substr ps 0 (n-1)

-- | /O(1)/ The 'drop' function drops the first @n@ characters of a
-- 'PackedString'.
drop	:: Int -> PackedString -> PackedString
drop n ps = substr ps n (length ps - 1)

-- | /O(1)/ The 'splitWith' function splits a 'PackedString' at a given index.
splitAt :: Int -> PackedString -> (PackedString, PackedString)
splitAt  n ps  = (take n ps, drop n ps)

-- | /O(n)/ The 'takeWhile' function is analogous to the 'takeWhile' function.
takeWhile :: (Char -> Bool) -> PackedString -> PackedString
takeWhile pred ps = pack $ Prelude.takeWhile pred $ unpack ps

-- | /O(n)/ The 'dropWhile' function is analogous to the 'dropWhile' function.
dropWhile :: (Char -> Bool) -> PackedString -> PackedString
dropWhile pred ps = pack $ Prelude.dropWhile pred $ unpack ps

-- | /O(n)/ The 'elem' function returns True iff the given element is
-- in the string.
elem :: Char -> PackedString -> Bool
elem c ps = c `Prelude.elem` unpack ps

-- | /O(n)/ The 'span' function returns a pair containing the result of
-- running both 'takeWhile' and 'dropWhile'.
span :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
span  p ps = (takeWhile p ps, dropWhile p ps)

-- | /O(n)/ The 'break' function breaks a string at the first position which
-- satisfies the predicate.
break :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
break p ps = span (not . p) ps

-- | /O(n)/ The 'lines' function splits the input on line-breaks.  It behaves
-- in exactly the same way as 'Data.List.lines'.
lines :: PackedString -> [PackedString]
lines ps = dropFinalEmpty (split '\n' ps)
  where dropFinalEmpty [] = []
        dropFinalEmpty [x] | null x = []
	dropFinalEmpty (x:xs) = x : dropFinalEmpty xs

-- | /O(n)/ The 'unlines' function concatenates the input list after
-- interspersing newlines.  It behaves in exactly the same way as
-- 'Data.List.unlines'.
unlines :: [PackedString] -> PackedString
unlines pss = concat (Prelude.map (`append` pack "\n") pss)

-- | /O(n)/ The 'words' function is analogous to the 'words' function.
words :: PackedString -> [PackedString]
words ps = Prelude.filter (not.null) (splitWith isSpace ps)

-- | /O(n)/ The 'unwords' function is analogous to the 'unwords' function.
unwords :: [PackedString] -> PackedString
unwords pss = join (pack " ") pss

-- | /O(n)/ The 'reverse' function reverses the string.
reverse :: PackedString -> PackedString
reverse ps = pack $ Prelude.reverse $ unpack ps
	-- ToDo: could use the same trick as map above

-- | /O(n)/ The 'concat' function concatenates a list of 'PackedString's.
concat :: [PackedString] -> PackedString
concat pss = pack $ Prelude.concat $ Prelude.map unpack pss
	-- ToDo: could improve the performance here

------------------------------------------------------------

-- | /O(n)/ The 'join' function takes a 'PackedString' and a list of
-- 'PackedString's and concatenates the list after interspersing the
-- first argument between each element of the list.
join :: PackedString -> [PackedString] -> PackedString
join filler pss = concat (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * split x ls = ls'   
      where False = any (map (x `elem`) ls')

  * join (pack [x]) (split x ls) = ls
-}

-- | /O(n)/ The 'split' function splits the input string on each
-- occurrence of the given 'Char'.  This function does not copy the
-- substrings, it just constructs new 'PackedStrings' that are slices
-- of the original, so it is quite fast.
split :: Char -> PackedString -> [PackedString]
split c = splitWith (== c)

{-# INLINE splitWith #-}
splitWith :: (Char -> Bool) -> PackedString -> [PackedString]
splitWith pred (PS off 0 fp) = []
splitWith pred (PS off len fp) = splitWith' pred# off len fp
  where pred# c# = pred (C# c#)
	-- wrapper-ify

splitWith' pred off len fp =
  withPackedString fp $ \p -> splitLoop pred p 0 off len fp

STRICT6(splitLoop)
splitLoop pred p idx off len fp
	| idx >= len  = return [PS off idx fp]
	| otherwise = do
		w <- peekElemOff p (off+idx)
		if pred (case (w2c w) of C# c# -> c#)
		   then return (PS off idx fp : 
			          splitWith' pred (off+idx+1) (len-idx-1) fp)
		   else splitLoop pred p (idx+1) off len fp

-- | /O(1)/ The 'substr' function takes a 'PackedString' and two indices
-- and returns the substring of the input string between (and including)
-- these indices.
--
-- The definition of @_substr@ is essentially:
-- @take (end - begin + 1) (drop begin str)@ (provided @begin@ is >= 0).
-- If @begin@ and/or @end@ are out of range, they are truncated.
--
substr :: PackedString -> Int -> Int -> PackedString
STRICT3(substr)
substr (PS off len fp) begin end 
  | begin >= len || end < begin = nil
  | otherwise = PS (off+begin') len' fp
  where len'   = max 0 $ min (len-begin') (end-begin'+1)
	begin' = max 0 begin

-- -----------------------------------------------------------------------------
-- hPut

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: no encoding or decoding is done.  The bytes written to the file are
-- exactly as stored in the PackedString.
--
hPut :: Handle -> PackedString -> IO ()
hPut h (PS off l fp) =
  withForeignPtr fp $ \p ->
    hPutBuf h (p `offPS` off) (l * CHARBYTES)

-- -----------------------------------------------------------------------------
-- hGet

-- | Read a 'PackedString' directly from the specified 'Handle'.
-- This is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.  
--
-- NOTE: no encoding or decoding is done.  The bytes read from the file are
-- expected to be in exactly the same encoding as the PackedString.  The
-- 'Int' argument is the number of /bytes/ to read.
--
hGet :: Handle -> Int -> IO PackedString
hGet h i = do
  fp <- mallocForeignPtrBytes i
  withForeignPtr fp $ \p -> do
    l <- hGetBuf h p i
    return (PS 0 (l `quot` CHARBYTES) fp)

-- -----------------------------------------------------------------------------
-- hGetLine

#if CHARBITS == 8
hGetLine :: Handle -> IO PackedString
hGetLine h = do
  wantReadableHandle "hGetLine" h $ \ handle_ -> do
    	case haBufferMode handle_ of
    	   NoBuffering -> error "no buffering"
           _other -> hGetLineBuffered handle_

hGetLineBuffered handle_ = do
  let ref = haBuffer handle_
  buf <- readIORef ref
  hGetLineBufferedLoop handle_ ref buf 0 []

hGetLineBufferedLoop handle_ ref 
	buf@Buffer{ bufRPtr=r, bufWPtr=w, bufBuf=raw } len xss =
  len `seq` do
  off <- findEOL r w raw

  --putStr ("hGetLineBufferedLoop: r=" ++ show r ++ ", w=" ++ show w ++ ", off=" ++ show off ++ "\n")

  let new_len = len + off - r
  xs <- mkPS raw r off

  -- if eol == True, then off is the offset of the '\n'
  -- otherwise off == w and the buffer is now empty.
  if off /= w
	then do if (w == off + 1)
	    		then writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
		   	else writeIORef ref buf{ bufRPtr = off + 1 }
	        mkBigPS new_len (xs:xss)
	else do
	     maybe_buf <- maybeFillReadBuffer (haFD handle_) True (haIsStream handle_)
				buf{ bufWPtr=0, bufRPtr=0 }
	     case maybe_buf of
		-- Nothing indicates we caught an EOF, and we may have a
		-- partial line to return.
		Nothing -> do
		     writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
		     if new_len > 0
		        then mkBigPS new_len (xs:xss)
		        else ioe_EOF
		Just new_buf -> 
		     hGetLineBufferedLoop handle_ ref new_buf new_len (xs:xss)

-- find the end-of-line character, if there is one
findEOL r w raw 
   | r == w = return w
   | otherwise =  do
	(c,r') <- readCharFromBuffer raw r
	if c == '\n' 
	   then return r -- NB. not r': don't include the '\n'
	   else findEOL r' w raw

maybeFillReadBuffer fd is_line is_stream buf
  = catch 
     (do buf <- fillReadBuffer fd is_line is_stream buf
	 return (Just buf)
     )
     (\e -> do if isEOFError e 
		  then return Nothing 
		  else ioError e)

mkPS :: RawBuffer -> Int -> Int -> IO PackedString
mkPS buf start end = do
  let len = end - start
  fp <- mallocForeignPtrBytes (len * CHARBITS `quot` 8)
  withForeignPtr fp $ \p -> do 
     memcpy_ptr_baoff p buf start (fromIntegral len)
     return (PS 0 len fp)
    
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> Int -> CSize -> IO (Ptr ())

mkBigPS :: Int -> [PackedString] -> IO PackedString
mkBigPS _ [ps] = return ps
mkBigPS len pss = return $! concat (Prelude.reverse pss)
#endif

-- -----------------------------------------------------------------------------
-- unpacking

{-# INLINE unpack #-}
-- | /O(n)/ Converts a 'PackedString' to a 'String'.
unpack :: PackedString -> String
unpack ps = build (unpackFoldr ps)

{-# RULES
"unpack-list"  [1]  forall p  . unpackFoldr p (:) [] = unpackList p
 #-}

unpackList :: PackedString -> [Char]
unpackList (PS off len fp) =
   withPackedString fp $ \p -> do
      let loop p (-1) acc = return acc
	  loop p n acc = do
             a <- peekElemOff p n
	     loop p (n-1) (w2c a : acc)
      loop (p `offPS` off) (len-1) []

{-# INLINE [0] unpackFoldr #-}
unpackFoldr :: PackedString -> (Char -> a -> a) -> a -> a
unpackFoldr (PS off len fp) f c = 
   withPackedString fp $ \p -> do
      let loop p (-1) acc = return acc
	  loop p n acc = do
             a <- peekElemOff p n
	     loop p (n-1) (w2c a `f` acc)
      loop (p `offPS` off) (len-1) c

-- -----------------------------------------------------------------------------
-- Conversion to/from ForeignPtrs

-- | Converts a 'ForeignPtr' into a 'PackedString'.  This is useful
-- for marshalling strings obtained from foreign functions into
-- 'PackedStrings' that may be manipulated using the functions in
-- this module.
fromForeignPtr
   :: ForeignPtr CHARTYPE	-- ^ The packed string
   -> Int			-- ^ number of /elements/ (not bytes)
   -> PackedString
fromForeignPtr fp len = PS 0 len fp

-- | Returns the 'ForeignPtr' underlying the given 'PackedString'.
-- The return value is @(f, start, len)@, where @start@ is the offset
-- (in elements) of the start of the string from the beginning of the
-- 'ForeignPtr' f, and @len@ is the number of characters in the string.
toForeignPtr
   :: PackedString
   -> (ForeignPtr CHARTYPE, Int, Int)
toForeignPtr (PS start len fp) = (fp, start, len)

-- -----------------------------------------------------------------------------
-- Utils

-- Just like unsafePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

withPackedString :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPackedString fp io = inlinePerformIO (withForeignPtr fp io)
