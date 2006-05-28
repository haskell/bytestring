import Data.Word
import Data.ByteString
import Data.ByteString.Fusion

-- Bug in the down/down (and all down) fusion rules.

k :: Int -> Word8 -> (PairS Int (MaybeS Word8))
k _ w = (0 :*: if w < 50 then NothingS else JustS 50)

-- Unfused
f k = loopWrapper (sequenceLoops (doDownLoop kk (0::Int)) (doDownLoop k (0::Int)))

-- Fused
g k = loopWrapper (doDownLoop (kk `fuseAccAccEFL` k) (0 :*: 0))

kk = \_ w -> (0 :*: JustS (w :: Data.Word.Word8))

s = Data.ByteString.pack [49,50,51]

main = do
    print "Should be the same:"
    print $ f k s
    print $ g k s
