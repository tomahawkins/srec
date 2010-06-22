-- | Reading S-Record files.
module Data.SRec
  ( SRec (..)
  , readSRec
  ) where

import Data.Bits
import Data.Word

data SRec = SRec
  { blocks :: [(Int, [Word8])]  -- ^ Starting address of block and block data.
  , start  :: Int               -- ^ Starting address of program for S7 record.
  }

-- | Read an S-Record file.
readSRec :: FilePath -> IO SRec
readSRec f = readFile f >>= return . parseSRec

parseSRec :: String -> SRec
parseSRec a = SRec { blocks = blocks, start = start }
  where
  records = [ record l | l@('S':_) <- lines a ]
  blocks = mergeBlocks [ addr (a, b) | (a, b) <- records, elem a [1, 2, 3] ]
  start = fst $ head [ addr (a, b) | (a, b) <- records, elem a [7, 8, 9] ]

record :: String -> (Int, [Word8])
record a = (if validType then rType else error $ "only S1, S2, S3, S5, and S7 supported: " ++ a, if checksum /= checksum' then error $ "failed checksum: " ++ a else field)
  where
  rType = read $ take 1 $ tail a
  validType = elem rType [1, 2, 3, 5, 7, 8, 9]
  byteCount = read ("0x" ++ take 2 (drop 2 a))
  bytes = f $ take (2 * byteCount) $ drop 4 a
  f :: String -> [Word8]
  f [] = []
  f [_] = undefined
  f (a : b : c) = read ("0x" ++ [a, b]) : f c
  checksum = last bytes
  field = init bytes
  checksum' = 0xFF .&. complement (sum (fromIntegral byteCount : field))

addr :: (Int, [Word8]) -> (Int, [Word8])
addr (t, a) = (addr, drop n a)
  where
  addr = foldl (.|.) 0 [ shiftL (fromIntegral a) s | (a, s) <- zip (reverse (take n a)) [0, 8 ..] ]
  n = case t of
    1 -> 2
    2 -> 3
    3 -> 4
    7 -> 4
    8 -> 3
    9 -> 2
    _ -> undefined

mergeBlocks :: [(Int, [Word8])] -> [(Int, [Word8])]
mergeBlocks a = [ (a, b) | (a, _, b) <- f [ (a, length b, b) | (a, b) <- a ] ]
  where
  f ((a0, l0, d0) : (a1, l1, d1) : rest) | a0 + l0 == a1 = f $ (a0, l0 + l1, d0 ++ d1) : rest
                                         | otherwise     = (a0, l0, d0) : f ((a1, l1, d1) : rest)
  f a = a

