-- | Parsing and processing s-records.
module Data.SRec
  ( SRec (..)
  , parseSRec
  , mergeBlocks
  , mergeAllBlocks
  , mergeContiguousBlocks
  , splitBlock
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Bits
import Data.Word
import Text.Printf

data SRec = SRec
  { blocks :: [(Int, ByteString)]  -- ^ Starting address of block and block data.
  , start  :: Int                  -- ^ Starting address of program for S7 record.
  }

-- | Parse an s-record.
parseSRec :: String -> SRec
parseSRec a = SRec { blocks = blocks, start = start }
  where
  records = [ record l | l@('S':_) <- lines a ]
  blocks = mergeContiguousBlocks [ addr (a, b) | (a, b) <- records, elem a [1, 2, 3] ]
  start = fst $ head [ addr (a, b) | (a, b) <- records, elem a [7, 8, 9] ]

  record :: String -> (Int, ByteString)
  record a = (if validType then rType else error $ "only S1, S2, S3, S5, and S7 supported: " ++ a, if checksum /= checksum' then error $ "failed checksum: " ++ a else B.pack field)
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
  
  addr :: (Int, ByteString) -> (Int, ByteString)
  addr (t, a) = (addr, B.drop n a)
    where
    addr = foldl (.|.) 0 [ shiftL (fromIntegral a) s | (a, s) <- zip (reverse (take n $ B.unpack a)) [0, 8 ..] ]
    n = case t of
      1 -> 2
      2 -> 3
      3 -> 4
      7 -> 4
      8 -> 3
      9 -> 2
      _ -> undefined

-- | Formates an s-record file.
printSRec :: SRec -> String
printSRec a = undefined
  where
  [ printf "S3%02X%08X%s%02X\n" (B.length dat + 5) addr (concat [ printf "%02X" byte | byte <- dat ])  splitBlock 64 | block <- blocks a, (addr, dat) <- splitBlock 64 block ]

-- | Merge consecutive blocks into one.  If blocks are not contiguous, the padding byte is used to fill in the extra space.  If the blocks overlap, an error is thrown.
mergeBlocks :: Word8 -> [(Int, ByteString)] -> (Int, ByteString)
mergeBlocks padding blocks = foldl1 (mergeTwoBlocks padding) blocks
  where
  mergeTwoBlocks :: Word8 -> (Int, ByteString) -> (Int, ByteString) -> (Int, ByteString)
  mergeTwoBlocks padding (addrA, dataA) (addrB, dataB)
    | addrA > addrB                  = error $ printf "blocks are out of order: 0x%08x  0x%08x" addrA addrB
    | addrA + B.length dataA > addrB = error $ printf "blocks are overlapping: address 1 = 0x%08x,  length 1 = 0x%08x,  address 2 = 0x%08x" addrA (B.length dataA) addrB
    | otherwise = (addrA, B.concat [dataA, B.replicate (addrB - (addrA + B.length dataA)) padding, dataB])

-- | Merge all blocks in s-record into one.
mergeAllBlocks :: Word8 -> SRec -> SRec
mergeAllBlocks padding a = a { blocks = [mergeBlocks padding $ blocks a] }

-- | Merge contiguous consecutive blocks.
mergeContiguousBlocks :: [(Int, ByteString)] -> [(Int, ByteString)]
mergeContiguousBlocks [] = []
mergeContiguousBlocks [a] = [a]
mergeContiguousBlocks ((addrA, dataA) : (addrB, dataB) : rest)
    | addrA + B.length dataA == addrB = mergeContiguousBlocks $ (addrA, B.append dataA dataB) : rest
    | otherwise = (addrA, dataA) : mergeContiguousBlocks ((addrB, dataB) : rest)

-- | Split a block into sub-blocks given a block length.
splitBlock :: Int -> (Int, ByteString) -> [(Int, ByteString)]
splitBlock n (addr, dat)
  | B.null dat = []
  | otherwise = (addr, B.take n dat) : splitBlock (addr + n, B.drop n dat)

