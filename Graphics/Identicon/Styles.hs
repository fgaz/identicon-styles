{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE CPP                  #-}

module Graphics.Identicon.Styles where

import Graphics.Identicon
import Graphics.Identicon.Primitive
import Codec.Picture (PixelRGB8 (PixelRGB8))
import Data.Proxy (Proxy)
import Data.Bits (Bits, FiniteBits, testBit, finiteBitSize)
import GHC.TypeLits
import Data.Word (Word8)

import Data.Function.Polyvariadic

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable
import Data.Monoid
#endif


type Squares n = Identicon (3 + NecessaryBytes n) :+ Consumer (NecessaryBytes n) :+ Consumer 3

-- | Bytes necessary to generate the given number of columns on one side
type NecessaryBytes sideColumns = NearestByte (Cells sideColumns)

-- | Calculate the total number of cells in the grid from the number
--   of columns on one side
type Cells sideColumns = (sideColumns+sideColumns+1)*(sideColumns+1)

-- | Raise the number of bits to the nearest byte
type NearestByte n = NearestByte' (CmpNat 8 n) n

type family NearestByte' c n where
  NearestByte' comp 0 = 0
  NearestByte' 'GT  n = 1
  NearestByte' comp n = 1 + NearestByte (n-8)

squares :: (KnownNat n, Polyvariadic [Word8] Layer (ToLayer (NecessaryBytes n)))
        => Proxy n -> Implementation (Squares n)
squares proxy = Identicon :+ polyvariadic mempty maskingSquares :+ solidColorLayer
  where
    maskingSquares :: [Word8] -> Layer
    maskingSquares xs = foldMap makeMaskingSquare
                      $ filterOn positions
                      $ foldMap extractBits xs
    makeMaskingSquare = foldMap (\n -> onGrid columns columns n $ color white)
    positions = foldMap columnCellToRowCells centralColumnCells -- on which cells each bit will act
    columnCellToRowCells n = [n] : fmap (\offset -> [n-offset, n+offset]) [1..sideColumns]
    centralColumnCells = [sideColumns,columns+sideColumns..columns^(2::Int) -sideColumns -1]
    sideColumns = fromIntegral $ natVal proxy
    columns = sideColumns * 2 + 1

solidColorLayer :: Word8 -> Word8 -> Word8 -> Layer
solidColorLayer r g b = color (PixelRGB8 r g b)

white :: PixelRGB8
white = PixelRGB8 255 255 255

filterOn :: [a] -> [Bool] -> [a] --MAYBE swap args
filterOn xs conditions = fmap fst $ filter snd $ zip xs conditions

extractBits :: (Bits bits, FiniteBits bits) => bits -> [Bool]
extractBits bits = fmap (testBit bits) [0..finiteBitSize bits -1]

