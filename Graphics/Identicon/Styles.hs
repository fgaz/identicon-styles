{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module Graphics.Identicon.Styles where

import Graphics.Identicon
import Graphics.Identicon.Primitive
import Codec.Picture (PixelRGB8 (PixelRGB8))
import Data.Proxy (Proxy)
import Data.Bits (Bits, FiniteBits, testBit, finiteBitSize)
import GHC.TypeLits
import Data.Word (Word8)

import Data.Function.Polyvariadic


type GeneralizedGithub n = Identicon (3 + NecessaryBytes n) :+ Consumer (NecessaryBytes n) :+ Consumer 3

-- | Bytes necessary to generate the given number of columns on one side
type NecessaryBytes sideColumns = NearestByte (Cells sideColumns)

-- | Calculate the total number of cells in the grid from the number
--   of columns on one side
type Cells sideColumns = (sideColumns+sideColumns+1)*(sideColumns+1)

-- | Raise the number of bits to the nearest byte
type family NearestByte n where
  --NearestByte bits = if bits `mod` 8 == 0 then bits `div` 8 --MAYBE calculate the number of bits somehow?
  --                   else nearestByte (bits+1)
  NearestByte 0 = 0
  NearestByte 1 = 1
  NearestByte 2 = 1
  NearestByte 3 = 1
  NearestByte 4 = 1
  NearestByte 5 = 1
  NearestByte 6 = 1
  NearestByte 7 = 1
  NearestByte n = 1 + NearestByte (n-8)

generalizedGithubStyle :: (KnownNat n, Varargs [Word8] Layer (ToLayer (NecessaryBytes n)))
                       => Proxy n -> Implementation (GeneralizedGithub n)
generalizedGithubStyle proxy = Identicon :+ applyToAccumulatedArgs maskingSquares :+ solidColorLayer
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


-- Specific version here
{-
type Github = Identicon 5 :+ Consumer 2 :+ Consumer 3

githubStyle :: Implementation Github
githubStyle = Identicon :+ maskingSquares :+ colorBackground
  where
    maskingSquares x1 x2 = foldMap makeMaskingSquare
                         $ filterOn positions
                         $ foldMap extractBits [x1,x2]
    makeMaskingSquare = foldMap (\n -> onGrid gridSide gridSide n $ color white)
    positions = foldMap columnCellToRowCells centralColumnCells -- on which cells each bit will act
    columnCellToRowCells n = [n] : fmap (\offset -> [n-offset, n+offset]) [1..sqn]
    centralColumnCells = [sqn,gridSide+sqn..gridSide^2 -sqn -1]
    colorBackground r g b = color (PixelRGB8 r g b)
    sqn=2
    gridSide = sqn * 2 + 1
-}

