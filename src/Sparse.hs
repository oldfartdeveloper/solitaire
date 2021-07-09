module Sparse where

import CardTypes
import Utils

sparseMatrix :: [Card] -> Matrix
sparseMatrix cards = do-- Matrix { _positions = [[]] }
  let positions = replicate suitsCount $ replicate ranksCount NoCard

      suitOffsets = [0 .. fromEnum (maxBound :: Suit)]
      rankOffsets = [0 .. fromEnum (maxBound :: Rank)]
  Matrix {_positions = positions }
