module Sparse where

import CardTypes
import Utils

sparseMatrix :: [Card] -> Matrix
sparseMatrix cards = do 
  Matrix { _positions = replicate suitsCount $ replicate ranksCount NoCard }
      -- updatedPositions = fmapl populate positions cards
    

      -- where populate ps cs = [[]]
  

-- populate :: [Card] -> [[Position]] -> [[Position]]
-- populate card positions =
