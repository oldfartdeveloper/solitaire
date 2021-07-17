{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( canPlace
  , deckCardCount
  , hasWon
  , initialDeal
  , initialTableauCardCount
  , mkInitS
  , newGame
  , toColor
  , undoMove
  ) where

import Data.List (concat, sortBy)
import Data.List.Split (splitPlaces)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Pile
makeLenses ''Field
makeLenses ''GSt

-------------------------------------------------------------------------------

toColor :: Suit -> Color -- assigns colors to suits
toColor Spade = Black
toColor Club  = Black
toColor _     = Red

canPlace :: Card -> Pile -> Bool -- says whether a card can be placed on a pile
-- given an empty foundation pile, a card needs to match both biases
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
-- nonempty foundation piles reject aces
canPlace (Card RA _ ) Pile { _pileType = FoundP
                           , _cards    = (dc:_)
                           } = False
-- nonempty foundation piles accept cards if they match suit and ascend rank
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _suitBias = Just sb
                           } = (pred r == r') && (s == sb)
-- given an empty tableau pile, a card needs to match its rankbias
canPlace (Card r  _ ) Pile { _pileType = TableP
                           , _cards    = []
                           , _rankBias = Just rb
                           } = r == rb
-- nonempty tableau piles reject kings
canPlace (Card RK _ ) Pile { _pileType = TableP
                           , _cards    = (dc:_)
                           } = False
-- nonempty tableau piles accept cards if they alternate color and descend rank
canPlace (Card r  s ) Pile { _pileType = TableP
                           , _cards    = (DCard{_card=Card r' s'}:_)
                           , _rankBias = Just rb
                           } = (succ r == r') && (toColor s /= toColor s')
canPlace _ _ = False -- if not covered above, default invalid

allRanks :: [Rank]
allRanks = [minBound .. maxBound] :: [Rank] -- list of all ranks

allSuits :: [Suit]
allSuits = [minBound .. maxBound] :: [Suit] -- list of all suits

-- given a game with a seed, get a new seed and use it to spawn a new game
newGame :: GSt -> GSt  
newGame s = let seed' = snd $ R.next $ s ^. seed 
            in  mkInitS seed'

undoMove :: GSt -> GSt
undoMove s = if hasHistory
               then s & field .~ oldField
                      & history %~ drop 1
                      & score .~ oldScore
                      & moves %~ pred

               else s
  where (oldField, oldScore) = s ^. history ^?! _head -- assured if called
        hasHistory = not $ null $ s ^. history

-- if a game is won, there are no facedown cards in the tableau
hasWon :: GSt -> Bool
hasWon s = do
  let cards = (s ^. field . table) >>= _cards
  not (any (\c -> FaceDown == _facedir c) cards)

-- hasWon s = 20 == length $ filter (\fd -> fd == FaceDown) (map  _facedir ((s ^. field . table) >>= _cards) )
-- hasWon s = do
--    let x = s ^. field . table . traverse . card
--        l = length $ concat x
--    20 == l

-- the default deal is a sorted list of cards. to be shuffled below
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

-- how many cards are in a deck (52 actually)
deckCardCount :: Int
deckCardCount = (1 + fromEnum (maxBound :: Rank)) * (1 + fromEnum (maxBound :: Suit))

initialTableauCardDistribution :: [Int]
initialTableauCardDistribution = [7,6..1]

initialTableauCardCount :: Int
initialTableauCardCount = sum initialTableauCardDistribution

initialWasteCardCount :: Int
initialWasteCardCount = deckCardCount - initialTableauCardCount

distributeCardsBySuitSorted :: [Card] -> [[Card]]
distributeCardsBySuitSorted =
    sortCardsBySuit
    -- [[], [], [], []]                

sortCardsBySuit :: [Card] -> [[Card]]
sortCardsBySuit cards = do
        let clubs = filter (\(Card _ s) -> s == Club) cards
            diamonds = filter (\(Card _ s) -> s == Diamond) cards
            hearts = filter (\(Card _ s) -> s == Heart) cards
            spades = filter (\(Card _ s) -> s == Spade) cards
        [ sort clubs    , sort diamonds    , sort hearts    , sort spades    ] 

sort :: [Card] -> [Card]
sort = sortBy compare  -- compare works probably because only the rank is 
                       -- changing within a column; suit is constant.

-- take a random generator and create a game state...
mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _field = field 
                   , _seed  = seed  , _history = [] 
                   , _score = 0     , _moves = 0
                   }
  where
    deal  = R.shuffle' initialDeal deckCardCount seed -- ...by shuffling the initialDeal
    field = Field { _waste = waste                 -- and doling it out amongst
                  , _table = table, _found = found -- waste, tableau, and foundation
                  }
    waste = [ Pile { _cards    = [ DCard { _card    = c
                                         , _facedir = FaceUp 
                                         } 
                                 | c <- cs
                                 ]
                   , _display  = Splayed -- shows cards within the waste faceup.
                   , _rankBias = Nothing
                   , _suitBias = Nothing
                   , _pileType = WasteP
                   }
            | cs <- distributeCardsBySuitSorted $ drop initialTableauCardCount deal
            ]
    table = [ Pile { _cards    = [ DCard { _card    = c
                                         , _facedir = d
                                         } 
                                 | (c,d) <- zip cs (FaceUp:repeat FaceDown) 
                                 ]
                   , _display  = Splayed
                   , _rankBias = Just RK -- tableaus only accept base kings
                   , _suitBias = Nothing
                   , _pileType = TableP
                   } 
            | cs <- splitPlaces initialTableauCardDistribution deal -- list of lists of lengths 7,6..
            ]
    found = [ Pile { _cards    = []
                   , _display  = Stacked
                   , _rankBias = Just RA -- foundations only accept base aces
                   , _suitBias = Just s
                   , _pileType = FoundP
                   } 
            | s <- allSuits ]
 

