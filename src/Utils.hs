{-# LANGUAGE TemplateHaskell #-}

module Utils
  ( canPlace
  , deckCardCount
  , hasWon
  , initialDeal
  , mkInitS
  , newGame
  , ranksCount
  , suitsCount
  , toColor
  , undoMove
  ) where

import Data.List.Split (splitPlaces)
import Lens.Micro ( (^.), (%~), (&), (.~), (^?!), _head )
import Lens.Micro.TH (makeLenses)
import qualified System.Random         as R (next, StdGen)
import qualified System.Random.Shuffle as R (shuffle')

import CardTypes

-------------------------------------------------------------------------------

makeLenses ''DCard
makeLenses ''Position
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
                           , _positions    = []
                           , _rankBias = Just rb
                           , _suitBias = Just sb
                           } = (r == rb) && (s == sb)
-- nonempty foundation piles reject aces
canPlace (Card RA _ ) Pile { _pileType = FoundP
                           , _positions = (dc:_)
                           } = False
-- nonempty foundation piles accept cards if they match suit and ascend rank
canPlace (Card r  s ) Pile { _pileType = FoundP
                           , _positions = HaveCard {_dcard = DCard{_card=Card r' s'}:_}:_
                           , _suitBias = Just sb
                           } = (pred r == r') && (s == sb)
-- given an empty tableau pile, a card needs to match its rankbias
canPlace (Card r  _ ) Pile { _pileType = TableP
                           , _positions = []
                           , _rankBias = Just rb
                           } = r == rb
-- nonempty tableau piles reject kings
canPlace (Card RK _ ) Pile { _pileType = TableP
                           , _positions = (dc:_)
                           } = False
-- nonempty tableau piles accept cards if they alternate color and descend rank
canPlace (Card r  s ) Pile { _pileType = TableP
                           , _positions = HaveCard {_dcard = DCard{_card=Card r' s'}:_}:_
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

-- if a game is won, all 52 cards are in the foundation
hasWon :: GSt -> Bool
hasWon s = length (s ^. field . found . traverse . cards) == 52

-- the default deal is a sorted list of cards. to be shuffled below
initialDeal = [ Card r s | r <- allRanks, s <- allSuits ]

suitsCount :: Int
suitsCount = 1 + fromEnum (maxBound :: Suit)

ranksCount :: Int
ranksCount = 1 + fromEnum (maxBound :: Rank)

-- how many cards are in a deck (52 actually)
deckCardCount :: Int
deckCardCount = ranksCount * suitsCount

inititalTableauCardCount :: Int
inititalTableauCardCount = 28

mkDeal :: R.StdGen -> [Card]
mkDeal = R.shuffle' initialDeal deckCardCount

-- take a random generator and create a game state...
mkInitS :: R.StdGen -> GSt
mkInitS seed = GSt { _field = field 
                   , _seed  = seed  , _history = [] 
                   , _score = 0     , _moves = 0
                   }
  where
    deal  = R.shuffle' initialDeal deckCardCount seed -- ...by shuffling the initialDeal
    field = Field { _waste = waste         -- and doling it out amongst
                  , _table = table, _found = found -- waste, tableau
                  }
    waste = [ Pile { _positions = [ DCard { _card    = c
                                         , _facedir = FaceUp 
                                         } 
                                  | c <- drop inititalTableauCardCount deal -- last 24 cards in deck
                                  ]
                   , _display  = Splayed -- shows cards within the waste faceup.
                   , _rankBias = Nothing
                   , _suitBias = Nothing
                   , _pileType = WasteP
                   }
            | cs <- splitPlaces [13, 13, 13, 13] deal -- Scott: deal is temporary
            ]
    table = [ Pile { _positions = [ DCard { _card    = c
                                         , _facedir = d
                                         } 
                                  | (c,d) <- zip cs (FaceUp:repeat FaceDown) 
                                  ]
                   , _display  = Splayed
                   , _rankBias = Just RK -- tableaus only accept base kings
                   , _suitBias = Nothing
                   , _pileType = TableP
                   } 
            | cs <- splitPlaces [7,6..1] deal -- list of lists of lengths 7,6..
            ]
    found = [ Pile { _positions = []
                   , _display  = Stacked
                   , _rankBias = Just RA -- foundations only accept base aces
                   , _suitBias = Just s
                   , _pileType = FoundP
                   } 
            | s <- allSuits ]

