data Card = ValueCard Int | Jack | Queen | King | Ace deriving (Eq, Show) -- TODO: constrain Int to [2..10]
data HandValue = HandValue Int | HandBJ deriving (Eq, Ord, Show)
data Result = Lose | Push | Win | WinByBJ deriving (Show)

compare (HandValue _) (HandBJ)      = LT
compare (HandBJ)      (HandValue _) = GT
compare (HandBJ)      (HandBJ)      = EQ

cardValue :: Card -> Int
cardValue card = case card of
    Jack          -> 10
    Queen         -> 10
    King          -> 10
    Ace           -> 11
    (ValueCard x) -> x

handValue :: [Card] -> HandValue
handValue cards
    | (length cards) == 2 && cardSum == 21 = HandBJ
    | otherwise = HandValue cardSum
    where cardSum = sum (map cardValue cards)

notBust :: HandValue -> Bool
notBust x = case x of
    (HandValue x) -> x < 22
    HandBJ        -> True

playerResult :: HandValue -> HandValue -> Result
playerResult p d
    | p < d && notBust d = Lose
    | p == d             = Push
    | p == HandBJ        = WinByBJ
    | otherwise          = Win

payout :: (Integral a, Fractional b) => a -> Result -> b
payout bet res = case res of
    Lose    -> 0
    Push    -> realToFrac bet
    Win     -> realToFrac (2 * bet)
    WinByBJ -> ((5/2) * realToFrac bet)
