module QuestionOne where

data Bit = On | Off
  deriving (Show, Eq, Ord, Enum, Read)

data Word = Empty | Cons Bit QuestionOne.Word
  deriving (Show, Read, Eq, Ord)

someFunc :: IO ()
someFunc = print On

bitToInt :: Bit -> Int
bitToInt On = 1
bitToInt Off = 0

wordToInts :: QuestionOne.Word -> [Int]
wordToInts Empty = []
wordToInts (Cons bit word) = (bitToInt bit) : (wordToInts word)

intsToString :: [Int] -> String
intsToString ints = concat (map show ints)

showWord :: QuestionOne.Word -> String
showWord w = (intsToString . wordToInts) w

-- Given some Word, treat it as a sequence of bits from LSB on the
-- left to MSB on the right. Convert it to the equivalent decimal Int.
wordToDecimal :: QuestionOne.Word -> Int
wordToDecimal = wordToDecimal' 0
  where wordToDecimal' e Empty = 0
        wordToDecimal' e (Cons On wrd) = (2^e) + wordToDecimal' (e + 1) wrd
        wordToDecimal' e (Cons Off wrd) = wordToDecimal' (e + 1) wrd
