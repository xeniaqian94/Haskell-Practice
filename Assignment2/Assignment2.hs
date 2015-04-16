module Assignment2 where

import Data.Char
import Test.QuickCheck
import Test.HUnit

-- Exercise 1
tailsLC :: [a] -> [[a]]
tailsLC xs = [drop i xs|i<-[0..length xs]]    -- drop 0,1,...length xs elements from xs

-- Exercise 2
tailsHOF :: [a] -> [[a]]
tailsHOF xs = foldl (\ys y->ys++[tail $ last ys]) [xs] xs  -- fold left by taking tail of the last element of the accumulator

type PhonePad = [(Int, String)]

usualPad :: PhonePad
usualPad = [ (1, ""), (2, "abc"), (3, "def"),
            (4, "ghi"), (5, "jkl"), (6, "mno"),
            (7, "pqrs"),(8, "tuv"), (9, "wxyz"),
                        (0, " ")]

usualPad2 :: PhonePad
usualPad2 = [ (1, ""), (2, "ABC"), (3, "dEF"),
            (4, "ghi"), (5, "jkl"), (6, "mno"),
            (7, "pqrs"),(8, "tuv"), (9, "wxyz"),
                        (0, " ")]


-- Exercise 3
toLowerPad :: [(Int,String)]->[(Int,String)] -- make the keyboard layout to be case insensitive
toLowerPad pPad = [(i,[toLower x|x<-xs])|(i,xs)<-pPad,xs/=""] 

encodeSingle :: Char -> PhonePad -> String -- every single character is encoded to phone button press string
encodeSingle c pPad = concat [if toLower c==c' then take j $ repeat $ chr (i+48) else ""|(i,xs)<-toLowerPad pPad, j<-[1..length xs], let c'=xs!!(j-1)] 

encodeLC :: PhonePad -> String -> String -- concatenate the strings for each character together 
encodeLC pPad s = concat [encodeSingle x pPad|x<-s]


-- Exercise 4
toLowerPad' :: [(Int,String)]->[(Char,String)] -- modify from [(1,""),(2,"abc")] to [('a',"2"),('b',"22"),('c',"222")]
toLowerPad' pPad = concat $ map (\(i,xs)-> map (\j->(toLower $ xs!!(j-1), take j $ repeat $ chr (i+48))) [1..length xs]) pPad

encodeSingle' :: Char -> PhonePad -> String -- e.g. Given 'e', return "33"
encodeSingle' c pPad = concat $ map (\(c',cs)->if toLower c==c' then cs else "") (toLowerPad' pPad)

encodeHOF :: PhonePad -> String -> String -- concatenate the strings for each character together
encodeHOF pPad s = concat $ map (\x -> encodeSingle' x pPad) s

-- Exercise 5
-- | Based on http://oz.ccnet.us/dayofweek/method14.txt
type Mnemonic = String

-- | An Int representing a Gregorian year.
type Year = Int

-- | An Int representing a Gregorian month (1-based).
type Month = Int

-- | An Int representing a Gregorian day.
type Day = Int
type Date = (Year, Month, Day)

baseMnemonic :: Mnemonic
baseMnemonic = "IRS FLU FOX IRA"

{-
For month m, select the mth letter from the 12-letter mnemonic
"IRS FLU FOX IRA", and then find that letter on the phone keypad.
That key is that month's phone number.  (For example, December is
the 12th month, so select the mnemonic's 12th letter, "A", which
is found on phone button 2.):

> monthToPhone baseMnemonic usualPad 12
2
-}
-- Exercise 5(a)
findButton :: Char -> PhonePad -> Int -- e.g. Given 'a'/'A', find that it is on button 2
findButton c pPad = fst.head $ filter (\(i,cs)-> if any (==toLower c) cs then True else False) (toLowerPad pPad)

monthToPhone :: Mnemonic -> PhonePad -> Month -> Int -- Filter out non-letter the mnemonic, find the m-th letter, then call findButton 
monthToPhone mn p m = findButton ((filter isAlpha mn)!!(m-1))  p

{-
Using that "phone number" in place of "#", follow these steps on
the phone's calculator ("YYYY.MM" is the 4-digit year followed by
a decimal point and the 2-digit month, and DD is the day):

YYYY.MM * 1.25 + 0.97 = (drop the fraction, integer part is ZZZZ)
ZZZZ + # + DD =
/ 7 =
-}
-- Exercise 5(b)
dowNumber :: Mnemonic -> PhonePad -> Date -> Float
dowNumber mn p (yyyy, mm, dd) = fromIntegral (monthToPhone mn p mm + dd + truncate (1.25*(read $ show yyyy ++"."++mm' :: Float) + 0.97))/7
                             where mm' = if mm<10 then "0"++show mm else show mm
                             -- Convert String to Float, then calculate the value 
                             -- Note: (2015, 03, 07) => mm=04, dd=07, show mm =>"04" instead of "4", need some modification!

{-
This method suggests
looking at the first digit of the fraction part and using the
phone keypad to help visualize the result as follows:

.1=Mon .2=Tue .3=unused
.4=Wed .5=Thu .6=unused
.7=Fri .8=Sat .9=unused
.0=Sun

-}
-- Exercise 6(a)
firstDecimalDigit :: Float -> Int
firstDecimalDigit x = (ord $ head $ tail $ dropWhile (/='.') (show x) ) - 48 
          -- convert to string, then dropWhile encounter '.', take the first element after it

dayPad :: PhonePad
dayPad = [ (1, "Monday"), (2, "Tuesday"), (3, ""),
            (4, "Wednesday"), (5, "Thursday"), (6, ""),
            (7, "Friday"),(8, "Saturday"), (9, ""),
                        (0, "Sunday")]

data PhoneDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded)

-- Exercise 6(b)
instance Enum PhoneDay where
  fromEnum = fromEnumCustom dayPad
    where fromEnumCustom :: PhonePad -> PhoneDay -> Int
          fromEnumCustom p d = fst.head $ filter (\(d,pd)->if pd == show d then True else False) p -- use show operation of Show class to convert PhoneDay to String
  toEnum = toEnumCustom dayPad
    where toEnumCustom :: PhonePad -> Int -> PhoneDay
          toEnumCustom p d = read.snd.head $ filter (\(d,pd)->if d==d'' then True else False) p -- use read operation of Read class to convert String to PhoneDay
            where max = maximum $ fst $ unzip p -- get the largest button number
                  d'  = abs d -- negate d if it's negative
                  d'' = if d' > max then d' `mod` max else d' -- necessary modification

dayOfWeek :: Date -> PhoneDay
dayOfWeek (yyyy, mm, dd) = toEnum $ firstDecimalDigit $ dowNumber baseMnemonic usualPad (yyyy, mm, dd)
