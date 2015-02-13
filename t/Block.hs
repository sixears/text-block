#!/usr/local/bin/runghc -i/home/martyn/bin/hlib

import Test.QuickCheck  ( Arbitrary(..), Gen
                        , elements, listOf, oneof, quickCheck, resize )

import Data.Char     ( isAlphaNum, isSpace )
import Data.Default  ( def )
import Data.List     ( intercalate )
import Data.Maybe    ( fromJust, isJust )

import System.IO.Unsafe  ( unsafePerformIO )

import Text.Printf  ( printf )

import Fluffy.Data.List   ( findEnds, splitBy )
import Fluffy.Devel.Test  ( is, check, test )
import Fluffy.Text.Block  ( Block, HAlign(..), TableOptions(..), WrapOptions(..)
                          , VAlign(..)
                          , height, width, hjoin, hpads
                          , mkBlock, stripEnds, stripStrings, strings
                          , table, table', vpad
                          , wrap, wrap'
                          )

dump s x =
  unsafePerformIO $ do
    putStrLn $ "\n# " ++ s ++ ": " ++ show x ++ " (" ++ s ++ ") ##"
    return x

-- vpad ------------------------------------------------------------------------

instance Arbitrary VAlign where
  arbitrary = elements [ VTop, VMiddle, VBottom ]

instance Arbitrary HAlign where
  arbitrary = elements [ HLeft, HMiddle, HRight ]

myWords = elements [ "the", "quick", "brown", "fox", "jumps", "over", "the"
                   , "lazy", "dog" ]

-- MyString ----------------------------

newtype MyString = S String

_s :: MyString -> String
_s (S s) = s

_ss :: [MyString] -> [String]
_ss = map _s

instance Show MyString where
  show (S s) = show s

instance Arbitrary MyString where
  arbitrary = fmap S myString

myString :: Gen String
myString = fmap concat $ resize 7 $ listOf $ oneof [ elements [" "], myWords ]

-- MyInt -------------------------------

newtype MyInt = I Int

instance Show MyInt where
  show (I i) = show i

instance Arbitrary MyInt where
  arbitrary = fmap I myInt

myInt :: Gen Int
myInt = elements [1..12]

-- Block Arbitrary ---------------------

-- generate arbitrary blocks as lists of shortish strings to allow for fast
-- testing on my lil' X101-CH

instance Arbitrary Block where
  arbitrary = fmap (mkBlock . _ss) arbitrary

-- vpad ------------------------------------------------------------------------

eachIs :: Eq a => a -> [a] -> Bool
eachIs x xs = and $ map (x ==) xs
            
prop_vpad_length :: VAlign -> MyInt -> Block -> Bool
prop_vpad_length v (I n) b = 
  height (vpad v n b) == max n (height b)

prop_vpad_content :: VAlign -> MyInt -> Block -> Bool

prop_vpad_content valign (I n) b =
  let padded        = vpad valign n b
      pad           = replicate (width b) ' '
      splitHeight   = -- not used in VMiddle
                    if valign == VBottom then n - height b else height b
      (top, bottom) = splitAt splitHeight (strings padded)
   in case valign of 
        VTop    -> top == strings b && eachIs pad bottom
        VMiddle -> case findEnds (strings b) (strings padded) of
                     Just (s,e) -> eachIs pad (s ++ e)
                     Nothing    -> False        
        VBottom -> bottom == strings b && eachIs pad top

--------------------

-- test that content is middle-aligned; to do this, we surround the incoming
-- string with non-empty lines to ensure that we don't get ambiguity over the
-- result of "findEnds"

prop_vpad_mcontent :: MyInt -> Block -> Bool
prop_vpad_mcontent (I n) b 
  | []  == filter (not . null) (strings b) = 
    let padded = vpad VMiddle n b
     in    height padded == max n (height b)
        && [] == filter (not . null) (strings padded)
  | otherwise =
    let surround = replicate (width b) '='
        b'       = [surround] ++ strings b ++ [surround]
        padded   = vpad VMiddle n (mkBlock b')
        pad      = replicate (width b) ' '
     in case findEnds b' (strings padded) of
          Just (start, end) ->    eachIs pad (start ++ end)
                               && 2 > abs(length end - length start)
          Nothing           -> False

-- hpads -----------------------------------------------------------------------

prop_hpads_length :: HAlign -> MyInt -> MyString -> Bool
prop_hpads_length v (I n) (S s) = 
  length (hpads v n s) == max n (length s)

--------------------

prop_hpads_content :: HAlign -> MyInt -> MyString -> Bool
prop_hpads_content HLeft (I n) (S s) = 
  let padded        = hpads HLeft n s
      (left, right) = splitAt (length s) padded
   in left == s && [] == filter (not . isSpace) right

prop_hpads_content HMiddle (I n) (S s) =
   let padded = hpads HMiddle n s
    in case findEnds s padded of
         Just (start, end) -> [] == filter (not . isSpace) (start ++ end)
         Nothing           -> False

prop_hpads_content HRight (I n) (S s) = 
  let padded        = hpads HRight n s
      (left, right) = splitAt (n - length s) padded
   in right == s && [] == filter (not . isSpace) left

--------------------

-- test that content is middle-aligned; to do this, we surround the incoming
-- string with non-spaces to ensure that we don't get ambiguity over the 
-- result of "findEnds"

prop_hpads_mcontent :: MyInt -> MyString -> Bool
prop_hpads_mcontent (I n) (S s) = 
   let s' = "=" ++ s ++ "="
       padded = hpads HMiddle n s'
    in case findEnds s' padded of
         Just (start, end) ->    [] == filter (not . isSpace) (start ++ end)
                              && 2 > abs(length end - length start)
         Nothing           -> False

-- wrap ------------------------------------------------------------------------

prop_wrap :: String -> Int -> Bool
prop_wrap s' n = let s = map (\ c -> if isAlphaNum c then c else ' ') s'
                  in prop_wrap' (stripEnds $ wrap n s) s n

prop_wrap' :: [String] -> String -> Int -> Bool
prop_wrap' xs s n = null $ filter (not . null) $ check_lines xs s n

check_lines :: [String] -> String -> Int -> [String]
check_lines [] _ _ = []
check_lines xs s n =
  fst $ foldl check ([],s) xs
  where check :: ([String],String) -> String -> ([String],String)
        check (errs, t) x = let (err, t') = check_line x t n
                             in (err : errs, t')

check_lines' :: String -> Int -> [String]
check_lines' s n = check_lines (stripStrings $ wrap n s) s n

{- take a wrapped line x, and a full line s; and a wrap length n.  Check that x
   is a well-wrapped prefix of s, meaning that
   -) x is a prefix of s
   -) x is of length <= n, or else x is a single word
   -) x ++ first next word (and its preceding space) would be longer than n

  Return a string of errors (empty string if all is well), plus the checkable
  suffix of s after x (i.e., drop x and any whitespace immediately after x)
 -}
check_line :: String -> String -> Int -> (String, String)
check_line x s n =
  -- s = h ++ l ++ m
  -- where h is the bit as long as x; l is the intervening space, m is the next
  -- word after (which we use for checking that we couldn't wrap further)
  let (h, t) = splitAt (length x) s
      (l, m) = span isSpace t
      -- can reconstitute string
      reconstitute = if h == x
                     then Nothing
                     else Just $ printf "'%s' != '%s'" x h
      -- not too long; either <n, or is a single word
      too_long = if (length x <= n)
                    -- check for a single word, possibly prefixed with space
                    || ([] == dropWhile (not . isSpace) (dropWhile isSpace x))
                 then Nothing
                 else Just $ printf "length %d > %d (%s)" (length x) n x
      -- next word (if any) couldn't've been tacked onto the end
      too_short = if (null t) || (null m) || (length (s ++ l ++ m) > n)
                  then Nothing
                  else Just $ printf "should wrap '%s' onto '%s'" (l++m) s
  in ( intercalate " " $ map fromJust
                       $ filter isJust [reconstitute, too_long, too_short]
     , m )

-- hjoin -----------------------------------------------------------------------

newtype MyBlocks = MyBlocks [Block] deriving ( Show )

instance Arbitrary MyBlocks where
  arbitrary = fmap MyBlocks $ resize 5 $ listOf arbitrary

prop_hjoin_width :: VAlign -> MyBlocks -> Bool
prop_hjoin_width valign (MyBlocks bs) = 
  let joined = hjoin valign bs
   in width joined == (sum $ map width bs)
                                 
prop_hjoin_height :: VAlign -> MyBlocks -> Bool
prop_hjoin_height valign (MyBlocks bs) = 
  let joined = hjoin valign bs
   in height joined == (maximum $ 0 : map height bs)

--------------------------------------------------------------------------------

main = do
  let s   = "the quick brown fox jumps over the lazy dog"
      bss = [ map mkBlock [[ "the", "quick", "brown" ], [ "fox", "jumps" ]]
            , map mkBlock [[ "over", "the" ], [ "lazy", "dog" ]]
            ]
            
  test [ is (strings $ wrap 10 s)
            [ "the quick "
            , "brown fox "
            , "jumps over"
            , "the lazy  "
            , "dog       "
            ]
            "wrap"
         
       , is ( strings $ wrap' def { wrap_width = 9 } s)
            [ "the quick"
            , "brown fox"
            , "jumps    "
            , "over the "
            , "lazy dog "
            ]
            "wrap' 1"
         
       , is ( strings $ wrap' def { wrap_width = 9, wrap_prefix = " " } s)
            [ "the quick"
            , " brown   " 
            , " fox     "
            , " jumps   "
            , " over the"
            , " lazy dog"
            ]
            "wrap' 2"
    
       , is (strings $ table bss) 
            [ "the  fox  "
            , "quickjumps" 
            , "brown     "
            , "over lazy "
            , "the  dog  "
            ] 
            "table"
       
       , is (strings $ table' def bss) 
            [ "the  fox  "
            , "quickjumps" 
            , "brown     "
            , "over lazy "
            , "the  dog  "
            ] 
            "table'"
       
       , is (strings $ table' def { table_valign        = VBottom
                                  , table_col_separator = " | "
                                  , table_row_separator = "- "
                                  } 
                              bss) 
            [ "the   |      "
            , "quick | fox  "
            , "brown | jumps"
            , "- - - - - - -"
            , "over  | lazy "
            , "the   | dog  "
            ] 
            "table'"

                , is (strings $ table' def { table_valign = VBottom
                                           , table_cell_halign = HRight
                                           , table_col_separator = " | "
                                           , table_row_separator = "- "
                                           } 
                              bss) 
            [ "the   |      "
            , "quick | fox  "
            , "brown | jumps"
            , "- - - - - - -"
            , " over |  lazy"
            , " the  |  dog "
            ] 
            "table'"
       
       , check prop_wrap "prop_wrap"
       , check prop_vpad_length "prop_vpad_length"
       , check prop_vpad_content "prop_vpad_content"
       , check prop_vpad_mcontent "prop_vpad_mcontent"
       , check prop_hpads_length "prop_hpads_length"
       , check prop_hpads_content "prop_hpads_content"
       , check prop_hpads_mcontent "prop_hpads_mcontent"
       , check prop_hjoin_width "prop_hjoin_width"
       , check prop_hjoin_height "prop_hjoin_height"
       ]
