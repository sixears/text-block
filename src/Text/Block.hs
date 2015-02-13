{-|

Module      : Text.Block
Description : handle blocks (rectangles) of text
Copyright   : (c) Martyn J. Pearce 2014
License     : BSD
Maintainer  : haskell@sixears.com

handle blocks (rectangles) of text
 
 -}

module Text.Block
  ( Block, VAlign(..), HAlign(..), TableOptions(..), WrapOptions(..)
  , height, hjoin, hpads
  , mkBlock, mkBlockn, stripEnds, stripStrings, strings
  , table, table'
  , vjoin, vpad, width, wrap, wrap'
  )
where

-- base --------------------------------

import Data.Char     ( isSpace )
import Data.List     ( intercalate, intersperse, transpose )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- stripEnd --------------------------------------------------------------------

-- | remove whitespace from the end of a string

stripEnd :: String -> String
stripEnd = reverse . dropWhile isSpace . reverse 

-- strip -----------------------------------------------------------------------

-- | remove whitespace from start & end of a string

strip :: String -> String
strip = dropWhile isSpace . stripEnd

-- mSpan -------------------------------

{- | split a list into segments, each the longest possible contiguous segment
     either satisying P or not satisfying P; thus alternating
 -}

mSpan :: (a -> Bool) -> [a] -> [[a]]
mSpan p xs' =
  case span p xs' of
    (xs, []) -> [xs] -- terminating condition comes first!  else mSpan p "" would
                     -- never terminate
    ([], xs) -> mSpan (not . p) xs
    (hs, ts) -> hs : mSpan (not . p) ts

--------------------------------------------------------------------------------

-- | vertical alignment within a block
data VAlign = VTop     -- ^ place text at top
            | VMiddle  -- ^ place text in middle
            | VBottom  -- ^ place text at bottom
       deriving ( Eq, Show )

-- | horizontal alignment within a string
data HAlign = HRight   -- ^ place text on right
            | HMiddle  -- ^ place text in middle
            | HLeft    -- ^ place text on left
       deriving ( Eq, Show )


-- | A block of text, to be rendered as a rectangle
newtype Block = Block [String]

instance Show Block where
  show b = let header = "+" ++ replicate (width b) '-' ++ "+"
               line s = "|" ++ s ++ "|"
            in intercalate "\n" ([header] ++ fmap line (strings b) ++ [header])

-- mkBlockn --------------------------------------------------------------------

-- | create a text block from a list of strings; each string will be a row

mkBlockn :: HAlign   -- ^ alignment of each row
         -> Int      -- ^ minimum width of resulting block
         -> [String] -- ^ each string will be a row of the block, top-to-bottom
         -> Block
mkBlockn align n ss = Block $ fmap (hpads align n) ss

-- mkBlock ---------------------------------------------------------------------

-- | like 'mkBlockn'; resulting width will be the length of the longest string
mkBlock :: [String] -> Block
mkBlock ss = mkBlockn HLeft (maximum $ fmap length ss) ss

-- | extract rows from a block, with 
strings :: Block -> [String]
strings (Block ss) = ss

-- stripStrings ----------------------------------------------------------------

-- | extract rows from a block, with end-of-line spaces stripped off

stripStrings :: Block -> [String]
stripStrings = fmap strip . strings

-- stripEnds ----------------------------------------------------------------

-- | extract rows from a block, with end-of-line spaces stripped off

stripEnds :: Block -> [String]
stripEnds = fmap stripEnd . strings

-- height ----------------------------------------------------------------------

-- | the height of a block in characters

height :: Block -> Int
height = length . strings

-- width -----------------------------------------------------------------------

-- | the width of a block in characters

width :: Block -> Int
width b = maximum $ 0 : fmap length (strings b)

-- wrap ------------------------------------------------------------------------

-- | detailed options for 'wrap''
data WrapOptions = 
  WrapOptions { wrap_width  :: Int    -- ^ wrap at or before this width
              , wrap_prefix :: String -- ^ prefix for subsequent lines when 
                                      --   wrapping
              }
instance Default WrapOptions where 
  def = WrapOptions { wrap_width = 80, wrap_prefix = "" }

-- | like wrap', but with an empty wrap_prefix

wrap :: Int -> String -> Block
wrap n = wrap' def { wrap_width = n }

-- wrap' -----------------------------------------------------------------------

-- | wrap a string into a block, breaking lines as required to fit into a
--   number of columns

wrap' :: WrapOptions -> String -> Block
wrap' opts s = let spans = mSpan isSpace s
                   pairs (x : y : xs) = (x, y) : pairs xs
                   pairs _            = []
                   -- blocks are pairs of strings, being a prefix-space and a word,
                   -- such that to add a word to the end of a line we add both; but
                   -- to start a new line (excepting the first), we add just the
                   -- word
                   blocks = if 0 < length s && isSpace ((head . head) spans)
                            then pairs spans
                            else ("", head spans) : pairs (tail spans)
                in mkBlock . reverse . fst $ foldl (wrapw opts) ([], 0) blocks

-- split out to make it easier to test
wrapw :: WrapOptions -> ([String], Int) -> (String, String) -> ([String], Int)
wrapw _ ([], _) (w, t) = let l = w ++ t in ([l], length l)
wrapw o (ss, l) (w, t) = let n = wrap_width  o
                             p = wrap_prefix o
                          in if l + length w + length t > n
                             then -- wrap to a new line
                                  let t' = p ++ t
                                   in (t' : ss, length t')
                             else -- continue on this line
                                  let l' = head ss ++ w ++ t
                                   in (l' : tail ss, length l')

-- vpad ----------------------------------------------------

-- | vertically pad a block with blank lines; each blank line will be the width
--   of the block

vpad :: VAlign -- ^ vertical block alignment
     -> Int    -- ^ block height to pad to 
     -> Block  -- ^ block to pad
     -> Block
vpad valign n b =
  let pad = (replicate (width b) ' ')
   in case valign of
        VTop    -> Block $ strings b ++ replicate (n - height b)  pad
        VMiddle -> let d  = (n - height b) `div` 2
                       r  = (n - height b) `rem` 2
                       b' = strings b
                    in Block $ replicate d pad ++ b' ++ replicate (d+r) pad
        VBottom -> Block $ replicate (n - height b) pad ++ strings b

-- hpads ---------------------------------------------------

-- | horizontally pad a string with spaces

hpads :: HAlign -- ^ horizontal string alignment
      -> Int    -- ^ pad to this width (no-op if < length input string)
      -> String -- ^ string to pad
      -> String
hpads HLeft   n s = s ++ replicate (n - length s) ' '
hpads HMiddle n s = let d = (n - length s) `div` 2
                        r = (n - length s) `rem` 2
                    in replicate d ' ' ++ s ++ replicate (d+r) ' '
hpads HRight  n s = replicate (n - length s) ' ' ++ s

-- hpad ----------------------------------------------------

hpad :: HAlign -> Int -> Block -> Block
hpad halign n b = mkBlock . fmap (hpads halign n) $ strings b

-- hjoin ---------------------------------------------------

{- | Join a list of blocks horizontally. The resulting list is as tall as the
     tallest block.
-}

hjoin :: VAlign -> [Block] -> Block
hjoin valign bs = let maxh   = maximum $ fmap height bs
                      bs'    = fmap (strings . vpad valign maxh) bs
                   in mkBlock . fmap concat $ transpose bs'

-- vjoin ---------------------------------------------------

{- | Join a list of blocks vertically. The resulting list is as wide as the
     widest block.
-}

vjoin :: HAlign -> [Block] -> Block
vjoin halign bs = let maxw   = maximum $ fmap width bs
                   in mkBlock . concat $ fmap (strings . hpad halign maxw) bs

-- table -----------------------------------------------------------------------

-- | render a list of lists of blocks as a table, each outer list being a row

table :: [[Block]] -> Block
table = table' def

-- | detailed options for rendering a table with 'table''

data TableOptions = TableOptions { table_valign        :: VAlign
                                 , table_halign        :: HAlign
                                 , table_cell_halign   :: HAlign
                                 , table_col_separator :: String
                                 , table_row_separator :: String
                                 }

instance Default TableOptions where
  def = TableOptions { table_valign        = VTop
                     , table_halign        = HLeft
                     , table_cell_halign   = HLeft
                     , table_col_separator = ""
                     , table_row_separator = ""
                     }
                
-- | render a list of lists of blocks as a table, each outer list being a row

table' :: TableOptions -> [[Block]] -> Block
table' opts bss =
  let col_separator  = table_col_separator opts :: String
      row_separator  = table_row_separator opts :: String
      row_height     = maximum . fmap height     :: [Block] -> Int
      separator_col  = (`replicate` col_separator) . row_height
      mk_sep_col row = intersperse $ separator_col row
      col_inter row  = fmap mkBlock . mk_sep_col row $ fmap strings row
      bss'           = fmap col_inter bss :: [[Block]]
      col_widths     = fmap maximum . transpose $ fmap (fmap width) bss' :: [Int]
      mk_row_sep     :: Int -> Block
      mk_row_sep w   = mkBlock [take w $ cycle row_separator]
      row_sep        = [mk_row_sep $ sum col_widths]
      bss''          = if null row_separator
                       then bss'
                       else intersperse row_sep bss'
      halign         = table_halign opts
      valign         = table_valign opts
      cell_halign    = table_cell_halign opts
  in vjoin halign $
       fmap (hjoin valign . fmap (uncurry (hpad cell_halign)) . zip col_widths) 
            bss''
