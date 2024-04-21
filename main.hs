{-# LANGUAGE TypeOperators, TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin MonadicBang #-}
{-# LANGUAGE LambdaCase #-}

import Bmp (sporcle)
import Control.Lens
import Control.Monad
import Control.Zipper
import Data.Char
import Data.List
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy
import Math.Combinat.Permutations
import Prelude hiding (log)
import System.Directory
import System.IO
import System.Random
import Text.Printf


data Card = Card { _front :: String, _img :: Maybe Picture, _backs :: [String] }

data S = S { _log :: FilePath, _cards :: Top :>> [Card] :>> Card,
        _input, _inputErr :: [Char],
        _clean :: String -> String,
        _finished :: Bool }
foldMap makeLenses [ ''S,  ''Card ]


draw :: S -> IO Picture
draw s = return $ Translate (-w*fromIntegral ncol/2) (-h*nrow/2) $ Pictures $ zipWith f (s^.cards&rezip) [0 .. ]
 where
 nrow = lengthOf (cards . to rezip . folded) s `div` ncol & fromIntegral
 h = 50
 w = 80
 f (Card f p _) i = Translate (w * y) (h * x) $ box <> fromMaybe (text f) p
        where
        (x,y) = divMod i ncol & each %~ fromIntegral
        box | i < iCur = Color (withAlpha 0.1 white) $ rectangleSolid w h
            | i == iCur = Color white $ rectangleSolid w h
            | otherwise = Color (withAlpha 0.4 white) $ rectangleSolid w h
         
 iCur = s^.cards & tooth
 ncol = 16

evt :: Event -> S -> IO S
evt (EventKey (Char k) Up _ _) = advanceCard . (input %~ (k:)) . (inputErr %~ (k:))
 where
 advanceCard :: S -> IO S
 advanceCard s
        | let inp = (s^.clean) (s^.inputErr),
          stripped : _ <- s^..cards.focus.backs.traversed.reversed.to (`stripPrefix` inp)._Just
                = s & inputErr .~ stripped
                    & cards %%~ rightward
                    & maybe (do -- can't go farther right
                                unless (s^.finished) (logGame s)
                                s & finished .~ True & return)
                        return
         | otherwise = return s
evt _ = return

-- | log csv
logGame :: S -> IO ()
logGame s = do
  e <- doesFileExist (s^.log)
  h <- openFile (s^.log) AppendMode
  when (not e) $ hPutStrLn h "front,back,input,err,finished"

  let (fs,bs) = s^.cards & rezip & map (\ (Card f _ b) -> (f,head b)) & unzip

  hPutStrLn h $ intercalate "," $
        [intercalate "|" fs,
         intercalate "|" bs,
         s^.input,
         s^.inputErr,
         show (s^.finished)]
         & traversed.traversed %~ \case
                '\n' -> ' '
                ',' -> '_'
                c -> c

permuteIO :: [a] -> IO [a]
permuteIO xs = do
  let (p, _g) = randomPermutation (length xs) !initStdGen
  return $ permuteList p xs

main = do
  sporcle <- permuteIO sporcle
  fs <- mapM (loadJuicyPNG . printf "bmp/%s.png" . map toLower) $ sporcle ^.. traversed . _1
  let clean = map toLower . filter (not . isSpace)
  let c0 = [ Card a (scale 0.2 0.2 <$> f) (map clean b) | ((a, _emoji, b), f) <- sporcle `zip` fs ]
  let s0 = S { _cards = zipper c0 & fromWithin traversed,
                _clean = clean,
                _input = "",
                _inputErr = "",
                _finished = False,
                _log = "log.csv" }
  interactIO FullScreen black s0 draw evt (const (return ()))
