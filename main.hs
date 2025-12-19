{-# LANGUAGE TypeOperators, TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin MonadicBang #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
import Data.List.Split

import Paths_worldflags
import Data.Time
import Data.Time.Clock.POSIX


data Card = Card { _front :: String, _img :: Maybe Picture, _backs :: [String] }

data S = S { _log :: FilePath, _cards :: Top :>> [Card] :>> Card,
        _input, _inputErr :: [Char],
        _inputTime, _inputErrTime :: [POSIXTime],
        _clean :: String -> String,
        _finished :: Bool }
foldMap makeLenses [ ''S,  ''Card ]


draw :: S -> IO Picture
draw s = return $ Translate (-w*fromIntegral ncol/2) (h -h*nrow/2) $ Pictures $ (++[currentText]) $ zipWith f (s^.cards&rezip) [0 .. ]
 where
 currentText = Translate 0 (-2.5*h) $ Color white $ Text $ unwords $ addCursor $ map reverse $ take 3 $ splitOn "\t" $ _input s
 addCursor ([]: xs) = "_" : xs
 addCursor xs = xs
 nrow = lengthOf (cards . to rezip . folded) s `div` ncol & fromIntegral
 h = 65
 w = 110
 f (Card f p _) i = Translate (w * y) (h * x) $ box <> fromMaybe (text f) p
        where
        (x,y) = divMod i ncol & each %~ fromIntegral
        box | i < iCur = Color (withAlpha 0.1 white) $ rectangleSolid w h
            | i == iCur = Color white $ rectangleSolid w h
            | otherwise = Color (withAlpha 0.4 white) $ rectangleSolid w h
         
 iCur = s^.cards & tooth
 ncol = 15

insertCM :: S -> IO S
insertCM s = s & input %~ ('\t':)
        & inputTime %%~ (\case
                x:xs -> return (x:x:xs)
                [] -> (:[]) <$> getPOSIXTime)

evt :: Event -> S -> IO S
evt (EventKey (Char k) Down _ _) = \s0 -> do
        t <- getPOSIXTime
        s0 & inputErr %~ (k:)
           & input %~ (k:)
           & inputTime %~ (t:)
           & inputErrTime %~ (t:)
           & advanceCard
 where
 advanceCard :: S -> IO S
 advanceCard s
        | let inp = (s^.clean) (s^.inputErr),
          stripped : _ <- s^..cards.focus.backs.traversed.reversed.to (`stripPrefix` inp)._Just
                = s & inputErr .~ stripped
                    & insertCM
                    <&> cards %%~ rightward
                    >>= maybe (do -- can't go farther right
                                -- the last flag needs a trailing \t
                                s' <- insertCM s <&> finished .~ True
                                unless (s^.finished) $ do
                                        logGame s'
                                        putStrLn "logged"
                                return s')
                        return
         | otherwise = return s
evt _ = return

-- | log csv
logGame :: S -> IO ()
logGame s = do
  e <- doesFileExist (s^.log)
  h <- openFile (s^.log) AppendMode
  when (not e) $ do
    hPutStrLn h "# 2 letter country code, one of the allowable country names, every input character, input charaters discarded, did the game end, time in unix seconds for the first character, seconds relative to time0 for input, seconds relative to time0 for error"
    hPutStrLn h "front,back,input,err,ok,finished,time0,inputTime,errTime,inputOkTime"

  let (fs,bs) = s^.cards & rezip & map (\ (Card f _ b) -> (f,head b)) & unzip

  let takePrec (break (=='.') -> (a,b)) = takeWhile (/='s') $ a ++ take 5 b
      fmtTS = intercalate "|" . reverse . map (takePrec . show . (subtract t0))
      t0 = s ^?! inputTime . _last

      (inputOkT, inputOk) = unzip
                $ zip (s^.inputTime) (s^.input)
                        `sub` zip (s^.inputErrTime) (s^.inputErr)
        where
        -- more robust but slower equivalent:
        -- sub xs ys = Set.toDescList (Set.fromList xs Set.\\ Set.fromList ys)
        sub x [] = x
        sub (x@(t1,c1):xs) (y@(t2,c2):ys)
                | x == y = sub xs ys
                | otherwise = x : sub xs ys

  hPutStrLn h $ intercalate "," $
        [intercalate "|" fs,
         intercalate "|" bs,
         s^.input & reverse,
         s^.inputErr & reverse,
         reverse inputOk,
         show (s^.finished),
         init (show t0),
         fmtTS (s^.inputTime),
         fmtTS (s^.inputErrTime),
         fmtTS inputOkT]
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
  fs :: [Maybe Picture] <- do
    let twoLetter = sporcle ^.. traversed . _1
    ps <- mapM (getDataFileName . printf "bmp/%s.png" . map toLower) twoLetter
    mapM loadJuicyPNG ps
  let clean = map toLower . filter (not . isSpace)
  let c0 = [ Card a (scale 0.2 0.2 <$> f) (map clean b) | ((a, _emoji, b), f) <- sporcle `zip` fs ]
  let s0 = S { _cards = zipper c0 & fromWithin traversed,
                _clean = clean,
                _input = "",
                _inputErr = "",
                _inputErrTime = [], _inputTime = [],
                _finished = False,
                _log = "log.csv" }
  interactIO FullScreen black s0 draw evt (const (return ()))
