-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Weather
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A weather monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Weather where

import Plugins.Monitors.Common

import Control.Monad (when)
import System.Process
import System.Exit
import System.IO

import Text.ParserCombinators.Parsec


weatherConfig :: IO MConfig
weatherConfig = mkMConfig
       "<station>: <tempC>C, rh <rh>% (<hour>)" -- template
       ["station"                               -- available replacements
       , "stationState"
       , "year"
       , "month"
       , "day"
       , "hour"
       , "wind"
       , "visibility"
       , "skyCondition"
       , "tempC"
       , "tempF"
       , "dewPoint"
       , "rh"
       , "pressure"
       ]

data WeatherInfo =
    WI { stationPlace :: String
       , stationState :: String
       , year         :: String
       , month        :: String
       , day          :: String
       , hour         :: String
       , wind         :: String
       , visibility   :: String
       , skyCondition :: String
       , tempC        :: Int
       , tempF        :: Int
       , dewPoint     :: String
       , humidity     :: Int
       , pressure     :: Int
       } deriving (Show)

pTime :: Parser (String, String, String, String)
pTime = do y <- getNumbersAsString
           char '.'
           m <- getNumbersAsString
           char '.'
           d <- getNumbersAsString
           char ' '
           (h:hh:mi:mimi) <- getNumbersAsString
           char ' '
           return (y, m, d ,([h]++[hh]++":"++[mi]++mimi))

pTemp :: Parser (Int, Int)
pTemp = do let num = digit <|> char '-' <|> char '.'
           f <- manyTill num $ char ' '
           manyTill anyChar $ char '('
           c <- manyTill num $ char ' '
           skipRestOfLine
           return $ (floor (read c :: Double), floor (read f :: Double))

pRh :: Parser Int
pRh = do s <- manyTill digit $ (char '%' <|> char '.')
         return $ read s

pPressure :: Parser Int
pPressure = do manyTill anyChar $ char '('
               s <- manyTill digit $ char ' '
               skipRestOfLine
               return $ read s

parseData :: Parser [WeatherInfo]
parseData = 
    do st <- getAllBut "," 
       space
       ss <- getAllBut "("
       skipRestOfLine >> getAllBut "/"
       (y,m,d,h) <- pTime
       w <- getAfterString "Wind: "
       v <- getAfterString "Visibility: "
       sk <- getAfterString "Sky conditions: "
       skipTillString "Temperature: "
       (tC,tF) <- pTemp
       dp <- getAfterString "Dew Point: "
       skipTillString "Relative Humidity: "
       rh <- pRh
       skipTillString "Pressure (altimeter): "
       p <- pPressure
       manyTill skipRestOfLine eof
       return $ [WI st ss y m d h w v sk tC tF dp rh p]

defUrl :: String
defUrl = "http://weather.noaa.gov/pub/data/observations/metar/decoded/"

getData :: String -> IO String
getData url=
        do (i,o,e,p) <- runInteractiveCommand ("curl " ++ defUrl ++ url ++ ".TXT")
           exit <- waitForProcess p
           let closeHandles = do hClose o
                                 hClose i
                                 hClose e
           case exit of
             ExitSuccess -> do str <- hGetContents o
                               when (str == str) $ return ()
                               closeHandles
                               return str
             _ -> do closeHandles
                     return "Could not retrieve data"

formatWeather :: [WeatherInfo] -> Monitor String
formatWeather [(WI st ss y m d h w v sk tC tF dp r p)] =
    do cel <- showWithColors show tC
       far <- showWithColors show tF
       parseTemplate [st, ss, y, m, d, h, w, v, sk, cel, far, dp, show r , show p ]
formatWeather _ = return "N/A"

runWeather :: [String] -> Monitor String
runWeather str =
    do d <- io $ getData $ head str
       i <- io $ runP parseData d
       formatWeather i
