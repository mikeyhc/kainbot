module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Kain
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data OptionType = OptionHostName String
                | OptionHostPort String
                | OptionHelp
    deriving (Eq)

isHostOption :: OptionType -> Bool
isHostOption (OptionHostName _) = True
isHostOption _                  = False

isPortOption :: OptionType -> Bool
isPortOption (OptionHostPort _) = True
isPortOption _                  = False

optionValues :: OptionType -> String
optionValues (OptionHostName h) = h
optionValues (OptionHostPort p) = p
optionValues _                  = undefined

optlist :: [OptDescr OptionType]
optlist = [ Option "H" ["host"] (ReqArg OptionHostName "HOST")
            "the host to connect to"
          , Option "p" ["port"] (ReqArg OptionHostPort "PORT")
            "the port to connect on"
          , Option "h" ["help"] (NoArg OptionHelp) "this dialog"
          ]

main :: IO ()
main = do
    argv <- getArgs
    progname <- getProgName
    let (opts, _, errs) = getOpt Permute optlist argv
    case errs of
        [] -> if OptionHelp `elem` opts
                then putStrLn $ usageInfo (mkUsage progname) optlist
                else do
                    r <- checkArgs opts
                    case r of
                        Just (host, port) -> startKain host port
                        Nothing           -> failmsg progname
        _  -> do
            handleErrors errs
            failmsg progname
  where
    failmsg progname = do
        hPutStrLn stderr (usageInfo (mkUsage progname) optlist)
        exitFailure

handleErrors :: [String] -> IO ()
handleErrors = mapM_ (hPutStrLn stderr . (++) "\nerror: ")

mkUsage :: String -> String
mkUsage name = "usage:\n  " ++ name ++ " -H HOST -p PORT [-h]\n"

checkArgs :: [OptionType] -> IO (Maybe (HostName, Port))
checkArgs opts = let mhost = optionValues <$> find isHostOption opts
                     mport = optionValues <$> find isPortOption opts
                in case (mhost, mport) of
                        (Just host, Just port) -> return $ Just (host, port)
                        (Just host, Nothing)   -> return $ Just (host, "6667")
                        _                      -> do
                            hPutStrLn stderr "error: no host provided"
                            return Nothing
