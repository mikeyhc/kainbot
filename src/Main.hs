module Main where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Kain
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data OptionType = OptionHostName String
                | OptionHostPort String
                | OptionNick String
                | OptionPass String
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
            "the port to connect on [default: 6667]"
          , Option "n" ["nick"] (ReqArg OptionNick "NICK")
            "the nickname to use [default: kain]"
          , Option "P" ["pass"] (ReqArg OptionPass "PASSWORD")
            "the admin password [default: kainpass]"
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
                    r <- checkArgs opts (Nothing, Nothing, Nothing, Nothing)
                    case r of
                        Just (nick, pass, host, port) ->
                            startKain nick pass host port
                        Nothing                       -> failmsg progname
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
mkUsage name = "usage:\n  " ++ name ++ " -H HOST [-p PORT] [-n NICK] "
            ++ "[-P PASSWORD] [-h]\n"

checkArgs :: [OptionType]
          -> (Maybe String, Maybe String, Maybe String, Maybe String)
          -> IO (Maybe (String,  String, String, String))
checkArgs (OptionNick n:xs) (_, mpass, mh, mport) =
    checkArgs xs (Just n, mpass, mh, mport)
checkArgs (OptionPass p:xs) (mn, _, mh, mport) =
    checkArgs xs (mn, Just p, mh, mport)
checkArgs (OptionHostName h:xs) (mn, mpass, _, mport) =
    checkArgs xs (mn, mpass, Just h, mport)
checkArgs (OptionHostPort p:xs) (mn, mpass, mh, _) =
    checkArgs xs (mn, mpass, mh, Just p)
checkArgs [] (mn, mpass, mh, mport) =
    if isNothing mh then hPutStrLn stderr "error: no host provided"
                      >> return Nothing
                    else return $ fillBlanks (mn, mpass, mh, mport)

fillBlanks :: (Maybe String, Maybe String, Maybe String, Maybe String)
           -> Maybe (String, String, String, String)
fillBlanks (Nothing, mpass, h, mport) =
    fillBlanks(Just "kain", mpass, h, mport)
fillBlanks (mn, Nothing, h, mport) =
    fillBlanks(mn, Just "kainpass", h, mport)
fillBlanks (mn, mpass, h, Nothing) =
    fillBlanks(mn, mpass, h, Just "6667")
fillBlanks (mn, mpass, mh, mp) =
    case (mn, mpass, mh, mp) of
        (Just n, Just pass, Just h, Just port) -> Just (n, pass, h, port)
        _                                      -> error "failed!"
