{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, mzero, when, void)
import Control.Exception (Exception, SomeException, throw, catch)
import Data.Char (toLower)
import Data.Functor
import Data.List (nub)
import Data.Monoid
import Data.Typeable (Typeable)
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Process
import System.Exit
import Data.Time

import Text.Parsec


data TimeSpec = TimeSpec { units :: TimeSpecUnits, instances :: [Int],
                           every :: Int }
  deriving (Show, Eq)

data Flag = Verbose | Version | RunInShell | TimeSpecFlag !TimeSpec |
            FailureExecutable FilePath
  deriving (Show, Eq)
data TimeSpecUnits = Minutes | Hours
  deriving (Show, Eq)

data TimeSpecParseException = TimeSpecParseException TimeSpecUnits ParseError
  deriving (Typeable)

instance Exception TimeSpecParseException
instance Show TimeSpecParseException where
  show (TimeSpecParseException units err) =
    "Failed to parse spec for " ++ (map toLower $ show units) ++ ": " ++
      (show err)


progName :: String
progName = "roger"


timeSpecAllows :: TimeSpec -> Int -> Bool
timeSpecAllows spec n = isAnInstance && allowedForEvery
  where isAnInstance = case instances spec of
                         [] -> True
                         is -> n `elem` is
        sEvery = every spec
        allowedForEvery = sEvery == 0 || (n `mod` sEvery == 0)

parseTimeSpec :: TimeSpecUnits -> String -> TimeSpec
parseTimeSpec units s = case parse parser "" s of
                        Left err   -> throw $
                          TimeSpecParseException units err
                        Right spec -> spec
                  where parser = do
                          nestedInstances <- instancesP
                          every <- option 0 everyP
                          eof
                          return TimeSpec {
                            units,
                            instances = nub $ concat nestedInstances,
                            every
                          }
                        instancesP = allInstancesP <|>
                          (sepBy1 instanceP $ char ',')
                        allInstancesP = char '*' >> return []
                        instanceP = try instanceRangeP <|> singleInstanceP
                        instanceRangeP = do
                          start <- many1 digit
                          void $ char '-'
                          end <- many1 digit
                          return [readInt start..readInt end]
                        singleInstanceP = do
                          i <- many1 digit
                          return [readInt i]
                        everyP = do
                          void $ char '/'
                          e <- many1 digit
                          return $ readInt e
                        readInt :: String -> Int
                        readInt = read

options :: [OptDescr Flag]
options =
  [ Option ['v'] ["verbose"] (NoArg Verbose) "chatty output on stderr"
  , Option [] ["version"] (NoArg Version) "show version number"
  , Option [] ["notify-failure"] (ReqArg FailureExecutable "FILE") "run executable on failure"
  , Option ['s'] ["shell"] (NoArg RunInShell) "run command in shell"
  , Option [] ["minutes"] (ReqArg (parseTimeSpecFlag Minutes) "SPEC") "minutes spec"
  , Option [] ["hours"] (ReqArg (parseTimeSpecFlag Hours) "SPEC") "hours spec"
  ]

parseTimeSpecFlag :: TimeSpecUnits -> String -> Flag
parseTimeSpecFlag units = TimeSpecFlag . parseTimeSpec units

parseArgv :: [String] -> ([Flag], [String])
parseArgv argv =
  case getOpt RequireOrder options argv of
     (o, n, [])    -> (o,n)
     (_, _ , errs) -> error $ concat errs ++ usageInfo header options
  where header = "Usage: roger [OPTION...] cmdargs..."

extractTimeSpecs :: [Flag] -> [TimeSpec]
extractTimeSpecs = foldl ex []
  where ex specs flag = case flag of
                          TimeSpecFlag spec -> spec : specs
                          _                 -> specs

timeSpecForUnits :: [TimeSpec] -> TimeSpecUnits -> Maybe TimeSpec
timeSpecForUnits specs u = case filter (\s -> (units s) == u) specs of
                             []  -> Nothing
                             [s] -> Just s
                             _   -> error $ "multiple specs for " ++ (show u)

mainLoop :: Maybe TimeSpec -> Maybe TimeSpec -> CreateProcess -> Maybe FilePath -> [Flag] -> IO ()
mainLoop minsSpec hoursSpec cmd failureExec flags = forever $ do
  zonedTime <- getCurrentTime >>= utcToLocalZonedTime
  when (shouldRunCmd (zonedTimeToLocalTime zonedTime)) $ do
    verbosePrintLn "Running command..."
    (_, _, _, cmdH) <- createProcess cmd
    exitCode <- waitForProcess cmdH
    case exitCode of
      ExitSuccess -> verbosePrintLn "Exited successfully"
      ExitFailure n -> do
        verbosePrintLn $ "Exited with error code: " ++ (show n)
        maybeNotifyFailure

  threadDelay 1000000 -- microseconds, so one second

  where isVerbose = Verbose `elem` flags
        verbosePrintLn s = when isVerbose $ putStrLn s
        maybeNotifyFailure = maybe mzero notifyFailure failureExec
        notifyFailure fn = void $ spawnProcess fn [] >>=
                                    (forkIO . void . waitForProcess)
        shouldRunCmd localTime = let t = localTimeOfDay localTime
                                     hours = todHour t
                                     mins = todMin t
                                     secs = todSec t
                                     allowed spec n = maybe True (flip timeSpecAllows n) spec
                                     in
          (floor secs) == (0 :: Integer) &&
            all (uncurry allowed) [(minsSpec, mins), (hoursSpec, hours)]

main' :: IO ()
main' = do
  (flags, args) <- parseArgv <$> getArgs
  when (null args) $ error "no command given"

  cmdspec <- getCmdSpec flags args
  let cmd = CreateProcess {cmdspec, cwd = Nothing, env = Nothing,
                           std_in = Inherit, std_out = Inherit,
                           std_err = Inherit, close_fds = True,
                           create_group = True, delegate_ctlc = True}
  let ts = timeSpecForUnits $ extractTimeSpecs flags
  mainLoop (ts Minutes) (ts Hours) cmd (failureExec flags) flags

  where getCmdSpec flags args = do
          let shouldRunInShell = RunInShell `elem` flags
          when (shouldRunInShell && length args > 1) $
            putStrLn "warning: only first argument will be used for shell command"
          return $ if shouldRunInShell
                   then ShellCommand $ head args
                   else RawCommand (head args) (tail args)
        failureExec flags = getFirst . mconcat . (<$> flags) $
                              \f -> case f of
                                      FailureExecutable fn -> First (Just fn)
                                      _ -> First Nothing

mainErrHandler :: SomeException -> IO ()
mainErrHandler e = putStrLn (progName ++ ": " ++ show e) >> exitFailure

main :: IO ()
main = catch main' mainErrHandler
