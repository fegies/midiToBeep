module Main where

import Codec.Midi
import Control.Arrow      (second)
import Control.Monad      (zipWithM_,unless)
import Data.List.Split    (chunksOf)
import System.Directory   (createDirectoryIfMissing)
import System.Environment (getArgs)


nonempty [] = False
nonempty _  = True

keyToHertz :: Int -> Frequency
keyToHertz key = 2**((fromIntegral key-69)/12) * 440

apL2 f [a,b] = f a b

main :: IO ()
main = do
    opts <- getArgs >>= parseOptions
    unless (inputFile opts == "" || outputDir opts == "") $ 
        runMidi (inputFile opts) (outputDir opts) (mergeTracks opts)

data Options = Options {
    mergeTracks :: Bool,
    inputFile :: FilePath,
    outputDir :: FilePath
}

parseOptions :: [String] -> IO Options
parseOptions = parseOptions' $ Options False "" "" where
    parseOptions' o [] = return o
    parseOptions' o ("-m":xs) = parseOptions' ( o {mergeTracks=True} ) xs
    parseOptions' o (i:od:[]) = return $ o {inputFile=i, outputDir=od}
    parseOptions' _ _ = putStrLn "Usage: midiToBeep [Flags] MIDIFILE OUTPUTDIR" >> return (Options False "" "")

runMidi :: FilePath -> FilePath -> Bool -> IO ()
runMidi input outdir mergeTracks = do
    createDirectoryIfMissing False outdir
    midi <- fromRight (error $ "error parsing " ++ input) <$> importFile input
    let (Midi fileType timediff tracks) = if mergeTracks then toSingleTrack midi else midi
    let cleanedtracks = filter nonempty $ map (notesOff . filterNotes) tracks
    let generateOutput tracknum track = zipWithM_ writeTrack [1..] splittracks
            where
            splittracks = map (toBeepString . toBeepList . toRealTime timediff) . splitTrack $ track
            writeTrack agentnum = writeFile (outdir ++ "/track_" ++ show tracknum ++ "_agent_" ++ show agentnum ++ ".sh" )
    zipWithM_ generateOutput [1..] cleanedtracks


toBeepString :: [(Time,Maybe Frequency)] -> String
toBeepString l = "beep" ++ concatMap toArgs l  where
    toArgs (t,mf) = case mf of
        Nothing -> " -D " ++ time
        Just f  -> " -nf " ++ show f ++ " -l " ++ time
        where time = show (round $ t * 1000 :: Int)

--turns NoteOn with a velocity of 0 to NoteOff commands, makes processing easier
notesOff :: Track a -> Track a
notesOff = map (second analyseNote) where
    analyseNote (NoteOn chan key 0) = NoteOff chan key 0
    analyseNote a                   = a

--removes all midi events that are not note events
filterNotes :: Track a -> Track a
filterNotes = filter (isNoteControl . snd) where
    isNoteControl a = isNoteOn a || isNoteOff a

fromRight :: b -> Either a b -> b
fromRight a (Left _)  = a
fromRight _ (Right a) = a

type Frequency = Double
toBeepList :: Track Time -> [(Time, Maybe Frequency)] --Value of Nothing Means pause
toBeepList [] = []
toBeepList ((tw,NoteOn _ k _):(tn,NoteOff{}):xs)
    | tw > 0 = (tw,Nothing):(tn,Just $ keyToHertz k) : toBeepList xs
    | otherwise = (tn,Just $ keyToHertz k) : toBeepList xs

type Agent a = (Maybe Int, [(a,Message)]) -- Nothing when the agent is not playing anything, Just key when he is
initialAgents = repeat (Nothing, [])

--splits up a track so that there are no notes being played at the same time
--runs into an inifinte loop on malformed midi files!
splitTrack :: Num a => Track a -> [Track a]
splitTrack = map (fromAbsTime . reverse) . takeWhile nonempty . map snd . runsplit initialAgents . toAbsTime where

    runsplit :: [Agent a] -> [(a,Message)] -> [Agent a]
    runsplit agents []                   = agents
    runsplit agents (m@(_,NoteOn{}):xs)  = runsplit (setNote agents m) xs
    runsplit agents (m@(_,NoteOff{}):xs) = runsplit (unsetNote agents m) xs
    runsplit agents (_:xs)               = runsplit agents xs

    setNote :: [Agent a] -> (a,Message) -> [Agent a]
    setNote ((Nothing, al) : as) m@(_,NoteOn _ k _) = (Just k,m:al) : as
    setNote (a@(Just _,_) : as) m                   = a:setNote as m

    unsetNote :: [Agent a] -> (a,Message) -> [Agent a]
    unsetNote (a@(Nothing, _):as) m = a : unsetNote as m
    unsetNote (a@(Just ak, al):as) m@(_,NoteOff _ nk _)
        | ak == nk = (Nothing,m:al) : as
        | otherwise = a : unsetNote as m
