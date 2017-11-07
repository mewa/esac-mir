module Converter
  (
  )
where

data MidiNote = MidiNote {
  pitch :: Int
  , duration :: Float
  } deriving (Show)

data MidiState = MidiState {
  now :: Ticks
  , baseDuration :: Ticks
  , basePitch :: Key
  } deriving (Show)

mel44 = "1-23b4#  5_.4#  654#5  3b2_. //"

makeTrack :: Ticks -> [MidiNote] -> Track Ticks
makeTrack base notes = join $ evalState (mapM makeNote notes) (MidiState 0 base 48)

makeNote :: MidiNote -> State MidiState (Track Ticks)
makeNote note = do
  s <- get
  let dur = round $ (fromIntegral $ baseDuration s) * duration note
  return $ [(0, NoteOn 0 (pitch note + basePitch s) 40)
           , (dur, NoteOff 0 (pitch note + basePitch s) 40)]

midi :: [MidiNote] -> Tempo -> Int -> Midi
midi notes tempo shortest = let
  ticksPerShortest = round $ 96 / (fromIntegral shortest / 4)
  in Midi SingleTrack (TicksPerBeat 96)
     [makeTempo tempo : makeTrack ticksPerShortest notes]


makeTempo tempo = (0, TempoChange (floor $ 1000000.0 / (fromIntegral tempo / 60.0)))

esacToMidi melody = parseEsac melody >>= \notes -> return (midi notes 90 8)

midiBytes = toLazyByteString . buildMidi
