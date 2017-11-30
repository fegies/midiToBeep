[![Build Status](https://travis-ci.org/fegies/midiToBeep.svg?branch=master)](https://travis-ci.org/fegies/midiToBeep)
# midiToBeep

## Usage:
`stack build && stack exec midiToBeep -- [Flags] InputMidi OutputDir`
### Example:
`stack exec midiToBeep -- midi/grieg_mountainking.mid mountainking`
### Supported Flags:
- -m : merge all Tracks before splitting to agents
