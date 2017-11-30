# midiToBeep

## Usage:
`stack build && stack exec midiToBeep -- [Flags] InputMidi OutputDir`
### Example:
`stack exec midiToBeep -- midi/grieg_mountainking.mid mountainking`
### Supported Flags:
- -m : merge all Tracks before splitting to agents
