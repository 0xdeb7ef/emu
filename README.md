# emu

This repo is ment to hold my attempts at emulating random stuff that I find interesting.

At the moment, there's only an unfinished CHIP-8 implementation.

My goal is to make it a sort of library that can be used to emulate various different CPUs
and/or hardware.

## CHIP-8

All the CHIP-8 opcodes have been implemented and tested against the
[CHIP-8 Test Suite](https://github.com/Timendus/chip8-test-suite).

The CHIP-8 core passes the the following test ROMs:

- `1-chip8-logo.ch8`
- `2-ibm-logo.ch8`
- `3-corax+.ch8`
- `4-flags.ch8`

The followings tests are currently skipped because they require functionality that
hasn't been implemented yet:

- `5-quirks.ch8`
- `6-keypad.ch8`
- `7-beep.ch8`

### Things left to do

- The keyboard opcodes haven't been tested yet.
- The core does not yet handle the sound and delay timers, so that needs a bit
  of work to get working.
- Based on the previous task, the whole emulation cycle needs to be worked on so it can
  handle different speeds/frequencies, you could probably just do those manually for now
  but it would be nicer to have the core handle it, maybe a `cycle()` function.

### References

- [Guide to making a CHIP-8 emulator](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#logical-and-arithmetic-instructions)
- [Cowgod's CHIP-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM)
- [CHIP-8 Variant Opcode Table](https://chip8.gulrak.net/)
- and many more... [emudev.org](https://emudev.org/system_resources)
