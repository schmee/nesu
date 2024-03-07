# Nesu

Nesu is a NES emulator written from scratch in Zig. It has a single goal: to be able to emulate Super Mario Bros with some degree of accuracy, including audio. It makes no attempt to be cycle accurate, and it is very limited: almost no configuration options, only runs Mapper #0 games, only runs on macOS etc. I made it open-source since it is "complete" in the sense that it does what I set out to do, but there is no reason to use Nesu over any of the fully-featured NES emulators already out there.

## Why?

I've been playing games on emulator since way before I learned programming, and I remember thinking about emulators as some sort of magic: "what do you mean I can play SNES games on my computer!?". Many years and lots of programming later emulators have gradually changed from some arcane device into something intelligible. So I set out to write my own, as a throwback to my childhood and as a concrete example of how something that seems impossible when you start out can become possible with enough dedication, time and practice.

## Building

Nesu is tested on macOS, it may or may not work on other platforms, you're welcome to give it a try!

1. Download Zig 0.12.0 for your platform from https://ziglang.org/download/.
1. Clone this repo and `cd` in to it.
1. In `config.ini`, set the `roms_path` option to where you keep your ROMs (note: this is relative to the `nesu` folder).
1. Run `zig build run -Drelease`.
1. Select your game from the list and press ENTER to play.

## Controls

Game:
- F: A
- D: B
- Arrow keys: Left/Right/Up/Down
- Enter: Start
- V: Select

Other:
- C: show FPS counter
- L: show ROM list
- R: hold to reset
- Esc: quit

## Acknowledgements

This project would not have been possible without the many amazing resources on NES and emulator development out there. Here are some of the most valuable resources I used.

[The NesDev wiki](https://www.nesdev.org/wiki/Nesdev_Wiki): _the_ resource for NES game and emulator development and without it this project would have been impossible. Contains everything you need to know to write your own emulator, although it may not be obvious where to find it or how to piece thing together until after the fact.

[NesDev forums](https://forums.nesdev.org/): has the answer to almost any question imaginable provided by people with a seemingly endless knowledge about NES hardware, software and everything in between. In particular, I found a lot of details about APU implementation I was not able to find anywhere else.

[FCEUX](https://fceux.com/web/home.html): an actual, fully fledged cross platform NES emulator. I used it throughout development as a gold standard for "correct" behavior, and the built-in debugger and tile viewers were also extremely helpful.

[NES Emulation: The Good, The Bad, and The Tedious](https://ltriant.github.io/2019/11/22/nes-emulator.html): a great blog post detailing one man's journey to implement his own NES emulator. It was refreshing to see that someone else went through many of the same struggles as I did in their development, and the single mention at the end of "Mario not showing up on the title screen of Super Mario Bros was how I really learned about how the SBC instruction worked" easily saved me a couple of days of debugging.

[Test ROMs](https://www.nesdev.org/wiki/Emulator_tests): a collection of ROMs to validate emulators implementations. Invaluable for feedback on progress in the early stages of development before the emulator can run any actual games, especially the `Nestest` CPU test ROM really helped me get off the ground.

[6502 Instruction Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html): combined with test ROMs mentioned above, this made implementing the CPU a breeze.

[FIIIR! - Design FIR & IIR Filters](https://fiiir.com/): for generating the FIR filter used to reduce the aliasing distortion when downsampling the NES waveforms to a modern-day soundcard.
