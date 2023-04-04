# nsfx
Nsfx is a project that provides a powerful and flexible way to create music for the Nintendo Entertainment System (NES) using 6502 assembly language. The NES was released in the 1980s and features a unique sound chip known as the Ricoh 2A03. This sound chip produces a distinct 8-bit sound that is beloved by many retro gamers and chiptune musicians.

Nsfx is designed to give developers and musicians fine-grained control over the NES's sound hardware. It provides a set of APIs that allow you to compose and play music using the NES's five channels of sound: two pulse waves, one triangle wave, one noise channel, and one delta modulation channel.

### Features

+ Comprehensive and powerful 6502 assembly language API for composing and playing music
+ Support for up to 5 channels of sound (2 pulse waves, 1 triangle wave, 1 noise channel, and 1 delta modulation channel)
+ Efficient sound playback using the NES's sound hardware
+ Support for both PAL and NTSC NES systems
+ Integration with popular NES development tools, such as cc65 and FCEUX

### Getting Started

To get started with the nsfx, you will need to have an NES development environment set up. This typically involves installing a cross-compiler such as cc65, and an emulator such as FCEUX. Once you have these tools installed, you can download the nsfx source code from the gitHub repository and start integrating it into your project.

Nsfx includes a demo program that demonstrate how to use the API to play music. The demo can be built using the included Makefile. The Makefile also provides targets for building a ROM file that can be run on an NES emulator or hardware.

### License

Nsfx is released under the MIT License. See the LICENSE file for more information.
