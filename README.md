# chip8-emulator
Learning rust &amp; emulators through making a Chip 8 Emulator


# Setup

## Install SDL2 if you don't already have it
Download instructions for most OS can be found on their github page README => https://github.com/Rust-SDL2/rust-sdl2#sdl20-development-libraries

Holy headache downloading and getting sdl2 to work with Rust... 
I tried the framework on mac, and was unable to get it working. In the end, I uninstalled it all, then downloaded using homebrew.
After that, set the environment variables with 
- export LIBRARY_PATH="$LIBRARY_PATH:/opt/homebrew/opt/sdl2/lib"
- export CPLUS_INCLUDE_PATH="$CPLUS_INCLUDE_PATH:/opt/homebrew/opt/sdl2/include"
- export CPATH="$CPATH:/opt/homebrew/opt/sdl2/include"

and then all you need in the cargo.toml was the normal dependency.

# Sources used:

- https://tobiasvl.github.io/blog/write-a-chip-8-emulator/
- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
