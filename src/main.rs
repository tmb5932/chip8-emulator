use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;
use sdl2::{event::Event, keyboard::Keycode, pixels::PixelFormatEnum, render::{Canvas, Texture}, video::Window};
use std::{fs::File, io::{self, BufRead}, time::{Duration, Instant}};
use sdl2::audio::{AudioCallback, AudioSpecDesired};

// SDL2 Display Constants
const DISPLAY_WIDTH: usize = 64;
const DISPLAY_HEIGHT: usize = 32;
const SCALE: u32 = 10;

// Chip8 Memory Location Constants
const FONTSET_START: usize = 0x50;
const ROM_START: usize = 0x200;

// Emulator Cycle Return Values
const SUCCESSFUL_EXECUTION: u8 = 0;
const EXIT_ROM: u8 = 1;

struct SquareWave {
    phase_inc: f32,
    phase: f32,
    volume: f32,
}

impl AudioCallback for SquareWave {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        for x in out.iter_mut() {
            *x = if self.phase <= 0.5 {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

const FONTSET: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80  // F
];

pub fn map_keycode(key: Keycode) -> Option<usize> {
    match key {
        Keycode::Num1 => Some(0x1),
        Keycode::Num2 => Some(0x2),
        Keycode::Num3 => Some(0x3),
        Keycode::Num4 => Some(0xC),
        Keycode::Q    => Some(0x4),
        Keycode::W    => Some(0x5),
        Keycode::E    => Some(0x6),
        Keycode::R    => Some(0xD),
        Keycode::A    => Some(0x7),
        Keycode::S    => Some(0x8),
        Keycode::D    => Some(0x9),
        Keycode::F    => Some(0xE),
        Keycode::Z    => Some(0xA),
        Keycode::X    => Some(0x0),
        Keycode::C    => Some(0xB),
        Keycode::V    => Some(0xF),
        _ => None,
    }
}

struct Quirks {
    load_store: bool,
    shift: bool,
    jump: bool,
    vf_reset: bool,
    clip: bool 
}

impl Quirks {
    pub fn new(ld: Option<bool>, shift: Option<bool>, jump: Option<bool>, vf_reset: Option<bool>, clip: Option<bool>) -> Self {
        Quirks {
            load_store: ld.unwrap_or(true),
            shift: shift.unwrap_or(false),
            jump: jump.unwrap_or(false),
            vf_reset: vf_reset.unwrap_or(true),
            clip: clip.unwrap_or(true) 
        }
    }
}

struct Instruction {
    instruction: u16,
    nibble: u8,
    x: usize,
    y: usize,
    n: u8,
    nn: u8,
    nnn: u16
}

impl Instruction {
    pub fn new(inst: u16) -> Self {
        Instruction {
            instruction: inst,
            nibble: ((inst & 0xF000) >> 12) as u8,
            // X: The second nibble. Used to look up one of the 16 registers (VX) from V0 through VF.
            x: ((inst & 0x0F00) >> 8) as usize,
            // Y: The third nibble. Also used to look up one of the 16 registers (VY) from V0 through VF.
            y: ((inst & 0x00F0) >> 4) as usize,
            // N: The fourth nibble. A 4-bit number.
            n: (inst & 0x000F) as u8,
            // NN: The second byte (third and fourth nibbles). An 8-bit immediate number.
            nn: (inst & 0x00FF) as u8,
            // NNN: The second, third and fourth nibbles. A 12-bit immediate memory address.
            nnn: inst & 0x0FFF,
        }
    }
}

struct Chip8 {
    memory: [u8; 4096],
    v: [u8; 16],
    i: u16,
    pc: u16,
    display: [[bool; DISPLAY_WIDTH]; DISPLAY_HEIGHT],
    stack: [u16; 16],
    sp: usize,
    delay_timer: u8,
    sound_timer: u8,
    keypad: [bool; 16],
    draw_flag: bool,
    wait_for_release: bool,
    wait_key: usize,
    rng: ChaCha8Rng,

    // Quirks
    quirks: Quirks,

    // Debug
    debug: bool,
    paused: bool
}

impl Chip8 {
    pub fn new(quirks: Quirks) -> Self {
        let mut chip8 = Chip8 {
            memory: [0; 4096],
            v: [0; 16],
            i: 0,
            pc: 0x200,
            display: [[false; DISPLAY_WIDTH]; DISPLAY_HEIGHT],
            stack: [0; 16],
            sp: 0,
            delay_timer: 0,
            sound_timer: 0,
            keypad: [false; 16],
            draw_flag: false,
            wait_for_release: false,
            wait_key: 0,
            rng: ChaCha8Rng::from_seed(Default::default()),
            quirks: quirks,

            // Debug flags
            debug: false,
            paused: false
        };

        for (i, byte) in FONTSET.iter().enumerate() {
            chip8.memory[FONTSET_START + i] = *byte;
        }

        chip8
    }

    pub fn debug_print(&mut self) {
        println!("PC: 0x{:X}", self.pc);
        let mut line: u8 = 0;
        for register in 0..16 {
            print!("v{:X}: 0x{:X}  \t", register, self.v[register]);
            if line >= 4 {
                print!("\r\n");
                line = 0;
            } else {
                line += 1;
            }
        }

        print!(" I: 0x{:X}\r\n\n", self.i)
    }

    pub fn load_rom(&mut self, filename: &str) -> std::io::Result<()> {
        // Open the file and auto-return if it fails
        let data = std::fs::read(filename)?;

        // Copy each byte into memory starting at 0x200
        for (i, byte) in data.iter().enumerate() {
            self.memory[ROM_START + i] = *byte;
        }

        Ok(())
    }

    pub fn fetch(&mut self) -> Instruction {
        let raw = (self.memory[self.pc as usize] as u16) << 8 |  self.memory[(self.pc + 1) as usize] as u16;
        self.pc += 2;
        Instruction::new(raw)
    }

    pub fn execute(&mut self, inst: Instruction) -> std::io::Result<u8> {
                // Execute
                match inst.nibble {
                    0x0 => {
                        match inst.nn {
                            0xE0 => { self.display = [[false; DISPLAY_WIDTH]; DISPLAY_HEIGHT]; },
                            0xEE => {
                                // Return from main (close ROM)
                                if self.sp == 0 {
                                    return Ok(EXIT_ROM);
                                }
                                // return from sub function
                                self.sp -= 1;
                                self.pc = self.stack[self.sp];
                            },
                            _ => { println!("Unknown opcode: {:04X}", inst.instruction); }
                        }
                    }
                    0x1 => {
                        // Jump: PC = NNN
                        self.pc = inst.nnn;
                    }
                    0x2 => {
                        // JAL: STACK[SP] = PC AND PC = NNN
                        self.stack[self.sp] = self.pc;
                        self.sp += 1;
                        self.pc = inst.nnn;
                    }
                    0x3 => {
                        // Skip 1 instruction if VX == NN
                        if self.v[inst.x] == inst.nn {
                            self.pc += 2;
                        }
                    }
                    0x4 => {
                        // Skip 1 instruction if VX != NN
                        if self.v[inst.x] != inst.nn {
                            self.pc += 2;
                        }
                    }
                    0x5 => {
                        match inst.n {
                            0x0 => {
                                // Skip 1 instruction if VX == VY
                                if self.v[inst.x] == self.v[inst.y] {
                                    self.pc += 2;
                                }
                            }
                            _ => { panic!("Unknown opcode: {:04X}", inst.instruction); }
                        }
                    }
                    0x6 => {
                        // SET: VX = NN
                        self.v[inst.x] = inst.nn;
                    }
                    0x7 => {
                        // ADD: VX = VX + NN
                        let (result, _) = self.v[inst.x].overflowing_add(inst.nn);
                        self.v[inst.x] = result;
                    }
                    0x8 => {
                        match inst.n {
                            // BINARY OPS
                            0x0 => {
                                // SET: VX = VY
                                self.v[inst.x] = self.v[inst.y];
                            }
                            0x1 => {
                                // OR: VX = VX OR VY
                                self.v[inst.x] = self.v[inst.x] | self.v[inst.y];
                                if self.quirks.vf_reset {
                                    self.v[0xF] = 0; 
                                }
                            }
                            0x2 => {
                                // AND: VX = VX AND VY
                                self.v[inst.x] = self.v[inst.x] & self.v[inst.y];
                                if self.quirks.vf_reset {
                                    self.v[0xF] = 0; 
                                }
                            }
                            0x3 => {
                                // XOR: VX = VX XOR VY
                                self.v[inst.x] = self.v[inst.x] ^ self.v[inst.y];
                                if self.quirks.vf_reset {
                                    self.v[0xF] = 0; 
                                }
                            }
                            0x4 => {
                                // ADD (with overflow): VX = VX + VY
                                let (sum, carry) = self.v[inst.x].overflowing_add(self.v[inst.y]);
                                self.v[inst.x] = sum;
                                self.v[0xF] = if carry { 1 } else { 0 };
                            }
                            0x5 => {
                                // 8XY5 sets VX to the result of VX - VY.                        
                                let (result, borrowed) = self.v[inst.x].overflowing_sub(self.v[inst.y]);
                                self.v[inst.x] = result;
                                self.v[0xF] = if borrowed { 0 } else { 1 };
                            }
                            0x6 => {
                                // SHIFT QUIRK: 8XY6 VX = VY >> 1
                                // No QUIRK:    8XY6 VX = VX >> 1
                                let shift_src = if self.quirks.shift { inst.x } else { inst.y };
                                let lsb: u8 = self.v[shift_src] & 0x1;
                                self.v[inst.x] = self.v[shift_src] >> 1;
                                self.v[0xF] = lsb;
                            }
                            0x7 => {
                                // 8XY7 sets VX to the result of VY - VX.
                                let (result, borrowed) = self.v[inst.y].overflowing_sub(self.v[inst.x]);
                                self.v[inst.x] = result;
                                self.v[0xF] = if borrowed { 0 } else { 1 };
                            }
                            0xE => {
                                // YSHIFT:    8XYE VX = VY << 1
                                // No YSHIFT: 8XYE VX = VX << 1
                                let shift_src = if self.quirks.shift { inst.x } else { inst.y };
                                let msb: u8 = (self.v[shift_src] >> 7) & 0x1;
                                self.v[inst.x] = self.v[shift_src] << 1;
                                self.v[0xF] = msb;
                            }
                            _ => { panic!("Unknown opcode: {:04X}", inst.instruction); }
                        }
                    }
                    0x9 => {
                        match inst.n {
                            0x0 => {
                                // Skip 1 instruction if VX != VY
                                if self.v[inst.x] != self.v[inst.y] {
                                    self.pc += 2;
                                }
                            }
                            _ => { panic!("Unknown opcode: {:04X}", inst.instruction) }
                        }
                    }
                    0xA => {
                        // I = NNN
                        self.i = inst.nnn;
                    }
                    0xB => {
                        // CHIP-8:     0xBNNN Jump to NNN + V0
                        // SUPER-CHIP: 0xBXNN Jump to XNN + VX
                        let v_src = if self.quirks.jump { inst.x } else { 0 };
                        self.pc = inst.nnn + self.v[v_src] as u16;
                    }
                    0xC => {
                        // VX = random number bitwise & with NN
                        let random_byte: u8 = (self.rng.next_u32() & 0xFF) as u8;
                        self.v[inst.x] = random_byte & inst.nn;
                    }
                    0xD => {
                        // Alter Display
                        let x_coord = self.v[inst.x] as usize % DISPLAY_WIDTH;
                        let y_coord = self.v[inst.y] as usize % DISPLAY_HEIGHT;
                        self.v[0xF] = 0; // Reset collision flag

                        for index in 0..inst.n as usize {
                            let sprite_byte = self.memory[self.i as usize + index];
                            
                                    // Y-coordinate handling
                            let pixel_y = y_coord + index;
                            if self.quirks.clip && pixel_y >= DISPLAY_HEIGHT {
                                continue; // skip drawing if clipped vertically
                            }

                            for bit_index in 0..8 {
                                let pixel_x = x_coord + bit_index;
                                if self.quirks.clip && pixel_x >= DISPLAY_WIDTH {
                                    continue; // skip drawing if clipped horizontally
                                }
                    
                                // Apply wrapping if clipping is off
                                let px = if self.quirks.clip {
                                    pixel_x
                                } else {
                                    pixel_x % DISPLAY_WIDTH
                                };
                                let py = if self.quirks.clip {
                                    pixel_y
                                } else {
                                    pixel_y % DISPLAY_HEIGHT
                                };
                                            
                                let sprite_pixel_on = (sprite_byte >> (7 - bit_index)) & 1 == 1;
                                let current_pixel = self.display[py][px];
                        
                                if sprite_pixel_on {
                                    if current_pixel {
                                        self.v[0xF] = 1; // Collision
                                    }
                        
                                    self.display[py][px] ^= true;
                                }
                            }
                        }
                        self.draw_flag = true;
        
                    }
                    0xE => {
                        match inst.nn {
                            0x9E => {
                                // Skip next instruction if X key is pressed
                                if self.keypad[self.v[inst.x] as usize] {
                                    self.pc += 2;
                                }
                            }
                            0xA1 => {
                                // Skip next instruction if X key is NOT pressed
                                if !self.keypad[self.v[inst.x] as usize] {
                                    self.pc += 2;
                                }
                            }
                            _ => { panic!("Unknown opcode: {:04X}", inst.instruction) }
                        }
                    }
                    0xF => {
                        match inst.nn {
                            // Timer Instructions
                            0x07 => {
                                // Set VX to current value of Delay Timer
                                self.v[inst.x] = self.delay_timer;
                            }
                            0x0A => {
                                if !self.wait_for_release { // If not actively waiting for key release
                                    // Check if any button is pressed
                                    for (i, &pressed) in self.keypad.iter().enumerate() {
                                        if pressed {
                                            self.wait_for_release = true;
                                            self.wait_key = i;
                                            break;
                                        }
                                    }
                                } 
                                // If waiting for initial key press or key release
                                if !self.wait_for_release || (self.wait_for_release && self.keypad[self.wait_key]) {
                                    // Don't advance to next instruction
                                    self.pc -= 2;
                                } else { // The key was let go
                                    self.v[inst.x] = self.wait_key as u8;
                                    self.wait_for_release = false;
                                }
                            }
                            0x15 => {
                                // Sets the Delay Timer to the value in VX
                                self.delay_timer = self.v[inst.x];
                            }
                            0x18 => {
                                // Sets the sound timer to the value in VX
                                self.sound_timer = self.v[inst.x];
                            }
                            0x1E => {
                                // I = I + VX
                                let (result, overflow) = self.i.overflowing_add(self.v[inst.x] as u16);
                                self.i = result;
                                self.v[0xF] = if overflow { 1 } else { 0 };
                            }
                            0x29 => {
                                // I = location of sprite for digit in VX
                                self.i = FONTSET_START as u16 + (self.v[inst.x] as u16 * 5);
                            }
                            0x33 => {
                                // Store number in VX as three decimal digits, and stores in mem at location in reg I
                                let value = self.v[inst.x];
                                self.memory[self.i as usize] = value / 100;
                                self.memory[self.i as usize + 1] = (value % 100) / 10;
                                self.memory[self.i as usize + 2] = value % 10;
                            }
                            0x55 => {
                                // Store V0-VX variables in memory
                                for step in 0..=inst.x {
                                    self.memory[self.i as usize + step] = self.v[step];
                                }
                                // Original Chip-8 incremented I, but modern don't update I
                                if self.quirks.load_store {
                                    self.i += inst.x as u16 + 1;
                                }
                            }
                            0x65 => {
                                // Loads from memory variables into V0-VX
                                for step in 0..=inst.x {
                                    self.v[step] = self.memory[self.i as usize + step];
                                }
                                // Original Chip-8 incremented I, but modern don't update I
                                if self.quirks.load_store {
                                    self.i += inst.x as u16 + 1;
                                }
                            }
                            _ => { println!("Unknown opcode: {:04X}", inst.instruction); }
                        }
                    }
                    _ => {
                        panic!("IMPOSSIBLE NIBBLE! {}", inst.instruction)
                    }
                }
                Ok(SUCCESSFUL_EXECUTION)        
    }

    pub fn cycle(&mut self) -> std::io::Result<u8> {
        // Fetch
        let instruction: Instruction = self.fetch();
        
        if self.debug {
            print!("Instruction: 0x{:04X}\t", instruction.instruction);
            self.debug_print();            
        }

        // Decode/Execute
        let result = self.execute(instruction);

        if self.debug {
            self.paused = true;
        }
        
        result
    }
}

pub fn update_texture(
    canvas: &mut Canvas<Window>,
    texture: &mut Texture,
    display: [[bool; DISPLAY_WIDTH]; DISPLAY_HEIGHT],
) {
    texture
        .with_lock(None, |buffer: &mut [u8], pitch: usize| {
            for y in 0..DISPLAY_HEIGHT {
                for x in 0..DISPLAY_WIDTH {
                    let offset = y * pitch + x * 4;
                    let color = if display[y][x] { 0xFF } else { 0x00 };

                    buffer[offset] = color;     // R
                    buffer[offset + 1] = color; // G
                    buffer[offset + 2] = color; // B
                    buffer[offset + 3] = 0xFF;  // A
                }
            }
        })
        .unwrap();

    canvas.clear();
    canvas.copy(texture, None, None).unwrap();
    canvas.present();
}

fn run_emulator(video: &sdl2::VideoSubsystem, audio: &sdl2::AudioSubsystem, event_pump: &mut sdl2::EventPump, chip8: &mut Chip8) -> Result<u8, Box<dyn std::error::Error>> {
    // Build canvas, texture, audio, load ROM into chip8, start game loop
    let window = video.window("CHIP-8", DISPLAY_WIDTH as u32 * SCALE, DISPLAY_HEIGHT as u32 * SCALE)
    .position_centered()
    .opengl()
    .build()
    .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();
    let texture_creator = canvas.texture_creator();

    let mut texture = texture_creator
    .create_texture_streaming(PixelFormatEnum::RGBA8888, DISPLAY_WIDTH as u32, DISPLAY_HEIGHT as u32)
    .unwrap();

    let desired_spec = AudioSpecDesired {
        freq: Some(44100),
        channels: Some(1),  // mono
        samples: Some(512),
    };
    
    let audio_device = audio.open_playback(None, &desired_spec, |spec| {
        SquareWave {
            phase_inc: 440.0 / spec.freq as f32, // 440 Hz tone
            phase: 0.0,
            volume: 0.25,
        }
    }).unwrap();
    
    audio_device.pause(); // Start paused

    let timer_interval = Duration::from_millis(16);
    let mut last_timer_tick = Instant::now();    

    let frame_duration = std::time::Duration::from_micros(00); // 2_000 is roughly 60hz? But slow for keypress 

    'running: loop {
        let frame_start = Instant::now();

        // Handle keyboard
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                Event::KeyDown { keycode: Some(key), .. } => {
                    match key {
                        // Step one instruction when in debug mode
                        Keycode::Space => {
                            if chip8.debug {
                                chip8.paused = false;
                            }
                        }
        
                        // Normal key mapping
                        _ => {
                            if let Some(index) = map_keycode(key) {
                                chip8.keypad[index] = true;
                            }
                        }
                    }
                }
                Event::KeyUp { keycode: Some(key), .. } => {
                    if let Some(index) = map_keycode(key) {
                        chip8.keypad[index] = false;
                    }
                }
                _ => {}
            }
        }

        // Timers
        if last_timer_tick.elapsed() >= timer_interval {
            if chip8.delay_timer > 0 {
                chip8.delay_timer -= 1;
            }
            if chip8.sound_timer > 0 {
                audio_device.resume();
                chip8.sound_timer -= 1;
            } else {
                audio_device.pause();
            }
            last_timer_tick = Instant::now();
        }

        // Run Cycle 
        if !chip8.debug || (chip8.debug &&!chip8.paused) {
            let result = chip8.cycle().unwrap();
            
            if result == EXIT_ROM {
                break 'running;
            }
        }

        // Update Display
        if chip8.draw_flag {
            chip8.draw_flag = false;

            update_texture(&mut canvas, &mut texture, chip8.display);
        }

        //  Frame rate limiting
        let elapsed = frame_start.elapsed();
        if elapsed < frame_duration {
            let delay = frame_duration - elapsed;
            std::thread::sleep(delay);
        }
    };

    Ok(chip8.v[1])  // Return the ID of the game selected (for when it is the selection menu)
}

fn load_chip8_memory(chip8: &mut Chip8, path: String, start_location: usize) -> (&mut Chip8, Vec<String>) {
    // Open file
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    // For title in file
    let mut i: u8 = 0;
    let mut offset: usize = 0;
    let mut files: Vec<String> = Vec::new();
    for line_result in reader.lines() {
        i += 1;
        let line = line_result.unwrap();
        let trimmed = line.trim();
        if i % 2 == 0 {
            files.push(trimmed.to_owned()); // Add file names to file vector
            continue;
        }
        
        let padded = format!("{:<11}", trimmed);
        
        // Add to chip8 memory
        for ch in padded.chars() {
            let ascii_value = ch as u8;
            chip8.memory[start_location + offset as usize] = ascii_value;
            offset += 1;
        }
    }

    (chip8, files)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let audio = sdl.audio().unwrap();

    let mut event_pump: sdl2::EventPump = sdl.event_pump().unwrap();

    let debug = false;

    // Choosing game to laod
    let quirks: Quirks = Quirks::new(Some(true), Some(false), Some(false), Some(true), Some(true));
    let mut chip8 = Chip8::new(quirks);

    chip8.load_rom("roms/menu-v1.ch8")?;
    chip8.debug = debug;
    let (mut chip8, files) = load_chip8_memory(&mut chip8, "data/roms.txt".to_string(), 0x500);
    let id = run_emulator(&video, &audio, &mut event_pump, &mut chip8).unwrap();

    // Getting games filename
    let filename = &files[id as usize];
    let filename = format!("roms/{}", filename);
    
    // Running chosen game
    let quirks: Quirks = Quirks::new(Some(true), Some(false), Some(false), Some(true), Some(true));
    let mut chip8 = Chip8::new(quirks);

    chip8.load_rom(&filename)?;
    chip8.debug = debug;
    run_emulator(&video, &audio, &mut event_pump, &mut chip8)?;

    Ok(())
}
