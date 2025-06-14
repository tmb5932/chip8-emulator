use minifb::{Window, WindowOptions};
use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha8Rng;

const DISPLAY_WIDTH: usize = 64;
const DISPLAY_HEIGHT: usize = 32;
const FONTSET_START: usize = 0x50;
const ROM_START: usize = 0x200;

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
    rng: ChaCha8Rng,
    super_chip_support: bool
}

impl Chip8 {
    pub fn new(super_chip_support: bool) -> Self {
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
            rng: ChaCha8Rng::from_seed(Default::default()),
            super_chip_support: super_chip_support
        };

        for (i, byte) in FONTSET.iter().enumerate() {
            chip8.memory[FONTSET_START + i] = *byte;
        }

        chip8
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

    pub fn execute(&mut self, inst: Instruction) -> std::io::Result<()> {
                // Execute
                match inst.nibble {
                    0x0 => {
                        match inst.nn {
                            0xE0 => { self.display = [[false; DISPLAY_WIDTH]; DISPLAY_HEIGHT]; },
                            0xEE => { 
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
                            _ => { panic!("IMPOSSIBLE NIBBLE!") }
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
                            }
                            0x2 => {
                                // AND: VX = VX AND VY
                                self.v[inst.x] = self.v[inst.x] & self.v[inst.y];
                            }
                            0x3 => {
                                // XOR: VX = VX XOR VY
                                self.v[inst.x] = self.v[inst.x] ^ self.v[inst.y];
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
                                // YSHIFT:    8XY6 VX = VY >> 1
                                // No YSHIFT: 8XY6 VX = VX >> 1
                                let shift_src = if self.super_chip_support { inst.x } else { inst.y };
                                self.v[0xF] = self.v[shift_src] >> 7;
                                self.v[inst.x] = self.v[shift_src] >> 1;
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
                                let shift_src = if self.super_chip_support { inst.x } else { inst.y };
                                self.v[0xF] = self.v[shift_src] >> 7;
                                self.v[inst.x] = self.v[shift_src] << 1;
                            }
                            _ => {
                                panic!("IMPOSSIBLE NIBBLE!");
                            }
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
                            _ => { panic!("IMPOSSIBLE NIBBLE!") }
                        }
                    }
                    0xA => {
                        // I = NNN
                        self.i = inst.nnn;
                    }
                    0xB => {
                        // CHIP-8:     0xBNNN Jump to NNN + V0
                        // SUPER-CHIP: 0xBXNN Jump to XNN + VX
                        let v_src = if self.super_chip_support { inst.x } else { 0 };
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
                            
                            for bit_index in 0..8 {
                                let pixel_x = (x_coord + bit_index) % 64;
                                let pixel_y = (y_coord + index) % 32;
                        
                                let sprite_pixel_on = (sprite_byte >> (7 - bit_index)) & 1 == 1;
                                let current_pixel = self.display[pixel_y][pixel_x];
                        
                                if sprite_pixel_on {
                                    if current_pixel {
                                        self.v[0xF] = 1; // Collision
                                    }
                        
                                    self.display[pixel_y][pixel_x] ^= true;
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
                            _ => { println!("Unknown opcode: {:04X}", inst.instruction); }
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
                                // Wait til any button is pressed
                                let mut key_pressed: bool = false;
                                for (i, &pressed) in self.keypad.iter().enumerate() {
                                    if pressed {
                                        self.v[inst.x] = i as u8;
                                        key_pressed = true;
                                        break;
                                    }
                                }
                                if !key_pressed {
                                    // Don't advance to next instruction
                                    self.pc -= 2;
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
                                // if !self.super_chip_support {
                                //     self.i += inst.x as u16 + 1;
                                // }
                            }
                            0x65 => {
                                // Loads from memory variables into V0-VX
                                for step in 0..=inst.x {
                                    self.v[step] = self.memory[self.i as usize + step];
                                }
                                // Original Chip-8 incremented I, but modern don't update I
                                // if !self.super_chip_support {
                                //     self.i += inst.x as u16 + 1;
                                // }
                            }
                            _ => { println!("Unknown opcode: {:04X}", inst.instruction); }
                        }
                    }
                    _ => {
                        panic!("IMPOSSIBLE NIBBLE!");
                    }
                }
                Ok(())        
    }

    pub fn cycle(&mut self) -> std::io::Result<()> {
        // Fetch
        let instruction: Instruction = self.fetch();
        
        // Decode/Execute
        self.execute(instruction)
    }

    pub fn run_display(&self, window: &mut Window, buffer: &mut [u32; 64 * 32]) 
    -> Result<(), Box<dyn std::error::Error>> {    
        
        for y in 0..DISPLAY_HEIGHT {
            for x in 0..DISPLAY_WIDTH {
                let i: usize = y * DISPLAY_WIDTH + x;
                buffer[i] = if self.display[y][x] {
                    0xFFFFFF // white
                } else {
                    0x000000 // black
                };
            }
        }
    
        window.update_with_buffer(buffer, DISPLAY_WIDTH, DISPLAY_HEIGHT)?;
        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut chip8 = Chip8::new(false);
    
    chip8.load_rom("roms/3-test-opcode.ch8")?;
    
    let mut window = Window::new("CHIP-8", DISPLAY_WIDTH, DISPLAY_HEIGHT, 
    WindowOptions {
        scale: minifb::Scale::X16, // Scale the window by 16
        ..WindowOptions::default()
    })?;

    let mut buffer = [0u32; 64 * 32];
    while window.is_open() && !window.is_key_down(minifb::Key::Escape) {
        chip8.cycle()?; // run one instruction

        if chip8.draw_flag {
            chip8.run_display(&mut window, &mut buffer)?;
            chip8.draw_flag = false;
        }

        // Optional: sleep to control speed
        std::thread::sleep(std::time::Duration::from_millis(2));
    }

    Ok(())
}