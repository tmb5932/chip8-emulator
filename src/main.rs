use minifb::{Window, WindowOptions};

const DISPLAY_WIDTH: usize = 64;
const DISPLAY_HEIGHT: usize = 32;

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
            super_chip_support: super_chip_support
        };

        for (i, byte) in FONTSET.iter().enumerate() {
            chip8.memory[0x50 + i] = *byte;
        }
        
        chip8
    }

    pub fn load_rom(&mut self, filename: &str) -> std::io::Result<()> {
        // Open the file and auto-return if it fails
        let data = std::fs::read(filename)?;
        
        // Copy each byte into memory starting at 0x200
        for (i, byte) in data.iter().enumerate() {
            self.memory[0x200 + i] = *byte;
        }

        Ok(())
    }

    pub fn cycle(&mut self) -> std::io::Result<()> {
        // Infinitely loop
        // Fetch
        let opcode: u16 = (self.memory[self.pc as usize] as u16) << 8 |  self.memory[(self.pc + 1) as usize] as u16;
        self.pc += 2;
        
        // Decode
        // X: The second nibble. Used to look up one of the 16 registers (VX) from V0 through VF.
        let X: usize = ((opcode & 0x0F00) >> 8) as usize;
        // Y: The third nibble. Also used to look up one of the 16 registers (VY) from V0 through VF.
        let Y: usize = ((opcode & 0x00F0) >> 4) as usize;
        // N: The fourth nibble. A 4-bit number.
        let N: u8 = (opcode & 0x000F) as u8;
        // NN: The second byte (third and fourth nibbles). An 8-bit immediate number.
        let NN: u8 = (opcode & 0x00FF) as u8;
        // NNN: The second, third and fourth nibbles. A 12-bit immediate memory address.
        let NNN: u16 = opcode & 0x0FFF;

        // Execute
        match opcode & 0xF000 {
            0x0000 => {
                match opcode {
                    0x00E0 => { self.display = [[false; DISPLAY_WIDTH]; DISPLAY_HEIGHT]; },
                    0x00EE => { 
                        // return from sub function
                        self.sp -= 1;
                        self.pc = self.stack[self.sp];
                    },
                    _ => { println!("Unknown opcode: {:04X}", opcode); }
                }
            }
            0x1000 => {
                // Jump: PC = NNN
                self.pc = NNN;
            }
            0x2000 => {
                // JAL: STACK[SP] = PC & PC = NNN
                self.stack[self.sp] = self.pc;
                self.sp += 1;
                self.pc = NNN;
            }
            0x3000 => {
                // Skip 1 instruction if VX == NN
                if self.v[X] == NN {
                    self.pc += 2;
                }
            }
            0x4000 => {
                // Skip 1 instruction if VX != NN
                if self.v[X] != NN {
                    self.pc += 2;
                }
            }
            0x5000 => {
                // Skip 1 instruction if VX == VY
                if self.v[X] == self.v[Y] {
                    self.pc += 2;
                }
            }
            0x6000 => {
                // SET: VX = NN
                self.v[X] = NN;
            }
            0x7000 => {
                // ADD: VX = VX + NN
                self.v[X] += NN;
            }
            0x8000 => {
                match opcode & 0x000F {
                    // BINARY OPS
                    0x0000 => {
                        // SET: VX = VY
                        self.v[X] = self.v[Y];
                    }
                    0x0001 => {
                        // OR: VX = VX OR VY
                        self.v[X] = self.v[X] | self.v[Y];
                    }
                    0x0002 => {
                        // AND: VX = VX AND VY
                        self.v[X] = self.v[X] & self.v[Y];
                    }
                    0x0003 => {
                        // XOR: VX = VX XOR VY
                        self.v[X] = self.v[X] ^ self.v[Y];
                    }
                    0x0004 => {
                        // ADD (with overflow): VX = VX + VY
                        let (sum, carry) = self.v[X].overflowing_add(self.v[Y]);
                        self.v[X] = sum;
                        self.v[0xF] = if carry { 1 } else { 0 };
                    }
                    0x0005 => {
                        // 8XY5 sets VX to the result of VX - VY.                        
                        let (result, borrowed) = self.v[X].overflowing_sub(self.v[Y]);
                        self.v[X] = result;
                        self.v[0xF] = if borrowed { 0 } else { 1 };
                    }
                    0x0006 => {
                        // YSHIFT:    8XY6 VX = VY >> 1
                        // No YSHIFT: 8XY6 VX = VX >> 1
                        let shift_src = if self.super_chip_support { X } else { Y };
                        self.v[0xF] = self.v[shift_src] >> 7;
                        self.v[X] = self.v[shift_src] >> 1;
                    }
                    0x0007 => {
                        // 8XY7 sets VX to the result of VY - VX.
                        let (result, borrowed) = self.v[Y].overflowing_sub(self.v[X]);
                        self.v[X] = result;
                        self.v[0xF] = if borrowed { 0 } else { 1 };
                    }
                    0x000E => {
                        // YSHIFT:    8XYE VX = VY << 1
                        // No YSHIFT: 8XYE VX = VX << 1
                        let shift_src = if self.super_chip_support { X } else { Y };
                        self.v[0xF] = self.v[shift_src] >> 7;
                        self.v[X] = self.v[shift_src] << 1;
                    }
                    _ => {
                        panic!("IMPOSSIBLE NIBBLE!");
                    }
                }
            }
            0x9000 => {
                // Skip 1 instruction if VX != VY
                if self.v[X] != self.v[Y] {
                    self.pc += 2;
                }
            }
            0xA000 => {
                // I = NNN
                self.i = NNN;
            }
            0xB000 => {
                // CHIP-8:     0xBNNN Jump to NNN + V0
                // SUPER-CHIP: 0xBXNN Jump to XNN + VX
                let v_src = if self.super_chip_support { X } else { 0 };
                self.pc = NNN + self.v[v_src] as u16;
            }
            0xC000 => {
                match opcode {
                    _ => { println!("Unknown opcode: {:04X}", opcode); }
                }
            }
            0xD000 => {
                // Alter Display
                let x_coord = self.v[X] as usize % DISPLAY_WIDTH;
                let y_coord = self.v[Y] as usize % DISPLAY_HEIGHT;
                self.v[0xF] = 0; // Reset collision flag
                for index in 0..N as usize {
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
            0xE000 => {
                match opcode {
                    _ => { println!("Unknown opcode: {:04X}", opcode); }
                }
            }
            0xF000 => {
                match opcode {
                    _ => { println!("Unknown opcode: {:04X}", opcode); }
                }
            }
            _ => {
                panic!("IMPOSSIBLE NIBBLE!");
            }
        } 
        Ok(())
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
    chip8.load_rom("roms/ibm-logo.ch8")?;
    
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