use chrono::{DateTime, NaiveDateTime, Utc};
use memmap::{Mmap, MmapOptions};
use std::env::args;
use std::fmt;
use std::fs::File;
use std::process::exit;

fn secs_to_timestr(secs: u32) -> String {
    // Create a NaiveDateTime from the timestamp
    let naive = NaiveDateTime::from_timestamp(secs as i64, 0);
    // Create a normal DateTime from the NaiveDateTime
    let datetime: DateTime<Utc> = DateTime::from_utc(naive, Utc);
    // Format the datetime how you want
    datetime.format("%m/%d/%y %H:%M:%S").to_string()
}

fn map_to_file(filename: &str) -> Result<Mmap, &'static str> {
    let file = match File::open(filename) {
        Err(_) => {
            return Err("opening file");
        }
        Ok(f) => f,
    };
    return unsafe {
        match MmapOptions::new().map(&file) {
            Err(_) => {
                return Err("mmap error in map_to_file");
            }
            Ok(f) => Ok(f),
        }
    };
}

/* SUPERBLOCK offset
total number of blocks (decimal) s_blocks_count, 4, 4 bytes
total number of i-nodes (decimal) s_inodes_count, 0, 4 bytes
block size (in bytes, decimal) 1024 << s_log_block_size; 24, 4 bytes
i-node size (in bytes, decimal) s_inode_size 88, 2 bytes
blocks per group (decimal) s_blocks_per_group 32, 4 bytes
i-nodes per group (decimal) s_inodes_per_group 40, 4 bytes
first non-reserved i-node (decimal) s_first_ino 84, 4 bytes

s_block_group_nr 90, 2 bytes
*/

struct Superblock {
    s_blocks_count: u32,
    s_inodes_count: u32,
    s_log_block_size: u32,
    s_inode_size: u16,
    s_blocks_per_group: u32,
    s_inodes_per_group: u32,
    s_first_ino: u32,
}

struct Group {
    total_num_blocks: u32,
    total_num_inodes: u32,
    free_blocks_count: u16,
    free_inodes_count: u16,
    bg_block_bitmap: u32,
    bg_inode_bitmap: u32,
    bg_inode_table: u32,
}

struct Inode {
    inode_number: u32,
    file_type: char,
    mode: u16,
    owner: u16,
    group: u16,
    link_count: u16,
    last_change: String,
    mod_time: String,
    last_access: String,
    file_size: u32,
    num_blocks: u32,
    i_block: String,
    i_blocks: Vec<u32>
}

struct Dirent {
    parent_num: u32,
    logical_offset: u32,
    inode_num: u32,
    entry_len: u16,
    name_len: u8,
    name: String,
}

impl Inode {
    fn new(inode_num: u32, block: &[u8]) -> Option<Inode> {
        let i_mode = two_to_u16(&block[0..2]).unwrap();
	let n = if i_mode & 0xA000 == 0xA000 {
            's'
        } else if i_mode & 0x8000 == 0x8000 {
            'f'
        } else if i_mode & 0x4000 == 0x4000 {
            'd'
        } else {
            '?'
        };
        let file_size = four_to_u32(&block[4..8]).unwrap();
        let mut blks = String::new();
	let mut i_blocks = Vec::<u32>::new();
	if (n == 'f') || (n == 'd') || ((n == 's') && (file_size > 60)) {
            let ibytes = &block[40..100];
            blks.push_str(&String::from(","));
            for i in (0..60).step_by(4) {
		let a_u32 = four_to_u32(&ibytes[i..(i+4)]).unwrap();
                if a_u32 as usize > 1000 {
                    break;
                }
                blks.push_str(&a_u32.to_string());
		i_blocks.push(a_u32);
                blks.push_str(&String::from(","));
            }
            if blks.len() > 1 {
                blks = String::from(&blks[1..(blks.len() - 1)]);
            }
        }
	if n == 's' {
		blks = String::from("");
	}
	if (blks.len() == 0) && (n != 's') {
		None
	} else {
            Some(Inode {
            	inode_number: inode_num,
            	file_type: n,
            	mode: i_mode & 0xFFF,
            	owner: two_to_u16(&block[2..4]).unwrap(),
            	group: two_to_u16(&block[24..26]).unwrap(),
            	link_count: two_to_u16(&block[26..28]).unwrap(),
            	last_change: secs_to_timestr(four_to_u32(&block[16..20]).unwrap()),
            	mod_time: secs_to_timestr(four_to_u32(&block[16..20]).unwrap()),
            	last_access: secs_to_timestr(four_to_u32(&block[8..12]).unwrap()),
            	file_size: file_size,
            	num_blocks: four_to_u32(&block[28..32]).unwrap(),
            	i_block: blks,
	    	i_blocks: i_blocks,
		})
    	}
   }
}

impl fmt::Display for Inode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "INODE,{},{},{:o},{},{},{},{},{},{},{},{}{}",
            self.inode_number,
            self.file_type,
            self.mode,
            self.owner,
            self.group,
            self.link_count,
            self.last_change,
            self.mod_time,
            self.last_access,
            self.file_size,
            self.num_blocks,
            self.i_block
        )
    }
}

impl Superblock {
    fn new(block: &[u8]) -> Superblock {
        Superblock {
            s_blocks_count: four_to_u32(&block[4..8]).unwrap(),
            s_inodes_count: four_to_u32(&block[0..4]).unwrap(),
            s_log_block_size: 1024 << four_to_u32(&block[24..28]).unwrap(),
            s_inode_size: two_to_u16(&block[88..90]).unwrap(),
            s_blocks_per_group: four_to_u32(&block[32..36]).unwrap(),
            s_inodes_per_group: four_to_u32(&block[40..44]).unwrap(),
            s_first_ino: four_to_u32(&block[84..88]).unwrap(),
        }
    }
}

impl fmt::Display for Superblock {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "SUPERBLOCK,{},{},{},{},{},{},{}",
            self.s_blocks_count,
            self.s_inodes_count,
            self.s_log_block_size,
            self.s_inode_size,
            self.s_blocks_per_group,
            self.s_inodes_per_group,
            self.s_first_ino
        )
    }
}

impl Group {
    fn new(block_count: u32, inode_count: u32, block: &[u8]) -> Option<Group> {
        if block.len() < 16 {
            None
        } else {
            Some(Group {
                total_num_blocks: block_count,
                total_num_inodes: inode_count,
                free_blocks_count: two_to_u16(&block[12..14]).unwrap(),
                free_inodes_count: two_to_u16(&block[14..16]).unwrap(),
                bg_block_bitmap: four_to_u32(&block[0..4]).unwrap(),
                bg_inode_bitmap: four_to_u32(&block[4..8]).unwrap(),
                bg_inode_table: four_to_u32(&block[8..12]).unwrap(),
            })
        }
    }
}

impl fmt::Display for Group {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "GROUP,0,{},{},{},{},{},{},{}",
            self.total_num_blocks,
            self.total_num_inodes,
            self.free_blocks_count,
            self.free_inodes_count,
            self.bg_block_bitmap,
            self.bg_inode_bitmap,
            self.bg_inode_table
        )
    }
}

impl Dirent {
    fn new(parent_num: u32, logical_offset: u32, block: &[u8]) -> Dirent {
        let name_len = block[6];
        let name: String = block[8..(8 + (name_len as usize))]
            .to_vec()
            .into_iter()
            .map(|n| char::from(n))
            .collect();
        Dirent {
            parent_num: parent_num,
            logical_offset: logical_offset,
            inode_num: four_to_u32(&block[0..4]).unwrap(),
            entry_len: two_to_u16(&block[4..6]).unwrap(),
            name_len: name_len,
            name: name,
        }
    }
}

impl fmt::Display for Dirent {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(
            fmt,
            "DIRENT,{},{},{},{},{},'{}'",
            self.parent_num,
            self.logical_offset,
            self.inode_num,
            self.entry_len,
            self.name_len,
            self.name,
        )
    }
}

fn print_dentry(inode_num: u32, block_num: u32, data: &[u8]) {
	let len_offset = 4;
	let start = 1024*block_num;
	let mut ptr = start;
	while ptr <  1024*(block_num+1) {

	}
}

fn four_to_u32(byte_slice: &[u8]) -> Option<u32> {
    if byte_slice.len() != 4 {
        return None;
    }
    let array = <&[u8; 4]>::try_from(byte_slice).unwrap();
    Some(u32::from_le_bytes(*array))
}

fn two_to_u16(byte_slice: &[u8]) -> Option<u16> {
    if byte_slice.len() != 2 {
        return None;
    }
    let array = <&[u8; 2]>::try_from(byte_slice).unwrap();
    Some(u16::from_le_bytes(*array))
}

fn get_bit_at(input: u8, n: u8) -> bool {
    if n < 8 {
        input & (1 << n) != 0
    } else {
        false
    }
}

fn print_dirent(inode_num: u32, memmapd: &Mmap) {
    let len_offset = 4; // offset into each dentry to get length of entire dentry
    for iter in 1..65 {
    	    let mut start = 8192 + (iter - 1) * 1024;
	    let next = 8192 + iter * 1024;
	    while start + len_offset + 2 < memmapd.len() {
		let len = two_to_u16(
		    memmapd
			.get((start + len_offset)..(start + len_offset + 2))
			.unwrap(),
		)
		.unwrap() as usize;
		if len == 0 {
		    println!("{} {:#?}", start, memmapd.get(start..start + 10).unwrap_or(&[34, 59]));
		    return;
		}
		// if the next inode is zero done
		//let x =  *memmapd.get(start + len).unwrap();
		/*if start - 1024 * (iter) >= 8192 {
		    return;
		}*/
		if(start+len >  memmapd.len()) || start > next {
			return;
		}
		let v = memmapd.get(start..(start + len)).unwrap().to_vec();
		let d = Dirent::new(inode_num, (start - 8192 - (iter - 1) * 1024) as u32, &v);
		if (d.inode_num != 0) && (d.logical_offset < 1024) { //inode_num {
		    println!("{}", d);
		}
		start += len;
		//println!("start {} next {}", start, next);
		if start > next {
			return;
		}
	    }
	}
}

fn extract<'a>(input_name: &'a str) -> Result<(), &'static str> {
    let memmapd = match map_to_file(input_name) {
        Err(s) => {
            return Err(s);
        }
        Ok(f) => f,
    };
    // superblock at offset 1024 from the beginning of the file
    // does .get() get raw bytes? need to convert them to u32
    let mut block_size: usize = 0;
    let mut blocks_per_group = 0;
    let mut inodes_per_group = 0;
    // parsing superblock
    let block = Superblock::new(&memmapd.get(1024..(1024 + 4 + 88)).unwrap());
    println!("{}", block);
    block_size = block.s_log_block_size as usize;
    blocks_per_group = block.s_blocks_count;
    inodes_per_group = block.s_inodes_count;
    // assumes group number is 1
    let group = Group::new(blocks_per_group, inodes_per_group, 
		 &memmapd
			.get((1024 + block_size)..(1024 + 2 * block_size))
			.unwrap());
    match group {
                Some(g) => {
                    println!("{}", g);
                }
                None => {
                    eprintln!("Unable to parse block descriptor table");
                }
     }
     let block_bitmap = memmapd 
			.get((2048 + block_size)..(2048 + block_size * 2))
			.unwrap();
	
    let mut index = 1;
    for x in 0..block_size {
	for i in 0..8 {
                if !get_bit_at(block_bitmap[x], i) {
                    println!("BFREE,{}", index);
                }
                index += 1;
        }
     }
     index = 1;
     let inode_bitmap = memmapd
			 .get((2048 + block_size * 2)..(2048 + block_size * 3))
			.unwrap();
     for x in 0..block_size {
	for i in 0..8 {
                if !get_bit_at(inode_bitmap[x], i) {
                    println!("IFREE,{}", index);
                }
                index += 1;
        }
     }
    index = 1;
    /*let slice = memmapd
			.get((2048 + block_size*3)..(2048 + block_size*(3+28)))
			.unwrap();*/
    let mut inode_num = 0;
    for n in ((2048 + block_size * 3)..(2048 + block_size*(3+28)))
	.step_by(128) {
		if(n % 128 == 0 && inode_num <= inodes_per_group) {
			let inode_table = &memmapd
						.get(n..(n + 128))
						.unwrap();
			inode_num += 1;
			let inode = Inode::new(inode_num, inode_table);
			match inode {
				Some(ino) => {
					println!("{}", ino);
					if ino.file_type == 'd' {
						for k in 0..12 {
						     if ino.i_blocks.len() > 0 {
						     }
						 }
					} else if ino.file_type == 'f' {
						let base = [0, 256, 256*256+256];
						for k in 12..15 {
							if ino.i_blocks.len() > 0 {
							}
						}
					}
				},
				None => {},
			}
	    }
    }
    Ok(())
}

fn main() {
    let filename = match args().nth(1) {
        None => {
            eprintln!("Usage: ./lab3b [filesystem image]");
            exit(1);
        }
        Some(k) => k,
    };
    match extract(&filename) {
        Err(s) => {
            eprintln!("{}", s);
            exit(2);
        }
        _ => {}
    };
    exit(0);
}
