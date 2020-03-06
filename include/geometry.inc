bytes_per_sector		equ	0x0200
sectors_per_cluster		equ	0x01
reserved_sectors		equ	0x0001
number_of_fats			equ	0x02
root_entries			equ	0x00E0
total_sectors			equ	0x0B40
media_descriptor		equ	0xF0
sectors_per_fat			equ	0x0009
sectors_per_track		equ	0x0012
head_count			equ	0x0002
hidden_sectors			equ	0x00000000
large_sector_count		equ	0x00000000
drive_number			equ	0x00
reserved			equ	0x00
signature			equ	0x29

start_of_fat			equ	reserved_sectors + hidden_sectors
start_of_root			equ	start_of_fat + number_of_fats * sectors_per_fat

fat_size_in_bytes		equ	sectors_per_fat * bytes_per_sector
root_size_in_sectors		equ	root_entries * 32 / bytes_per_sector
root_size_in_bytes		equ	root_entries * 32
io_sys_size_in_bytes		equ	1536
kernel_sys_size_in_bytes	equ	512

bootloader_addr			equ	0x7c00
bpb_addr			equ	bootloader_addr + 3
os_base_addr			equ	0x500
fat_addr			equ	os_base_addr
root_addr			equ	fat_addr + fat_size_in_bytes
io_addr				equ	root_addr + root_size_in_bytes
kernel_addr			equ	io_addr + io_sys_size_in_bytes
