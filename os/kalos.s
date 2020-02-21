%include "geometry.inc"

[bits 16]
[cpu 8086]
[org kernel_addr]

	mov [k_io_table_addr], bx	; io.sys will tell us where the io table is using bx
; Memory ----------------------------------------
	; Interrupt 0x12 returns in ax the number of available contiguous kilobytes of memory starting from 0x00000
	; The size of conventional memory is 640kB, but the last part of it (at most 128kB) is used by the Extended Bios Data Area
	int 0x12
	mov [k_available_memory], ax

mov ah, 0x0e
mov al, 'K'
int 0x10

mov ah, 0x0e
mov al, 'E'
int 0x10

mov ah, 0x0e
mov al, 'R'
int 0x10

hlt

kernel_table:

; Memory ----------------------------------------
; All addresses are offset of segment 0x0000
k_kernel_table_addr	dw kernel_table
k_io_table_addr		dw 0x0000
k_io_addr		dw io_addr
k_kernel_addr		dw kernel_addr
k_fat_addr		dw fat_addr
k_root_addr		dw root_addr
k_bpb_addr		dw bpb_addr
k_available_memory	dw 0x0000
