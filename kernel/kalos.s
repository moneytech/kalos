%include "geometry.inc"

[bits 16]
[cpu 8086]
[org kernel_addr]

	jmp main

%include "kernel/fat.s"

main:
; Memory ----------------------------------------
	; Interrupt 0x12 returns in ax the number of available contiguous kilobytes of memory starting from 0x00000
	; The size of conventional memory is 640kB, but the last part of it (at most 128kB) is used by the Extended Bios Data Area
	int 0x12
	mov [k_available_memory], ax


	; This is some debug code that shows various ways to print character.
	; The user can use both BIOS and OS interrupts.
	; The important thing is being careful with the cursor.
	; Of course, if only os interrupts are used, the programmer only needs to update the hardware cursor when he is done writing his string.
	mov ah, 0x00
	mov al, 'K'
	int 0x61	; int 61,0: os, print_char

	; We need to update the hardware cursor because the next interrupt we use is a bios one
	mov ah, 0x03
	int 0x61	; int 61,3: os, update_hw_cursor.

	mov ah, 0x0e
	mov al, 'E'
	int 0x10	; int 10,e: bios, teletype output

	mov ah, 0x0e
	mov al, 'R'
	int 0x10	; int 10,e: bios, teletype output

	; Now we update the os cursor because the next interrupt is a os one
	mov ah, 0x05	; int 61,5: update_os_cursor
	int 0x61

	mov ah, 0x00
	mov al, 'N'
	int 0x61	; int 61,0: os, print_char

	mov ah, 0x01
	mov al, 'E'
	mov bl, 0x07	; Attribute: light grey on black
	int 0x61	; int 61,1: os, print_char_with_attr

	mov ah, 0x00
	mov al, 'L'
	int 0x61	; int 61,0: os, print_char

	mov ah, 0x00
	mov al, `\r`
	int 0x61	; int 61,0: os, print_char

	mov ah, 0x00
	mov al, `\n`
	int 0x61	; int 61,0: os, print_char

	mov ah, 0x03
	int 0x61	; int 61,3: os, update_hw_cursor



	; This code is here for debugging purposes. It demonstrates that the search_file and load_file subroutine work properly.
	; To check it, inspect memory and check if the file is loaded at 0x7e00.
	; Load CONFIG.SYS
	mov di, config_filename
	call search_file
	test cl, cl
	jnz config_not_found
config_found:
	mov bx, 0x7e00
	call load_file
	mov al, 'Y'
	jmp go_on
config_not_found:
	mov al, 'N'
go_on:
	mov ah, 0x00
	int 0x61	; int 61,0: print_char
	mov ah, 0x03
	int 0x61	; int 61,3: os, update_hw_cursor.



	; Loop forever
debug_loop:
	hlt
	jmp debug_loop

; DATA
config_filename		db "CONFIG  SYS"

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
