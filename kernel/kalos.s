%include "geometry.inc"

[bits 16]
[cpu 8086]
[org kernel_addr]

	jmp main

%include "kernel/fat.s"
%include "kernel/term.s"

main:
; Memory ----------------------------------------
	; Interrupt 0x12 returns in ax the number of available contiguous kilobytes of memory starting from 0x00000
	; The size of conventional memory is 640kB, but the last part of it (at most 128kB) is used by the Extended Bios Data Area
	int 0x12
	mov [k_available_memory], ax


	; This code is here for debugging purposes. It demonstrates that the search_file and load_file subroutine work properly.
	; To check it, inspect memory and check if the file is loaded at 0x7e00.
	; Load CONFIG.SYS
	mov bx, config_filename
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

	; Debug code to check that the terminal output subroutines work
	mov bl, 0x09
	call set_current_attr
	mov bx, welcome_msg
	call print_0_str
	mov bx, welcome_msg
	mov cx, 19
	call print_str
	mov bx, welcome_msg
	call print_0_str_with_attr
	mov bx, welcome_msg
	mov cx, 19
	call print_str_with_attr


	; Loop forever
debug_loop:
	hlt
	jmp debug_loop

; DATA
config_filename		db "CONFIG  SYS"
welcome_msg		db `\r\n`,"Welcome to KalOS!",0

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
