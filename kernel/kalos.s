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
	call load_file_synchronous
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
	mov ah, 0x21
	int 0x61	; int 61,21: get_raw_scancode_buffered
	cmp cl, 0xaa	; 0xaa means that the keyboard buffer is empty
	je debug_loop	; If it is, we don't print anything
	mov al, cl
	call print_hex
	mov ah, 0x00
	mov al, ' '
	int 0x61
	mov ah, 0x00
	mov al, ' '
	int 0x61
	mov ah, 0x03
	int 0x61	; int 61,3: update_hw_cursor
	jmp debug_loop

; SUBROUTINES
; These subroutines are needed for debugging purposes and will be removed

; hex_to_ascii subroutine begin
; Input: al = hex number to convert
; Output: ax = two ascii characters (ah high digit, al low digit)
; Modified registers: ax
hex_to_ascii:
	mov ah, al
	and ah, 11110000b	; High digit
	; Shift right four times to have the high digit in the 4 low bits of ah
	shr ah, 1
	shr ah, 1
	shr ah, 1
	shr ah, 1
	cmp ah, 0x9
	jle .high_less_than_nine
.high_more_than_nine:
	add ah, 'a'-10		; Convert to ascii digit (a-f)
	jmp .low
.high_less_than_nine:
	add ah, '0'		; Convert to ascii digit (0-9)
.low:
	and al, 00001111b	; Low digit
	cmp al, 0x9
	jle .low_less_than_nine
.low_more_than_nine:
	add al, 'a'-10		; Convert to ascii digit (a-f)
	jmp .end
.low_less_than_nine:
	add al, '0'		; Convert to ascii digit (0-9)
.end:
	ret
; hex_to_ascii subroutine end

; print_hex subroutine begin
; Input: al = number to print
; Modified registers: ax + those modified by print_char
print_hex:
	call hex_to_ascii
	push ax			; Save ascii number
	mov al, ah		; Move high digit to al
	mov ah, 0x00		; int 61,0: print_char
	int 0x61
	pop ax			; Restore ascii number
	mov ah, 0x00		; int 61,0: print_char
	int 0x61
	ret
; print_hex subroutine end



; DATA
config_filename		db "CONFIG  SYS"
welcome_msg		db "Welcome to KalOS!",`\r\n`,0

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
