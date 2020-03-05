%include "include/geometry.inc"

[bits 16]
[cpu 8086]
[org io_addr]	; We are in segment 0, so we have to shift our labels by io_addr (the address where we are loaded)

	jmp main

%include "io/vga.s"
%include "io/pic.s"
%include "io/keyb.s"
%include "io/floppy.s"

; MAIN SECTION ----------------------------------------------------------------
main:
	call init_vga
	call init_pic
	call init_keyb
	call init_floppy

	; Enable hardware interrupt handling by the CPU.
	sti

	; Jump to the kernel
	jmp kernel_addr
	hlt


; SUBROUTINES -----------------------------------------------------------------
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

; write_hex subroutine begin
; Input: al = number to print
; Modified registers: ax + those modified by print_char
print_hex:
	call hex_to_ascii
	push ax			; Save ascii number
	mov al, ah		; Move high digit to al
	call print_char	; Print high digit
	pop ax			; Restore ascii number
	call print_char	; Print low digit
	ret


; If this becomes negative, nasm will not assemble: we need to increase the constant io_sys_length by one sector.
; This is pretty ugly, but I think it is better to have bpb, io.sys and kernel in the same segment.
times 1024 - ($ - $$)	db 0x00
