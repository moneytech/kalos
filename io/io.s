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

	; Install new interrupt handler
	xor bx, bx				; The code segment is 0x0000
	mov word [4 * 0x61], io_interrupt	; Offset
	mov word [4 * 0x61 + 2], bx		; Segment

	; Enable hardware interrupt handling by the CPU.
	sti

	; Jump to the kernel
	jmp kernel_addr
	hlt


; SOFTWARE INTERRUPT HANDLER
; io_interrupt subroutine begin
; This subroutine calls the io functions indexed by ah.
; There is no rule about preserved registers: each io functions documents which ones are changed.
io_interrupt:
	; Set data segment to zero
	push ax
	xor ax, ax
	mov ds, ax
	pop ax

	; Set di to the address of the subroutine to call
	push bx
	xor bh, bh
	mov bl, ah
	shl bx, 1	; Multiply by 2 (each table entry is two bytes long)
	add bx, io_interrupt_table
	mov di, [bx]
	pop bx

	call di
	iret

; io_interrupt_installed subroutine begin
; This is a service subroutine. It returns some fixed values. It may be used to check if the handler is installed.
io_interrupt_installed:
	mov ax, 0xff
	mov bx, 0xab
	mov cx, 0xcd
	mov dx, 0xef
	ret
; io_interrupt_installed subroutine end

; io_interrupt_invalid subroutine begin
; This is a service subroutine. It returns some fixed values. It is executed when ah is not valid
io_interrupt_invalid:
	mov ax, 0xff
	mov bx, 0xff
	mov cx, 0xff
	mov dx, 0xff
	ret
; io_interrupt_invalid subroutine end

; Interrupt jump table
io_interrupt_table:
; VGA subroutines
.0x00	dw	print_char
.0x01	dw	print_char_with_attr
.0x02	dw	scroll
.0x03	dw	update_hw_cursor
.0x04	dw	set_hw_cursor
.0x05	dw	update_os_cursor
.0x06	dw	set_os_cursor
.0x07	dw	set_vga_register_compatibility_mode
times 0x0f-0x07	dw	io_interrupt_invalid
; Floppy subroutines
.0x10	dw	read_sector
times 0xfe-0x10	dw	io_interrupt_invalid
.0xff	dw	io_interrupt_installed



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
	mov ah, 0x00		; int 61,0: print_char
	int 0x61
	pop ax			; Restore ascii number
	mov ah, 0x00		; int 61,0: print_char
	int 0x61
	ret


; If this becomes negative, nasm will not assemble: we need to increase the constant io_sys_length by one sector.
; This is pretty ugly, but I think it is better to have bpb, io.sys and kernel in the same segment.
times 512*3 - ($ - $$)	db 0x00
