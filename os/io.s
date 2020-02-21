%include "geometry.s"

[bits 16]
[cpu 8086]
[org io_addr]	; We are in segment 0, so we have to shift our labels by io_addr (the address where we are loaded)

; MAIN SECTION ----------------------------------------------------------------


; Video -----------------------------------------
; TODO: Check video mode (it should be 0x07 by default, but I have to do more reaserch to understand if it is always true)
; TODO: cols and lines shouldn't be constants, since they depend on video mode. Perhaps save them in the kernel table?
; TODO: Understand if the VGA hardware cursor is the same used by the teletype int 10,e
	mov ax, 0xb800
	mov es, ax	; Set the extra segment to the VGA video memory for color output
cols	equ	80
lines	equ	25

	; We need to access the CRT controller registers to interact with the hardware cursor on screen.
	; To do so, we first tell the VGA that we wish to use CGA-compatible port numbers for the CRT registers.
	mov cl, 0xd0	; 0xd0 is for CGA-compatible mode
	call 0x0000:set_vga_register_compatibility_mode

	; We can now get the current cursor location from the CRT controller.
	call 0x0000:update_os_cursor

; PIC -------------------------------------------------------------------------
; This code initializes our two PICs (the primary and the slave).
	; ICW 1: the first initialization control word to send to the PIC
	; Bits 7-5: must be 0
	; Bit 4: initialization bit, 1 since we are initializing the PIC
	; Bit 3: 0 is level triggered mode, 1 is edge triggered mode. We choose 1 (the default)
	; Bit 2: ignored by x86, so 0
	; Bit 1: 0 because there are multiple PICs in the system
	; Bit 0: 1 so we can send ICW4
	mov al, 00010001b
	out 0x20, al	; Primary PIC command register
	out 0xa0, al	; Slave PIC command register

	; ICW2: second initialization word, used to tell the PIC which interrupts it should trigger
	mov al, 0x22	; Primary PIC maps IRQ 0 to 7 to interrupt 0x22 to 0x29
	out 0x21, al	; Primary PIC data register
	mov al, 0x52	; Slave PIC maps IRQ 8 to 15 to interrupt 0x52 to 0x59
	out 0xa1, al	; Primary PIC data register

	; ICW3: tell the PICs which IRQ line tu use to comunicate between each other
	; The format for primary and slave is different!
	mov al, 00000100b	; Primary PIC: bit 2 is set, so use IRQ2
	out 0x21, al		; Primary PIC data register
	mov al, 0x2		; Slave PIC: the byte is 0x2, so use IRQ2
	out 0xa1, al		; Slave PIC data register

	; ICW4: extra information
	; Bits 5-7: reserved, must be 0
	; Bit 4: not supported on x86 (special fully nested mode)
	; Bit 3: buffered mode. We are not interested, so 0
	; Bit 2: only use if bit 3 is set, so 0
	; Bit 1: we also don't need this (automatic EOI on acknowledge pulse), so 0
	; Bit 0: if 1, 80x86 mode (what we want). If clear, MCS-80/86 mode
	mov al, 00000001b
	out 0x21, al		; Primary PIC data register
	out 0xa1, al		; Slave PIC data regiter

	; Null out the data register
	xor al, al
	out 0x21, al
	out 0xa1, al

; Input -----------------------------------------
; TODO: implement input

; KERNEL CALL------------------------------------------------------------------

	; This code is here to test that everything works and will eventually be removed
	mov cx, 1119
testing:
	push cx
	mov al, 'I'
	call 0x0000:print_char
	pop cx
	loop testing
	call 0x0000:update_hw_cursor

	mov al, 'I'
	call 0x0000:print_char
	call 0x0000:update_hw_cursor
	mov al, 'I'
	call 0x0000:print_char
	call 0x0000:update_hw_cursor

	; Jump to the kernel
	mov ax, kernel_addr
	mov bx, io_table
	hlt
	jmp ax
	hlt


; SUBROUTINES FOR KERNEL ------------------------------------------------------
; In this section, there are the subroutines which will be available to the kernel and the programs trough the kernel table.
; There are subroutines with multiple entry points (like print_char, print_char_with_attr and scroll), so be careful.
; TODO: Choose if system calls are to be provided by interrupts or by giving addresses to functions
; TODO: Properly comment this mess

; print_char, print_char_with_attr, scroll subroutines
; TODO: Implement CR and LF characters

; print_char subroutine begin
; Print a character at the current os cursor, without advancing it.
; Input:	al = character to print
print_char:
	mov ah, 0x07	; Default character attribute: light grey on black background

; print_char_with_attr subroutine begin
; Like print_char, but the caller provides the character attribute.
; Input:	al = character to print, ah = character attribute
print_char_with_attr:
	push ax
	; bx = offset = 2 * (io_col + io_line * cols)
	mov al, [io_line]
	mov bl, cols
	mul byte bl		; ax = al * bl = io_line * cols
	xor bx, bx
	mov bl, [io_col]
	add bx, ax		; bx += ax = io_col + io_line * cols
	shl bx, 1 		; Multiply bx by 2, because each character occupies two bytes
	pop ax
	mov [es:bx], ax		; Move the byte to video memory

	mov ah, [io_col]
	inc ah
	mov [io_col], ah		; Next column
	mov al, cols
	cmp ah, al
	jl return		; End of line not reached

	; End of line reached
	xor ah, ah
	mov [io_col], ah		; Set column to 0
	mov ah, [io_line]
	inc ah
	mov [io_line], ah	; Next line
	mov al, lines
	cmp ah, al
	jl return		; End of screen not reached
	; End of screen reached
	mov cl, 1		; Number of lines to scroll

; scroll subroutine begin
; Input:	cl = number of lines to scroll
scroll:
	push ds			; Preserve data segment
	push es
	pop ds			; Set ds to video memory (curretly stored in es)
	mov dl, cl		; Preserve number of lines to scroll
	mov di, 0x0000		; Set di to start of video memory (we will move things here)
	mov al, 2*cols
	mul cl
	mov si, ax		; Set si to 2*cols*lines_to_scroll (the location of the byte that will be moved to the first position of the first line)
	mov al, lines
	sub al, cl
	mov cl, cols
	mul cl
	mov cx, ax
	rep movsw

	; Fill last line with spaces
	mov al, dl
	mov cl, cols
	mul cl
	mov cx, ax		; Set cx to cols*lines_to_scroll
	mov ah, 0x07
	mov al, ' '
.last_line:
	mov [es:di], ax
	inc di
	inc di
	loop .last_line

	pop ds
	mov al, lines
	sub al, dl
	mov [io_line], al
return:
	retf
; print_char subroutine end
; print_char_with_attr subroutine end
; scroll subroutine end


; update_hw_cursor, set_hw_cursor subroutines
; update_hw_cursor subroutine begin
; Set the hardware cursor equal to the os cursor
update_hw_cursor:
	mov al, [io_line]
	mov bl, [io_col]
; set_hw_cursor subroutine begin
; Input: al, bl = line and column to set the hw cursor to
set_hw_cursor:
	; Calculate cursor offset (cols*line + col)
	mov cl, cols
	mul cl
	xor bh, bh
	add bx, ax 

	; Set low byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f	; Low cursor
	out dx, al 
	mov al, bl
	mov dx, 0x305	; CRT data
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	out dx, al

	; Set high byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e	; high cursor
	out dx, al 
	mov al, bh
	mov dx, 0x305	; CRT data
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	out dx, al

	retf
; update_hw_cursor subroutine end
; set_hw_cursor subroutine end

; update_os_cursor, set_os_cursor subroutines
; update_os_cursor subroutine begin
; Set the os cursor equal to the hw cursor
update_os_cursor:
	; Get the current cursor location from the CRT controller.
	; We have to select the register referring to the high and low parts of cursor location by writing the corresponding value to the CRT address port.
	; Then, we can read the current value of the regsters from the data port.
	mov dx, 0x304		; CRT address port
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f		; Low byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bl, al		; Save low part of cursor
	mov dx, 0x304		; CRT address port
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e		; High byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dx, [io_vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bh, al		; Save high part of cursor

	mov ax, bx
	mov bl, cols
	div bl

; set_os_cursor subroutine begin
; Input:	al, ah = line and col to set the os cursor to
set_os_cursor:
	mov [io_line], al	; Current line is cursor / cols
	mov [io_col], ah		; Current col is cursor mod cols

	retf
; update_os_cursor subroutine end
; set_os_cursor subroutine end

; set_vga_register_compatibility_mode subroutine begin
; Some VGA used ports (e.g. the input and output ports for the CRT registers) depend on bit 0 of the VGA miscellaneous output register:
;  if bit 0 of this miscellaneous output register is set, then the ports are the same as the CGA (CGA compatibility)
;  if bit 0 is clear, then the ports are the same as the MDA (MDA compatibility)
; This subroutine sets the bit as requested by the caller
; Input:	cl = 0xd0 for CGA, 0xb0 for MBA
set_vga_register_compatibility_mode: 
	mov dx, 0x3cc		; VGA miscellaneous output register read port
	in al, dx		; Read from read port to ax
	cmp cl, 0xd0
	je .cga
.mba:
	and al, 11111110b	; Clear the lst bit of ax, leaving the others unchanged
	jmp .end
.cga:
	or al, 00000001b	; Set the last bit of ax to 1, leaving the others unchanged
.end:
	mov dx, 0x3c2		; VGA miscellaneus output register write port
	out dx, al		; Write back to write port
	mov [io_vga_reg_compatibility_mode], cl
	retf



; DATA ------------------------------------------------------------------------
; Empty section (for now)


; KERNEL TABLE ----------------------------------------------------------------
; This table is initialized to contain all the information useful for the kernel.
io_table:

; Video -----------------------------------------
io_col				db 0x00
io_line				db 0x00
io_vga_reg_compatibility_mode	db 0xd0

; I/O subroutines -------------------------------
; TODO: insert pointers to every subroutine we wish to pass to kernel

; If this becomes negative, nasm will not assemble: we need to increase the constant io_sys_length by one sector.
; This is pretty ugly, but I think it is better to have bpb, io.sys and kernel in the same segment.
times 512 - ($ - $$)	db 0x00
