; vga.s
; The VGA driver
; The Vector Graphics Adapter is the standard PC Graphics controller.
; This driver only deals with VGA text modes.


; INITIALIZATION
; init_vga subroutine begin
; TODO: Check video mode (it should be 0x07 by default, but I have to do more reaserch to understand if it is always true)
;		Qemu returns mode 0x00, but it's pretty strange since it should be a 40x25 mode.
; TODO: COLS and LINES shouldn't be constants, since they depend on video mode.
; TODO: Implement a way to use more vga pages (or at least decide if would be useful)
init_vga:
	; We need to access the CRT controller registers to interact with the hardware cursor on screen.
	; To do so, we first tell the VGA that we wish to use CGA-compatible port numbers for the CRT registers.
	mov cl, CGA_COMPATIBILITY	; 0xd0 is for CGA-compatible mode
	call set_vga_register_compatibility_mode

	; We can now get the current cursor location from the CRT controller.
	call update_os_cursor
	ret
; init_vga subroutine end



; SOFTWARE INTERRUPTS HANDLERS

; print_char, print_char_with_attr, scroll subroutines

; print_char subroutine begin
; Print a character at the current os cursor, without advancing it.
; Input:	al = character to print
; Modified registers: ax, bx, cx, dl, di, si
print_char:
	mov ah, 0x07	; Default character attribute: light grey on black background

; print_char_with_attr subroutine begin
; Like print_char, but the caller provides the character attribute.
; Input:	al = character to print, ah = character attribute
; Modified registers: ax, bx, cx, dl, di, si
print_char_with_attr:
	cmp al, `\r`
	je .carriage_return
	cmp al, `\n`
	je .line_feed
	push ax
	; bx = offset = 2 * (col + line * COLS)
	mov al, [line]
	mov bl, COLS
	mul byte bl		; ax = al * bl = line * COLS
	xor bx, bx
	mov bl, [col]
	add bx, ax		; bx += ax = col + line * COLS
	shl bx, 1 		; Multiply bx by 2, because each character occupies two bytes
	pop ax

	push es			; Save extra segment
	mov cx, VIDEO_BUFFER
	mov es, cx		; Set the extra segment to the VGA video memory for color output
	mov [es:bx], ax		; Move the byte to video memory
	pop es			; Restore the extra segment

	mov ah, [col]
	inc ah
	mov [col], ah	; Next column
	jmp .check_for_scroll_cols

.carriage_return:
	xor ah, ah
	mov [col], ah
	jmp return

.line_feed:
	mov ah, [line]
	inc ah
	mov [line], ah
	jmp .check_for_scroll_lines

.check_for_scroll_cols:
	mov al, COLS
	cmp ah, al
	jl return		; End of line not reached

	; End of line reached
	xor ah, ah
	mov [col], ah	; Set column to 0
	mov ah, [line]
	inc ah
	mov [line], ah	; Next line
.check_for_scroll_lines:
	mov al, LINES
	cmp ah, al
	jl return		; End of screen not reached
	; End of screen reached
	mov cl, 1		; Number of lines to scroll

; scroll subroutine begin
; Input:	cl = number of lines to scroll
; Modified registers: ax, cx, dl, si, di
scroll:
	mov dl, cl		; Preserve number of lines to scroll
	mov di, 0x0000		; Set di to start of video memory (we will move things here)
	mov al, 2*COLS
	mul cl
	mov si, ax		; Set si to 2*COLS*lines_to_scroll (the location of the byte that will be moved to the first position of the first line)
	mov al, LINES
	sub al, cl
	mov cl, COLS
	mul cl
	mov cx, ax

	push ds			; Preserve data segment
	push es			; Preserve extra segment
	mov bx, VIDEO_BUFFER
	mov ds, bx		; Set ds to video memory
	mov es, bx		; Set es to video memory
	rep movsw

	; Fill last line with spaces
	mov al, dl
	mov cl, COLS
	mul cl
	mov cx, ax		; Set cx to COLS*lines_to_scroll
	mov ah, 0x07
	mov al, ' '
.last_line:
	mov [es:di], ax
	inc di
	inc di
	loop .last_line

	pop es
	pop ds

	mov al, LINES
	sub al, dl
	mov [line], al
return:
	ret
; print_char subroutine end
; print_char_with_attr subroutine end
; scroll subroutine end


; update_hw_cursor, set_hw_cursor subroutines
; update_hw_cursor subroutine begin
; Set the hardware cursor equal to the os cursor
; Modified registers: ax, bx, cl, dx
update_hw_cursor:
	mov al, [line]
	mov bl, [col]
; set_hw_cursor subroutine begin
; Input: al, bl = line and column to set the hw cursor to
; Modified registers: ax, bh, cl, dx
; TODO: Set the bios cursor together with the hardware one.
set_hw_cursor:
	; Update BIOS cursor location stored in the bios data area
	; We do this to allow the user to continue using BIOS interrupts, if he desires
	mov [0x450], bl		; Line
	mov [0x451], al		; Col

	; Calculate cursor offset (COLS*line + col)
	mov cl, COLS
	mul cl
	xor bh, bh
	add bx, ax 

	; Set low byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f	; Low cursor
	out dx, al 
	mov al, bl
	mov dx, 0x305	; CRT data
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	out dx, al

	; Set high byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e	; high cursor
	out dx, al 
	mov al, bh
	mov dx, 0x305	; CRT data
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	out dx, al

	ret
; update_hw_cursor subroutine end
; set_hw_cursor subroutine end

; update_os_cursor, set_os_cursor subroutines
; update_os_cursor subroutine begin
; Set the os cursor equal to the hw cursor
; Modified registers: ax, bx, dx
update_os_cursor:
	; Get the current cursor location from the CRT controller.
	; We have to select the register referring to the high and low parts of cursor location by writing the corresponding value to the CRT address port.
	; Then, we can read the current value of the regsters from the data port.
	mov dx, 0x304		; CRT address port
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f		; Low byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bl, al		; Save low part of cursor
	mov dx, 0x304		; CRT address port
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e		; High byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dl, [reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bh, al		; Save high part of cursor

	mov ax, bx
	mov bl, COLS
	div bl

; set_os_cursor subroutine begin
; Input:	al, ah = line and col to set the os cursor to
; Modifie registers: none
set_os_cursor:
	mov [line], al		; Current line is cursor / COLS
	mov [col], ah		; Current col is cursor mod COLS

	ret
; update_os_cursor subroutine end
; set_os_cursor subroutine end

; set_vga_register_compatibility_mode subroutine begin
; Some VGA used ports (e.g. the input and output ports for the CRT registers) depend on bit 0 of the VGA miscellaneous output register:
;  if bit 0 of this miscellaneous output register is set, then the ports are the same as the CGA (CGA compatibility)
;  if bit 0 is clear, then the ports are the same as the MDA (MDA compatibility)
; This subroutine sets the bit as requested by the caller
; Input:	cl = 0xd0 for CGA, 0xb0 for MBA
; Modified registers: al, dx
set_vga_register_compatibility_mode: 
	mov dx, VGA_MISC_OUTPUT_REGISTER_READ
	in al, dx		; Read from read port to ax
	cmp cl, 0xd0
	je .cga
.mba:
	and al, 11111110b	; Clear the lst bit of ax, leaving the others unchanged
	jmp .end
.cga:
	or al, 00000001b	; Set the last bit of ax to 1, leaving the others unchanged
.end:
	mov dx, VGA_MISC_OUTPUT_REGISTER_WRITE
	out dx, al		; Write back to write port
	mov [reg_compatibility_mode], cl
	ret

; DATA
col			db 0x00
line			db 0x00
reg_compatibility_mode	db 0xd0

; CONSTANTS
VIDEO_BUFFER		equ	0xb800
COLS			equ	80
LINES			equ	25

CGA_COMPATIBILITY	equ	0xd0
MDA_COMPATIBILITY	equ	0xb0

CRT_ADDRESS_REGISTER		equ	0x304	; The compatibility constant has to be added: the register is 0x3d4 or 0x3b4
CRT_DATA_REGISTER		equ	0x305	; The compatibility constant has to be added: the register is 0x3d5 or 0x3b5
VGA_MISC_OUTPUT_REGISTER_READ	equ	0x3cc
VGA_MISC_OUTPUT_REGISTER_WRITE	equ	0x3cc
