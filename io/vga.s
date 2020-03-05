; INITIALIZATION

init_vga:
; TODO: Check video mode (it should be 0x07 by default, but I have to do more reaserch to understand if it is always true)
;		Qemu returns mode 0x00, but it's pretty strange since it should be a 40x25 mode.
; TODO: COLS and LINES shouldn't be constants, since they depend on video mode.
; TODO: Implement a way to use more vga pages (or at least decide if would be useful)
	mov ax, VIDEO_BUFFER
	mov es, ax		; Set the extra segment to the VGA video memory for color output

	; We need to access the CRT controller registers to interact with the hardware cursor on screen.
	; To do so, we first tell the VGA that we wish to use CGA-compatible port numbers for the CRT registers.
	mov cl, 0xd0	; 0xd0 is for CGA-compatible mode
	call set_vga_register_compatibility_mode

	; We can now get the current cursor location from the CRT controller.
	call update_os_cursor
	ret



; SUBROUTINES FOR KERNEL
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
	; bx = offset = 2 * (vga_col + vga_line * COLS)
	mov al, [vga_line]
	mov bl, COLS
	mul byte bl		; ax = al * bl = vga_line * COLS
	xor bx, bx
	mov bl, [vga_col]
	add bx, ax		; bx += ax = vga_col + vga_line * COLS
	shl bx, 1 		; Multiply bx by 2, because each character occupies two bytes
	pop ax
	mov [es:bx], ax		; Move the byte to video memory

	mov ah, [vga_col]
	inc ah
	mov [vga_col], ah	; Next column
	jmp .check_for_scroll_cols

.carriage_return:
	xor ah, ah
	mov [vga_col], ah
	jmp return

.line_feed:
	mov ah, [vga_line]
	inc ah
	mov [vga_line], ah
	jmp .check_for_scroll_lines

.check_for_scroll_cols:
	mov al, COLS
	cmp ah, al
	jl return		; End of line not reached

	; End of line reached
	xor ah, ah
	mov [vga_col], ah	; Set column to 0
	mov ah, [vga_line]
	inc ah
	mov [vga_line], ah	; Next line
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
	push ds			; Preserve data segment
	push es
	pop ds			; Set ds to video memory (curretly stored in es)
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

	pop ds
	mov al, LINES
	sub al, dl
	mov [vga_line], al
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
	mov al, [vga_line]
	mov bl, [vga_col]
; set_hw_cursor subroutine begin
; Input: al, bl = line and column to set the hw cursor to
; Modified registers: ax, bh, cl, dx
; TODO: Set the bios cursor together with the hardware one.
set_hw_cursor:
	; Calculate cursor offset (COLS*line + col)
	mov cl, COLS
	mul cl
	xor bh, bh
	add bx, ax 

	; Set low byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f	; Low cursor
	out dx, al 
	mov al, bl
	mov dx, 0x305	; CRT data
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	out dx, al

	; Set high byte of cursor using the CRT register 0x0f
	mov dx, 0x304	; CRT address
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e	; high cursor
	out dx, al 
	mov al, bh
	mov dx, 0x305	; CRT data
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
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
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0f		; Low byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bl, al		; Save low part of cursor
	mov dx, 0x304		; CRT address port
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	mov al, 0x0e		; High byte of cursor index
	out dx, al
	mov dx, 0x305		; CRT data port
	add dl, [vga_reg_compatibility_mode]	; 0xd0 if CGA, 0xb0 if MDA
	in al, dx
	mov bh, al		; Save high part of cursor

	mov ax, bx
	mov bl, COLS
	div bl

; set_os_cursor subroutine begin
; Input:	al, ah = line and col to set the os cursor to
; Modifie registers: none
set_os_cursor:
	mov [vga_line], al		; Current line is cursor / COLS
	mov [vga_col], ah		; Current col is cursor mod COLS

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
	mov [vga_reg_compatibility_mode], cl
	ret

; DATA
vga_col				db 0x00
vga_line			db 0x00
vga_reg_compatibility_mode	db 0xd0

; CONSTANTS
VIDEO_BUFFER		equ	0xb800
COLS			equ	80
LINES			equ	25
