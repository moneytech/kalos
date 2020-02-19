%include "geometry.s"

[bits 16]
[cpu 8086]
[org io_sys_addr]

; MAIN SECTION ----------------------------------------------------------------
; Memory ----------------------------------------
	; Interrupt 0x12 returns in ax the number of available contiguous kilobytes of memory starting from 0x00000
	; The size of conventional memory is 640kB, but the last part of it (at most 128kB) is used by the Extended Bios Data Area
	int 0x12
	mov [k_available_memory], ax


; Output ----------------------------------------
; - Video -----------------------------
; TODO: Check video mode (it should be 0x07 by default, but I have to do more reaserch to understand if it is always true)
; TODO: cols and lines shouldn't be constants, since they depend on video mode. Perhaps save them in the kernel table?
; TODO: Understand if the VGA hardware cursor is the same used by the teletype int 10,e
	mov ax, 0xb800
	mov es, ax	; Set the extra segment to the VGA video memory for color output
cols	equ	80
lines	equ	25

	; We need to access the CRT controller registers to interact with the hardware cursor on screen.
	; The input and output ports for the CRT registers depend on bit 0 in the VGA miscellaneous output register:
	;  if bit 0 of this miscellaneous output register is set, then the ports are the same as the CGA (0x3d4, 0x3d5);
	;  if bit 0 is clear, then the ports are the same as the MDA (0x3b4, 0x3b5).
	; While we are dealing with the miscellaneous output register, we also set the RAM enable bit (bit 1), so that
	;  we can be sure that the VGA hardware does not ignore writes to its address space.
	mov dx, 0x3cc		; VGA miscellaneous output register read port
	in al, dx		; Read from read port to ax
	or al, 00000011b	; Set the last two bits of ax to 1, leaving the others unchanged
	mov dx, 0x3c2		; VGA miscellaneus output register write port
	out dx, al		; Write back to write port

	; We can now get the current cursor location from the CRT controller.
	; We have to select the register referring to the high and low parts of cursor location by writing the corresponding value to the CRT address port.
	; Then, we can read the current value of the regsters from the data port.
	call 0x0000:update_os_cursor

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
	mov ax, kernel_sys_addr
	hlt
	jmp ax
	hlt


; SUBROUTINES FOR KERNEL ------------------------------------------------------
; In this section, there are the subroutines which will be available to the kernel and the programs trough the kernel table.
; There are subroutines with multiple entry points (like print_char, print_char_with_attr and scroll), so be careful.
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
	; bx = offset = 2 * (k_col + k_line * cols)
	mov al, [k_line]
	mov bl, cols
	mul byte bl		; ax = al * bl = k_line * cols
	xor bx, bx
	mov bl, [k_col]
	add bx, ax		; bx += ax = k_col + k_line * cols
	shl bx, 1 		; Multiply bx by 2, because each character occupies two bytes
	pop ax
	mov [es:bx], ax		; Move the byte to video memory

	mov ah, [k_col]
	inc ah
	mov [k_col], ah		; Next column
	mov al, cols
	cmp ah, al
	jl return		; End of line not reached

	; End of line reached
	xor ah, ah
	mov [k_col], ah		; Set column to 0
	mov ah, [k_line]
	inc ah
	mov [k_line], ah	; Next line
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
	mov [k_line], al
return:
	retf
; print_char subroutine end
; print_char_with_attr subroutine end
; scroll subroutine end


; update_hw_cursor, set_hw_cursor subroutines
; update_hw_cursor subroutine begin
; Set the hardware cursor equal to the os cursor
update_hw_cursor:
	mov al, [k_line]
	mov bl, [k_col]
; set_hw_cursor subroutine begin
; Input: al, bl = line and column to set the hw cursor to
set_hw_cursor:
	; Calculate cursor offset (cols*line + col)
	mov cl, cols
	mul cl
	xor bh, bh
	add bx, ax 

	; Set low byte of cursor using the CRT register 0x0f
	mov dx, 0x3d4	; CRT address
	mov al, 0x0f	; Low cursor
	out dx, al 
	mov al, bl
	mov dx, 0x3d5	; CRT data
	out dx, al

	; Set high byte of cursor using the CRT register 0x0f
	mov dx, 0x3d4	; CRT address
	mov al, 0x0e	; high cursor
	out dx, al 
	mov al, bh
	mov dx, 0x3d5	; CRT data
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
	mov dx, 0x3d4		; CRT address port
	mov al, 0x0f		; Low byte of cursor index
	out dx, al
	mov dx, 0x3d5		; CRT data port
	in al, dx
	mov bl, al		; Save low part of cursor
	mov dx, 0x3d4		; CRT address port
	mov al, 0x0e		; High byte of cursor index
	out dx, al
	mov dx, 0x3d5		; CRT data port
	in al, dx
	mov bh, al		; Save high part of cursor

	mov ax, bx
	mov bl, cols
	div bl

; set_os_cursor subroutine begin
; Input:	al, ah = line and col to set the os cursor to
set_os_cursor:
	mov [k_line], al	; Current line is cursor / cols
	mov [k_col], ah		; Current col is cursor mod cols

	retf



; DATA ------------------------------------------------------------------------
; Empty section (for now)


; KERNEL TABLE ----------------------------------------------------------------
; This table is initialized to contain all the information useful for the kernel.
kernel_table:

; Memory ----------------------------------------
; All addresses are offset of segment 0x0000
k_kernel_table_addr	dw kernel_table
k_io_sys_addr		dw io_sys_addr
k_kernel_sys_addr	dw kernel_sys_addr
k_fat_addr		dw fat_addr
k_root_addr		dw root_addr
k_bpb_addr		dw bpb_addr
k_available_memory	dw 0x0000

; Video -----------------------------------------
k_col			db 0x00
k_line			db 0x00

; I/O subroutines -------------------------------
; TODO: insert pointers to every subroutine we wish to pass to kernel

times 512 - ($ - $$)	db 0x00		; If this becomes negative, nasm will not assemble: we need to increase the constant io_sys_length by one sector
