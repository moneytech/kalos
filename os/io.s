%include "geometry.inc"
%include "io.inc"

[bits 16]
[cpu 8086]
[org io_addr]	; We are in segment 0, so we have to shift our labels by io_addr (the address where we are loaded)

; MAIN SECTION ----------------------------------------------------------------

main:
; Video -----------------------------------------
; TODO: Check video mode (it should be 0x07 by default, but I have to do more reaserch to understand if it is always true)
; TODO: cols and lines shouldn't be constants, since they depend on video mode. Perhaps save them in the kernel table?
; TODO: Understand if the VGA hardware cursor is the same used by the teletype int 10,e
	mov ax, video_buffer
	mov es, ax	; Set the extra segment to the VGA video memory for color output

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
	out pic_primary_addr, al	; Primary PIC command register
	out pic_secondary_addr, al	; Secondary PIC command register

	; ICW2: second initialization word, used to tell the PIC which interrupts it should trigger
	; The interrupt corresponding to IRQ0 (the byte we write to the data register) must be a multiple of 8.
	; We keep the default values of 0x08 and 0x70 (but we must write them anyway because during initialization the default gets overwritten).
	mov al, pic_irq0_int		; Primary PIC maps IRQ 0 to 7 starting from this interrupt
	out pic_primary_data, al	; Primary PIC data register
	mov al, pic_irq8_int		; Secondary PIC maps IRQ 8 to 15 starting from this interrupt
	out pic_secondary_data, al	; Secondary PIC data register

	; ICW3: tell the PICs which IRQ line to use to comunicate between each other
	; The format for primary and slave is different!
	mov al, 00000100b		; Primary PIC: bit 2 is set, so use IRQ2
	out pic_primary_data, al	; Primary PIC data register
	mov al, 0x2			; Slave PIC: the byte is 0x2, so use IRQ2
	out pic_secondary_data, al	; Slave PIC data register

	; ICW4: extra information
	; Bits 5-7: reserved, must be 0
	; Bit 4: not supported on x86 (special fully nested mode)
	; Bit 3: buffered mode. We are not interested, so 0
	; Bit 2: only use if bit 3 is set, so 0
	; Bit 1: we also don't need this (automatic EOI on acknowledge pulse), so 0
	; Bit 0: if 1, 80x86 mode (what we want). If clear, MCS-80/86 mode
	mov al, 00000001b
	out pic_primary_data, al
	out pic_secondary_data, al

	; OCW1: Operational Command Word 1
	; Disable all IRQs in both pics
	; We will enable each of them when we install the interrupt handler
	; Each bit corresponds to a different IRQ. 1 means IRQ disabled, 0 means IRQ enabled
	mov al, 11111111b		; Disable all 8 IRQs
	out pic_primary_data, al	; Write to primary PIC Interrupt Mask Register
	out pic_secondary_data, al	; Write to slave PIC Interrupt Mask Register

	; OCW3: Operational Command Word 3
	; Bit 7: reserved, must be 0
	; Bit 6: special mask mode (we don't want it, so 0)
	; Bit 5: mask mode (0 for normal)
	; Bits 4-3: 01b for OCW3
	; Bit 2: polling mode (we want interrupt mode, so 0)
	; Bits 1-0: which register to make available on address port (we are not interested, so 10b for IRR just because we have to choose one)
	mov al, 00001010b
	out pic_primary_data, al
	out pic_secondary_data, al

	; Enable interrupts in the cpu
	; At this point, in the PIC all interrupt are still disabled
	sti


; PIT (timer) -----------------------------------------------------------------
	; TODO: Configure timer
	; TODO: Check if this interrupt handler is really needed (bios should already provide one)
	; Install interrupt
	xor bx, bx
	mov word [4 * pic_irq0_int], irq_0_pit
	mov word [4 * pic_irq0_int + 2], bx

	; Enable interrupt in PIC
	in al, pic_primary_data
	and al, 11111110b
	out 0x21, al
	

; Keyboard --------------------------------------
	; Keyboard initialization
	; TODO: It seems that qemu doesn't emulate ps/2 keyboard specific encoder commands (for example, 0xf0, 0xf8, 0xf9). Not a problem of the os, but it's better to understand why.
	; TODO: The encoder uses set 2 for scancodes. Decide whether we want the controller to translate it to set 1 or not.

	; At first, we disable the keyboard and the mouse ports to do our configurations safely.
	; Disable first PS/2 port (keyboard). We will enable it back later.
	call wait_keyboard_write_ready
	mov al, 0xad			; Disable first port command
	out keyb_controller, al		; Write to controller command register
	; Disable second PS/2 port (mouse). We won't enable it back later.
	call wait_keyboard_write_ready
	mov al, 0xa7			; Disable second port command
	out keyb_controller, al		; Write to controller command register

	; Empty the register in the case it's full of data to read
	in al, keyb_encoder
	; Disable IRQs from the two PS/2 lines (keyboard and mouse)
	call wait_keyboard_write_ready
	mov al, 0x20
	out keyb_controller, al		; Ask for controller configuration byte
	call wait_keyboard_read_ready
	in al, keyb_encoder		; Read controller configuration byte from encoder
	and al, 10111100b		; Disable IRQs (bit 0 for first port, bit 1 for second) and translation (bit 6)
	mov ah, al			; Preserve the configuration byte

	; Tell the controller that we will be overwriting the configuration byte
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out keyb_controller, al		; Write to controller command register
	; Write back the configuration byte
	call wait_keyboard_write_ready
	mov al, ah			; Move the conf byte to al
	out keyb_encoder, al		; Write the conf byte to the encoder command register

	; Encoder test
	call wait_keyboard_write_ready
	mov al, 0xee			; Send echo command
	out keyb_encoder, al		; Write to encoder command register
	call wait_keyboard_read_ready
	in al, keyb_encoder		; Read response from encoder register
	cmp al, 0xee			; 0x55 = OK
	jne keyb_error

	; Set encoder to default mode
	; This means: scan set 2, default typematic rate/delay, all key types enabled (make, break, typematic)
	mov cx, 255			; Retry 255 times
.set_encoder_default_mode:
	call wait_keyboard_write_ready
	mov al, 0xf6			; Set default mode command
	out keyb_encoder, al		; Write to encoder command register
	call wait_keyboard_read_ready
	in al, keyb_encoder		; Read response from encoder register
	cmp al, 0xfa			; 0xfa = ACK, everything fine
	je .self_test
	loop .set_encoder_default_mode	; Otherwise (0xfe), repeat command
	jmp keyb_error			; After 255 retries, it's probably time to surrender

.self_test:
	; Keyboard controller self test
	call wait_keyboard_write_ready
	mov al, 0xaa			; Controller self test command
	out keyb_controller, al		; Write to controller command register
	call wait_keyboard_read_ready
	in al, keyb_encoder		; Read response from encoder register
	cmp al, 0x55			; 0x55 means test ok
	jne keyb_error			; Otherwise (0xfc), it's an error

	; Some controllers change their status on self test, so write back the configuration byte
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out keyb_controller, al		; Write to controller command register
	call wait_keyboard_write_ready
	mov al, ah			; ah still contains the conf byte
	out keyb_encoder, al		; Write the conf byte to the encoder register

	; Keyboard interface (first ps/2 port) self test
	; TODO: we could probably try to reset the keyboard once before jumping to error routine
	call wait_keyboard_write_ready
	mov al, 0xab			; Keyboard interface self test command
	out keyb_controller, al		; Write to controller command register
	call wait_keyboard_read_ready
	in al, keyb_encoder		; Read response from encoder register
	test al, al			; 0x00 means test ok
	jne keyb_error			; Otherwise, it's an error

	; Enable IRQ from first port. We leave the second port IRQ (the mouse) disabled.
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out keyb_controller, al		; Write to controller command register
	call wait_keyboard_write_ready
	or ah, 00000001b		; ah stil contains the conf byte. Reenable the keyboard IRQ (bit 0). Leave the mouse IRQ (bit 1) as is (disabled).
	mov al, ah			; Move conf byte to al
	out keyb_encoder, al		; Write the conf byte to the encoder register

	; Enable back first port (keyboard)
	call wait_keyboard_write_ready
	mov al, 0xae			; Enable first port command
	out keyb_controller, al		; Write to controller command register

	; Install interrupt
	xor bx, bx						; The code segment is 0x0000
	mov word [4 * (pic_irq0_int+1)], irq_1_keyboard		; Offset
	mov word [4 * (pic_irq0_int+1) + 2], bx			; Segment

	; Enable interrupt in PIC
	in al, pic_primary_data		; Read PIC interrupt mask byte
	and al, 11111101b		; Clear bit 1 (enable keyboard interrupts)
	out pic_primary_data, al	; Write back interrupt mask byte

; KERNEL CALL------------------------------------------------------------------

	; Debug code
	; Infinite loop before jumping to kernel
debug_loop:
	hlt
	jmp debug_loop

	; Jump to the kernel
	mov ax, kernel_addr
	mov bx, io_table

	jmp ax
	hlt

; TODO: write keyboard error handler (this is a stub)
keyb_error:
	mov al, 'E'
	call print_char
	hlt
	jmp keyb_error

; SUBROUTINES
; wait_keyboard_write_ready subroutine begin
; Returns when the keyboard controller status byte bit 1 is not set (i.e. the controller is ready to write to)
; Modified registers: al
wait_keyboard_write_ready:
	in al, keyb_controller		; Read controller status register
	and al, 00000010b		; Bit 1 is input status buffer
	jnz wait_keyboard_write_ready	; If it's set, the controller is not ready to read
	ret

; wait_keyboard_read_ready subroutine begin
; Returns when the keyboard controller status byte bit 0 is not set (i.e. the controller is ready to read to)
; Modified registers: al
wait_keyboard_read_ready:
	in al, keyb_controller		; Read controller status register
	and al, 00000001b		; Bit 0 is output status buffer
	jnz wait_keyboard_write_ready	; If it's set, the controller is not ready to read
	ret

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
	call 0x0000:print_char	; Print high digit
	pop ax			; Restore ascii number
	call 0x0000:print_char	; Print low digit
	ret

; INTERRUPT HANDLERS
irq_0_pit:
	; TODO: implement handler
	push ax

	; Uncomment these lines to check if the handler works
	; Registers are not preserved if this code is executed!
	;push ds
	;xor ax, ax
	;mov ds, ax
	;mov al, 'T'
	;call 0x0000:print_char
	;call 0x0000:update_hw_cursor
	;pop ds

	; OCW2
	mov al, 00100000b		; Bit 5 is set: End Of Interrupt (EOI) request
	out pic_primary_addr, al

	pop ax
	iret


irq_1_keyboard:
	; TODO: implement handler
	push ax
	call wait_keyboard_read_ready
	in al, 0x60

	; The actual handler code will go here. For now, it prints scancodes for debugging purposes.
	push bx
	push cx
	push dx
	push di
	push si
	call print_hex
	mov al, ' '
	call 0x0000:print_char
	mov al, ' '
	call 0x0000:print_char
	call 0x0000:update_hw_cursor
	pop si
	pop di
	pop dx
	pop cx
	pop bx

	; OCW2
	mov al, 00100000b		; Bit 5 is set: End Of Interrupt (EOI) request
	out pic_primary_addr, al

	pop ax
	iret

; SUBROUTINES FOR KERNEL ------------------------------------------------------
; In this section, there are the subroutines which will be available to the kernel and the programs trough the kernel table.
; There are subroutines with multiple entry points (like print_char, print_char_with_attr and scroll), so be careful.
; TODO: Choose if system calls are to be provided by interrupts or by giving addresses to functions
; TODO: Properly comment this mess
; TODO: Write which registers are modified

; print_char, print_char_with_attr, scroll subroutines
; TODO: Implement CR and LF characters

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
; Modified registers: ax, cx, dl, si, di
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
times 1024 - ($ - $$)	db 0x00
