; Keyboard --------------------------------------
	; Keyboard initialization
init_keyb:
	; This code is long and ugly. Before every read or write operation we have to check if the keyboard controller is ready.
	; We disable the keyboard not to get disturbed, do some tests, set the keyboard in default mode and enable it back.
	; We also disable the mouse permanently.
	; TODO: It seems that qemu doesn't emulate ps/2 keyboard specific encoder commands (for example, 0xf0, 0xf8, 0xf9). Not a problem of the os, but it's better to understand why.
	; TODO: The encoder uses set 2 for scancodes. Decide whether we want the controller to translate it to set 1 or not.

	; At first, we disable the keyboard and the mouse ports to do our configurations safely.
	; Disable first PS/2 port (keyboard). We will enable it back later.
	call wait_keyboard_write_ready
	mov al, 0xad			; Disable first port command
	out KEYB_CONTROLLER, al		; Write to controller command register
	; Disable second PS/2 port (mouse). We won't enable it back later.
	call wait_keyboard_write_ready
	mov al, 0xa7			; Disable second port command
	out KEYB_CONTROLLER, al		; Write to controller command register

	; Empty the register in the case it's full of data to read
	in al, KEYB_ENCODER
	; Disable IRQs from the two PS/2 lines (keyboard and mouse)
	call wait_keyboard_write_ready
	mov al, 0x20
	out KEYB_CONTROLLER, al		; Ask for controller configuration byte
	call wait_keyboard_read_ready
	in al, KEYB_ENCODER		; Read controller configuration byte from encoder
	and al, 10111100b		; Disable IRQs (bit 0 for first port, bit 1 for second) and translation (bit 6)
	mov ah, al			; Preserve the configuration byte

	; Tell the controller that we will be overwriting the configuration byte
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out KEYB_CONTROLLER, al		; Write to controller command register
	; Write back the configuration byte
	call wait_keyboard_write_ready
	mov al, ah			; Move the conf byte to al
	out KEYB_ENCODER, al		; Write the conf byte to the encoder command register

	; Encoder test
	call wait_keyboard_write_ready
	mov al, 0xee			; Send echo command
	out KEYB_ENCODER, al		; Write to encoder command register
	call wait_keyboard_read_ready
	in al, KEYB_ENCODER		; Read response from encoder register
	cmp al, 0xee			; 0x55 = OK
	jne keyb_error

	; Set encoder to default mode
	; This means: scan set 2, default typematic rate/delay, all key types enabled (make, break, typematic)
	mov cx, 255			; Retry 255 times
.set_encoder_default_mode:
	call wait_keyboard_write_ready
	mov al, 0xf6			; Set default mode command
	out KEYB_ENCODER, al		; Write to encoder command register
	call wait_keyboard_read_ready
	in al, KEYB_ENCODER		; Read response from encoder register
	cmp al, 0xfa			; 0xfa = ACK, everything fine
	je .self_test
	loop .set_encoder_default_mode	; Otherwise (0xfe), repeat command
	jmp keyb_error			; After 255 retries, it's probably time to surrender

.self_test:
	; Keyboard controller self test
	call wait_keyboard_write_ready
	mov al, 0xaa			; Controller self test command
	out KEYB_CONTROLLER, al		; Write to controller command register
	call wait_keyboard_read_ready
	in al, KEYB_ENCODER		; Read response from encoder register
	cmp al, 0x55			; 0x55 means test ok
	jne keyb_error			; Otherwise (0xfc), it's an error

	; Some controllers change their status on self test, so write back the configuration byte
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out KEYB_CONTROLLER, al		; Write to controller command register
	call wait_keyboard_write_ready
	mov al, ah			; ah still contains the conf byte
	out KEYB_ENCODER, al		; Write the conf byte to the encoder register

	; Keyboard interface (first ps/2 port) self test
	; TODO: we could probably try to reset the keyboard once before jumping to error routine
	call wait_keyboard_write_ready
	mov al, 0xab			; Keyboard interface self test command
	out KEYB_CONTROLLER, al		; Write to controller command register
	call wait_keyboard_read_ready
	in al, KEYB_ENCODER		; Read response from encoder register
	test al, al			; 0x00 means test ok
	jne keyb_error			; Otherwise, it's an error

	; Enable IRQ from first port. We leave the second port IRQ (the mouse) disabled.
	call wait_keyboard_write_ready
	mov al, 0x60			; Write controller conf byte command
	out KEYB_CONTROLLER, al		; Write to controller command register
	call wait_keyboard_write_ready
	or ah, 00000001b		; ah stil contains the conf byte. Reenable the keyboard IRQ (bit 0). Leave the mouse IRQ (bit 1) as is (disabled).
	mov al, ah			; Move conf byte to al
	out KEYB_ENCODER, al		; Write the conf byte to the encoder register

	; Enable back first port (keyboard)
	call wait_keyboard_write_ready
	mov al, 0xae			; Enable first port command
	out KEYB_CONTROLLER, al		; Write to controller command register

	; Install interrupt
	xor bx, bx						; The code segment is 0x0000
	mov word [4 * (PIC_IRQ0_INT+1)], irq_1_keyboard		; Offset
	mov word [4 * (PIC_IRQ0_INT+1) + 2], bx			; Segment

	; Enable interrupt in PIC
	in al, PIC_PRIMARY_DATA		; Read PIC interrupt mask byte
	and al, 11111101b		; Clear bit 1 (enable keyboard interrupts)
	out PIC_PRIMARY_DATA, al	; Write back interrupt mask byte

	ret



; SUBROUTINES
; wait_keyboard_write_ready subroutine begin
; Returns when the keyboard controller status byte bit 1 is not set (i.e. the controller is ready to write to)
; Modified registers: al
wait_keyboard_write_ready:
	in al, KEYB_CONTROLLER		; Read controller status register
	and al, 00000010b		; Bit 1 is input status buffer
	jnz wait_keyboard_write_ready	; If it's set, the controller is not ready to read
	ret

; wait_keyboard_read_ready subroutine begin
; Returns when the keyboard controller status byte bit 0 is not set (i.e. the controller is ready to read to)
; Modified registers: al
wait_keyboard_read_ready:
	in al, KEYB_CONTROLLER		; Read controller status register
	and al, 00000001b		; Bit 0 is output status buffer
	jnz wait_keyboard_write_ready	; If it's set, the controller is not ready to read
	ret
; wait_keyboard_read_ready subroutine end


irq_1_keyboard:
	push ax
	push bx
	push cx
	push ds

	; Data and subroutines that we need are in segment 0x0000
	xor ax, ax
	mov ds, ax
	call wait_keyboard_read_ready
	in al, 0x60

	; This code is for debugging purposes: it prints the scancodes
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	call print_hex
	mov al, ' '
	call print_char
	mov al, ' '
	call print_char
	call update_hw_cursor
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax

	; This is the actual handler
	; Our keyboard buffer is a circular buffer.
	; The position of the next scancode to write to the buffer is pointed by io_keyb_buf_pointer. This is where we store our scancode.
	; Then, we have to increment it. If after incrementing it we are at the top of the buffer, we make it point back to the beginning of the buffer.
	; Finally, we update the count of scancodes in the buffer.
	; If it has reached maximum, we don't increment it, and we also advance the first scancode pointer (since we have already overwritten the first scancode).
	; This means that if the input is not asked for some times, the keys pressed most early will be ignored and discarded (unfortunately, we don't have infinite memory).
	; The handler only stores the scancodes in the buffer! It doesn't do any kind of translation.
	; The translation to ascii or to more manageable keycodes will be done by the kernel when a program asks for keyboad input through a system call.
	; This is because a IRQ handler should be as short and fast as possible.
	; TODO: decide the proper size of the buffer (it is saved in io.inc). For now, it is 2048 scancodes, which means approximately 682 key presses and releases.

	; Store the input scancode
	mov bx, [io_keyb_buf_next_pointer]
	mov [bx], al

	; Take care of the last scancode pointer
	inc bx					; Increment the pointer
	cmp bx, [io_keyb_buf_end]		; The buffer is circular, so we can't simply increment it
	jl .next_not_end_of_buffer		; If it's below the end of buffer, we keep it as it is
.next_end_of_buffer:
	mov bx, [io_keyb_buf_begin]		; Otherwise, we circle back to the beginning of the buffer
.next_not_end_of_buffer:
	mov [io_keyb_buf_next_pointer], bx	; In any case, store back the pointer

	; Now take care of the scancodes count (and of the first scancode pointer in case the buffer is full)
	mov cx, [io_keyb_buf_count]
	cmp cx, [io_keyb_buf_max]		; Check if the buffer is full
	jne .max_not_reached			; If it's not, jump
.max_reached:					; If it is, it means that the first scancode has been overwritten by the last. We have to increment the first pointer
	mov bx, [io_keyb_buf_first_pointer]
	inc bx					; Increment the pointer
	cmp bx, [io_keyb_buf_end]		; The buffer is circular, so we can't simply increment it
	jl .first_not_end_of_buffer		; If it's below the end of buffer, we keep it as it is
.first_end_of_buffer:
	mov bx, [io_keyb_buf_begin]		; Otherwise, we circle back to the beginning of the buffer
.first_not_end_of_buffer:
	mov [io_keyb_buf_first_pointer], bx	; The first scancode is now the first one after the last read
	jmp .end				; Then we jump because we don't want to increment the scancodes count (we are already full)

.max_not_reached:
	inc cx
	mov [io_keyb_buf_count], cx		; Save incremented count
.end:

	; OCW2
	mov al, 00100000b		; Bit 5 is set: End Of Interrupt (EOI) request
	out PIC_PRIMARY_ADDR, al

	pop ds
	pop cx
	pop bx
	pop ax
	iret

; TODO: write keyboard error handler (this is a stub)
keyb_error:
	mov al, 'E'
	call print_char
	hlt
	jmp keyb_error

; DATA
io_keyb_buf_begin		dw kernel_addr + kernel_sys_size_in_bytes
io_keyb_buf_end			dw kernel_addr + kernel_sys_size_in_bytes + KEYB_BUF_MAX
io_keyb_buf_next_pointer	dw kernel_addr + kernel_sys_size_in_bytes
io_keyb_buf_first_pointer	dw kernel_addr + kernel_sys_size_in_bytes
io_keyb_buf_count		dw 0x0000
io_keyb_buf_max			dw KEYB_BUF_MAX

; CONSTANTS
KEYB_ENCODER		equ	0x60
KEYB_CONTROLLER		equ	0x64
KEYB_BUF_MAX		equ	2048
