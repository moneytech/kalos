; keyb.s
; The keyboard driver
; The keyboard can operate in two modes: raw and cooked.
; Raw mode is the default: the scancodes sent by the keyboard are saved in a buffer, and the user can access them directly.
; Cooked mode requires a keyboard layout to be set by the user. In this mode, scancodes are converted to single byte keycodes.
; These keycodes are stored in a buffer and the user can (if he needs them) still access them.
; However, the advatage of cooked mode is that the driver can convert the keycodes directly to printable characters, and keep track of modifier keys.
; TODO: Implement cooked mode


; INITIALIZATION
; init_keyb subroutine begin
; TODO: The encoder uses set 2 for scancodes. Decide whether we want the controller to translate it to set 1 or not.
;	It's probably better to use set 2
init_keyb:
	; This code is long and ugly. Before every read or write operation we have to check if the keyboard controller is ready.
	; We disable the keyboard not to get disturbed, do some tests, set the keyboard in default mode and enable it back.
	; We also disable the mouse permanently.

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

	; Install new interrupt handler
	xor bx, bx						; The code segment is 0x0000
	mov word [4 * (PIC_IRQ0_INT+1)], irq_1_keyboard		; Offset
	mov word [4 * (PIC_IRQ0_INT+1) + 2], bx			; Segment

	; Enable interrupt in PIC
	in al, PIC_PRIMARY_DATA		; Read PIC interrupt mask byte
	and al, 11111101b		; Clear bit 1 (enable keyboard interrupts)
	out PIC_PRIMARY_DATA, al	; Write back interrupt mask byte

	ret
; init_keyb subroutine end



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

; keyb_error subroutine begin
; TODO: write keyboard error handler (this is a stub)
keyb_error:
	mov al, 'E'
	call print_char
	hlt
	jmp keyb_error
; keyb_error subroutine end



; HARDWARE INTERRUPT HANDLER
; TODO: Consider whether it is a good idea to call the original bios interrupt before returning
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
;	push ax
;	push bx
;	push cx
;	push dx
;	push di
;	push si
;	call print_hex
;	mov al, ' '
;	call print_char
;	mov al, ' '
;	call print_char
;	call update_hw_cursor
;	pop si
;	pop di
;	pop dx
;	pop cx
;	pop bx
;	pop ax

	mov bl, [keyb_mode]
	test bl, bl
	jnz .cooked

.raw:
	; This is the actual handler
	; Our keyboard buffer is a circular buffer.
	; The position of the next scancode to write to the buffer is pointed by keyb_buf_pointer. This is where we store our scancode.
	; Then, we have to increment it. If after incrementing it we are at the top of the buffer, we make it point back to the beginning of the buffer.
	; Finally, we update the count of scancodes in the buffer.
	; If it has reached maximum, we don't increment it, and we also advance the first scancode pointer (since we have already overwritten the first scancode).
	; This means that if the input is not asked for some times, the keys pressed most early will be ignored and discarded (unfortunately, we don't have infinite memory).
	; The handler only stores the scancodes in the buffer! It doesn't do any kind of translation.
	; The translation to ascii or to more manageable keycodes will be done by the kernel when a program asks for keyboad input through a system call.
	; This is because a IRQ handler should be as short and fast as possible.
	; TODO: decide the proper size of the buffer (it is saved in io.inc). For now, it is 2048 scancodes, which means approximately 682 key presses and releases.

	; Store the input scancode
	mov bx, [keyb_buf_next_pointer]
	mov [bx], al

	; Take care of the last scancode pointer
	inc bx					; Increment the pointer
	cmp bx, [keyb_buf_end]			; The buffer is circular, so we can't simply increment it
	jl .next_not_end_of_buffer		; If it's below the end of buffer, we keep it as it is
.next_end_of_buffer:
	mov bx, [keyb_buf_begin]		; Otherwise, we circle back to the beginning of the buffer
.next_not_end_of_buffer:
	mov [keyb_buf_next_pointer], bx	; In any case, store back the pointer

	; Now take care of the scancodes count (and of the first scancode pointer in case the buffer is full)
	mov cx, [keyb_buf_count]
	cmp cx, [keyb_buf_max]		; Check if the buffer is full
	jne .max_not_reached			; If it's not, jump
.max_reached:					; If it is, it means that the first scancode has been overwritten by the last. We have to increment the first pointer
	mov bx, [keyb_buf_first_pointer]
	inc bx					; Increment the pointer
	cmp bx, [keyb_buf_end]		; The buffer is circular, so we can't simply increment it
	jl .first_not_end_of_buffer		; If it's below the end of buffer, we keep it as it is
.first_end_of_buffer:
	mov bx, [keyb_buf_begin]		; Otherwise, we circle back to the beginning of the buffer
.first_not_end_of_buffer:
	mov [keyb_buf_first_pointer], bx	; The first scancode is now the first one after the last read
	jmp .end				; Then we jump because we don't want to increment the scancodes count (we are already full)

.max_not_reached:
	inc cx
	mov [keyb_buf_count], cx		; Save incremented count
	jmp .end

	; TODO: Implement this
.cooked:

.end:

	; OCW2
	mov al, 00100000b		; Bit 5 is set: End Of Interrupt (EOI) request
	out PIC_PRIMARY_ADDR, al

	pop ds
	pop cx
	pop bx
	pop ax
	iret


; SOFTWARE INTERRUPT HANDLERS
; set_keyb_mode subroutine begin
; Input:	bl = 0 for raw mode, bl = 1 for cooked mode
set_keyb_mode:
	mov [keyb_mode], bl
	ret
; set_keyb_mode subroutine end

; get_keyb_code subroutine begin
; In raw mode, return scancodes.
; In cooked mode, return keycodes
; The subroutine is the same since scancodes and keycodes are saved the same way and in the same place.
; Output: cl = aa if buffer is empty, otherwise cl = oldest code in buffer
get_keyb_code:
	mov ax, [keyb_buf_count]
	test ax, ax			; Check if buffer is empty.
	je .empty_buffer		; If it is, jump to error handling
	dec ax				; Otherwise, decrease count of scancodes in buffer by one
	mov [keyb_buf_count], ax

	mov bx, [keyb_buf_first_pointer]
	mov cl, [bx]			; Put scancode to cl
	inc bx				; Point to next scancode
	cmp bx, [keyb_buf_end]		; Check if we are at the end of the buffer
	jl .not_end_of_buffer		; If we ar not, we are ok
.end_of_buffer:				; If we are, we need to wrap around our circular buffer
	mov bx, [keyb_buf_begin]
.not_end_of_buffer:			; Finally, save the new location of the first available scancde
	mov [keyb_buf_first_pointer], bx
	ret
.empty_buffer:
	mov cl, 0xaa			; 0xaa is sent by keyboard only when performing a self test.
	ret				;  Because of this, it should be safe to use as error value, since it can't be a scancode.
; get_keyb_code subroutine end

; DATA
keyb_buf_begin		dw kernel_addr + kernel_sys_size_in_bytes
keyb_buf_end		dw kernel_addr + kernel_sys_size_in_bytes + KEYB_BUF_MAX
keyb_buf_next_pointer	dw kernel_addr + kernel_sys_size_in_bytes
keyb_buf_first_pointer	dw kernel_addr + kernel_sys_size_in_bytes
keyb_buf_count		dw 0x0000
keyb_buf_max		dw KEYB_BUF_MAX
keyb_mode		db 0x00

; CONSTANTS
KEYB_ENCODER		equ	0x60
KEYB_CONTROLLER		equ	0x64
KEYB_BUF_MAX		equ	2048

; SCANCODE TO KEYCODE TABLE
scancode_to_keycode:
.0x00	db K_UNDEF
.0x01	db K_F9
.0x02	db K_UNDEF
.0x03	db K_F5
.0x04	db K_F3
.0x05	db K_F1
.0x06	db K_F2
.0x07	db K_F12
.0x08	db K_UNDEF
.0x09	db K_F10
.0x0a	db K_F8
.0x0b	db K_F6
.0x0c	db K_F4
.0x0d	db K_TAB
.0x0e	db K_GRAVE
.0x0f	db K_UNDEF
.0x10	db K_UNDEF
.0x11	db K_LALT
.0x12	db K_LSHIFT
.0x13	db K_UNDEF
.0x14	db K_LCTRL
.0x15	db K_Q
.0x16	db K_1
.0x17	db K_UNDEF
.0x18	db K_UNDEF
.0x19	db K_UNDEF
.0x1a	db K_Z
.0x1b	db K_S
.0x1c	db K_A
.0x1d	db K_W
.0x1e	db K_2
.0x1f	db K_UNDEF
.0x20	db K_UNDEF
.0x21	db K_C
.0x22	db K_X
.0x23	db K_D
.0x24	db K_E
.0x25	db K_4
.0x26	db K_3
.0x27	db K_UNDEF
.0x28	db K_UNDEF
.0x29	db K_SPACE
.0x2a	db K_V
.0x2b	db K_F
.0x2c	db K_T
.0x2d	db K_R
.0x2e	db K_5
.0x2f	db K_UNDEF
.0x30	db K_UNDEF
.0x31	db K_N
.0x32	db K_B
.0x33	db K_H
.0x34	db K_G
.0x35	db K_Y
.0x36	db K_6
.0x37	db K_UNDEF
.0x38	db K_UNDEF
.0x39	db K_UNDEF
.0x3a	db K_M
.0x3b	db K_J
.0x3c	db K_U
.0x3d	db K_7
.0x3e	db K_8
.0x3f	db K_UNDEF
.0x40	db K_UNDEF
.0x41	db K_COMMA
.0x42	db K_K
.0x43	db K_I
.0x44	db K_O
.0x45	db K_0
.0x46	db K_9
.0x47	db K_UNDEF
.0x48	db K_UNDEF
.0x49	db K_DOT
.0x4a	db K_SLASH
.0x4b	db K_L
.0x4c	db K_SEMICOLON
.0x4d	db K_P
.0x4e	db K_MINUS
.0x4f	db K_UNDEF
.0x50	db K_UNDEF
.0x51	db K_UNDEF
.0x52	db K_APOSTROPHE
.0x53	db K_UNDEF
.0x54	db K_LSQ_BRACKET
.0x55	db K_EQUAL
.0x56	db K_UNDEF
.0x57	db K_UNDEF
.0x58	db K_CAPS
.0x59	db K_RSHIFT
.0x5a	db K_ENTER
.0x5b	db K_RSQ_BRACKET
.0x5c	db K_UNDEF
.0x5d	db K_BACKSLASH
.0x5e	db K_UNDEF
.0x5f	db K_UNDEF
.0x60	db K_UNDEF
.0x61	db K_EU
.0x62	db K_UNDEF
.0x63	db K_UNDEF
.0x64	db K_UNDEF
.0x65	db K_UNDEF
.0x66	db K_BKSP
.0x67	db K_UNDEF
.0x68	db K_UNDEF
.0x69	db K_KP_1
.0x6a	db K_UNDEF
.0x6b	db K_KP_4
.0x6c	db K_KP_7
.0x6d	db K_UNDEF
.0x6e	db K_UNDEF
.0x6f	db K_UNDEF
.0x70	db K_KP_0
.0x71	db K_KP_DOT
.0x72	db K_KP_2
.0x73	db K_KP_5
.0x74	db K_KP_6
.0x75	db K_KP_8
.0x76	db K_ESC
.0x77	db K_NUM
.0x78	db K_F11
.0x79	db K_KP_PLUS
.0x7a	db K_KP_3
.0x7b	db K_KP_MINUS
.0x7c	db K_KP_STAR
.0x7d	db K_KP_9
.0x7e	db K_SCROLL
.0x7f	db K_UNDEF
.0x80	db K_UNDEF
.0x81	db K_UNDEF
.0x82	db K_UNDEF
.0x83	db K_F7

extended_scancode_to_keycode:
.0x00	db K_UNDEF
.0x01	db K_UNDEF
.0x02	db K_UNDEF
.0x03	db K_UNDEF
.0x04	db K_UNDEF
.0x05	db K_UNDEF
.0x06	db K_UNDEF
.0x07	db K_UNDEF
.0x08	db K_UNDEF
.0x09	db K_UNDEF
.0x0a	db K_UNDEF
.0x0b	db K_UNDEF
.0x0c	db K_UNDEF
.0x0d	db K_UNDEF
.0x0e	db K_UNDEF
.0x0f	db K_UNDEF
.0x10	db K_UNDEF
.0x11	db K_RALT
.0x12	db K_UNDEF
.0x13	db K_UNDEF
.0x14	db K_RCTRL
.0x15	db K_UNDEF
.0x16	db K_UNDEF
.0x17	db K_UNDEF
.0x18	db K_UNDEF
.0x19	db K_UNDEF
.0x1a	db K_UNDEF
.0x1b	db K_UNDEF
.0x1c	db K_UNDEF
.0x1d	db K_UNDEF
.0x1e	db K_UNDEF
.0x1f	db K_LGUI
.0x20	db K_UNDEF
.0x21	db K_UNDEF
.0x22	db K_UNDEF
.0x23	db K_UNDEF
.0x24	db K_UNDEF
.0x25	db K_UNDEF
.0x26	db K_UNDEF
.0x27	db K_RGUI
.0x28	db K_UNDEF
.0x29	db K_UNDEF
.0x2a	db K_UNDEF
.0x2b	db K_UNDEF
.0x2c	db K_UNDEF
.0x2d	db K_UNDEF
.0x2e	db K_UNDEF
.0x2f	db K_UNDEF
.0x30	db K_UNDEF
.0x31	db K_UNDEF
.0x32	db K_UNDEF
.0x33	db K_UNDEF
.0x34	db K_UNDEF
.0x35	db K_UNDEF
.0x36	db K_UNDEF
.0x37	db K_UNDEF
.0x38	db K_UNDEF
.0x39	db K_UNDEF
.0x3a	db K_UNDEF
.0x3b	db K_UNDEF
.0x3c	db K_UNDEF
.0x3d	db K_UNDEF
.0x3e	db K_UNDEF
.0x3f	db K_UNDEF
.0x40	db K_UNDEF
.0x41	db K_UNDEF
.0x42	db K_UNDEF
.0x43	db K_UNDEF
.0x44	db K_UNDEF
.0x45	db K_UNDEF
.0x46	db K_UNDEF
.0x47	db K_UNDEF
.0x48	db K_UNDEF
.0x49	db K_UNDEF
.0x4a	db K_KP_SLASH
.0x4b	db K_UNDEF
.0x4c	db K_UNDEF
.0x4d	db K_UNDEF
.0x4e	db K_UNDEF
.0x4f	db K_UNDEF
.0x50	db K_UNDEF
.0x51	db K_UNDEF
.0x52	db K_UNDEF
.0x53	db K_UNDEF
.0x54	db K_UNDEF
.0x55	db K_UNDEF
.0x56	db K_UNDEF
.0x57	db K_UNDEF
.0x58	db K_UNDEF
.0x59	db K_UNDEF
.0x5a	db K_KP_ENTER
.0x5b	db K_UNDEF
.0x5c	db K_UNDEF
.0x5d	db K_UNDEF
.0x5e	db K_UNDEF
.0x5f	db K_UNDEF
.0x60	db K_UNDEF
.0x61	db K_UNDEF
.0x62	db K_UNDEF
.0x63	db K_UNDEF
.0x64	db K_UNDEF
.0x65	db K_UNDEF
.0x66	db K_UNDEF
.0x67	db K_UNDEF
.0x68	db K_UNDEF
.0x69	db K_END
.0x6a	db K_UNDEF
.0x6b	db K_LARROW
.0x6c	db K_HOME
.0x6d	db K_UNDEF
.0x6e	db K_UNDEF
.0x6f	db K_UNDEF
.0x70	db K_INS
.0x71	db K_DEL
.0x72	db K_DARROW
.0x73	db K_UNDEF
.0x74	db K_RARROW
.0x75	db K_UARROW
.0x76	db K_UNDEF
.0x77	db K_UNDEF
.0x78	db K_UNDEF
.0x79	db K_UNDEF
.0x7a	db K_PGDN
.0x7b	db K_UNDEF
.0x7c	db K_UNDEF
.0x7d	db K_PGUP


; KEYCODE DEFINITIONS
; These name refer to the US layout and are used merely to identify the keys.
; A keyboard layout can change the meaning of each of these.
K_F1		equ 0x00
K_F2		equ 0x01
K_F3		equ 0x02
K_F4		equ 0x03
K_F5		equ 0x04
K_F6		equ 0x05
K_F7		equ 0x06
K_F8		equ 0x07
K_F9		equ 0x08
K_F10		equ 0x09
K_F11		equ 0x0a
K_F12		equ 0x0b
K_0		equ 0x0c
K_1		equ 0x0d
K_2		equ 0x0e
K_3		equ 0x0f
K_4		equ 0x10
K_5		equ 0x11
K_6		equ 0x12
K_7		equ 0x13
K_8		equ 0x14
K_9		equ 0x15
K_A		equ 0x16
K_B		equ 0x17
K_C		equ 0x18
K_D		equ 0x19
K_E		equ 0x1a
K_F		equ 0x1b
K_G		equ 0x1c
K_H		equ 0x1d
K_I		equ 0x1e
K_J		equ 0x1f
K_K		equ 0x20
K_L		equ 0x21
K_M		equ 0x22
K_N		equ 0x23
K_O		equ 0x24
K_P		equ 0x25
K_Q		equ 0x26
K_R		equ 0x27
K_S		equ 0x28
K_T		equ 0x29
K_U		equ 0x2a
K_V		equ 0x2b
K_W		equ 0x2c
K_X		equ 0x2d
K_Y		equ 0x2e
K_Z		equ 0x2f
K_GRAVE		equ 0x30
K_SPACE		equ 0x31
K_COMMA		equ 0x32
K_DOT		equ 0x33
K_SLASH		equ 0x34
K_SEMICOLON	equ 0x35
K_MINUS		equ 0x36
K_APOSTROPHE	equ 0x37
K_LSQ_BRACKET	equ 0x38
K_RSQ_BRACKET	equ 0x39
K_BACKSLASH	equ 0x3a
K_EQUAL		equ 0x3b
K_TAB		equ 0x3c
K_INS		equ 0x3d
K_DEL		equ 0x3e
K_BKSP		equ 0x3f
K_ESC		equ 0x40
K_ENTER		equ 0x41
K_LGUI		equ 0x42
K_RGUI		equ 0x43
K_LALT		equ 0x44
K_RALT		equ 0x45
K_LSHIFT	equ 0x46
K_RSHIFT	equ 0x47
K_LCTRL		equ 0x48
K_RCTRL		equ 0x49
K_CAPS		equ 0x4a
K_NUM		equ 0x4b
K_SCROLL	equ 0x4c
K_HOME		equ 0x4d
K_END		equ 0x4e
K_UARROW	equ 0x4f
K_DARROW	equ 0x50
K_RARROW	equ 0x51
K_LARROW	equ 0x52
K_PGDN		equ 0x53
K_PGUP		equ 0x54
K_KP_0		equ 0x55
K_KP_1		equ 0x56
K_KP_2		equ 0x57
K_KP_3		equ 0x58
K_KP_4		equ 0x59
K_KP_5		equ 0x5a
K_KP_6		equ 0x5b
K_KP_7		equ 0x5c
K_KP_8		equ 0x5d
K_KP_9		equ 0x5e
K_KP_DOT	equ 0x5f
K_KP_SLASH	equ 0x60
K_KP_PLUS	equ 0x61
K_KP_MINUS	equ 0x62
K_KP_STAR	equ 0x63
K_KP_ENTER	equ 0x64
K_EU		equ 0x65
K_UNDEF		equ 0xff
