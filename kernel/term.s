; print_0_str subroutine begin
; Print 0-terminated string
; Input:	es:bx = Address of 0-terminated string to print
; Modified registers:	di, ax, bh
print_0_str:
.loop:
	mov al, [es:bx]	; Character to print
	test al, al
	jz .loop_end	; If the character is 0, we are done
	mov ah, 0x00
	push bx
	int 0x61	; Otherwise, print it (int 61,0 print_char)
	pop bx
	inc bx		; Next character, please
	jmp .loop

.loop_end:
	mov ah, 0x03
	int 0x61
	ret
; print_0_str end

; print_0_str_with_attr subroutine begin
; Print 0-terminated string using the current terminal attribute
; Input:	es:bx = Address of 0-terminated string to print
; Modified registers:	di, ax, bh
print_0_str_with_attr:
.loop:
	mov al, [es:bx]	; Character to print
	test al, al
	jz .loop_end	; If the character is 0, we are done
	mov ah, 0x01
	push bx
	mov bl, [current_attr]
	int 0x61	; Otherwise, print it (int 61,1 print_char_with_attr)
	pop bx
	inc bx		; Next character, please
	jmp .loop

.loop_end:
	mov ah, 0x03
	int 0x61
	ret
; print_0_str end

; print_str subroutine begin
; Print string
; Input:	es:bx = Address of 0-terminated string to print
;		cx: length of string
; Modified registers:	di, ax, bh
print_str:
.loop:
	mov al, [es:bx]	; Character to print
	mov ah, 0x00
	push bx
	push cx
	int 0x61	; Otherwise, print it (int 61,00 print_char)
	pop cx
	pop bx
	inc bx		; Next character, please
	loop .loop

.loop_end:
	mov ah, 0x03
	int 0x61
	ret
; print_str end

; print_str_with_attr subroutine begin
; Print string using the current terminal attribute
; Input:	es:bx = Address of 0-terminated string to print
;		cx: length of string
; Modified registers:	di, ax, bh
print_str_with_attr:
.loop:
	mov al, [es:bx]	; Character to print
	mov ah, 0x01
	push bx
	push cx
	mov bl, [current_attr]
	int 0x61	; Otherwise, print it (int 61,1 print_char_with_attr)
	pop cx
	pop bx
	inc bx		; Next character, please
	loop .loop

.loop_end:
	mov ah, 0x03
	int 0x61
	ret
; print_str_with_attr end

; set_current_attr subroutine begin
; Input:	bl = attribute to make current
set_current_attr:
	mov [current_attr], bl
	ret
; set_current_attr subroutine end

; DATA
current_attr	db 0x07
