; floppy.s
; The floppy driver
; TODO: The floppy is ugly to program. I decided to create a stub that uses the BIOS interrupts to interact with the floppy.
;	This is not optimal, but I prefer to focus on other things before.

; INITIALIZATION
; init_floppy subroutine begin
; TODO: This is just a stub.
init_floppy:
	; Save current drive number
	mov dl, [0x7c24]
	mov [current_drive], dl
	ret
; init_floppy subroutine end



; SUBROUTINES
; lba_to_chs subroutine begin
; Take an LBA address and convert it to CHS setting up the registers for an int 13 call
; TODO: Properly document the subroutine
; TODO: Check if the high bits of cylinder are used in floppy disks
; Input:        cx = LBA address
; Output:       dh, cx ready for int 13
; Modified registers:   ax, bx, cx, dx
lba_to_chs:
	mov ax, cx
	mov bx, head_count * sectors_per_track
	xor dx, dx
	div bx
	mov ch, al
	mov cl, 6
	shr ah, cl
	mov cl, ah

	mov ax, dx	; Remainder of division
	xor dx, dx
	mov bx, sectors_per_track
	div bx		; We use the memory location and not the constant because div imm is not permitted
	add cx, dx	; Remainder
	inc cx
	mov dh, al
        ret
; lba_to_chs end



; SOFTWARE INTERRUPT HANDLERS

; read_sector subroutine begin
; TODO: Implement without BIOS
; Input:	cx = LBA address
;		dl = drive
;		es:bx = data buffer
; Modified registers: ax, bx, cx, dh
read_sector:
	push bx		; Save buffer
	push dx		; Save drive
	call lba_to_chs
	pop bx
	mov dl, bl	; Restore drive
	pop bx		; Restore buffer
	mov ah, 0x02	; Read sectors
	mov al, 1	; Number of sectors to read
	int 0x13
	ret
; read_sector subroutine end

; wait_floppy_operation_terminated subroutine begin
; This is a stub. Our read_sector subroutine is blocking, so there's nothing to wait for.
; When we will have a non-blocking floppy io subroutine, this subroutine will be useful.
wait_floppy_operation_terminated:
	ret
; wait_floppy_operation_terminated subroutine end

; get_current_drive subroutine begin
; Output:	dl = current drive
get_current_drive:
	mov dl, [current_drive]
	ret
; get_current_drive subroutine end


; DATA
current_drive	db	0x00
