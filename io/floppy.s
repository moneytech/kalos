; floppy.s
; The floppy driver
; TODO: The floppy is ugly to program. I decided to create a stub that uses the BIOS interrupts to interact with the floppy.
;	This is not optimal, but I prefer to focus on other things before.

; INITIALIZATION
; init_floppy subroutine begin
; TODO: This is just a stub.
init_floppy:
	; Do nothing
	ret
; init_floppy subroutine end


; SOFTWARE INTERRUPT HANDLERS

; read_sector subroutine begin
; TODO: Implement without BIOS
; Input:	ch = cylinder, cl = sector
;		dh = head, dl = drive
;		es:bx = data buffer
; Modified registers: ax
read_sector:
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
