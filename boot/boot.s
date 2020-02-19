; boot.s ----------------------------------------------------------------------
; The bootloader of our os
; This executable is stored in the first sector of our floppy disk.
; It starts (after a jump) with the Bios Parameter Block, which is standardized in the FAT12 and contains useful information
; about the geometry of the disk and the FAT.
; It sets up a stack, prepares the data segments and then loads the FAT and the root directory.
; Afterwards, it looks for two files in the root directory: IO.SYS and KALOS.SYS
; If it finds both of them, it loads them after the root directory and then transfers execution to IO.SYS, also setting up the code segment.
; Otherwise, it prints an error message and halts the cpu


; NASM DIRECTIVES -------------------------------------------------------------
; This file contains basic constants about the geometry of our standard floppy disk
; and about our FAT12. Most of them appear also in the BPB few lines down here.
; Their purpose is however different: those in the BPB are necessary because of the FAT standard.
; Their constant counterpart is useful to do calculations at assembly-time rather than at run-time,
; allowing us to save space thet would be used by arithmetic instruction (we have to stay below 512 bytes).
; The hard coded value is stored in the geometry file, so that is the one to change if the OS is ported
; to another floppy format (very unlikely to happen).
%include 'geometry.s'

[bits 16]	; The pc starts in real mode
[cpu 8086]	; And we don't want to use i386 instructions (bootloaders usually assume to be on a 8086, and it's more fun)
[org bootloader_addr]	; We set the data segment registers to zero, therefore we have to instruct nasm about the offset of our labels


; JUMP TO START ---------------------------------------------------------------
; Dummy jump instruction as required by the FAT standard
	jmp start
	nop

; BIOS PARAMETER BLOCK --------------------------------------------------------
bpb_oem_name		db	'KALOS   '
bpb_bytes_per_sector	dw	bytes_per_sector
bpb_sectors_per_cluster	db	sectors_per_cluster
bpb_reserved_sectors	dw	reserved_sectors
bpb_number_of_fats	db	number_of_fats
bpb_root_entries	dw	root_entries
bpb_total_sectors	dw	total_sectors
bpb_media_descriptor	db	media_descriptor
bpb_sectors_per_fat	dw	sectors_per_fat
bpb_sectors_per_track	dw	sectors_per_track
bpb_head_count		dw	head_count
bpb_hidden_sectors	dd	hidden_sectors
bpb_large_sector_count	dd	large_sector_count

; FAT Extended boot record
bpb_drive_number	db	drive_number
bpb_reserved		db	reserved
bpb_signature		db	signature
bpb_volume_id		dd	0x00000000
bpb_volume_label	db	'KALOS      '
bpb_type		db	'FAT12   '


; MAIN CODE -------------------------------------------------------------------
start:
	; TODO: Understand when we can reenable interrupts (other bootloaders seem to do it pretty soon)
	cli	; Do not disturb, please

	; Set ds and es segments to 0x07c0 (where we are loaded)
	xor ax, ax
	mov ds, ax
	mov es, ax

	; Set stack segment to 0x0000 and stack pointer to ss:0x7c00
	; This is just below our bootloader.
	; It makes sense, since we are going to load the os code and data at 0x0000:0x0500
	; TODO: Not sure if this is the best position, I will decide it after designing the kernel
	mov ss, ax		; ax is already zero
	mov sp, 0x7c00

	; Save drive number for future use
	mov [bpb_drive_number], dl

; Locate the File Allocation Table and load it at the beginning of the heap
load_fat:
	mov ax, start_of_fat	; LBA of first sector to read
	mov cx, sectors_per_fat	; Number of sectors to read
	mov bx, os_base_addr		; Where to load it

	call read_sectors
	mov di, io_err
	jc err		; CF is set in case of error

; Locate the root directory
load_root:
	mov ax, start_of_root	; LBA of first sector to read
	mov cx, root_size_in_sectors

	; Load the root directory after the FAT.
	mov bx, os_base_addr + sectors_per_fat * bytes_per_sector
	mov [root], bx

	call read_sectors
	mov di, io_err
	jc err		; CF is set in case of error

; Search for the os basic io routines
search_io_sys:
	mov di, io_filename		; Name of the file to search for
	call search_file
	mov ax, [si]			; Save location of first file cluster in FAT
	mov [io_first_cluster], ax	; using ax because mov mem, mem is forbidden
	mov di, os_err
	jc err				; CF is set in case of error
	
; Search for the kernel
search_kalos_sys:
	mov di, kernel_filename	; Name of the file to search forame
	call search_file
	mov ax, [si]			; Save location of first file cluster in FAT
	mov [kernel_first_cluster], ax	; using ax because mov mem, mem is forbidden
	mov di, os_err
	jc err				; CF is set in case of error

; Load io file
	; Calculate where to load file
	mov bx, [root]
	add bx, root_size_in_bytes

	mov dx, [io_first_cluster]	; First cluster of file in FAT
	call load_file
	mov di, io_err
	jc err				; CF is set in case of error

; Load kernel
	; bx is already set after the previous call to load_file
	; to point to the sector after the previous loaded file.
	; This is where we want to load the new file, so we don't touch it
	mov dx, [kernel_first_cluster]	; First cluster of file in FAT
	call load_file
	mov di, io_err
	jc err				; CF is set in case of error

; Jump to IO.SYS
	; TODO: It is probably better to set cs to 0x0x7c0, so that the kernel can simply access the BPB
	; Store the absolute address of the IO.SYS file to ax

	xor ax, ax
	push ax
	mov ax, [root]
	add ax, root_size_in_bytes
	push ax
	retf

	mov di, ret_err	; Just in case
err:
	call print_str	; di is supposed to be already set by the caller
	hlt		; At this point there aren't many things we can do, so we stop


; SUBROUTINES -----------------------------------------------------------------

; print_str subroutine begin
; Print 0-terminated string
; Input:	di = Address of 0-terminated string to print
; Modified registers:	di, ax, bh
print_str:
	mov ah, 0x0e	; int 10,e is teletype output
	xor bh, bh	; write on page 0
.loop:
	mov al, [di]	; Character to print
	test al, al
	jz .loop_end	; If the character is 0, we are done
	int 0x10	; Otherwise, print it
	inc di		; Next character, please
	jmp .loop

.loop_end:
	ret
; print_str end


; lba_to_chs subroutine begin
; Take an LBA address and convert it to CHS setting up the registers for an int 13 call
; TODO: Properly document the subroutine
; TODO: Check if the high bits of cylinder are used in floppy disks
; Input:        ax = LBA address
; Output:       dx, cx ready for int 13
; Modified registers:   ax, bx, cx, dx
lba_to_chs:
	mov bx, head_count * sectors_per_track
	xor dx, dx
	div bx
	mov ch, al
	mov cl, 6
	shr ah, cl
	mov cl, ah

	mov ax, dx	; Remainder of division
	xor dx, dx
	div word [bpb_sectors_per_track]	; We use the memory location and not the constant because div imm is not permitted
	add cx, dx	; Remainder
	inc cx
	mov dh, al
	
	; Drive number
        mov dl, [bpb_drive_number]
        ret
; lba_to_chs end


; read_sectors subroutine begin
; Read a number of sectors starting from a LBA address and write them to a memory location.
; This subroutine is needed because int 13,2 can't be used if the sectors to read across atrack boundaries.
; Because of this, we read sectors one by one.
; TODO: Retry the reading at least three times resetting the controller with int 13,0 each time
; TODO: Get documented on the bugs (some bioses don't set the carry flag properly)
; Input:        ax = LBA address, cx = number of sectors to read, bx = offset to load the sectors to
; Output:       CF = 1 if error, CF = 0 if ok
; Modified registers:   ax, bx, cx, dx
read_sectors:
	push ax				; Preserve LBA address
	push cx				; Preserve count of remaining sectors to read
	push bx				; Preserve location to put sectors
	call lba_to_chs
	pop bx				; Restore location to write sectors to

	mov ah, 0x2			; int 13,2 loads sectors
	mov al, 1			; Number of sectors to read (one) 
	int 0x13

	jc .end				; Disk error
	pop cx				; Restore the remaining sectors count
	pop ax				; And the LBA address
	inc ax				; Next sector on floppy
	add bx, bytes_per_sector	; And next sector in ram
	loop read_sectors		; If there are still sectors to read, loop back
.end:
	ret				; CF should already be set suitably
; read_sectors subroutine end


; search_file subroutine begin
; Search for a file entry in the root directory (located at address os_base_addr)
; Input:	di = Filename to search
; Output:	If file is found: CF = 0, si = Pointer to the number of first cluster of file
;		Otherwise: CF = 1
; Modified registers:	bx, cx, dx
search_file:
	cld				; We want to auto-increment when comparing strings
	mov ax, di			; Save di
	mov bx, [root]			; bx contains the root address
	mov si, bx			; Also si
	mov cx, root_entries		; cx contains the number of entries to check

.next_entry:
	xchg cx, dx			; Save number of remaining entries in dx
	mov cx, 11			; Number of bytes to compare (filenames are 11 bytes long)
	repe cmpsb			; Compare es:di (the filename) and ds:si (the root entry name)
	je .found			; File found (the strings are equal)

	add bx, 0x0020			; The next entry is 32 (0x0020) bytes next to the current one
	mov si, bx			; Restore the root directory address
	mov di, ax			; Restore di
	xchg cx, dx			; Go back to outer loop (entries)
	loop .next_entry		; If there are still entries, retry

.not_found:
	stc				; File not found
	ret

.found:
	clc				; Ok!
	add si, 15			; In a diretory entry, the first cluster of file is stored 15 bytes after the end of the filename
	ret
; search_file subroutine end


; load_file subroutine begin
; Loads a file at the specified address
; TODO: Properly document the subroutine
; Input:	dx = Offset of the file to load
;		bx = Location to load the file
; Output:	bx = First byte after the located file
; Modified registers:	ax, bx, cx, dx, si
load_file:
.load_sector:
	; First, calculate the address of the first cluster of the file.
	; Since the first two clusters of the FAT are reserved and do not correspond to actual data, we have
	; cluster start = (cluster number - 2) * secs_per_cluster + start of user data
	; In our case:
	mov ax, dx
	add ax, 31
	mov cx, 1
	push bx
	push dx
	call read_sectors
	pop dx
	pop bx

	jc .end

.calculate_next_cluster:
	; byte = cluster * 12 bits/cl / 8 bits/byte
	mov ax, 12
	mul word dx
	mov cx, 8
	div word cx	; ax contains the byte and dx the remainder (which can be 0 or 4)
	mov si, ax
	add si, os_base_addr

	test dx, dx
	jz .even

.odd:
	; if cluster is odd
	;	next_high = [byte+1]
	;	next_low = [byte] shr 4
	mov cl, 4
	mov dh, [si+1]
	mov dl, [si]
	shr dx, cl
	jmp .next_cluster_cont
.even:
	; else if cluster is even
	;	next = [byte] and 0x0fff
	mov dx, [si]
	and dx, 0x0fff

.next_cluster_cont:
	add bx, 512
	cmp dx, 0xff8
	jae .end
	jmp .load_sector

.end:
	ret

; load_file subroutine end


; DATA ------------------------------------------------------------------------
; TODO: Rationalize variables containing memory locations (i.e. root)
io_filename		db "IO      SYS"
kernel_filename		db "KALOS   SYS"
root			dw 0x0000
io_first_cluster	dw 0x0000
kernel_first_cluster	dw 0x0000
io_err			db 'IO error',0
os_err			db 'Operating System files not found',0
ret_err			db 0

times 510 - ($-$$)	db 0
dw 0xaa55	; Boot signature
