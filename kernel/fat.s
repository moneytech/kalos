; search_file subroutine begin
; Search for a file entry in the root directory (located at address os_base_addr)
; Input:	es:bx = Filename to search
; Output:	If file is found: cl = 0, 0x0000:si = Pointer to the file directory entry
;		Otherwise: cl = 1
; Modified registers:	bx, cx, dx
search_file:
	cld				; We want to auto-increment when comparing strings
	mov di, bx			; Move filename to di (so that we can use cmpsb)
	mov ax, root_addr		; ax contains the root address
	mov si, ax			; Also si
	mov cx, root_entries		; cx contains the number of entries to check

.next_entry:
	xchg cx, dx			; Save number of remaining entries in dx
	mov cx, 11			; Number of bytes to compare (filenames are 11 bytes long)
	repe cmpsb			; Compare es:di (the filename) and ds:si (the root entry name)
	je .found			; File found (the strings are equal)

	add ax, 0x0020			; The next entry is 32 (0x0020) bytes next to the current one
	mov si, ax			; Restore the root directory address
	mov di, bx			; Restore di
	xchg cx, dx			; Go back to outer loop (entries)
	loop .next_entry		; If there are still entries, retry

.not_found:
	mov cl, 1			; File not found
	ret

.found:
	xor cl, cl			; Ok!
	sub si, 11			; Set si to the beginning of the directory entry
	ret
; search_file subroutine end


; load_file_synchronous subroutine begin
; Loads a file at the specified address
; This subroutine is synchronous: it returns only after finishing loading the file.
; I plan to create an asynchronous version later, after I implement the floppy driver (which is needet to use the DMA).
; TODO: Properly document the subroutine
; Input:	0x0000:si = Directory entry address for the file
;		es:bx = Location to load the file
; Modified registers:	ax, bx, cx, dx, si
load_file_synchronous:
	add si, 26
	mov dx, [si]
.load_sector:
	; First, calculate the address of the first cluster of the file.
	; Since the first two clusters of the FAT are reserved and do not correspond to actual data, we have
	; cluster start = (cluster number - 2) * secs_per_cluster + start of user data
	; In our case:
	mov cx, dx
	add cx, 31
	push bx
	push dx
	mov ah, 0x12
	int 0x61	; int 61,12: get_current_drive
	mov ah, 0x10
	int 0x61	; int 61,10: read_sector
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
	add si, fat_addr

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
	cmp dx, 0xff8
	jae .end
	add bx, 512
	jmp .load_sector

.end:
	ret	
; load_file_synchronous subroutine end
