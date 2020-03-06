; pic.s
; The PIC driver
; The PIC (Programmable Interrupt Controller) is a controller that allows various peripheral devices to ask the CPU to issue interrupts.
; A standard PC has two PICs. The primary is directly connected to the CPU. The secondary one is connected to the primary by the IRQ line 2


; INITIALIZATION
; init_pic subroutine begin
; This code initializes our two PICs (the primary and the secondary).
init_pic:
	; ICW 1: the first initialization control word to send to the PIC
	; Bits 7-5: must be 0
	; Bit 4: initialization bit, 1 since we are initializing the PIC
	; Bit 3: 0 is level triggered mode, 1 is edge triggered mode. We choose 1 (the default)
	; Bit 2: ignored by x86, so 0
	; Bit 1: 0 because there are multiple PICs in the system
	; Bit 0: 1 so we can send ICW4
	mov al, 00010001b
	out PIC_PRIMARY_ADDR, al	; Primary PIC command register
	out PIC_SECONDARY_ADDR, al	; Secondary PIC command register

	; ICW2: second initialization word, used to tell the PIC which interrupts it should trigger
	; The interrupt corresponding to IRQ0 (the byte we write to the data register) must be a multiple of 8.
	; We keep the default values of 0x08 and 0x70 (but we must write them anyway because during initialization the default gets overwritten).
	mov al, PIC_IRQ0_INT		; Primary PIC maps IRQ 0 to 7 starting from this interrupt
	out PIC_PRIMARY_DATA, al	; Primary PIC data register
	mov al, PIC_IRQ8_INT		; Secondary PIC maps IRQ 8 to 15 starting from this interrupt
	out PIC_SECONDARY_DATA, al	; Secondary PIC data register

	; ICW3: tell the PICs which IRQ line to use to comunicate between each other
	; The format for primary and secondary is different!
	mov al, 00000100b		; Primary PIC: bit 2 is set, so use IRQ2
	out PIC_PRIMARY_DATA, al	; Primary PIC data register
	mov al, 0x2			; Secondary PIC: the byte is 0x2, so use IRQ2
	out PIC_SECONDARY_DATA, al	; Secondary PIC data register

	; ICW4: extra information
	; Bits 5-7: reserved, must be 0
	; Bit 4: not supported on x86 (special fully nested mode)
	; Bit 3: buffered mode. We are not interested, so 0
	; Bit 2: only use if bit 3 is set, so 0
	; Bit 1: we also don't need this (automatic EOI on acknowledge pulse), so 0
	; Bit 0: if 1, 80x86 mode (what we want). If clear, MCS-80/86 mode
	mov al, 00000001b
	out PIC_PRIMARY_DATA, al
	out PIC_SECONDARY_DATA, al

	; OCW1: Operational Command Word 1
	; Disable all IRQs in both pics
	; We will enable each of them when we install the interrupt handler
	; Each bit corresponds to a different IRQ. 1 means IRQ disabled, 0 means IRQ enabled
	mov al, 11111111b		; Disable all 8 IRQs
	out PIC_PRIMARY_DATA, al	; Write to primary PIC Interrupt Mask Register
	out PIC_SECONDARY_DATA, al	; Write to secondary PIC Interrupt Mask Register

	; OCW3: Operational Command Word 3
	; Bit 7: reserved, must be 0
	; Bit 6: special mask mode (we don't want it, so 0)
	; Bit 5: mask mode (0 for normal)
	; Bits 4-3: 01b for OCW3
	; Bit 2: polling mode (we want interrupt mode, so 0)
	; Bits 1-0: which register to make available on address port (we are not interested, so 10b for IRR just because we have to choose one)
	mov al, 00001010b
	out PIC_PRIMARY_DATA, al
	out PIC_SECONDARY_DATA, al

	ret
; init_pic subroutine begin



; CONSTANTS
PIC_PRIMARY_ADDR	equ	0x20
PIC_PRIMARY_DATA	equ	0x21
PIC_IRQ0_INT		equ	0x08
PIC_SECONDARY_ADDR	equ	0xa0
PIC_SECONDARY_DATA	equ	0xa1
PIC_IRQ8_INT		equ	0x70
