; Video
video_buffer		equ	0xb800
cols			equ	80
lines			equ	25

; PIC
pic_primary_addr	equ	0x20
pic_primary_data	equ	0x21
pic_irq0_int		equ	0x08
pic_secondary_addr	equ	0xa0
pic_secondary_data	equ	0xa1
pic_irq8_int		equ	0x70

; Keyboard
keyb_encoder		equ	0x60
keyb_controller		equ	0x64
keyb_buf_max		equ	2048
