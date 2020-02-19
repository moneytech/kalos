%include "geometry.s"

[bits 16]
[cpu 8086]
[org kernel_sys_addr]

mov ah, 0x0e
mov al, 'K'
int 0x10

mov ah, 0x0e
mov al, 'E'
int 0x10

mov ah, 0x0e
mov al, 'R'
int 0x10

hlt
