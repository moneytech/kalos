# Boot
## Kalos Bootloader
This is the piece of code that loads our operating system.
It loads IO.SYS and KALOS.SYS from the FAT12 formatted floppy, and then after setting the segment registers, it jumps to IO.SYS.
