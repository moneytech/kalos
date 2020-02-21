# kalos
KalOS: a simple real mode 8086 operating system

KalOS is an operating system for the late IBM and IBM-compatible Personal Computer.
It aims at being simple, and takes inspiration from OSs from the time (like DOS and CP/M).

## System requirements
KalOS aims at being compatible with PCs from as early as the 90s, while being able to run on modern computers.
Of course, some requirements are needed:
* VGA-compatible card
* 2 8259A PIC (or compatible) microcontrollers
* A PS/2-compatible keyboard
* more will be added as the developement continues

## Build instructions
To build the executables, `nasm` is required. To move them to the floppy image, `mtools` are required.

The system is to be installed on a 3.5 inches, 1440kB, FAT12-formatted floppy disks.
To setup the floppy image, use `make init` (you may need to change the Makefile if mkfs is not installed in `/sbin`).
To assemble and put the executables on the floppy image, use `make all`.
To try the os, you can use `qemu-system-i386 -fda disk.img`.
