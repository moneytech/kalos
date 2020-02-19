# kalos
KalOS: a simple real mode 8086 operating system

To setup the floppy image, use `make init` (you may need to change the Makefile if mkfs is not installed in `/sbin`).
To assemble and put the executables on the floppy image, use `make all`.
To try the os, you can use `qemu-system-i386 -fda disk.img`.

TODO: Write the README
