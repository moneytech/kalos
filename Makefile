AS=nasm
DISK=disk.img

all: boot/boot io/io.sys kernel/kalos.sys config/config.sys
	mcopy -i "${DISK}" config/config.sys ::

boot/boot: boot/boot.s include/geometry.inc
	$(AS) -i include/  boot/boot.s -o boot/boot
	dd if=boot/boot of="${DISK}" conv=notrunc	# Copy bootloader to first floppy sector

io/io.sys: io/io.s io/vga.s io/pic.s io/keyb.s io/floppy.s include/geometry.inc
	$(AS) io/io.s -o io/io.sys
	mcopy -i "${DISK}" io/io.sys ::

kernel/kalos.sys: kernel/kalos.s kernel/fat.s kernel/term.s include/geometry.inc
	$(AS) -i include/  kernel/kalos.s -o kernel/kalos.sys
	mcopy -i "${DISK}" kernel/kalos.sys ::

clean:
	- rm boot/boot io/io.sys kernel/kalos.sys

init:
	/sbin/mkfs.msdos -C "${DISK}" 1440
