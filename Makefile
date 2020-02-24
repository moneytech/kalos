AS=nasm
DISK=disk.img

all: boot/boot os/io.sys os/kalos.sys

boot/boot: boot/boot.s include/geometry.inc
	$(AS) -i include/  boot/boot.s -o boot/boot
	dd if=boot/boot of="${DISK}" conv=notrunc	# Copy bootloader to first floppy sector

os/io.sys: os/io.s include/geometry.inc include/io.inc
	$(AS) -i include/  os/io.s -o os/io.sys
	mcopy -i "${DISK}" os/io.sys ::

os/kalos.sys: os/kalos.s include/geometry.inc
	$(AS) -i include/  os/kalos.s -o os/kalos.sys
	mcopy -i "${DISK}" os/kalos.sys ::

clean:
	- rm boot/boot os/io.sys os/kalos.sys

init:
	/sbin/mkfs.msdos -C "${DISK}" 1440
