# IO.SYS
## The input/output utilities of Kalos
IO.SYS is an executable file which initializes the I/O devices and provides drivers to use them.
It installs an interrupt (`0x61`) which gives access to I/O functions.
Finally, it jumps to the already loaded KALOS.SYS.
