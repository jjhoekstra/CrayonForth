
as -mcpu=cortex-a53  -mfpu=vfpv4 jh4th2ra.s -o jh4th2ra.o
ld -T lscript jh4th2ra.o -o jh4th2ra.elf
objcopy jh4th2ra.elf -O ihex jh4th2ra.hex

echo "compiling for pi3 - done!"
