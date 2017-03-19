
as -mcpu=cortex-a53  -mfpu=vfpv4 jh4th2ra.s -o jh4th2ra.o
ld -T lscript jh4th2ra.o -o jh4th2ra.elf
objcopy jh4th2ra.elf -O binary jh4th2ra.img

echo "compiling binary image for pi3 - done!"
