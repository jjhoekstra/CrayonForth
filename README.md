# CrayonForth
A stand-alone, subroutine-threaded, Forth for the Raspberry pi3. 

This program is in no way a finalised program, not even version 0.1a. But if you want to give it a try, be my guest. Just copy the three files tp a directory on a Raspberry 3 and run the mymake macro. An image will be created which can be used instead of the kernel.img file of a Raspberry pi3. Presently it only runs a Forth-program included in the source-code. It will only run on a pi3, as it relies on armv8 specific opcodes and MMU-management.
