@  ***************  copyright J.J. Hoekstra 2017  **************  @
@  22/Feb/17, 17:06
@  
@  *********************** register model **********************{

@					r0				scratch 1
@					r1				scratch 2
@					r2				scratch 3
@					r3				scratch 4
@					r4				scratch 5
	top		.req	r5				@ top datastack
	fps		.req	r6				@ float stack pointer - alleen 64 bit floats!
	loopi	.req	r7				@ loop I - J en do via userstack
	looplim	.req	r8				@ loop limit - mn do loops en evt count down loops
	dts 	.req	r9				@ data stack pointer
	uss		.req	r10				@ user stack pointer
									@ r11 evt loop stack pointer, v helaas wel meer en meer in gebruik
	v		.req	r11 			@ second Forth working register - primitives use only
	w		.req	r12				@ classic Forth working register - primitives use only
@	sp  		    r13				return stack pointer
@	lr  		    r14				return link
@	pc  		    r15				feitelijk de instructie pointer}

@  ***********************  header flags  **********************{
    @.set HiddenFlag,		0x00}

@  *******************  MEMORY MAP en TASKS  *******************{
@ Define memory-map:
		.equ NO_CACHE_MEM,	0x3A000000			@ is 16 Mb onder de 64 Mb GPU memory #DEBUG: kijken of dit beter werkt
			.equ MAIL_DATA,	NO_CACHE_MEM+0x0	@ YEAH!!! Dit is waar de mailboxes heen schijven
@			.set FB_INFO,	NO_CACHE_MEM+0x1000	@ experimentele FrameBuffer locatie in non-cachable gebied - crasht. maar heeft gewerkt!

		.equ TASK_BLOCK,	0x39000000			@ 16 Mb voor 256 tasks a maximaal 64 k data per task
		.equ TEXT_BLOCK,	0x31000000			@ 8*16 = 128 Mb voor String-handling
		.equ TOP_DATA,		0x31000000			@ dit is eerste plek waar niet meer kan worden geschreven!

@ Define Taskblock - vectors naar de verschillende dingen per task
		.equ TaskContext,	0x0					@ 1024 bytes voor context, moet genoeg zijn
		.equ TaskPad,		0x0400				@ een userpad/data voor elke task - 4092 bytes - telt omhoog!
		@ hier 31744 bytes vrij voor user-variabelen voor een task
		.equ TaskDTS,		0x9F00				@ 8192 bytes voor datastack (telt naar beneden)
		.equ TaskUSS,		0xBF00				@ 8192 bytes voor userstack
		.equ TaskFPS,		0xDF00				@ 8192 bytes voor float point stack = 1024 values
		.equ TaskRTS,		0xFF00				@ 8192 bytes voor return-stack= 2048 values
		
		.equ TaskSize, 		16					@ een task neemt 2^16 bytes in taskblock in beslag - een task altijd macht 2 bytes
		
		.equ RetSt,			TASK_BLOCK+TaskRTS	@ werkt - stacks van task 0 nu in TaskBlock!!
		.equ DatSt,			TASK_BLOCK+TaskDTS
		.equ UseSt,			TASK_BLOCK+TaskUSS
		.equ FloSt,			TASK_BLOCK+TaskFPS

	@ TaskContext - vectors van de 'TaskTable'
		.equ MaxTask,		128					@ aantal tasks
		.equ TaskTableByte,	8					@ breedte van apart tabelletje buiten het TaskBlock

		.equ TaskCoreNo,	0					@ core0=main, core1=free, core2=free, core3=sound (8 bit-regs)
		.equ TaskClass,		1					@ class of task: 0=core, 1=event (ie: irq oid), 2=shared, 3=empty, 0xff=EOT (8 bit)
		.equ TaskStatus,	2					@ status of task: active=0, paused=1, to-be-switched-to-core=4,5,6,7 (8 bit)
												@ loop (die loopt op elke core apart): kijk of core jouw core is...
												@ zo ja: check TaskClass=event, zo nee: op naar volgende taak
												@ Check TaskClass: als core of shared: do corecheck of do slice-check, otherwise go to next task
												@ Als een ronde zonder activatie van een taak: put taskmanager to sleep till next interrupt. 		

		@.equ empty								@ vrije byte
		.equ TaskPrioCnt,	4					@ if zero: give a slice to task and reset to PrioSet-value; otherwise reduce by 1 (16 bit)
		.equ TaskPrioSet,	6					@ Set value of priority: 0 = highest prio, =run at every slice (16 bit)
												@ 1 run at every other slice, etc.
		
	@ de eerste 5, meest gebruiukte, woorden van het 'TASK_BLOCK' nu in een aparte tabel (nl: 'tasktable' ) ivm. caching - staat hier direct boven
		
		.equ TaskExToken,	16					@ 1 word => start van deze task bij new- or restart
		.equ TaskWinNo,		20					@ 1 word => window waar deze task print - 0 tot max_win of -1 (ie. do not print at a window)
		.equ TaskUARTen,	24					@ 1 word - true (ie. print at UART) or false (ie. do not print at UART)
		.equ TaskCPUState,	28					@ 4 (?) words = 16
		.equ emptyregs,		44					@ to align the next two at 64
		.equ TaskSavedRgx,	64					@ 16 words = 64 bytes lang
		.equ TaskSavedFpx,	128					@ 16 words = 64 bytes lang
		.equ TaskFromTaskM,	192					@ 2 words - message from task-manager - (?) Bijvoorbeeld:stop zodra mogelijk
		.equ TaskToTaskM,	200					@ 2 words - message to task-manager - (?) Bijvoorbeeld: 
		.equ TaskMessWrite,	208					@ 1 word - message write pointer van een 256 word circulaire buffer
		.equ TaskMessRead,	212					@ 1 word - message read pointer
		.equ TaskMessBuff,	216					@ 192 words - start message circulaire buffer van 192 words
		.equ TaskDataEnd,	984					@ einde circulaire buffer en einde van Taskdata
@ checked }
		
@  ********************  standard tasks etc  *******************{
@  dit zijn de tasks die permanent in het TaskBlock ( 'TASK_BLOCK' ) en in de 'TaskTable' zitten
		
		.equ TaskNoCore0,	0
		.equ TaskNoCore1,	1
		.equ TaskNoCore2,	2
		.equ TaskNoCore3,	3
		.equ TaskNoPIXEL,	4
		.equ TaskNoSOUND,	5
		
		.equ TaskNoFREE,	6					@ eerste vrije veld in TaskBlock
		.equ TaskNoEND,		MaxTask-13			@ boven dit nummer nooit kijken door de taskmanagers - is ook laatste vrije veld
		
		.equ TaskNoIRQ0,	MaxTask-12			@ dit is de task-manager van core0 - hoofdtaskmanager
		.equ TaskNoIRQ1,	MaxTask-11			@ dit is de task-manager van core1 - #PIXEL
		.equ TaskNoIRQ2,	MaxTask-10			@ dit is de task-manager van core2
		.equ TaskNoIRQ3,	MaxTask-9			@ dit is de task-manager van core3 - #WAVE=sound-manager
		.equ TaskNoFIQ0,	MaxTask-8
		.equ TaskNoFIQ1,	MaxTask-7
		.equ TaskNoFIQ2,	MaxTask-6
		.equ TaskNoFIQ3,	MaxTask-5			@ dit is de sound-manager - core3 - gaat voor de task-manager
		.equ TaskNoSMC0,	MaxTask-4
		.equ TaskNoSMC1,	MaxTask-3
		.equ TaskNoSMC2,	MaxTask-2
		.equ TaskNoSMC3,	MaxTask-1
@ }

@  *************************** MACROs **************************{

		.macro testhead name, link, codelen, immflag @ 64byte lange header met 64byte aligned execution-token -> spaart cache-lines
			.section .rodata
			.align 6					@ begin altijd ge-aligned op 64 bytes
			.word \link
			.byte \codelen				@ Lengte Word voor Inlining, alleen voor Code
        	.byte \immflag				@ -1:immediate, 1:normaal, 2:compile only
        	.byte 0x0					@ hidden tijdens definitie niet nodig -> byte nog vrij
        	.byte 2f-1f					@ lengte van de naam van het WORD
        	.word 0x0					@ crc - hashtable code
        	.word 0x0					@ vocabulary - code (of ascii??)
        	.skip 16
        1:	.ascii "\name"				@ name, gebruikt in assembler
        2:  .space 32-2b+1b				@ zou moeten kunnen
		.endm

		.macro codehead name, namelen, link, codelen, immflag
        	@ bijv: codehead "/", 1, DROP, 0 - waarbij 'DROP' het vorige woord in de lijst is
        	.section .rodata
        	.align 2
        	.word \link					@ link naar vorige woord -> dword
        	.byte \codelen				@ Lengte Word voor Inlining, alleen voor Code
        	.byte \immflag				@ -1:immediate, 1:normaal, 2:compile only
        	.byte 0x0					@ hidden tijdens definitie niet nodig -> byte nog vrij
        	.byte \namelen				@ lengte van de naam van het WORD
        	@ plus 4 bytes vrij
        	.ascii "\name"          	@ name, gebruikt in assembler
        	.space 32-\namelen			@ fixed space of 32 chars for name
        .endm

        .macro wordhead name, namelen, link, immflag
        	@ bijv: codehead "dup", 3, DROP, 2, 0 - waarbij DROP het vorige woord in de lijst is
        	.section .rodata
        	.align 2
        	.word \link					@ link naar vorige woord
        	.byte 0						@ Lengte Word voor Inlines, alleen voor Code
        								@ word ook te inlinen als push en pop specifiek aangepast worden bij inlinen
        	.byte \immflag				@ -1:immediate, 1:normaal, 2:compile only
        	.byte 0x0					@ hidden tijdens definitie niet nodig -> byte nog vrij
        	.byte \namelen				@ lengte van de naam van het WORD
        	.ascii "\name"          	@ naam van woord in de assembler
        	.space 32-\namelen			@ fixed space of 32 chars for name
        .endm

@ de volgende macro, defvar, is inline-able.
@ een var is een asm-routine die een voor de variabele vast adres op stack zet
@ er lijken geen bijzondere dingen nodig voor een variabele
@ in interpreter mode kan het gewoon gecalled worden
@ in compiler kan het gewoon gecalled of ge-inlined worden

@	macro om in de assembler een variabele te definieren in de dictionary  6c - 3-4c YEAH!!!
        .macro defvar asmname, dictname, dictnamelen, link, initial=0
        	.section .rodata
        	.align 2
        	.word \link
        	.byte 3						@ aantal opcodes voor inline, deze macro prima te inlinen
        	.byte 1						@ 1:normaal
        	.byte 0x0					@ hidden tijdens definitie niet nodig -> byte nog vrij
        	.byte \dictnamelen
        	.ascii "\dictname"			@ naam van VARIABLE in dictionary
        	.space 32-\dictnamelen		@ 32 lange naam, fixed length
        \asmname :						@ naam van VARIABLE in assembler omgeving
			str top, [dts, #-4]!		@ save oude top op stack
			movw top, #:lower16:1f		@ zet address van 1: in top
   			movt top, #:upper16:1f
   			bx lr
		1:	.word \initial				@ dit is de initieele waarde van de variabele
										@ direct na de definitie van een var kan via 'allot' extra ruimte
										@ gegeven worden! Bijvoorbeeld voor string of array
			.text						@ dit moet na .word anders staat de data niet in dictionary
		.endm
		
@	macro om in assembler waarde uit variabele te halen ipv: 'bl varXXX, bl FETCH, popdts' - 3-4c ipv 22c oid
		.macro getvalvar myreg, varname
			ldv32 \myreg, (16+\varname) @ fast way of getting value from variable defined in assembler
			ldr \myreg, [\myreg]		@ loads value of variable into myreg
		.endm
		
@	macro om in assembler waarde in een variabele te zetten ipv: 'pushdts, bl varXXX, bl STORE' - 4c ipv 22c oid
		.macro putvalvar myreg, scratch, varname	@ clobbers both regs
			ldv32 \scratch, (16+\varname) @ fast way of getting value from variable defined in assembler
			str \myreg, [\scratch]	@ stores value of myreg into variable
		.endm
		
@	macro om de waarde van een variabele 1 op te hogen ipv: 'bl varXXX bl FETCH bl ONEPLUS bl varXXX bl STORE' - 
		.macro add1valvar scratch1, scratch2, varname	@ clobbers both regs - 6c ipv 30c oid
			ldv32 \scratch1, (16+\varname) @ fast way of getting value from variable defined in assembler
			ldr \scratch2, [\scratch1]
			add \scratch2, \scratch2, #1
			str \scratch2, [\scratch1]	@ stores value of myreg into variable
		.endm
		
@	macro om een waarde in TaskOnCore0 te zetten
		.macro valtocore0task value, myrega, myregb
			ldv32 \myrega, TaskOnCore0
			ldv16 \myregb, \value
			str \myregb, [\myrega]		@ zet waarde in TaskOnCore0
		.endm
		
@	macro om een waarde in TaskOnCore1 te zetten
		.macro valtocore1task value, myrega, myregb
			ldv32 \myrega, TaskOnCore1
			ldv16 \myregb, \value
			str \myregb, [\myrega]		@ zet waarde in TaskOnCore1
		.endm
		
@ data en instructie barrier macro
		.macro barrier
			isb
			dsb
			isb
		.endm

@	Kleine getallen zijn 1 opcode korter. Derde variant: als een getal past in
@	8 bit plus 5 bit shift, dit moet nog.
@	macro om in de assembler een constante te definieren in de dictionary
		.macro defcon32 asmname, dictname, dictnamelen, link, value
        	@ bijv: defcon HOOGTE, "hoogte", 6, <vorig woord> - wordt automatich op value gezet
        	.section .rodata
        	.align 2
        	.word \link
        	.byte 3						@ aantal opcodes voor inlining, ook voor constants dus
        	.byte 1						@ 1 is normaal - dus niet immediate
        	.byte 0x0					@ dit gaat vrij zeker een aanduiding worden van wat het is
        	.byte \dictnamelen
        	.ascii "\dictname"			@ naam van CONSTANTE
        	.space 32-\dictnamelen		@ 32 lange naam
        \asmname :
        	str top,[dts, #-4]!
			movw top, #:lower16:\value
   			movt top, #:upper16:\value
        	bx lr
        	.text
		.endm

@	macro om in de assembler een constante te definieren in de dictionary
		@ ALLEEN voor positieve 16 bits getallen!!!!
		.macro defcon16 asmname, dictname, dictnamelen, link, value
        	@ bijv: defcon16 HOOGTE, 6, <vorig woord> - wordt automatich op value gezet
        	.section .rodata
        	.align 2
        	.word \link
        	.byte 2						@ aantal opcodes voor inline, ook voor constants dus
        	.byte 1						@ 1 is normaal - dus niet immediate
        	.byte 0x0					@ dit gaat vrij zeker een aanduiding worden van wat het is
        	.byte \dictnamelen
        	.ascii "\dictname"			@ naam van 16bit CONSTANTE
        	.space 32-\dictnamelen		@ 32 lange naam
        \asmname :
        	str top,[dts, #-4]!			@ zet oude top op stack
			movw top, #:lower16:\value	@ en zet value in nieuwe top
        	bx lr
        	.text
		.endm

@ macro assembler strings. Niet geschikt voor Forth Interpreter
		.macro defstr name, string
			.align 2
		\name :
			.word 2f-1f
		1:	.ascii "\string"
		2:	.byte 0
			.align 2
		.endm

@ macro unnamed string. Voor programmas in source-code. Geeft string met count er aan vooraf
		.macro mycode string
			.align 2
			.word 2f-1f
		1:	.ascii "\string"
		2:
			@.byte 0
			.align 2
		.endm

@ macro om chars te printen - MOET in woord!!! NIET in code!!
		.macro prchar char
			lit16 \char
			bl	EMIT
		.endm

@ macro om chars te debug-printen - MOET in woord!!! NIET in code!!
		.macro prdbchar char
			getvalvar r0, varDEBUG
			@bl	varDEBUG
			@bl	FETCH
			@popdts r0					@ top weer up to date

			cmp r0, #0					@ als debug 0, dan geen debug printen
			beq	69f

			lit16 \char
			bl	EMIT
	69:
		.endm

@ maxcro om string te printen - MOET in woord!!! NIET in code!!
		.macro prstr addr
			lit32 \addr
			bl	PR_CST
		.endm

@ macro om string te debug-printen - MOET in woord!!! NIET in code!!
		.macro prdbstr addr
			getvalvar r0, varDEBUG
			@bl	varDEBUG
			@bl	FETCH
			@popdts r0					@ top weer up to date

			cmp r0, #0					@ als debug 0, dan geen debug printen
			beq	69f

			lit32 \addr
			bl	PR_CST
	69:
		.endm

@	prolog is het begin van elk WORD
        .macro prolog
			STMFD r13!,{r0-r3, lr}	@ r0-r1 kost niets extra en is dus standaard
		.endm						@ alleen lr crasht door aannames in design door mij
									@ alleen r0 is 3c langzamer!! 14c ipv 11c
									@ WDUMMY: r4->13c, r3->11c, r2->13.5c!, r1->11c, r0->14c!
									@ WCDUMMY: r4->13c, r3->11c, r2->11c,    r1->10c, r0->10c

@	next is het einde van elk WORD
        .macro next
			LDMFD r13!,{r0-r3, pc}
        	.ltorg
        	.text
        .endm

@	prolog is het begin van elk WORD
        .macro prologmax
			STMFD r13!,{r0-r4, r6-r8, v, w, lr}	@ r0-r1 kost niets extra en is dus standaard
		.endm

@	next is het einde van elk WORD
        .macro nextmax
			LDMFD r13!,{r0-r4, r6-r8, v, w, pc}
        	.ltorg
        	.text
        .endm

@	codenext is het einde van elke primitive
        .macro codenext
        @	MOV pc, lr				@ deze kost 20c op RasPi 2!!
        	bx lr					@ deze maar 4c !! 6-7 bij slow R2
        	.ltorg
        	.text
        .endm

@   push register on datastack
        .macro pushdts myreg
			str top, [dts, #-4]!	@ zet top boven op de stack, en pas dts aan
			mov top, \myreg			@ en zet myreg in de top
		.endm

@   pop datastack into register
        .macro popdts myreg
        	mov \myreg, top			@ put top in myreg
			ldr top, [dts], #4		@ update top, correct stack
        .endm
        
@	pop 2 datastack-cells into regs -> ie 2xpopdts in one - saves 1-2 cycles as less stack-traffic
		.macro poppopdts myreg1 myreg2
        	mov \myreg1, top		@ myreg1 now filled with top of stack
			ldr \myreg2, [dts], #4	@ myreg2 now filled with 1 below top of stack
			ldr top, [dts], #4		@ top restored, stack ok again
		.endm

@   push register on userstack (=r10)
        .macro pushuss myreg
			str \myreg,[uss, #-4]!	@ userstack heeft geen top register
        .endm

@   pop userstack (=r10) into register
        .macro popuss myreg
			ldr \myreg, [uss], #4	@ userstack heeft geen top register
		.endm						@ en is dus sneller voor tijdelijk pop/push als de datastack

@	push number onto datastack		@ 2c-3c
		.macro lit16 number
			str top, [dts, #-4]!
			movw top, #:lower16:\number
		.endm

@	push number onto datastack		@ 3-4c -> kennelijk soms 1c latentie in de str
		.macro lit32 number
			str top, [dts, #-4]!	@ hier soms 1c latentie!
			movw top, #:lower16:\number
   			movt top, #:upper16:\number
		.endm

@	laad 32 bit getal in reg		@ 2c
		.macro ldv32 myreg value
			movw \myreg, #:lower16:\value
   			movt \myreg, #:upper16:\value
		.endm

@	laad 16 bit getal in reg		@ 1c
		.macro ldv16 myreg value
			movw \myreg, #:lower16:\value
		.endm

@	deel 1 loop
		.macro loopcheck
			add loopi, loopi, #1	@ verhoog loop index met 1
			cmp loopi, looplim		@ vergelijk lim en i
		.endm

@	deel 1 plusloop
		.macro plloopcheck
			add loopi, loopi, top	@ tel top bij index op
			ldr top, [dts], #4		@ herstel top
			cmp loopi, looplim		@ vergelijk lim en i
		.endm

@	deel 2 loop
		.macro loopend
			popuss loopi			@ maak van J weer I als loop klaar
			popuss looplim			@ en zet de oude limiet weer terug
		.endm
		
@	clean datacache!
		.macro CleanDataCacheLevel0 @ clobbers r0-r3
			isb
			dsb
			MRC p15, 1, r0, c0, c0, 0
			LDR r3, =0x1ff
			AND r0, r3, r0, LSR #13

			MOV r1, #0
		1:								@ =way_loop
			MOV r3, #0

		2:								@ =set_loop
			MOV r2, r1, LSL #30
			ORR r2, r3, LSL #5
			MCR p15, 0, r2, c7, c6, 2
			ADD r3, r3, #1
			CMP r0, r3
			BGT 2b

			ADD r1, r1, #1
			CMP r1, #4
			BNE 1b
			barrier
		.endm
		
@	Clean and invalidate multilevel datacache
		.macro ResetDataCache @ => clobbers 0-4, 6-8, 11, 12
			dmb
			mrc	p15, 1, r0, c0, c0, 1		@ read clidr
			ands r3, r0, #0x7000000			@ extract loc from clidr
			mov	r3, r3, lsr #23				@ left align loc bit field
			@beq	5f							@ if loc is 0, then no need to clean
		
			mov	r12, #0						@ start clean at cache level 0
			
	1:		add	r2, r12, r12, lsr #1		@ work out 3x current cache level
			mov	r1, r0, lsr r2				@ extract cache type bits from clidr
			and	r1, r1, #7					@ mask of the bits for current cache only
			cmp	r1, #2						@ see what cache we have at this level
			blt	4f							@ skip if no cache, or just i-cache
		
			mcr	p15, 2, r12, c0, c0, 0		@ select current cache level in cssr
			isb
			mrc	p15, 1, r1, c0, c0, 0		@ read the new csidr
			and	r2, r1, #7					@ extract the length of the cache lines
			add	r2, r2, #4					@ add 4 (line length offset)
			ldr	r4, =0x3ff
			ands r4, r4, r1, lsr #3			@ find maximum number on the way size
			clz	r6, r4						@ find bit position of way size increment
			ldr	r7, =0x7fff
			ands r7, r7, r1, lsr #13		@ extract max number of the index size
			
	2:		mov	r8, r4						@ create working copy of max way size
			
	3:		orr	r11, r12, r8, lsl r6		@ factor way and cache number into r11
			orr	r11, r11, r7, lsl r2		@ factor index number into r11

			mcr	p15, 0, r11, c7, c14, 2		@ flush & invalidate by set/way
			subs r8, r8, #1					@ decrement the way
			bge	3b
		
			subs r7, r7, #1					@ decrement the index
			bge	2b
		
	4:		add	r12, r12, #2				@ increment cache number
			cmp	r3, r12
			bgt	1b
		
	5:		mov	r12, #0						@ switch back to cache level 0
			mcr	p15, 2, r12, c0, c0, 0		@ select current cache level in cssr
			barrier
		.endm

@	Flush multilevel datacache
		.macro FlushDataCache @ => clobbers 0-4, 6-8, 11, 12
			dmb
			mrc	p15, 1, r0, c0, c0, 1		@ read clidr
			ands r3, r0, #0x7000000			@ extract loc from clidr
			mov	r3, r3, lsr #23				@ left align loc bit field
			@beq	5f							@ if loc is 0, then no need to clean
		
			mov	r12, #0						@ start clean at cache level 0
			
	1:		add	r2, r12, r12, lsr #1		@ work out 3x current cache level
			mov	r1, r0, lsr r2				@ extract cache type bits from clidr
			and	r1, r1, #7					@ mask of the bits for current cache only
			cmp	r1, #2						@ see what cache we have at this level
			blt	4f							@ skip if no cache, or just i-cache
		
			mcr	p15, 2, r12, c0, c0, 0		@ select current cache level in cssr
			isb
			mrc	p15, 1, r1, c0, c0, 0		@ read the new csidr
			and	r2, r1, #7					@ extract the length of the cache lines
			add	r2, r2, #4					@ add 4 (line length offset)
			ldr	r4, =0x3ff
			ands r4, r4, r1, lsr #3			@ find maximum number on the way size
			clz	r6, r4						@ find bit position of way size increment
			ldr	r7, =0x7fff
			ands r7, r7, r1, lsr #13		@ extract max number of the index size
			
	2:		mov	r8, r4						@ create working copy of max way size
			
	3:		orr	r11, r12, r8, lsl r6		@ factor way and cache number into r11
			orr	r11, r11, r7, lsl r2		@ factor index number into r11

			mcr	p15, 0, r11, c7, c10, 2		@ flush datacache by set/way
			isb
			subs r8, r8, #1					@ decrement the way
			bge	3b
		
			subs r7, r7, #1					@ decrement the index
			bge	2b
		
	4:		add	r12, r12, #2				@ increment cache number
			cmp	r3, r12
			bgt	1b
		
	5:		mov	r12, #0						@ switch back to cache level 0
			mcr	p15, 2, r12, c0, c0, 0		@ select current cache level in cssr
			dsb
			isb
		.endm


@  *******************  debug macros  **************************

@ 	macro print registers, r0 bevat output reg
		.macro prntreg strvari
			pushdts r0
			lit32 \strvari
			bl	PR_CST
			bl	OUT32BIN
			bl	CR
		.endm

@	debug flag - clobbers nothing!!
		.macro debugflag flag addr			@ put value on address
			STMFD r13!,{r0-r1}				@ for use in following 3 macros
			ldr r0, =\flag
			ldr r1, =\addr
			str r0, [r1]
			LDMFD r13!,{r0-r1}
		.endm

		.macro debugflag1 flag				@ put flag on 0x3000 without changing any regs
			debugflag \flag, 0x3000
		.endm

		.macro debugflag2 flag
			debugflag \flag, 0x3004
		.endm

		.macro debugflag3 flag
			debugflag \flag, 0x3008
		.endm
		
		.macro incrmem addr @ clobbers nothing
			stmfd r13!, {r0-r1}
			ldv32 r0, \addr
			ldr r1, [r0]
			add r1, r1, #1
			str r1, [r0]
			ldmfd r13!, {r0-r1}
		.endm
		
		.macro reg2mem myreg1, myreg2, addr @ myreg2 = scratch
			ldv32 \myreg2, \addr
			str \myreg1, [\myreg2]
		.endm
		
@	debugflags sysinfodbvars - for use of various debug-activities - assembly can change, addresses are stable
		.macro ussdepth addr				@ stores depth of uss at \addr
			ldv32 r0, \addr					@ clobbers r0, r1
			ldv32 r1, UseSt					@ UseSt= #CORE0
			sub r1, r1, uss					@ (= UseSt) is altijd groter of gelijk aan uss
			@asr r1, r1, #2					@ depth in bytes!!!
			str r1, [r0]
		.endm

@  *******************  einde debug macros  ********************}

@  **************  constants UART en ARM clock  ****************{

	.set ARM_TIMER_LOD,		0x3F00B400		@ ARM timer
	.set ARM_TIMER_VAL,		0x3F00B404
	.set ARM_TIMER_CTL,		0x3F00B408
	.set ARM_TIMER_IRQ,		0x3F00B40C
	.set ARM_TIMER_RAW,		0x3F00B410
	.set ARM_TIMER_MAS,		0x3F00B414
	.set ARM_TIMER_REL,		0x3F00B418
	.set ARM_TIMER_DIV,		0x3F00B41C
	.set ARM_TIMER_CNT,		0x3F00B420

	.set TIM_CLO,			0x3F003004		@ system timer low

@	zet op ARM_TIMER_CTL op 0x00F90200 geeft 250Mhz/(249(=F9)+1) = 1 Mhz precies en enabled clock (=bit 9)

@  ****************  Clock en PWM for sound  *******************
	.set CM_BASE,		   	0x3F101000 		@ Clock Manager base-addr
	.set CM_PWMDIV,		   	0x3F1010A4 		@ Clock Manager PWM Clock Divisor - checked
	.set CM_PWMCTL,		   	0x3F1010A0 		@ Clock Manager PWM Clock Control

	.set PWM_BASE,	 		0x3F20C000		@ PWM base-addr
	.set PWM_CTL, 			0x3F20C000		@ equal to base-addr
	.set PWM_RNG1,		 	0x3F20C010 		@ PWM Channel 1 - Range
	.set PWM_RNG2,		 	0x3F20C020 		@ PWM Channel 2 - Range
	.set PWM_STA,  			0x3F20C004 		@ PWM status-addr
	.set PWM_FIF1,		 	0x3F20C018		@ PWM FIFO Input-addr

@  ****************  constants voor de UART - SPI  *************

    .equ GPPUD,    			0x3F200094		@ maar 1?
	.equ GPPUDCLK0,    		0x3F200098		@ maar 1 reg?
	.equ GPCLR0,    		0x3F200028
	.equ GPCLR1,			0x3F20002C
	.equ GPSET0,			0x3F20001C
	.equ GPSET1,			0x3F200020
	.equ GPFSEL0,			0x3F200000
	.equ GPFSEL1,    		0x3F200004
	.equ GPFSEL2,    		0x3F200008
	.equ GPFSEL3,    		0x3F20000C
	.equ GPFSEL4,    		0x3F200010

	@GPLEV0 en 1 (GPIO Pin Level Registers)
	@GPEDS (GPIO Event Detect Status Registers)
	@GPREN( GPIO Rising Edge Detect Enable Registers)
	@GPFEN(GPIO Falling Edge Detect Enable Registers)
	@GPHEN(GPIO High Detect Enable Registers )
	@GPLEN( GPIO Low Detect Enable Registers)
	@GPAREN(GPIO Asynchronous rising Edge Detect Enable Registers)
	@GPAFEN(GPIO Asynchronous Falling Edge Detect Enable Registers)

	.equ AUX_ENABLES,     	0x3F215004
	
	.equ AUX_MU_IO_REG,   	0x3F215040
	.equ AUX_MU_IER_REG,  	0x3F215044
	.equ AUX_MU_IIR_REG,  	0x3F215048
	.equ AUX_MU_LCR_REG,  	0x3F21504C
	.equ AUX_MU_MCR_REG,  	0x3F215050
	.equ AUX_MU_LSR_REG,  	0x3F215054
	.equ AUX_MU_MSR_REG,  	0x3F215058
	.equ AUX_MU_SCRATCH,  	0x3F21505C
	.equ AUX_MU_CNTL_REG, 	0x3F215060
	.equ AUX_MU_STAT_REG, 	0x3F215064
	.equ AUX_MU_BAUD_REG, 	0x3F215068

	.equ AUX_SPI0_CS,		0x3F204000		@ #ERROR?! was 0x37 - nu 0x3F
	.equ AUX_SPI0_FIFO,		0x3F204004
	.equ AUX_SPI0_CLK,		0x3F204008
	.equ AUX_SPI0_DLEN,		0x3F20400C
	.equ AUX_SPI0_LTOH, 	0x3F204010
	.equ AUX_SPI0_DC,		0x3F204014
@ checked }

@  ***************  START en BEGIN DICTIONARY  *****************{
.globl _start
_start:	b	_ENTRY					@ dit zit op 0x8000

@ ******************  BEGIN DICTIONARY  ************************
codehead "startdic", 8, 0x0, 0, 0
STARTDIC:							@ first link dictionary
codenext	@}

@  ***************  variabelen constants ***********************{

defvar varBASE, "base", 4, STARTDIC, 10					@ base van nummer conversie en printing
defvar varSTATE, "state", 5, varBASE, 0					@ state=>compiling of interpreting
defvar varTOIN, ">in", 3, varSTATE, 0					@ positie in een text-buffer waar geinterpreteerd gaat worden
defvar varHERE, "here", 4, varTOIN, FreeRam				@ eerste lege address na dictionary
defvar varHEREDATA, "heredata", 8, varHERE,(TOP_DATA-4)	@ zie boven voor memory-map
defvar varOLDHERE, "oldhere", 7, varHEREDATA			@
defvar varLAST, "last", 4, varOLDHERE, EndDict			@ lijkt geen temp LAST nodig
defvar varOUT, "out", 3, varLAST, 0						@ positie van cursor bij EMIT - niet voor windows, die hebben ieder een eigen out variabele
defvar varCHARSIN, "charsin", 7, varOUT, 0				@ number of chars recieved with RECEIVE since last reset
defcon32 conSTARTFREE, "startfree", 9, varCHARSIN, FreeRam	@ waar de interne dict eindigt - om size software ed te berekenen
defcon32 conSTARTDICT, "startdict", 9, conSTARTFREE, STARTDIC
defcon32 conMYSOURCE, "mysource", 8, conSTARTDICT, MYSOURCE
defcon32 conTEXTBLOCK, "textblock", 9, conMYSOURCE, TEXT_BLOCK
defcon32 conTASKBLOCK, "taskblock", 9, conTEXTBLOCK, TASK_BLOCK
defcon32 conTASKTABLE, "tasktable", 9, conTASKBLOCK, TaskTable
defcon32 conNEWFONT, "font1", 5, conTASKTABLE, NEWFONT
defcon32 conPOSTBUS10, "postbus10", 9, conNEWFONT, postbus1_0
defvar varMAXINLINE, "maxinline", 9, conPOSTBUS10, 5	@ -1 inline alles, 0 inline niets, 1-n inline routines tot die lengte
defvar varNEXTLINE, "nextline", 8, varMAXINLINE, MYSOURCE
defvar varNEG, "neg#", 4, varNEXTLINE, 0				@ voor nummer conversie - om sign te onhouden
defvar varNOISE, "noise", 5, varNEG, 0xfff				@ voor de noise emulatie
defvar varMZ, "m_z", 3, varNOISE						@ voor de rando generator
defvar varMW, "m_w", 3, varMZ							@ idem
defvar varDEBUG, "debug", 5, varMW, 0					@ false=geen debug, true is debug bij compileren - functions!!

@  ******************** SYSTEM INFO VARS ***********************

defcon32 conSYSINFOVARS, "sysinfovars", 11, varDEBUG, sysinfovars
defcon32 conSYSDBVARS, "sysdbvars", 9, conSYSINFOVARS, sysdbvars
defcon32 conSIVDB0, "sivdb0", 6, conSYSDBVARS, sivdb0
defcon32 conSIVDB1, "sivdb1", 6, conSIVDB0, sivdb1
defcon32 conSIVDB2, "sivdb2", 6, conSIVDB1, sivdb2
defcon32 conSIVDB3, "sivdb3", 6, conSIVDB2, sivdb3
defcon32 conSIVC0INIT, "sivc0init", 9, conSIVDB3, sivc0init
defcon32 conSIVISC0SCTLR, "sivisc0sctlr", 12, conSIVC0INIT, sivisc0SCTLR
defcon32 conSIVISC0CPSR, "sivisc0cpsr", 11, conSIVISC0SCTLR, sivisc0CPSR
defcon32 conSIVISC0SPSRHYP, "sivisc0spsrhyp", 14, conSIVISC0CPSR, sivisc0SPSRhyp
defcon32 conSIVISC0ELRHYP, "sivisc0elrhyp", 13, conSIVISC0SPSRHYP, sivisc0ELRhyp
defcon32 conSIVISC0R13PRE, "sivisc0r13pre", 13, conSIVISC0ELRHYP, sivisc0R13pre
defcon32 conSIVISC0R14PRE, "sivisc0r14pre", 13, conSIVISC0R13PRE, sivisc0R14pre
defcon32 conSIVISC0R15PRE, "sivisc0r15pre", 13, conSIVISC0R14PRE, sivisc0R15pre
defcon32 conSIVISC0R13PST, "sivisc0r13pst", 13, conSIVISC0R15PRE, sivisc0R13pst
defcon32 conSIVISC0R14PST, "sivisc0r14pst", 13, conSIVISC0R13PST, sivisc0R14pst
defcon32 conSIVISC0R15PST, "sivisc0r15pst", 13, conSIVISC0R14PST, sivisc0R15pst
defcon32 conSIVISC0CPACR, "sivisc0cpacr", 12, conSIVISC0R15PST, sivisc0CPACR
defcon32 conSIVISC0ACTLRNS, "sivisc0actlrns", 14, conSIVISC0CPACR, sivisc0ACTLRns
defcon32 conSIVISC0CNTKCTL, "sivisc0cntkctl", 14, conSIVISC0ACTLRNS, sivisc0CNTKCTL
defcon32 conSIVENC0R13S0, "sivenc0r13s0", 12, conSIVISC0CNTKCTL, sivenc0R13s0
defcon32 conSIVENC0R14S0, "sivenc0r14s0", 12, conSIVENC0R13S0, sivenc0R14s0
defcon32 conSIVENC1R13S0, "sivenc1r13s0", 12, conSIVENC0R14S0, sivenc1R13s0
defcon32 conSIVENC1R14S0, "sivenc1r14s0", 12, conSIVENC1R13S0, sivenc1R14s0
defcon32 conSIVENC2R13S0, "sivenc2r13s0", 12, conSIVENC1R14S0, sivenc2R13s0
defcon32 conSIVENC2R14S0, "sivenc2r14s0", 12, conSIVENC2R13S0, sivenc2R14s0
defcon32 conSIVC0R0, "sivc0r0", 7, conSIVENC2R14S0, sivc0R0
defcon32 conSIVC1R0, "sivc1r0", 7, conSIVC0R0, sivc1R0
defcon32 conSIVC2R0, "sivc2r0", 7, conSIVC1R0, sivc2R0
defcon32 conSIVC3R0, "sivc3r0", 7, conSIVC2R0, sivc3R0

@  ***********************  WAVE STUFF  ************************

defcon32 conWVCOUNTER, "wvcounter", 9, conSIVC3R0, wvcounter
defcon32 conWVCMPWMCTL, "wvcmpwmctl", 10, conWVCOUNTER, wvcmpwmctl

@  ***********************  PIXEL STUFF  ***********************

defvar varSCRPITCH, "scrpitch", 8, conWVCMPWMCTL, 0		@ pitch of screen and win0
defvar varSCRSIZE, "scrsize", 7, varSCRPITCH, 0			@ size in bytes of screen
defvar varSCRBASE, "scrbase", 7, varSCRSIZE, 0			@ start van scherm geheugen waar geschreven wordt, wisselt 30 Hz
defvar varSCROFFSET, "scroffset", 9, varSCRBASE, 0		@ offset van het scherm - voor switchen van scherm
defvar varSCR0BASE, "scr0base", 8, varSCROFFSET, 0		@ vaste pointer naar nul scherm, wordt eenmalig gezet
defvar varSCRVISIBLE, "scrvisible", 10, varSCR0BASE, 1	@ welk van de 2 screens visible is - start met 0

defvar varCANVAS, "canvas", 6, varSCRVISIBLE, 0x004040	@ default background is vd cyan
defvar varINK, "ink", 3, varCANVAS, 0xf0f0f0			@ default ink=offwit

defcon32 conPXWOBASE, "pxwobase", 8, varINK, pxwobase	@ address van pxwo-lijstje - obsolete
defcon32 conPXCOUNTER, "pxcounter", 9, conPXWOBASE, pxcounter @ counter die IRQ activatiobs van core1 telt

defvar varSCRY, "scry", 4, conPXCOUNTER, 0				@ cursor y-positie screen - tussen 0 en winhiy kan getekend worden
defvar varSCRX, "scrx", 4, varSCRY, 0					@ cursor x-positie screen - tussen 0 en winhix kan getekend worden
defvar varWINCURY, "wincury", 7, varSCRX, 0				@ cursor y-positie write-on window - 0,0 is 0,0 origin plus rand
defvar varWINCURX, "wincurx", 7, varWINCURY, 0			@ cursor x-positie write-on window
defvar varWINPITCH, "winpitch", 8, varWINCURX, 0		@ pitch write-on window = pitch buffer
defvar varWINSIZEX, "winsizex", 8, varWINPITCH, 0		@ size x write-on window - rand zit hier dus ook in!
defvar varWINSIZEY, "winsizey", 8, varWINSIZEX, 0		@ size y write-on window - rand zit hier dus ook in!
defvar varWINHIX, "winhix", 6, varWINSIZEY, 0			@ is winsizex-2*rand - (winlowx=altijd 0)
defvar varWINHIY, "winhiy", 6, varWINHIX, 0				@ is winsizey-2*rand - (winlowy=altijd 0)

defcon32 conWINTABLE, "wintable", 8, varWINHIY, WinTable @ plek waar windows-tabel staat
defcon32 conFBINFO, "fbinfo", 6, conWINTABLE, FB_INFO	@ plek waar GPU Frame buffer dat heen schijft
defcon32 conWINMAX, "winmax", 6, conFBINFO, max_win		@ maximaal aantal windows in WinTable - tussen 1 en max_win
defcon32 conWINBYTES, "winbytes", 8, conWINMAX, bytes_win @ aantal bytes per regel in WinTable
defvar varWRITEON, "writeon", 7, conWINBYTES, 0			@ window waar op getekend wordt - altijd tussen 0 en win#-1
defvar varWINNO, "win#", 4, varWRITEON, 1				@ actueel aantal windows in WinTable, nooit lager als 1!
														@ 16bit:5bitRed-6bitGreen-5bitBlue
defcon32 conBLACK, "black", 5, varWINNO, 0x000000		@ 32bit:ARGB - Alpha werkt nog niet - komt ook niet...
defcon32 conWHITE, "white", 5, conBLACK, 0xf0f0f0
defcon32 conGRAY, "gray", 4, conWHITE, 0x808080
defcon32 conDGRAY, "dgray", 5, conGRAY, 0x2A3430
defcon32 conVDGRAY, "vdgray", 6, conDGRAY, 0x162016
defcon32 conLGRAY, "lgray", 5, conVDGRAY, 0xB0B3B3
defcon32 conVLGRAY, "vlgray", 6, conLGRAY, 0xc4D0D0
defcon32 conRED, "red", 3, conVLGRAY, 0xf00000
defcon32 conDRED, "dred", 4, conRED, 0x360000
defcon32 conVDRED, "vdred", 5, conDRED, 0x200000
defcon32 conLRED, "lred", 4, conVDRED, 0xf08585
defcon32 conVLRED, "vlred", 5, conLRED, 0xf0c0c8
defcon32 conBLUE, "blue", 4, conVLRED, 0x0000f0			@ very nice color
defcon32 conDBLUE, "dblue", 5, conBLUE, 0x030660		@ very nice color
defcon32 conVDBLUE, "vdblue", 6, conDBLUE, 0x000030		@ very nice color
defcon32 conLBLUE, "lblue", 5, conVDBLUE, 0x0890F0		@ very nice color 0x0080FF
defcon32 conVLBLUE, "vlblue", 6, conLBLUE, 0x08D0F0		@ very nice color
defcon32 conGREEN, "green", 5, conVLBLUE, 0x02f004		@
defcon32 conDGREEN, "dgreen", 6, conGREEN, 0x025004		@
defcon32 conVDGREEN, "vdgreen", 7, conDGREEN, 0x003000	@
defcon32 conLGREEN, "lgreen", 6, conVDGREEN, 0x40E83A	@
defcon32 conVLGREEN, "vlgreen", 7, conLGREEN, 0xA0F098	@ ok
defcon32 conYELLOW, "yellow", 6, conVLGREEN, 0xf0f000	@
defcon32 conDYELLOW, "dyellow", 7, conYELLOW, 0x505000
defcon32 conVDYELLOW, "vdyellow", 8, conDYELLOW, 0x303000	@
defcon32 conLYELLOW, "lyellow", 7, conVDYELLOW, 0xf0f080
defcon32 conVLYELLOW, "vlyellow", 8, conLYELLOW, 0xf0f0d0
defcon32 conCYAN, "cyan", 4, conVLYELLOW, 0x00f0f0
defcon32 conDCYAN, "dcyan", 5, conCYAN, 0x009090		@ blauwgrijzig
defcon32 conVDCYAN, "vdcyan", 6, conDCYAN, 0x004040		@ very nice color
defcon32 conLCYAN, "lcyan", 5, conVDCYAN, 0x60f0f0
defcon32 conVLCYAN, "vlcyan", 6, conLCYAN, 0xa0f0f0
defcon32 conMAGENTA, "magenta", 7, conVLCYAN, 0xf000f0
defcon32 conDMAGENTA, "dmagenta", 8, conMAGENTA, 0x360036
defcon32 conVDMAGENTA, "vdmagenta", 9, conDMAGENTA, 0x200020
defcon32 conLMAGENTA, "lmagenta", 8, conVDMAGENTA, 0xf080f0
defcon32 conVLMAGENTA, "vlmagenta", 9, conLMAGENTA, 0xf0C0f0

@defvar varCURX, "curx", 4, conVLMAGENTA, 0				@ gehele scherm -> straks obsoleet want dit gaat via win0
@defvar varCURY, "cury", 4, varCURX, 0					@ gehele scherm -> straks obsoleet want dit gaat via win0

defcon32 conNEOFORTH, "neoforth", 8, conMAGENTA, MyNeoForth
@ checked }

@  *******************  strings 4 printing  ********************{
defstr presskey, "   Press ANY key..."
defstr myforth1, "   === CrayOnForth ============="
defstr myforth2, "   === Copyright J.J. Hoekstra ="
defstr myforth3, "   === 2017 Leeuwarden ========="
defstr unknown1, "  ABORT! \""
defstr unknown2, "\" unknown"
defstr immstr, "imm xt: "
defstr ordinary, "ord xt: "
defstr literal, "lit no: "
defstr debugfl1, "   debugfl 1 : "
defstr debugfl2, "   debugfl 2 : "
defstr debugfl3, "   debugfl 3 : "
defstr stackempty, "empty"
defstr stack, "stack: "
defstr charstyped, "chars received"
defstr goreset, "   > REBOOT..."
defstr redefined1, "warning: "
defstr redefined2, " redefined"
defstr nextstring, "addr next string: 0x"
defstr backfromimm, "     <bckimm>"
defstr backfromnor, "     <bcknor>"
defstr clearingsource, "<resetting source>"
defstr dotibio, "     <tibio> "
defstr carriage, "_"
defstr pcis,	"pc=0x"
defstr spis,	"sp=0x"
defstr dtsis, 	"dts=0x"
defstr ussis,	"uss=0x"
defstr fpsis,	"fps=0x"
defstr lris,	"lr=0x"
defstr sizedict,	"===== forth-dict bytes: "
defstr sizeprog,	"====== user-dict bytes: "
defstr noword, 		"=== defs in dictionary: "
defstr bytesfree,	"=========== BYTES FREE: "
@defstr sizeword,	"====== opcodes per def: "
defstr sizebanner,	"------------------------------"
defstr fbinit, "framebuffer initiated..."
@ }

@  ***********************  START SYSTEM  **********************{
codehead "entry", 5, conNEOFORTH, 0, 1	@ traditioneel CPU setup - maar geen perifere hardware à la UART of io
_ENTRY:								@{
		@bl	INITUART				@ #DEBUG - INITUART nodig als voor _COLD debug data geprint moet worden, anders niet
			reg2mem r13, r1, sivenc0R13s0	@ here the value of sp from the bootloader is visible
			reg2mem r14, r1, sivenc0R14s0
		ldv32 r13, 0x1000000		@ tijdelijke stackpointer #DEBUG: om te kijken of dit wat doet vwb booten-> nee!
		bl	INITSYS					@ setup van system modus ed. - hier switch van hyp via supervisor naar system
			reg2mem r13, r1, sivenc0R13s1
			reg2mem r14, r1, sivenc0R14s1
		incrmem sivc0init
		bl	INITMMU					@ eerst systeem, dan memory.
			reg2mem r13, r1, sivenc0R13s2
			reg2mem r14, r1, sivenc0R14s2
		incrmem sivc0init
		bl	CLEARTASKBLOCK			@ clear het taskblock om zeker te zijn dat alles nul is.
			reg2mem r13, r1, sivenc0R13s3
			reg2mem r14, r1, sivenc0R14s3
		incrmem sivc0init
		bl	INITTASKBLOCK			@ kan evt ook voor MMU, daar MMU de caches toch reset. Deze eerste keer voor INITCORES!
			reg2mem r13, r1, sivenc0R13s4
			reg2mem r14, r1, sivenc0R14s4
		incrmem sivc0init
		bl	INITTASKTABLE			@ dit maar een keer voor alle cores
			reg2mem r13, r1, sivenc0R13s5
			reg2mem r14, r1, sivenc0R14s5
		incrmem sivc0init
		bl	INITCORES				@ start, initieert en pauseert dan cores 1-3  (reactivatie via sev)
			reg2mem r13, r1, sivenc0R13s6
			reg2mem r14, r1, sivenc0R14s6
		incrmem sivc0init
		bl	INITSCREEN				@ zet de buffers van win0 - eenmalig want alloceert buffer geheugen
			reg2mem r13, r1, sivenc0R13s7
			reg2mem r14, r1, sivenc0R14s7
		incrmem sivc0init
		lit16 848					@ 704*528=ok 768*576=ok 720*540=ok 800*600=ok 640*480=werkt niet op onze tv!
		lit16 480					@ hoogte intern altijd meervoud van 8 -> daardoor soms meer geheugen voor scherm
		bl	SETWINZERO				@ setup win0 en zet initieel scherm in GPU - kan (nog) niet herhaald worden
		incrmem sivc0init
		cpsie ifa					@ start de interrupts na INITTASKBLOCK, TASKTABLE en SETWINZERO
		incrmem sivc0init
		b 	_COLD
codenext	@}

	@ _COLD is een restart zonder de computer te resetten
	@ oa uservariabelen voor een multi-user omgeving
	@ en reset van hardware en dergelijke. UART en MCS zijn goed hier.

codehead "cold", 4, _ENTRY, 0, 1
_COLD:	bl 	INITUART				@ COLD is herstart van systeem zonder systeem aan en uit te doen{
		incrmem sivc0init
		@bl	INITWAVE				@ #nadenken: geen idee of dit hier een goed idee is>
		@incrmem sivc0init
		bl	INITMCS					@ init ARM clock
		incrmem sivc0init
		bl	INITTRNG				@ init true random generator
		incrmem sivc0init
		bl	INITPIXEL				@ (her)start de interrupt van het pixel systeem
		incrmem sivc0init			@ init core0 in geeft 16 steps
		bl	BANNER					@ moet in ieder geval na SETWINZERO #checked
		bl	CR

		b	_ABORT
codenext	@ geen codenext nodig want ingang van COLD is een b en geen bl - maar wel voor .text!!}

@ _ABORT is voor als een programma of compilatie afbreekt door een fout
codehead "abort", 5, _COLD, 0, 1	@ maw alle FORTH-systeem variabelen en stacks resetten{
_ABORT:
		@ldv32 sp, RetSt			@ crash niet meer - geen idee waarom...
		sev							@ start core1-core3 als die slapen
		isb							@ wachten tot sev klaar is
		bl	INITTASKBLOCK
		@bl	INITCORES				@ #UPDATE - het kan zijn dat het beter is dat de CORES1-3 gewoon doorlopen
									@ voor langlopende taken. #NADENKEN

		bl	INITRNDM				@ nu nog met vaste waarde, dat wordt straks met _MCS of zoiets
		@bl	CLEARTIB				@ dit zet ook >in op 0

		@prstr presskey
		@bl	ANYKEY

		b	_QUIT
codenext	@ checked }
@}

	@ _QUIT is de standard loop van de interpreter, _QUIT is een
	@ eindeloze loop en reset de data en user stacks NIET!
	@ maar wel de return stack (van core 0!) - WAAROM? #CHECK

codehead "quit", 4, _ABORT, 0, 1
_QUIT:								@ lees input van user into input buffer
									@ interpret input in input buffer
									@ print ok als interpret succesvol
		@ldr sp, =RetSt				@ start van QUIT reset de return stack - want??

		lit16 0x0
		bl	varTOIN					@ addr van >in op stack
		bl	STORE					@ zet 0 in >in
		bl	CLEARSOURCE

	@  ************************************************************
@meethang:
@hang:
		bl	PROK

@meethang:
		@bl	ANYKEY
		@bl	TEST10M
		@bl	SPACE
		@b 	meethang

hang:	bl	TIBIO					@ nu address te lezen regel of 0 op stack
		cmp top, #0x0
		beq	1f						@ klaar met deze source

			bl	DOTDBTBIO			@ debug printing bij interpretatie
		bl	FILLTIB					@ fill TIB met de regel uit source van het addres op stack
		bl	INTERPRET				@ interpreteer string in TIB tot het einde van de string
		b	hang					@ en verder

	1:	bl	DROP					@ drop de nul die aangeeft dat de source op is
		bl	ANYKEY
		b _QUIT
codenext

@  ***********************  INTERPRETER  *************************{
wordhead "interpret", 9, _QUIT, 1	@ interpreteert string in TIB ( -- )
INTERPRET:
		prolog
verder:
			bl DOTDBCR

		lit16 32
		bl	F83WORD					@ pars volgende woord tot een spatie of einde TIB, niet ANSI
		cmp top, #0
		beq	1f						@ geen volgende woorden in TIB
			bl	DOTDBINT1			@ print het volgende woord als db-flag=true

		bl	FIND					@ zoek het woord in de dictionary
		cmp top, #0					@ #0=woord niet gevonden
		beq 2f

	@ hier is er een woord gevonden in de dictionary
			prdbchar '>'
		cmp top, #-1				@ kijk of gevonden word 'immediate' is. DIT MOET 1 ZIJN!!!
		bne 4f						@ zo nee, dan naar 4f voor alle andere words

	@ hier een immediate woord		@ -> drop de -1 en doe de gevonden xt en ga dan naar verder:
			prdbstr immstr
		bl	DROP					@ drop de -1 van het gevonden immediate woord
			bl	DOTDBOUTHEX			@ print de gevonden xt
			prdbchar '-'

		popdts r0					@ laad xt in r0
		blx r0						@ en blx er heen - blx heeft GEEN +-32 Mb limit maar is ook niet relatief
		b	verder					@ en op naar het volgende woord -> functions!!!

	4:		prdbstr ordinary		@ (state=0):doe xt (state=1): of (maak BL naar xt vanaf HERE) óf (get/make inline code), en pas dan HERE aan
		bl	DROP					@ drop de 1 die aangeeft dat het een normaal woord is
			bl	DOTDBOUTHEX			@ print de gevonden xt als db-flag=true

		bl	varSTATE
		bl	FETCH
			bl	DOTDBOUTNUM			@ print de state als db-flag=true

		cmp top, #0x0				@ test STATE
		bne 3f						@ branch als state<>0

		bl	DROP					@ als hier-> interpretive state -> woord doen - eerst 0 van STATE droppen

		popdts r0					@ laadt xt van stack in r0
		blx	r0
		b	verder

	3:	bl	DROP					@ drop de 1 die de compiling-STATE aangaf
		bl	COMPILEBL				@ ( xt -- ) maw compile het woord (al dan niet als inline), gekenmerkt door xt, op HERE
		b	verder

	@ als hier dan een onbekend woord - kijken of het niet een nummer is
	2:	bl	OVER					@ ( -- addr 0 addr )

		bl	TONUMBER				@ ( -- addr, num, 0 ) als succesvol
		cmp top, #0					@ 0 betekent hele woord succesvol geconverteerd naar een nummer
		beq 5f

	@ als hier dan onbekend word/nummer ( -- addr-string, tussenresultaat, aantal niet geconv chars )
		bl	TWODROP					@ ( -- addr-string )

		bl	CR
		prstr unknown1
		bl	PR_CST					@ print woord wat op addr staat
		prstr unknown2

		bl	ANYKEY					@ en wacht op input van user

		b _ABORT					@ en start alles opnieuw - dictionary wordt NIET gereset, stacks wel

	@ als hier dan bekend nummer
	5:	bl	DROP					@ weg met de nul die 'alles geconverteerd' betekend
		bl	NIP						@ delete addr waar woord stond

			prdbchar '>'
			prdbstr literal
			bl	DOTDBOUTNUM

		bl	varSTATE
		bl	FETCH
		cmp top, #0					@ als STATE=0 dan keep nummer on stack
		bne 6f

		bl	DROP					@ drop de 0 van STATE en weer terug - nummer staat al op de stack
		b	verder					@ en dus verder met interpreting

	6:	bl	DROP					@ drop de 1 van STATE en zet lit met nummer in het woord wat gecompileerd wordt
		bl	MAKELIT
		b	verder

	@ als hier dan TIB leeg
	1:	bl	DROP					@ drop de nul die 'TIB leeg' aangeeft
next	@ checked }

@  **********************  COMPILE POSTPONE  *********************{
wordhead "compilebl", 9, INTERPRET, 1	@ ( xt -- ) compile bl of inline van woord xt op HERE
COMPILEBL:
		prolog
		bl GETINLINE				@ get lengte inline-code or return 0 for 'no inlining' ( xt -- xt lengte )
		cmp top, #0					@ als inlinelengte=0 dan geen inline
		beq 2f

		bl MAKEINLINE				@ ( xt lengte -- )
		b 1f

	2:	bl	DROP					@ haal de 0 van 'geen inline' weg
		bl	MAKEBL					@ maak opcode BL op stack van HERE naar xt ( xt -- opcode )
		bl	COMMA					@ ( opcode -- )
	1:
next		@ checked

wordhead "postpone", 8, COMPILEBL, -1	@ ( -- ) haalt next woord uit TIB, vind het en zet de execution semantics op HERE
POSTPONE:
		prolog
		lit16 ' '
		bl	F83WORD					@ ( -- address flag ) haal volgende woord tot een ' ' of einde TIB, niet ANSI
		cmp top, #0
		beq	_ABORT					@ geen volgend woord in TIB

			@ het volgende stukje is nog debug-code!!
			@bl	DUP					@ kopieer address op stack -> is Trans area = tijdelijk string gebied
			@bl	CR
			@lit16 11
			@bl	TAB
			@bl	PR_CST				@ print geparst woord

		bl	FIND					@ zoek het woord in de dictionary
		cmp top, #0					@ #0=woord niet gevonden
		beq _ABORT

		bl	DROP					@ drop flag - laat xt staan
		bl	COMPILEBL				@ zet execution semantics woord in huidige definitie
next

codehead "execute", 7, POSTPONE, 3, 1	@ ( xt -- )	execute xt
EXECUTE:
		popdts v					@ laadt xt van stack in v = 2 opcodes
		blx	v						@ en execute xt
codenext	@ CHECK!!!!}

wordhead "testje", 6, EXECUTE, 1	@ om dingen te testen
TESTJE:	prolog
		@ ***************************

		@ ***************************
next
@  *************************  MAKE more opcodes  *****************{
wordhead "getinline", 9, TESTJE, 1	@ ( xt -- xt inline ) inline byte staat op xt-40+4
GETINLINE:
		prolog						@ AANPASSEN ALS lengte van de #HEADER VERANDERD!!!
									@ Dit woord dropt de input niet van de stack!!

		sub r0, top, #36			@ r0 wijst nu naar inline-veld
		ldrb r1, [r0]				@ r1=inline-lengte codeword

		bl	varMAXINLINE			@ variabele maxinline die aangeeft of en tot welke lengte er geinlined wordt
		bl	FETCH					@ top nu maxlengte inline
		cmp r1, top
		bls 1f						@ als lengte codeword=r1 korter of gelijk aan maxlengte
									@ dit kan ook met movhi direct na cmp en bls weg
		mov r1, #0x0				@ hier is lengte vh codeword langer als de max > 0 teruggeven

	1:	mov top, r1					@ zet de lengte van r1 in top
next

wordhead "makeinline", 10, GETINLINE, 1	@ ( xt lengte -- )
MAKEINLINE:
		prolog
		popdts r0					@ r0 is lengte
		popdts r1					@ r1 is xt
	1:	ldr r2, [r1], #4			@ haal waarde op r1 en verhoog r1 met 4
		pushdts r2					@ eerste opcode op stack
		bl	COMMA					@ zet in definitie van woord
		sub r0, r0, #1				@ trek 1 van lengte af
		cmp r0, #0					@ al nul bereikt??
		bne 1b
next		@ checked

wordhead "makebl", 6, MAKEINLINE, 1	@ ( xt -- opcode van BL van HERE naar xt )
@ overwegen om bij groter dan 32M branch om een alternatief te maken (met ldv32 v en dan blx v)
MAKEBL:	prolog						@ opcode = 0xEB met dan 24 bits voor sprong
		mov r0, top					@ r0 is nu xt
		getvalvar r1, varHERE

		sub r2, r0, r1				@ als dit dus groter is als +- 32 Mb, dan een long jump maken!!
		sub r2, r2, #8				@ en nog 8 extra eraf door pipeline effect -> r2 nu de benodigde offset voor BL
		ldv32 r3, 0x03ffffff
		and r2, r2, r3
		lsr r2, r2, #2				@ shift logically 2 bits to right
		ldv32 top, 0xEB000000		@ top vullen met opcode, met nullen voor de sprong
		orr top, top, r2			@ voeg r2 toe aan top, top nu opcode van BL
next		@ checked

wordhead "makeblt", 7, MAKEBL, 1	@ ( xt -- opcode van BLT voor do-loop van HERE naar xt )
MAKEBLT:	prolog					@ opcode = 0xBA met dan 24 bits voor sprong
		mov r0, top					@ r0 is nu xt
		getvalvar r1, varHERE

		sub r2, r0, r1
		sub r2, r2, #8				@ en nog 8 extra eraf door pipeline effect -> r2 nu de benodigde offset voor BL
		ldv32 r3, 0x03ffffff
		and r2, r2, r3
		lsr r2, r2, #2				@ shift logically 2 bits to right
		ldv32 top, 0xBA000000		@ top vullen met opcode, met nullen voor de sprong
		orr top, top, r2			@ voeg r2 toe aan top, top nu opcode van BL
next		@ checked

wordhead "recurse", 7, MAKEBLT, 1	@ ( -- ) zet oldHERE+40 op stack, do MAKEBL en zet BL op HERE via comma
RECURSE:							@ ANSI: geen RECURSE na DOES>! -> zou bij mij wel moeten werken omdat CREATE oldHERE gebruikt!
		prolog						@ AANPASSEN ALS lengte van de #HEADER VERANDERD!!!
		bl	varOLDHERE				@ recurse alleen tijdens compilation -> oldHERE heeft alleen dan betekenis
		bl	FETCH
		lit32 40					@ 40 bytes later is de xt van het woord dat nu gecompileerd wordt
		bl	_ADD
		bl	MAKEBL					@ zet bl op HERE
		bl	COMMA
next		@ CHECK!!!}

@  ***************************  VARIOUS  *************************{
wordhead "anykey", 6, RECURSE, 1	@ ( -- ) wacht op any key, verder niets
ANYKEY:	prolog
	1:	bl	KEY
		popdts r0					@ loop nodig ivm nieuwe time-out op KEY!
		cmp r0, #-1					@ -1 is time-out flag van KEY
		beq 1b
		
		@bl	DROP					@ drop char van KEY
next		@ checked}

@  *********************  debug print routines  ******************{
wordhead "dup.hex", 7, ANYKEY, 1
DUPOUTHEX:
		prolog
		bl	DUP
		bl	OUTHEX
next		@  checked

wordhead "dup.", 4, DUPOUTHEX, 1
DUPOUTNUM:
		prolog
		bl	DUP
		bl	OUTNUM
next		@ checked

wordhead ".dbhex", 6, DUPOUTNUM, 1
DOTDBOUTHEX:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		bl	DUP
		bl	OUTHEX
	1:
next		@  checked

wordhead ".db.", 4, DOTDBOUTHEX, 1
DOTDBOUTNUM:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		bl	DUP
		bl	OUTNUM
	1:
next		@ checked

wordhead ".dbtbio", 7, DOTDBOUTNUM, 1	@ ( addr_TIB --  addr_TIB ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBTBIO:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 11
		bl	TAB
		prchar 171					@'\"'
		bl	DUP
		bl	PR_CST
		prchar 187					@'\"'
		bl	CR
	1:
next

wordhead ".dbcomma1", 9, DOTDBTBIO, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBCOMMA1:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 29
		bl	TAB
		prchar '!'
		bl	DUPOUTHEX			@ print word voor debug
	1:
next

wordhead ".dbcomma2", 9, DOTDBCOMMA1, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBCOMMA2:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 39
		bl	TAB
		bl	DUPOUTHEX
		bl	DUPOUTNUM
		bl	CR
	1:
next

wordhead ".dbint1", 7, DOTDBCOMMA2, 1	@ ( addr_trans -- addr_trans )
DOTDBINT1:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		bl	DUP						@ kopieer address op stack (=Trans area -> tijdelijk string gebied)
		bl	PR_CST					@ print woord wat daar staat
		lit16 11
		bl	TAB
	1:
next

wordhead ".dbmkhead1", 10, DOTDBINT1, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBMKHEAD1:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 29
		bl	TAB
		prchar '!'
	1:
next

wordhead ".dbmkhead2", 10, DOTDBMKHEAD1, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBMKHEAD2:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 39
		bl	TAB
		bl	varHERE
		bl	FETCH
		bl	DUP
		bl	OUTHEX
		bl	OUTDEC
		bl	CR
	1:
next

wordhead ".dbmkcohd1", 10, DOTDBMKHEAD2, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBMKCOHD1:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 39
		bl	TAB
		bl	varHERE
		bl	FETCH
		bl	OUTHEX
		bl	CR
	1:
next

wordhead ".dbrepeat1", 10, DOTDBMKCOHD1, 1	@ ( -- ) print de inhoud van textbuffer bij interpretatie tbv debug
DOTDBREPEAT1:
		prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		lit16 39
		bl	TAB
		prchar '>'
		bl	DUPOUTHEX
		bl	CR
	1:
next

wordhead ".pointers", 9, DOTDBREPEAT1, 1
PRINTPOINTERS:
		prolog
		bl	CR
		prstr pcis
		mov r0, pc
		pushdts r0
		bl	OUTHEX

		prstr spis
		mov r0, sp
		pushdts r0
		bl	OUTHEX

		prstr dtsis
		mov r0, dts
		pushdts r0
		bl	OUTHEX

		prstr ussis
		mov r0, uss
		pushdts r0
		bl	OUTHEX

		prstr fpsis
		mov r0, fps
		pushdts r0
		bl	OUTHEX

		prstr lris
		mov r0, lr
		pushdts r0
		bl	OUTHEX

		bl	CR
next		@ checked}

@  *************************** BENCHMARKING **********************{

wordhead "test10m", 7, PRINTPOINTERS, 1		@ ( -- ) start op 0x7CC
TEST10M:
		prolog
		bl	MCS						@ interne counter op stack

		lit32 10000000				@ 10 miljoen cycles -> 100c duurt 1 sec
		lit32 0
		bl	DODO
	99:								@ }
@  **********************			2c

		@lit32 10
		@lit32 1000

		bl	CORETIMERTO


		@ldr top,[dts, #12]			@ fourdrop inline
		@add dts, dts, #16

		@ldr top,[dts, #8]			@ threedrop inline
		@add dts, dts, #12

		ldr top,[dts, #4]			@ twodrop inline
		add dts, dts, #8

		@ldr top,[dts], #4			@ drop inline

@  **********************{		
		loopcheck
		blt 99b
		loopend

		bl	MCS
		bl	SWAP
		bl	_SUB					@ hier het aantal mcs op stack
		bl	DUP
		bl	SPACE
		bl	OUTDEC
		bl	SPACE
		@lit16 500					@ pi2 @ 1 Ghz
		@lit16 385					@ pi3 @ 1.3 Ghz
		lit16 417					@ pi3 @ 1.2 Ghz
		bl	_ADD					@ tel 0.05 cycle bij top op
		@lit16 1000					@ deel door de tijd van 0.1 cycle
		@lit16 769					@ deel door de tijd van 0.1 cycle bij 1.3 Ghz
		lit16 833					@ pi3 op 1.2 Ghz
		bl	_UDIV					@ nu cycles*10 op stack
		lit16 10
		bl	UDIVMOD					@ nu tiende cycles en cycles op stack
		bl	OUTDEC					@ onafhankelijk van BASE altijd in decimaal
		prchar '.'
		bl	OUTDEC
		prchar 'c'
next

wordhead "wcdummy", 7, TEST10M, 1
WCDUMMY:							@ 9c pi2 - 12c pi3!!
		prolog						@ bewijs effectivere pre-fetching van de pi2 of geheugen pi3 is niet sneller
		bl	WDUMMY
next		@ checked


wordhead "wdummy", 6, WCDUMMY, 1
WDUMMY:	prolog						@ 7c=prolog 9c=prolog4 èn prolog5
next		@ checked				  9c=prolog on pi3

codehead "cdummy", 6, WDUMMY, 0, 1
CDUMMY:								@ 4c on pi2, 6c on pi3!!!
codenext	@ checked

wordhead "speed", 5, CDUMMY, 1
SPEED:
	@ 1050003 mcs - 63c @  600 Mhz
	@  700000 mcs - 63c @  900 Mhz yeah!
	@  630002 mcs - 63c @ 1000 Mhz YEAH!!! Goed genoeg voor nu!!
	@  610000 mcs - 61c @ 1000 Mhz - soms - ook na multicore nog 61 of 62 of 63c
	@  zou rond 520000 mcs of beter moeten zijn bij armv8! en 480000 @ 1.3 Ghz
	@  469231 mcs - 61c @ 1300 Mhz pi3 Meestal
	@  446154 mcs - 58c @ 1300 Mhz pi3 YEAH!!

		prolog						@ dit is een benchmark voor testen van config.txt settings ed
		bl	CDUMMY					@ het zijn zorgvuldig uitgekozen words...
		bl	WCDUMMY					@ maar er wordt mn getest hoe snel een bl is...
		lit32 1234567
		lit16 4321
		bl	TWODUP
		bl	SWAP
		bl	DROP
		bl	THREEDROP
next		@ checked}

@  ****************************  VARIOUS  **************************{
wordhead ".debugflags", 11, SPEED, 1
PRDEBFL:							@{
		prolog
		lit16 0x3000
		bl	FETCH
		prstr debugfl1
		bl	DUP
		bl	OUTHEX
		prchar '-'
		bl	OUT32BIN
		bl	CR
		lit16 0x3004
		bl	FETCH
		prstr debugfl2
		bl	DUP
		bl	OUTHEX
		prchar '-'
		bl	OUT32BIN
		bl	CR
		lit16 0x3008
		bl	FETCH
		prstr debugfl3
		bl	DUP
		bl	OUTHEX
		prchar '-'
		bl	OUT32BIN
		bl	CR
		bl	CR
next		@}

wordhead "banner", 6, PRDEBFL, 1
BANNER:	prolog						@{
		bl	CR
		prstr myforth1
		lit16 34
		bl	TAB
		bl	conSTARTFREE
		bl	conSTARTDICT
		bl	_SUB
		prstr sizedict
		bl	OUTNUM
		bl	CR

		prstr myforth2
		lit16 34
		bl	TAB
		prstr noword
		bl	WORDNO
		bl	OUTNUM
		bl	CR

		prstr myforth3
		lit16 34
		bl	TAB
		@prstr sizeword
		prstr bytesfree
		bl varHEREDATA
		bl FETCH
		bl varHERE
		bl FETCH
		bl _SUB
		lit32 134217728			@ is 128Mb voor text-area
		bl	_ADD
		lit32 16777216			@ is 16Mb non-cachable
		bl	_ADD
		lit32 4176				@ daar weer 4k vanaf voor MAILBOX area
		bl	_SUB
		bl	OUTNUM
		bl	CR
		bl	CR
next		@ checked}
@}

@  ************************  multicore words  **********************{

wordhead "initcores", 9, BANNER, 1	@ start en initieer alle cores - hier zit geen power-nadeel aan door 'wfe'
INITCORES:							@{
		prolog						@ dit draait altijd op core0
		dsb
		isb		
		
	 	ldv32 r2, 100000			@ delay om te verhinderen dat cores tegelijk starten
	2:	subs r2, r2, #1				@ deze heeft een korte delay nodig
		bne 2b
		dsb

		ldv32 r1, 0x400000AC
    	ldv32 r0, INITCORE2			@ initcores now with dedicated routines to avoid non-coherency
    	dsb
    	str r0, [r1]
    	dsb
    	isb	
		
		ldv32 r2, 100000			@ delay om te verhinderen dat cores tegelijk starten #CHECK!
	1:	subs r2, r2, #1
		bne 1b
		dsb

		ldv32 r1, 0x4000009C
		ldv32 r0, INITCORE1			@ core1 wordt de #PIXEL core
		dsb
    	str r0, [r1]
    	dsb
    	isb							@ na INITCORE1 is een lange delay nodig!! Anders geen start!
    								@ daarom core1 als laatste
    	
@    	ldv32 r2, 10000000			@ delay om te verhinderen dat cores tegelijk starten #CHECK!
@	3:	subs r2, r2, #1
@		bne 3b

    	@ldr r0,=INITCORE3
    	@str r0, [r1, #0xBC]
    	@dsb
    	@isb
    	
    	@sev						@ haalt alle cores uit slaap om zodat bl INITCORES ook een 2de keer werkt
next		@}

wordhead "initcore1", 9, INITCORES, 1	@ dit 'word' moet hidden zijn - niet te gebruiken door user.
INITCORE1:							@ deze initcore1 draait op core1 #PIXEL{
									@ geen prolog nodig want er is niets wat gesaved moet worden
			reg2mem r13, r1, sivenc1R13s0
			reg2mem r14, r1, sivenc1R14s0
			
		ldv32 r13, 0x1100000		@ early stack-pointer - deze is minder belangrijk dan die in INITSYSC1!!
		ldv32 r14, 0x0				@ clear link reg #DEBUG
			incrmem sivc1init
		
		bl	INITSYSC1				@ Nu MMU en VFPv4 actief voor core 1 INITSYSC1=codeword->geen sp nodig
			reg2mem r13, r1, sivenc1R13s1
			reg2mem r14, r1, sivenc1R14s1
			incrmem sivc1init	
			
		bl	INITTASKBLOCKC1			@ zet stackpointers naar geheugen in het TASKBLOCK
			reg2mem r13, r1, sivenc1R13s2
			reg2mem r14, r1, sivenc1R14s2
			incrmem sivc1init
			
		bl	DISMMUC1				@ INITPAGE overbodig want die wordt gedeeld - werkt
			reg2mem r13, r1, sivenc1R13s3
			reg2mem r14, r1, sivenc1R14s3
			incrmem sivc1init
		
		bl	MMUNUC1					@ #UPDATE: aanpassen aan wat in INITSYS gedaan is voor core0
			reg2mem r13, r1, sivenc1R13s4
			reg2mem r14, r1, sivenc1R14s4
			incrmem sivc1init
		
			ldv32 r0, pxcounter
			ldv32 r1, 0
			str r1, [r0]
			incrmem sivc1init		@ sivcore1init is 6 bij succesvolle start
		
	@ ************  wacht op interrupt  ************

	1:	dsb							@ voor wfe altijd dsb
		wfe							@ wacht op any event en doe dan PIXEL
		
		ldv32 r0, pxcounter
		ldr r1, [r0]
		add r1, r1, #1				@ verhoog pxcounter met 1
		str r1, [r0]

		bl	WINTOBUFFER				@ copieer windows in onzichtbaar deel van screenbuffer - eerst dit dan switch!!
		
		dsb
		wfe							@ dit moet natuurlijk veel beter!!! #UPDATE
		bl	SWITCHSCR				@ en dan switchscr
		
		b 1b
next		@ maakt niet echt uit want hier komt het programma nooit, wel .text nodig}

wordhead "initcore2", 9, INITCORE1, 1
INITCORE2: 							@ Er hoeft hier geen prolog want het is een nieuwe start{
			reg2mem r13, r1, sivenc2R13s0
			reg2mem r14, r1, sivenc2R14s0
			
			ldv32 r13, 0x1200000	@ early stack-pointer #DEBUG
			ldv32 r14, 0x0			@ clear link reg #DEBUG	
			incrmem sivc2init
			
		bl	INITSYSC2				@ Nu MMU en VFPv4 actief voor core 1 INITSYSC1=codeword->geen sp nodig
			reg2mem r13, r1, sivenc2R13s1
			reg2mem r14, r1, sivenc2R14s1
			incrmem sivc2init
		
		bl	INITTASKBLOCKC2			@ zet stackpointers naar geheugen in het TASKBLOCK
			reg2mem r13, r1, sivenc2R13s2
			reg2mem r14, r1, sivenc2R14s2
			incrmem sivc2init
						
		bl	DISMMUC2				@ INITPAGE overbodig want die wordt gedeeld - werkt
			reg2mem r13, r1, sivenc2R13s3
			reg2mem r14, r1, sivenc2R14s3
		
			incrmem sivc2init
		bl	MMUNUC2					@ #UPDATE: aanpassen aan wat in INITSYS gedaan is voor core0
			reg2mem r13, r1, sivenc2R13s4
			reg2mem r14, r1, sivenc2R14s4
			incrmem sivc2init
			
		bl	INITWAVE
			reg2mem r13, r1, sivenc2R13s5
			reg2mem r14, r1, sivenc2R14s5
			incrmem sivc2init			@ sivcore2init is 6 bij succesvolle start
		
	@ ************  NOISE LOOP  ************

	9:	ldv32 r3, 252				@ r3 = down-counter sinus = 64*4
		
	2:	ldv32 r2, wvsinus6_10		@ is table with sinus-values
		ldr r1, [r2, r3]			@ get value from table, r3 = index-counter
		asr r1, r1, #5				@ deel door 16, van 12 naar 10 bits en nog 2 bit minder voor geluidsterkte
		
      	ldv32 r0, PWM_FIF1
		str r1, [r0] 				@ r3 to FIFO Address (channel 1)
    	str r1, [r0]				@ r3 to FIFO Address (channel 2)
      	
      	incrmem wvcounter			@ debugging purposesm should count up with 22-48kHz, usually much too fast

    6:	ldv32 r0, PWM_STA,  	 	@ r0 = PWM status-address
      	ldr r1, [r0]				@ value PWM status to r3
      	tst r1, #1					@ PWM_FULL1 - test bit 1 FIFO full
      	bne 6b						@ wait when FIFO full
      	
      	subs r3, r3, #4
      	bmi 9b						@ if lower than zero, go to start of table			
      	
      	b 2b						@ restart sinus-table at 63
next		@}

wordhead "initcore3", 9, INITCORE2, 1	@ dit word moet hidden zijn - niet te gebruiken door user.
INITCORE3: prolog					@{
		bl	INITSYS					@ Nu MMU en VFPv4 actief voor core 3 INITSYS=codeword->geen sp nodig
		bl	INITTASKBLOCK			@ zet alle stackpointers naar geheugen in het TASKBLOCK

		bl	DISMMU					@ INITPAGE overbodig want die wordt gedeeld
		bl	MMUNU					@ Werkt ook voor de vierde core!!

	@ ************  wacht op address in postbus0_1  ************

	1:	ldr w, =postbus0_3
		ldr v, [w]
		cmp v, #0					@ wacht tot address
		beq 2f

		blx v						@ linkjump to address

		lit16 0
		lit32 postbus0_3
		bl	STORE					@ en zet postbus0_3 weer terug op 0
		b 1b

	2:	dsb
		wfe							@ als geen address, dan wachten tot event is verzonden
		b 1b
next		@}

wordhead "startcore", 9, INITCORE3, 1 @ dit als na initiering een core gestart moet worden
STARTCORE:							@ ( address corenummer - ){
		prolog
		popdts r0					@ top naar r0

		cmp r0, #1
		bne 1f

		ldv32 r1, postbus0_1
		pushdts r1					@ zet address postbus0_1 op stack ( address postbus0_1 -- )
		b 4f

	1:	cmp r0, #2
		bne 2f

		ldv32 r1, postbus0_2
		pushdts r1					@ zet address postbus0_2 op stack ( address postbus0_2 -- )
		b 4f

	2:	cmp r0, #3
		bne 5f						@ spring naar 5f bij geen goed cpu nummer

		ldv32 r1, postbus0_3
		pushdts r1					@ zet address postbus0_3 op stack ( address postbus0_3 -- )

	4:	bl	STORE					@ zet het address in de correcte postbus
		barrier						@ wacht tot alles klaar - werkt meestal ook zonder maar is minder zeker
		sev							@ send 'event' naar alle cores
									@ die dan checken of er een address klaar staat
		b 6f						@ en dan terug

	5:	bl	DROP					@ drop address als geen goede cpu en return zonder start van core
	6:
next		@}

wordhead "testcore1", 9, STARTCORE, 1
TESTCORE1:							@{
		prolog

		lit16 'A'
		bl POSTBUS
next

wordhead "testcore2", 9, TESTCORE1, 1
TESTCORE2:
		prolog

	2:	bl	COREID
		bl	POSTBUS
		b 2b
next

wordhead "testcore3", 9, TESTCORE2, 1
TESTCORE3:
		prolog

	2:	bl	COREID
		bl	POSTBUS
		b 2b
next		@}

wordhead "postbus", 7, TESTCORE3, 1	@ zet waarde van stack in CPU-specifieke POSTBUS gericht aan core0
POSTBUS:							@ ( message -- ){
		prolog

		bl	COREID
		popdts r0					@ nu id va core die dit laat lopen op stack (maw: wie ben ik eigenlijk)

		cmp r0, #0x1
		bne 1f
		lit32 postbus1_0
		bl	STORE
		b 4f
	1:	cmp r0, #0x2
		bne 2f
		lit32 postbus2_0
		bl	STORE
		b 4f
	2:	cmp r0, #0x3
		bne 3f						@ spring naar 3f bij geen goed cpu nummer
		lit32 postbus3_0
		bl	STORE					@ zet het address in de correcte postbus
		b 4f						@ en dan terug
	3:	bl DROP						@ drop message als core 0 is (anders kan hier niet) en return zonder message
	4:
next		@ werkt}

codehead "coreid", 6, POSTBUS, 4, 1	@ ( -- core-id )
COREID:
		mrc p15, 0, r0, c0, c0, 5	@ haal id van core waar deze routine loopt
		and r0, r0, #0b11			@ r0 nu id van core - 00, 01, 10 of 11
		pushdts r0
codenext	@ checked }

@  ***************************  VARIOUS  ***************************{
wordhead "sieve", 5, COREID, 1		@ ( odd# -- #primes, mcs ) does 10 iterations of the sieve - as in BYTE jan'83
SIEVE:	prologmax					@ 1164 mcs tot 1406 mcs voor 10 its - geen idee waar dat van afhangt{
		
		bl	MCS						@ clock base op stack
		bl	SWAP					@ maar wel onder odd#
		
		ldv16 r0, 10				@ r0=M -> 10 iterations
		bl	DUP
		bl	ALLOCATE				@ maak ruimte voor array
		popdts r1					@ r1=start array in bytes
		popdts r6					@ r6= aantal oneven getallen om te checken
		
		ldv16 v, 3					@ voor verderop in 1c optellen
	
	@ m-loop -> 10 iterations	
	1:	ldv16 r2, 0					@ r2=count -> op nul voor elke iteration
		mov r3, r6					@ r3=i test 8190 of ander getal 'oneven getallen'
		
	@ set array to '1'
		ldv16 r4, 1
	2:	strb r4, [r1, r3]			@ vul array met '1'nen
		subs r3, r3, #1
		bne 2b
		
		ldv16 r4, 0
		
	@ i-loop
	3:	ldrb r7, [r1, r3]			@ get r3'th array element
		cmp r7, #0
		beq 4f
		
		@add r7, v, r3, lsl #1		@ r7=prime => prime is 3+(2*i)
		@add r8, r7, r3				@ dit is NIET sneller als de volgende 3 opcodes!!!
		
		add r7, r3, r3
		add r7, r7, #3				@ r7=prime => prime is i+i+3
		add r8, r7, r3				@ r8=k => k=prime+i
		
	6:	cmp r8, r6					@ if k>odd#(8190)
		bgt 5f
		
		strb r4, [r1, r8]			@ flags[k]=0
		add r8, r8, r7				@ k=r8 = k=r8 + prime=r7
		
		b 6b						@ do as long as k<=8190
		
	5:	add r2, r2, #1				@ r2=count -> tel aantal primes	
		
	4:	adds r3, r3, #1
		cmp r3, r6
		blt 3b						@ verder met i-loop
		
	@ einde i-loop
		subs r0, r0, #1				@ trek 1 af van r0=m
		bne	1b
		
	@ einde m-loop
		bl	MCS						@ einde tijdmeting
		bl	SWAP					@ omdraaien
		bl	_SUB					@ aftrekken van elkaar
		pushdts r2					@ zet aantal primes op stack
		bl	SWAP					@ en corrigeer stack
nextmax		@ works}

wordhead ":", 1, SIEVE, -1
DOCOL:	prolog						@{
		bl	STATEONE				@ state nu 1

		bl	varHERE
		bl	FETCH
		bl	varOLDHERE				@ nodig voor recursing en update varLAST
		bl	STORE					@ voor myself/recurse doel-address is OLDHERE + 40!

		bl	CREATEWORD				@ create een header en zet LAST op start van de nieuwe eigenlijke code
next		@ checked}
@}

@  **************************  VARIABLE etc  ***********************{
wordhead "variable", 8, DOCOL, -1	@ ( dit is de ansi variant zonder start-waarde van stack ) NAAR DE FORTH83 versie gaan!
VARIABLE:	prolog					@ state-smart niet meer nodig{

		bl	varHERE
		bl	FETCH
		bl	varOLDHERE				@ nodig voor recursing en update varLAST
		bl	STORE

		bl	CREATECODE				@ create een codeheader en zet HERE op start van de eigenlijke code
									@ geen prolog want variabele zelf is een codewoord (en dus ook met inlining)
		lit32 0xe5295004 			@ =str r5, [r9, #-4]! = str top, [dts, #-4]!
		bl	COMMA					@ en zet op here

		bl	varHERE
		bl	FETCH					@ nu waarde van HERE op stack
		lit16 12
		bl	_ADD					@ addres wijst nu 3 woorden na HERE

		bl	DUP						@ onthoud 1 nummer voor MAKEMOVT, nu eerst lower 16 bits

		bl	MAKEMOVW				@ maak de opcode voor MOVW
		bl	COMMA					@ en zet op here

		bl	MAKEMOVT				@ maak de opcode voor MOVT
		bl	COMMA					@ en zet op here - MOVT kan weggelaten als 16 bit address voor waarde variabele
									@ maar dan is de add iets terug niet goed - evt een soort fast variabele overwegen
		bl	MAKE_CODENEXT			@ opcode voor codenext

		lit32 0x0					@ allot 1 word en zet op nul = initiëele waarde - deze lit32 weg en het is gelijk Forth83!!!
		bl	COMMA

		bl	UPDATELAST
		
		FlushDataCache
		bl	CLEANCACHE				@ zal wel nodig zijn - weet dat niet zeker
next		@ checked}

wordhead "constant", 8, VARIABLE, -1	@ compilation ( n -- ), execution ( -- n )
CONSTANT:	prolog					@ state-smart niet meer nodig{

		bl	varHERE
		bl	FETCH
		bl	varOLDHERE				@ nodig voor recursing en update varLAST
		bl	STORE

		bl	CREATECODE				@ create een codeheader en zet HERE op start van de eigenlijke code
									@ geen prolog want constante zelf is een codewoord (en dus ook inlinebaar)
		lit32 0xe5295004 			@ =str r5, [r9, #-4]! = str top, [dts, #-4]!
		bl	COMMA					@ en zet op here

		bl	DUP						@ onthoud 1 nummer voor MAKEMOVT, nu eerst lower 16 bits

		bl	MAKEMOVW				@ maak de opcode voor MOVW = lower 16 bits
		bl	COMMA					@ en zet op here

		bl	MAKEMOVT				@ maak de opcode voor MOVT
		bl	COMMA					@ op het moment ook bij 0 i upper 16bits een opcode -> cave length voor inlining!!!

		bl	MAKE_CODENEXT

		bl	UPDATELAST
		
		FlushDataCache				@ #UPDATE: is nu te veel en te vaak
		bl	CLEANCACHE				@ zal wel nodig zijn - weet dat niet zeker
next		@ checked}
@}

@  ***************************  CREATE DOES  ***********************{
wordhead "createword", 10, CONSTANT, 1
CREATEWORD:	prolog					@ make new header in dictionary met het volgende woord in TIB
		@ zet flags - mn hidden op 1 mist nog

		lit16 ' '
		bl	F83WORD					@ haal volgende woord van TIB f83
		cmp top, #0x0
		bne	1f						@ als nog minstens 1 woord in TIB -> sucess! Ga verder naar 1f

			b _ABORT					@ anders start opnieuw met schone stacks
			@ deze _ABORT moet straks vervangen worden door IO nav lege TIB
			@ en print error 'name expected'

	1:	@ als hier dan een volgende woord gevonden in TIB dat gedefinieerd gaat worden
		bl	DUPDEFWARN				@ waarschuw bij evt herdefinitie en ga verder
		bl	MAKEHEADER				@ bouw vanaf HERE een standaard header en pas HERE aan
		bl	MAKEPROLOG				@ voeg de code van prolog toe want het gaat om een woord!
next		@ checked

wordhead "create", 6, CREATEWORD, 1	@ dit is de CREATE van CREATE .. DOES>
CREATE:	prolog						@ make new header in dictionary met het volgende woord in TIB

		bl	varHERE
		bl	FETCH
		bl	varOLDHERE				@ nodig voor recursing en update varLAST
		bl	STORE

		bl	CREATEWORD

		lit32 0xe5295004 			@ =str r5, [r9, #-4]! = str top, [dts, #-4]!
		bl	COMMA					@ en zet op here

		bl	varHERE
		bl	FETCH					@ nu waarde van HERE op stack
		lit32 12
		bl	_ADD					@ addres wijst nu 3 woorden na HERE

		bl	DUP						@ onthoud 1 addres (HERE+12) voor MAKEMOVT, nu eerst lower 16 bits

		bl	MAKEMOVW				@ maak de opcode voor MOVW
		bl	COMMA					@ en zet op here

		bl	MAKEMOVT				@ maak de opcode voor MOVT
		bl	COMMA					@ en zet op here
		bl	MAKENEXT				@ NEXT wordt op HERE gezet - deze opcode wordt door DOES> vervangen door een B naar net na DOES>
									@ datafield is net na de NEXT - hier dus - CREATE doet zelf GEEN ALLOT!!!

		bl	UPDATELAST				@ nieuw gedefinieerd woord hiermee vindbaar - voordat does> is gelopen
		bl	CLEANCACHE				@ opcodes geschreven en dus cleancache nodig
next		@ checked!

wordhead ">body", 5, CREATE, -1		@ ( -- dfa van woord na >body )	alleen buiten compilatie?
TOBODY:	prolog						@ geeft het dfa van een woord gecreerd met CREATE, is xt van een woord +16
		lit16 ' '					@ Dit woord moet betere fout-afhandeling krijgen, maar is op zich goed
		bl	F83WORD					@ haal volgende woord van TIB f83

		cmp top, #0x0
		bne	1f						@ als nog minstens 1 woord in TIB -> sucess! Ga verder naar 1f

			b _ABORT				@ anders start opnieuw met schone stacks
			@ deze _ABORT moet straks vervangen worden door IO nav lege TIB
			@ en print error 'name expected'

	1:	@ als hier dan een volgende woord gevonden in TIB dat gevonden moet worden
		bl	FIND					@ kijk of dat woord in de Dictionary staat

		cmp top, #0x0
		bne	2f						@ als woord gevonden in dictionary -> sucess! Ga verder naar 2f

			b _ABORT				@ anders start opnieuw met schone stacks
			@ en print error 'name expected'

	2:	bl	DROP					@ drop de flag die aangeeft dat het woord in de dictionary zit
		lit16	20
		bl	_ADD					@ tell 16 op bij de gevonden xt -> is de dfa van een woord gecrëerd met CREATE
next		@ checked

wordhead "does>", 5, TOBODY, 1		@ ( -- )
DOESTO:	prolog						@ oa op 56 bytes na LAST de opcode vervangen door een B naar net na DOESTO => via r14!
									@ #HEADER!! aanpassen als header-lengte vernaderd.
		mov r0, r14					@ get returnlink, dat is waar de branch van/na CREATE heen moet!
		pushdts r0					@ r14 nu op stack = locatie waar jump heen moet

		bl	varLAST					@ LAST is al ge-update door CREATE en dus niet oldHERE maar LAST gebruiken
		bl	FETCH
		lit16 56
		bl	_ADD					@ nu plek op stack waar een next vervangen moet worden door een B naar net na DOES>
		bl	MAKETODOES				@ nu opcode van b net na CREATE naar na DOES> op stack ( r14 opcode )

		bl	varLAST
		bl	FETCH
		lit16 56
		bl	_ADD					@ op stack nu opcode en adres waar opcode gezet moet worden
		bl	STORE					@ opcode nu op plek geschreven

		bl	CLEANCACHE				@ opcode geschreven en dus caches cleanen.

		ldmfd r13!,{r0-r3, lr}		@ dit voorkomt dat hierna de dingen na does> al tijdens de definitie gerund worden
next		@ checked}

@  *****************************  VARIOUS  *************************{
wordhead "(", 1, DOESTO, -1			@ ( -- ) works #UPDATE - moet nog genestte haakjes!
PAREN:	prolog
		@mov r1, #1					@ aantal haakjes

	2:	bl	NEXTCHAR
		popdts r0					@ stack nu weer neutraal
		cmp r0, #0x0
		beq 1f						@ TIB leeg -> klaar

		cmp r0, #41					@ is al een haakje gezien, zo nee dan verder kijken
		bne 2b
	1:
next		@ checked #UPDATE meer nivos haakjes }

@  *************************  OPCODE creation  *********************{
wordhead "createcode", 10, PAREN, 1	@ nu nog met inlinen van #3
CREATECODE:	prolog					@ make new codeheader in dictionary met het volgende woord in TIB{
		lit16 ' '
		bl	F83WORD					@ haal volgende woord van TIB f83
		cmp top, #0x0
		bne	1f						@ als nog minstens 1 woord in TIB -> sucess! Ga verder naar 1f

			b _ABORT				@ anders start opnieuw met schone stacks
			@ deze _ABORT moet straks vervangen worden door IO nav lege TIB
			@ tot dan: print error 'name expected'

	1:	@ als hier dan een volgende woord gevonden in TIB dat gedefinieerd gaat worden
		bl	DUPDEFWARN				@ waarschuw bij evt herdefinitie en ga verder

		lit16 3
		bl	MAKECODEHEADER			@ bouw vanaf HERE een standaard code header met stack=3 als inline#
next		@ checked}

wordhead "dupdefwarning", 13, CREATECODE, 1	@ ( addr word to be checked -- addr still there )
DUPDEFWARN: prolog					@{
		bl	DUP						@ dup van addr ( addr addr )
		bl	FIND					@ ( addr, xt/addr, -1/1/0 )
		cmp top, #0x0				@ FIND geeft addr en nul terug als woord niet gevonden
		beq	1f

		@ als hier dan woord al aanwezig in dictionary
		bl	TWODROP					@ drop flag die aangeeft dat het woord er al is
									@ drop xt van het gevonden woord ( addr )
			bl	DUP					@ kopieer addres word to be checked ( addr addr )
			bl	SPACE
			bl	SPACE
			prstr redefined1		@ print eerste string waarschuwing
			bl	PR_CST				@ print word - addres weer verbruikt ( addr )
			prstr redefined2		@ print tweede string waarschwuwing
			bl	CR
		b 2f						@ klaar!

	1:	bl	TWODROP					@ drop addr en '0' van 'FIND'
	2:
next		@ checked}

wordhead "makelit", 7, DUPDEFWARN, 1	@ ( nummer -- ) zet op HERE de executie-semantics voor een literal 16 of 32 bit
MAKELIT:							@{
		prolog
		lit32 0xe5295004 			@ =str r5, [r9, #-4]! = str top, [dts, #-4]!
		bl	COMMA

		bl	DUP						@ onthoud 1 nummer voor later, nu eerst lower 16 bits
		bl	MAKEMOVW				@ maak de opcode voor MOVW
		bl	COMMA

		bl	MAKEMOVT				@ maak de opcode voor MOVT
		cmp top, #0x0				@ maar zet alleen op plek als movt nodig (voor pos>16 bit en alle neg getallen)
		beq 1f

		bl	COMMA
		b 2f

	1:	bl 	DROP					@ drop de 0x0 die aangeeft dat geen MOVT nodig
	2:
next		@ checked}

wordhead "makemovw", 8, MAKELIT, 1	@ ( nummer -- opcode voor movw )
MAKEMOVW:							@ e30f5fff -> movw r5, #0xffff{
		prolog
		mov r0, top					@ zet 'nummer' in r0

		ldv32 top, 0xe3005000		@ top nu basis van opcode - 16 bits moeten er nog in ge-orred worden

		ldv16 r1, 0xfff				@ mask voor de laagste 12 vd 16 bits
		and r2, r0, r1				@ r2 nu laagste 12 bits
		orr top, top, r2			@ deze laagste bits nu op hun plek in de opcode

		ldv16 r1, 0xf000			@ mask voor de hoogste 4 vd 16 bits
		and r2, r0, r1				@ r2 nu de hoogste 4 vd 16 bits
		mov	r2, r2, lsl #4			@ shift left met 4 bits
		orr top, top, r2			@ en zet die bits op hun plek - top is nu correct
next		@ checked}

wordhead "makemovt", 8, MAKEMOVW, 1	@ ( nummer -- opcode voor movt) geeft nu altijd een opcode terug, ook bij 0!!
MAKEMOVT:							@ e34a5aaa = movt r5, #0xaaaa{
		prolog
		mov r0, top, lsr #16		@ zet 16 hoogste bits van nummer in r0 op bit (15:0)

		@cmp r0, #0x0				@ dus een positief 16 bits getal
		@beq 1f						@ en movt dus niet nodig #UPDATE MOVT altijd 2 opcodes, moet weer 1 opcode worden!!
		
		ldv32 top, 0xe3405000		@ top nu basis van opcode - 16 bits moeten er nog in ge-orred worden

		ldv16 r1, 0xfff				@ mask voor de laagste 12 vd 16 bits
		and r2, r0, r1				@ r2 nu laagste 12 bits
		orr top, top, r2			@ deze laagste bits nu op hun plek in de opcode

		ldv16 r1, 0xf000			@ mask voor de hoogste 4 vd 16 bits
		and r2, r0, r1				@ r2 nu de hoogste 4 vd 16 bits
		mov	r2, r2, lsl #4			@ shift right met 4 bits
		orr top, top, r2			@ en zet die bits op hun plek - top is nu correct
		@b 2f

	@1:	mov top, #0x0				@ 0x0 is signal that MOVT is not needed
	@2:								@ tijdelijk buiten werking ivm (wat was het ook al weer??!)
next		@ checked}

wordhead "makemovwv", 9, MAKEMOVT, 1	@ ( nummer -- opcode voor movw v )
MAKEMOVWV:							@ e30fbfff -> movw v, #0xffff{
		prolog
		mov r0, top					@ zet 'nummer' in r0

		ldv32 top, 0xe300b000		@ top nu basis van opcode - 16 bits moeten er nog in ge-orred worden

		ldv16 r1, 0xfff				@ mask voor de laagste 12 vd 16 bits
		and r2, r0, r1				@ r2 nu laagste 12 bits
		orr top, top, r2			@ deze laagste bits nu op hun plek in de opcode

		ldv16 r1, 0xf000			@ mask voor de hoogste 4 vd 16 bits
		and r2, r0, r1				@ r2 nu de hoogste 4 vd 16 bits
		mov	r2, r2, lsl #4			@ shift left met 4 bits
		orr top, top, r2			@ en zet die bits op hun plek - top is nu correct
next		@ checked}

wordhead "makemovtv", 9, MAKEMOVWV, 1	@ ( nummer -- opcode voor movt v )
MAKEMOVTV:							@ e34abaaa = movt v, #0xaaaa{
		prolog
		mov r0, top, lsr #16		@ zet 16 hoogste bits van nummer in r0 op bit (15:0)

		ldv32 top, 0xe340b000		@ top nu basis van opcode - 16 bits moeten er nog in ge-orred worden

		ldv16 r1, 0xfff				@ mask voor de laagste 12 vd 16 bits
		and r2, r0, r1				@ r2 nu laagste 12 bits
		orr top, top, r2			@ deze laagste bits nu op hun plek in de opcode

		ldv16 r1, 0xf000			@ mask voor de hoogste 4 vd 16 bits
		and r2, r0, r1				@ r2 nu de hoogste 4 vd 16 bits
		mov	r2, r2, lsl #4			@ shift right met 4 bits
		orr top, top, r2			@ en zet die bits op hun plek - top is nu correct
next		@ checked}

wordhead "makeprolog", 10, MAKEMOVTV, 1
MAKEPROLOG:							@{
		prolog
		lit32 0xe92d400f			@ is opcode voor prolog4
		bl	COMMA
next		@ checked}

wordhead "makenext", 8, MAKEPROLOG, 1
MAKENEXT:							@{
		prolog
		lit32 0xe8bd800f			@ is opcode voor next
		bl	COMMA					@ het zou kunnen dat CLEANCACHE hier handig is
next		@ checked}

wordhead "make_codenext", 13, MAKENEXT, 1
MAKE_CODENEXT:						@{
		prolog
		lit32 0xe12fff1e 			@ =: bx lr is opcode voor codenext
		bl	COMMA
next		@ checked}
@}

@  ******************************  COMMA  **************************{
wordhead ",", 1, MAKE_CODENEXT, 1	@ maak 4 bytes vrij op HERE en sla stack daar op
COMMA:	prolog						@{
			bl	DOTDBCOMMA1

		bl	varHERE
		bl	FETCH					@ haal address in varHERE ( waarde address )
			bl	DOTDBCOMMA2

		bl	STORE					@ sla waarde van stack op op address wat in varHERE zat

		bl	varHERE
		bl	FETCH
		bl	CLEANCACHECOMMA			@ clean instructie-cache na elk comma

		lit16 4
		bl	varHERE
		bl	PLUSSTORE				@ en verhoog varHERE met 4
next		@ checked}

wordhead "c,", 2, COMMA, 1			@ maak 1 bytes vrij op HERE en sla byte op stack daar op
CCOMMA:	prolog						@{
			bl	DOTDBCOMMA1

		bl	varHERE
		bl	FETCH					@ haal waarde in varHERE
			bl	DOTDBCOMMA2

		bl	CSTORE					@ sla byte waarde van stack op op varHERE

		bl	varHERE
		bl	FETCH
		bl	CLEANCACHECOMMA			@ clean instructie-cache na elk comma

		lit16 1
		bl	varHERE
		bl	PLUSSTORE				@ en verhoog varHERE met 1
next		@ checked}
@}

@  *****************************  HEADERS  *************************{
wordhead "makeheader", 10, CCOMMA, 1
MAKEHEADER:							@ ( address waar naam van woord staat als string -- )
		prologmax						@ maakt de header van een woord in de dictionary tot aan code
									@ #HEADER Uiteraard!
		bl	varLAST					@ dit wijst naar de laatste link
		bl	FETCH
		lit16 40
		bl	_ADD					@ nu naar de xt van het laatste woord
		bl	COMMA					@ zet een link naar deze xt als eerste in nieuwe header!!

		lit16 0x00					@ 0=no inlining
		bl	CCOMMA

		lit16 0x01					@ 1=normal word - dus niet immediate
		bl	CCOMMA

		lit16 0x00					@ 0=not hidden
		bl	CCOMMA

		bl	DUP						@ kopieer address van geparsed 'word' op stack - stond nog op stack
		bl	FETCH					@ nu lengte woord op stack - top is nu lengte
		mov r1, top					@ r1 is nu lengte word, top ook nog lengte
		bl	CCOMMA					@ zet top=lengte van woord in header in 8 bits - top nu weer address

			bl	DOTDBMKHEAD1

		popdts r2					@ r2 nu het address van word als counted string en stack leeg

		bl	varHERE
		bl	FETCH					@ get HERE
		popdts r4					@ r4 is waar chars heen moeten
		add r2, r2, #4				@ r2 wijst nu naar 1ste char van word - string lengte is 32 bit

	1:	ldrb r3, [r2], #1			@ haal char van r2 en verhoog r2 met 1
		strb r3, [r4], #1			@ store char op r4 en verhoog r4 met 1

			getvalvar v, varDEBUG
			@bl	varDEBUG
			@bl	FETCH
			@popdts v					@ top weer up to date
			@ #DEBUG

			cmp v, #0					@ als debug 0, dan geen debug printen
			beq	2f

			pushdts r3
			bl	EMIT					@ print char voor debug

	2:	subs r1, r1, #1				@ verlaag lengte counter met 1
		bne 1b

			bl	DOTDBMKHEAD2

		lit16 32
		bl	varHERE					@ HERE is al 8 opgehoogd, maar niet bij het kopieren van naam. Daarom nog 32 erbij
		bl	PLUSSTORE				@ varHERE nu naar begin van code van woord
nextmax		@ checked

wordhead "makecodeheader", 14, MAKEHEADER, 1	@ ( (address waar naam van woord staat als string) (inline lengte) -- )
MAKECODEHEADER:
		prologmax					@ maakt de header van een woord in de dictionary tot aan code
									@ #HEADER uiteraard!
		bl	varLAST					@ dit wijst naar de laatste link
		bl	FETCH
		lit16 40
		bl	_ADD					@ nu naar de xt van het laatste woord
		bl	COMMA					@ zet een link naar deze xt als eerste in nieuwen header!!

		@lit16 0x00					inlinelengte van de stack
		bl	CCOMMA					@ is lengte inlining - CCOMMA is al met cachecleaning!

		lit16 0x01					@ 1=normal word - dus niet immediate
		bl	CCOMMA

		lit16 0x00					@ 0=not hidden

		bl	CCOMMA

		bl	DUP						@ kopieer address van geparsed 'word' op stack - stond nog op stack
		bl	FETCH					@ nu lengte woord op stack - top is nu lengte
		mov r1, top					@ r1 is nu lengte word, top ook nog lengte
		bl	CCOMMA					@ zet top=lengte van woord in header in 8 bits - top nu weer address

		bl	DOTDBMKHEAD1

		popdts r2					@ r2 nu het address van word als counted string en stack leeg

		getvalvar r4, varHERE		@ r4 is nu waar chars heen moeten
		
		add r2, r2, #4				@ r2 wijst nu naar 1ste char van word - string lengte is 32 bit

	1:	ldrb r3, [r2], #1			@ haal char van r2 en verhoog r2 met 1
		strb r3, [r4], #1			@ store char op r4 en verhoog r4 met 1

			getvalvar r0, varDEBUG
			
			cmp r0, #0					@ als debug 0, dan geen debug printen
			beq	2f

			pushdts r3
			bl	EMIT					@ print char voor debug

	2:	subs r1, r1, #1				@ verlaag lengte counter met 1
		bne 1b

		bl	DOTDBMKCOHD1

		lit16 32
		bl	varHERE					@ HERE is al 8 opgehoogd, maar niet bij het kopieren van naam. Daarom 32
		bl	PLUSSTORE				@ varHERE nu naar begin van code van woord
nextmax		@ checked

wordhead "immediate", 9, MAKECODEHEADER, -1
IMMEDIATE:							@ set de flag van het laatst gedefinieerde woord op immediate
		prolog						@ #HEADER
		getvalvar r0, varLAST
		ldv32 r1, -1				@ -1 is immediate
		strb r1, [r0, #5]			@ en zet die -1 5 bytes na de start van het woord als byte
next		@ checked

wordhead ";", 1, IMMEDIATE, -1
DOSEMI:	prolog
		bl	STATENULL				@ state nu weer 0
			@ resolve open ends - DOES> sluit zichzelf af
		bl	MAKENEXT				@ sluit woord af met 'next'
		bl	UPDATELAST				@ dit via oldHERE
		bl	CLEANCACHE				@ zodat de cache weet dat er een nieuw woord is dat uit opcodes bestaat...
next		@ checked

wordhead "state0", 6, DOSEMI, 1
STATENULL:	prolog
		lit16 0
		bl	varSTATE
		bl	STORE					@ state nu 0
next		@ checked

wordhead "state1", 6, STATENULL, 1
STATEONE:	prolog
		lit16 1
		bl	varSTATE
		bl	STORE					@ state nu 1
next		@ checked

wordhead "getstate", 8, STATEONE, 1	@ ( -- state:(0/1) )
GETSTATE:	prolog					@ #NAME: STATETO state> EN dit ding wordt 2* niet gebruikt
		bl	varSTATE
		bl	FETCH
next

wordhead "updatelast", 10, GETSTATE, 1
UPDATELAST: prolog
		bl	varOLDHERE				@ begin updaten 'last'
		bl	FETCH					@ stack wijst nu naar nieuwe laatste link!
		bl	varLAST
		bl	STORE					@ sla dit op in varLAST
next		@ checked}

@  ****************************  VARIOUS  **************************{
wordhead ".sizeuserdic", 12, UPDATELAST, 1
PRINTSIZEUD: prolog					@ #NOTCORE
		bl	CR
		bl	conSTARTFREE
		bl	conSTARTDICT
		bl	_SUB
		prstr sizebanner
		bl	CR
		prstr sizedict
		bl	OUTNUM
		bl	CR
		bl	varHERE
		bl	FETCH
		bl	conSTARTFREE
		bl	_SUB
		prstr sizeprog
		bl	OUTNUM					@ print de grootte van het nieuw gecompileerde in bytes
		bl	CR
		prstr sizebanner
		bl	CR
next		@ checked}

@  ********************  STATE en INLINE setting  ******************{
wordhead "inline#", 7, PRINTSIZEUD, -1	@ ( n -- ) change the amount of inlining -1:inline alles, 0:niets, n:tot n
INLINENO:							@ dit blijft, maar inline#=5 is in 99.9% of cases precies goed
		prolog
		bl	varMAXINLINE
		bl	STORE
next		@ checked

wordhead "[", 1, INLINENO, -1
LITERAL:
		prolog
		bl	STATENULL				@ state nu 0
next		@ checked #UPDATE STATENULL en LITERAL doen het zelfde!!

wordhead "]", 1, LITERAL, -1
COMPILE:
		prolog
		bl	STATEONE				@ state nu 1
next		@ checked #UPDATE STATEONE en COMPILE doen het zelfde!!}

@  ************************  DO - LOOP words  **********************{
wordhead "do", 2, COMPILE, -1		@ IMMEDIATE - compile only!!
DO:		prolog
		lit32 DODO					@ nu xt van DODO op stack
		bl	COMPILEBL				@ compile DODO as inline at HERE
		lit32	0xe320f000			@ is nop - hier komt, na DODO, de code voor het (-12)
		bl	COMMA					@ op uss zetten van het LEAVE-address
		lit32	0xe320f000			@ is nop (-8)
		bl	COMMA
		lit32	0xe320f000			@ is nop (-4)
		bl	COMMA
		bl	varHERE					@ doel address
		bl	FETCH					@ nu het doel van de terugloop op de stack - checked - dit naar uss
		bl	TO_R					@ doeladdres op uss
next		@ checked

wordhead "loop", 4, DO, -1			@ IMEDIATE - compile only!!
LOOP:	prolog
		lit32 DOLOOP
		bl	COMPILEBL				@ compile DOLOOP as inline at HERE

		bl	FROM_R					@ haal doeladdress van uss
		sub r0, top, #12			@ r0 nu daar waar de code voor LEAVE heen moet

		bl	MAKEBLT					@ al la MAKEBL
		bl	COMMA					@ hierna

		pushdts r0					@ nu address waar code voor LEAVE heen moet op stack
		bl INITLEAVE

		lit32	UNLOOP
		bl	COMPILEBL
next		@ checked

wordhead "+loop", 5, LOOP, -1		@ IMEDIATE - compile only!!
PLLOOP:	prolog
		lit32 DOPLLOOP
		bl	COMPILEBL				@ compile DOLOOP as inline at HERE

		bl	FROM_R					@ haal doeladdress van uss
		sub r0, top, #12			@ r0 nu daar waar de code voor LEAVE heen moet

		bl	MAKEBLT					@ al la MAKEBL
		bl	COMMA

		pushdts r0					@ nu address waar code voor LEAVE heen moet op stack
		bl INITLEAVE

		lit32	UNLOOP
		bl	COMPILEBL
next		@ checked

wordhead "4+loop", 6, PLLOOP, -1	@ IMEDIATE - compile only!!
FOURPLLOOP:	prolog
		lit32 DO4PLLOOP
		bl	COMPILEBL				@ compile DOLOOP as inline at HERE
		bl	FROM_R					@ haal doeladdress van uss
		sub r0, top, #12			@ r0 nu daar waar de code voor LEAVE heen moet

		bl	MAKEBLT					@ al la MAKEBL
		bl	COMMA

		pushdts r0					@ nu address waar code voor LEAVE heen moet op stack
		bl INITLEAVE

		lit32	UNLOOP
		bl	COMPILEBL
next		@ checked

wordhead "initleave", 9, FOURPLLOOP, 1 @ ( a-addr (plek waar code voor initleave heen gaat) -- )
INITLEAVE:
		prolog						@ #ERROR: er mist caching hier, geen comma gebruikt maar store!! => nog cleancache inbouwen

		popdts r0

		bl	varHERE					@ voor LEAVE address
		bl	FETCH					@ nu address op stack waar LEAVE heen moet springen
		bl	DUP

		bl	MAKEMOVWV				@ dit is het eerste deel van de code, nu op stack
		pushdts	r0					@ hier moet dat geschreven worden
		bl	STORE

		add r0, r0, #4
		bl	MAKEMOVTV
		pushdts r0
		bl	STORE

		add r0, r0, #4
		lit32 0xe52ab004			@ =str v, [uss]
		pushdts r0
		bl	STORE
next		@ checked

codehead "(do)", 4, INITLEAVE, 5, 1	@ compile only!! ( lim i -- ) 7-8c oid inline
DODO:								@ loop springt net na het einde van deze routine!!
		str looplim, [uss, #-4]!	@ zet looplim op userstack
		str loopi, [uss, #-4]!		@ zet looplim op userstack

		mov loopi, top				@ i komt uit top
		ldr looplim, [dts], #4		@ lim komt van pos 2
		ldr top, [dts], #4			@ en herstel top
codenext	@ checked

codehead "ic@", 3, DODO, 2, 1		@ haal een char van [i] en zet op stack
ICFETCH:
		str top, [dts, #-4]!		@ save top
		ldrb top, [loopi]			@ en haal char van address in i
codenext	@ checked

codehead "i@", 2, ICFETCH, 2, 1		@ ( -- word ) haal een woord van [i] ( van de do...loop) en zet op stack
IFETCH:
		str top, [dts, #-4]!		@ save top
		ldr top, [loopi]			@ en haal word van address in i
codenext	@ checked

codehead "i", 1, IFETCH, 2, 1		@ compile only ( -- i ) 5 - 2c
_DOI:	str top, [dts, #-4]!		@ save top
		mov top, loopi				@ en zet i op de stack
codenext	@ checked

codehead "j", 1, _DOI, 2, 1			@ compile only ( -- j ) 5 - 3c inline
_DOJ:	str top, [dts, #-4]!		@ save top

		ldr top, [uss, #4]			@ bij LEAVE 1 pos onder de bovenkant van uss omdat daar het address van leave staat
codenext	@ checked

codehead "k", 1, _DOJ, 2, 1			@ compile only ( -- k ) 5 - 3c inline
_DOK:	str top, [dts, #-4]!		@ save top
									@ en zet K op de stack - #8 want er staat ook een limiet op de stack!!
		ldr top, [uss, #16]			@ er staan 2 LEAVE adressen en een limiet op de uss-stack
codenext	@ checked

codehead "(loop)", 6, _DOK, 2, 1	@ compile only ( -- ) 2c loop inline
DOLOOP:
		add loopi, loopi, #1		@ verhoog loop index met 1
		cmp loopi, looplim			@ vergelijk lim en i
		@ blt naar do				@ dat adres moet ingevuld worden door _LOOP bij het compileren
codenext	@ checked

codehead "unloop", 6, DOLOOP, 3, 1	@ compile only ( -- ) 2c loop inline
UNLOOP:
		add uss, uss, #4			@ drop LEAVE address van uss
		ldr loopi, [uss], #4		@ maak van J weer I als loop klaar
		ldr looplim, [uss], #4		@ en zet de oude limiet weer terug
codenext	@ checked

codehead "leave", 5, UNLOOP, 2, 1	@ compile only ( -- ) leave do-loop (inlinen met 2, en niet met 1!!!
LEAVE:								@ want de codenext is essentieel voor het functioneren van LEAVE, OOK bij inlinen
		ldr r14, [uss]				@ haal addres van 'unloop' van uss in r14
codenext	@ checked				@ en gebruik dat nieuwe address om naar unloop te springen

codehead "(4+loop)", 8, LEAVE, 2, 1	@ compile only ( -- ) 2c loop inline
DO4PLLOOP:
		add loopi, loopi, #4		@ verhoog loop index met 4
		cmp loopi, looplim			@ vergelijk lim en i
		@ blt  naar do				@ adres ingevuld door _LOOP bij compileren
codenext	@ checked

codehead "(+loop)", 7, DO4PLLOOP, 8, 1	@ compile only ( n -- ) 5c loop inline plus 1c voor lit16
DOPLLOOP:							@ van 9c naar 5c loop pi2 naar 5c pi3 (maar dan wel correct!)
		add loopi, loopi, top		@ tel top bij index op
		cmp top, #0
		ldr top, [dts], #4			@ herstel top
		mvnlt v, loopi				@ v nu neg-index als top<0
		mvnlt w, looplim			@ w nu neg-limiet als top<0
		movge v, loopi				@ v nu index als top>=0
		movge w, looplim			@ w nu index als top>=0
		cmp v, w					@ vergelijk i en lim
		@ blt	naar do				@ als signed kleiner dan spring
codenext	@ checked!! Erg clever!!}

@  ************************  ARM regs to stack  ********************{
codehead "dts>", 4, DOPLLOOP, 3, 1
DTS2ST:								@ zet de waarde van dts:index op de stack VOORDAT dts op de stack is gezet
		mov r0, dts
		str top, [dts, #-4]!
		mov top, r0
codenext	@ checked

codehead "pc>" 3, DTS2ST, 0, 1		@ ( -- pc ) NIET INLINEN!! Want r14 wordt gebruikt om pc te bepalen!!
PC2ST:								@ ( zet de waarde van pc op de stack, pc is pc waar PC2ST staat )
		mov v, r14					@ r14 is de terugkeer-link, en dus de pc 4 bytes na PC2ST
		sub v, v, #4				@ nu het address van PC2ST zelf
		str top, [dts, #-4]!
		mov top, v
codenext	@ checked

codehead "sp>" 3, PC2ST, 3, 1		@ ( -- sp )
SP2ST:								@ ( zet de waarde van sp op de stack - voordat sp op de stack is gezet)
		mov v, sp
		str top, [dts, #-4]!
		mov top, v
codenext	@ checked}

wordhead "getregs", 7, SP2ST, 1		@ puts a copy of every register in memory for debugging et al
GETREGS:	prolog
			
		pushuss r1					@ save r1 for later

	@ See which core calls this routine
		mrc p15, 0, r1, c0, c0, 5	@ haal id van core waar deze routine loopt
		and r1, r1, #0b11			@ r0 nu id van core - 00, 01, 10 of 11
		
	@ fill r1 with correct pointer for the identified
		cmp r1, 0x0
		bne 1f
		ldv32 r1, sivc0R0
		b 4f
		
	1:	cmp r1, 0x1
		bne 2f
		ldv32 r1, sivc1R0
		b 4f
		
	2:	cmp r1, 0x2
		bne 3f
		ldv32 r1, sivc2R0
		b 4f
		
	3:	ldv32 r1, sivc3R0
	
	@ do the actual saving of the regs to memory
	4:	str r0, [r1], #4			@ r0 to memory, r0 can now be used for the rest of routine
		mov r0, r1					@ next address to write on now in r0
		
		popuss r1					@ r1 now again on original value
		
		str r1, [r0], #4			@ r1 to memory	#UPDATE use STM!
		str r2, [r0], #4			@ r2 to memory
		str r3, [r0], #4			@ r3 to memory
		str r4, [r0], #4			@ r4 to memory
		str r5, [r0], #4			@ r5 to memory
		str r6, [r0], #4			@ r6 to memory
		str r7, [r0], #4			@ r7 to memory
		str r8, [r0], #4			@ r8 to memory
		str r9, [r0], #4			@ r9 to memory
		str r10, [r0], #4			@ r10 to memory
		str r11, [r0], #4			@ r11 to memory
		str r12, [r0], #4			@ r12 to memory
		
	@	prolog uses STMFD (ie: STMDB ie: decrease before)	
	@	next uses LDMFD (ie: LDMIA ie: increase after)
	@	due to bl to GETREGS, r13 is decreased 5 times due to STMFD r13!,{r0-r3, lr}
	@	consequently r13 is 5*4=20 to low compared to where the call came from
	@	r13 cannot be decreased with 20 here as the return would crash -> thus the use of r2
	@	r2 will be restored anyhow by the next
	
		sub r2, r13, #20
		str r2, [r0], #4			@ r13 via r2 to memory
		
	@	r14 here contains the return-address from the calling routine
	@	we would like to have the r14 as available within the calling routine
	@	that r14 is on the return-stack, 6 positions up from where r13 is pointing
	@ 	It is possible that something alse has been put on top of that value but
	@	there is presently now way of knowing that
	
		str r14, [r0], #4			@ r14 to memory
		str r15, [r0], #4			@ r15 to memory
next		@ #CHECK


@  **************************  TIB handling  ***********************{
wordhead "clearTIB", 8, GETREGS, 1	@ ( -- ) Zet nullen in hele TIB - nuttig?
CLEARTIB:
		prolog

		lit16 0x0
		bl	varTOIN
		bl	STORE					@ zet >in op nul

		ldr r0, =0x0
		ldr r1, =TIB
		ldr r2, =TIBLen

	3:	strb r0, [r1], #1			@ clear TIB
		subs r2, r2, #1
		bne 3b
next		@ checked maar nu beter

wordhead "TIBio", 5, CLEARTIB, 1	@ ( -- a-addr/false )
TIBIO:	prolog						@ zet de volgende regel klaar, false (=klaar) of a-addr

		bl	varNEXTLINE
		bl	FETCH					@ stack nu startaddres volgende regel om in te lezen

		@ maak nu varNEXTLINE voor volgende ronde

		mov r0, top					@ r0 nu startaddress als basis om volgende startadres te berekenen
		ldr r1, [r0]				@ r1 nu lengte string - checked

		cmp r1, #0x0				@ als lengte 0 dan klaar met source
		beq 1f

		add r0, r0, #4				@ r0 wijst nu naar begin string
		add r2, r1, r0				@ r2 wijst nu naar einde string

		add r2, r2, #3
        and r2, r2, #~3				@ align r2 to next 4 byte boundary

        pushdts r2					@ zet nieuwe r2 op stack
        bl	varNEXTLINE				@ in varNEXTLINE
        bl	STORE
        b 2f

	1:	mov top, #0x0				@ source is leeg maak top FALSE
	2:
next		@ checked

wordhead "clearsource", 11, TIBIO, 1	@ source is de CrayOnFORTH-sourcecode aan het einde van deze tekst
CLEARSOURCE:	prolog
		lit32 MYSOURCE
		bl	varNEXTLINE
		bl	STORE
next		@ checked

wordhead "fillTIB", 7, CLEARSOURCE, 1	@ ( addr #string -- )
FILLTIB:							@ dit woord NIET weg -> erg nuttig voor het woord EXECUTE!!!
		prolog						@ copy counted string op addr naar TIB = text input buffer
		bl	CLEARTIB				@ #UPDATE - ensures that string in TIB is always  nul-terminated - evt anders
		popdts r1					@ r1 is address counted string die geinterpreteerd moet worden
									@ TIB heeft GEEN lengte woord vooraf!

		ldr r0, [r1]				@ r0 nu lengte
		cmp r0, #0
		beq 1f						@ als lengte nul dan klaar

		add r1, r1, #4				@ r1 nu start string
		ldr r3, =TIB				@ r3 nu start TIB
	2:	ldrb r2, [r1], #1
		strb r2, [r3], #1
		subs r0, r0, #1
		bne 2b
	1:
next		@ checked}

@  ************************  MEMORY handling  **********************{
wordhead "align", 5, FILLTIB, 1		@ ( -- ) alignes datapointer HERE
ALIGN:	prolog
		bl	varHERE
		bl	FETCH
		add top, top, #3
        and top, top, #~3			@ align to next 4 byte boundary
        bl	varHERE
        bl	STORE
next		@ checked

codehead "aligned", 7, ALIGN, 2, 1	@ ( addr --  a-addr ) alignes address on stack to next aligned address
ALIGNED:
		add top, top, #3
        and top, top, #~3			@ align to next 4 byte boundary
codenext	@ checked

wordhead "allot", 5, ALIGNED, 1		@ ( n (bytes!) -- ) add n, in bytes, to HERE and align HERE to word
ALLOT:	prolog
		bl	varHERE					@ verhoog HERE
		bl	FETCH
		bl	_ADD
		bl	ALIGNED					@ #ERROR hier kan gewoon ALIGN en dan de volgende 2 regels weg
		bl	varHERE
		bl	STORE
next		@ checked

wordhead "allocate", 8, ALLOT, 1	@ ( n -- addr ) make n bytes available, addr is start of gereserveerd memory-block
ALLOCATE:
		prolog
		bl	varHEREDATA
		bl	FETCH					@ ( n addr-heredata )
		bl	SWAP					@ ( addr-heredata n )
		bl	_SUB					@ ( addr-heredata-n )
		lit16 3						@ was 3, nu 7 ( addr-heredata-n-7 )
		bl	_SUB					@ ( addr-3 ) door de sub 3 wordt er zeker genoeg geheugen vrijgemaakt! maakt extra vrij naar beneden...
		bl	ALIGNED					@ ( a-addr )
		bl	DUP
		bl	varHEREDATA
		bl	STORE
next		@ checked}

@  ***************************  MOVES Etc  *************************{
wordhead "move", 4, ALLOCATE, 1		@ ( van, naar, aantal words -- ) - ANSI: altijd aligned - geen codehead door aantal regs
MOVE:	STMFD r13!, {r0-r4, v, w, lr}	@ van 330c zonder ldmia/stmia, via 208c met ldmia naar 134c met NEON-misbruik en cache-hints{
									@ van 1350c naar 1162c voor 1000 words down! 1.16-1.57c/word! ->

		ldr w, [dts], #4			@ w is naar-address - top blijft gewoon in register staan!!
		ldr v, [dts], #4			@ v is van-address

		cmp v, w
		bgt	1f						@ als v>w dan naar gewone move
		blt 6f						@ als v<w dan naar move-up

			@ als hier dan v=w en dus klaar
			ldr top,[dts], #4		@ =DROP
			LDMFD r13!, {r0-r4, v, w, pc}	@ en klaar = NEXT zonder text

	@ als hier, dan v>w en dus een gewone MOVE

	8:
		@ldv32 r0, 3600				@ 3600 bij pi2, bij pi3 is vldm altijd efficienter dan ldmia
		@cmp top, r0				@ als er meer dan ~3600 words gemoved moeten worden is alleen lmdia ed beter!
		@bgt 7f						@ ik ga uit van cache effect

	1:	cmp top, #16				@ kleiner dan 16? (32 words is weer veeeel langzamer!)
		blt 7f						@ dan naar 4 word step-deel

			sub top, top, #16		@ eerst move per 16 words
			pld [v, #0x20]			@ preload hint voor pi3 - maakt het soms sneller...
			pldw [w, #0x20]			@ en dit was dus een errug goed idee!!
			vldm v!, {d0-d7}		@ misbruik van neon functie voor move! Werkt ook op armv7
			vstm w!, {d0-d7}		@ 16 words per loop geeft maximum snelheid van geheugen
			b 1b

	7:	cmp top, #4					@ kleiner dan 4?
		blt 2f						@ dan naar 1 word step-deel

			sub top, top, #4		@ eerst move per 4 words
			ldmia v!, {r0-r3}  		@ -> aligment moet voor ldmia en stmia
        	stmia w!, {r0-r3}
			b 7b

	2:	subs top, top, #1
		blt 3f

			ldr r2, [v], #4			@ en dan per woord
			str r2, [w], #4
        	b 2b

	3:	ldr top,[dts], #4			@ top is nu weer up to date
		LDMFD r13!,{r0-r4, v, w, pc}	@ en klaar = NEXT

	@ als hier dan is v kleiner dan w en dus MOVEUP - de routine op 5: is per woord en kan dus altijd,
	@ ook als v en w maar 1 woord verschillen!

	6:	lsl r3, top, #2				@ r3 is aantal bytes te moven
		add r0, v, r3				@ tel r3 bij v op (r0 want later moet v+r3-4)
		cmp r0, w					@ als (v+aantal bytes te moven)< w dan geen overlap
		blt 8b						@ en kan de gewone move-down gebruikt worden

	@ als hier dan is move-up (helaas) mandatory
		sub r3, r3, #4				@ 4 bytes onder einde
		add v, v, r3				@ bereken laatste adres 'van' om naar beneden af te werken
		add w, w, r3				@ bereken laatste adres 'naar'
		sub r2, w, v				@ r2=w-v (w is bij move-up altijd groter als v)
		cmp r2, #16					@ als v en w minder dan 16 bytes verschillen...
		blt 5f						@ dan move per word mandatory

	10:	cmp top, #4					@ kleiner als 4
		blt 5f						@ dan naar move per woord

			sub top, top, #4		@ eerst move per 4 words
			ldmda v!, {r0-r3}  		@ -> aligment moet voor ldmia en stmia
        	stmda w!, {r0-r3}
        	b 10b

	@ vanaf hier move-up per word - kan altijd
	5:		ldr r2, [v], #-4		@ laad woord van v en verlaag v daarna
			str r2, [w], #-4		@ etc
			sub top, top, #1
			cmp top, #1
       		bge 5b

	9:	ldr top,[dts], #4			@ top is nu weer up to date
		LDMFD r13!, {r0-r4, v, w, pc}	@ big next
.ltorg
.text		@ CHECK!!}

wordhead "scrmove", 7, MOVE, 1		@ ( van, naar, aantal words -- ) - NEON en ldmia/stmia eruit als test
SCRMOVE: prologmax 					@ scherm-move heeft nooit een overlap{

		ldr w, [dts], #4			@ w is naar-address
		ldr v, [dts], #4			@ v is van-address

		cmp v, w
		beq 9f						@ klaar als v en w gelijk

	1:	subs top, top, #1
		blt 9f

		ldr r2, [v], #4				@ #DEBUG een access van het geheugen (core1) hier geeft een crash
		str r2, [w], #4				@ deze store laat het systeem crashen als vanuit core1!! #CORE1
        b 1b

	9:	ldr top,[dts], #4			@ top is nu weer up to date	
nextmax		@}

wordhead "cmove", 5, SCRMOVE, 1		@ ( van naar lengte -- ) 103-120c voor 100 moved char - was 331c!!
CMOVE:								@ 0.785c-0.94c both aligned/unaligned per char moved -> ~1Gchar/s!!{
		prolog						@ nog sneller door multi-word moves en unaligned access
		ldr r1,[dts], #4			@ r1 is naar-address
		ldr r0,[dts], #4			@ r0 is van-address

	1:	cmp top, #32				@ kleiner dan 32:
		blt 2f						@ dan naar single word step-deel
		subs top, top, #32
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		b 1b

	2:	cmp top, #4					@ kleiner dan 4:
		blt 3f						@ dan naar single char step-deel
		subs top, top, #4
		ldr r2, [r0], #4
		str r2, [r1], #4
		b 2b

	3:	subs top, top, #1			@ als top nu neatief, dan is/was top 0
		blt 4f						@ #ERROR:? hierdoor lengte beperkt to 2^31, wel ANSI compatible
		ldrb r2,[r1],#1
		strb r2,[r0],#1
        b 3b

	4:	ldr top,[dts], #4			@ top is nu weer up to date
next		@ checked}

wordhead "cmoveup", 7, CMOVE, 1		@ ( van naar lengte -- ) 100c-124c voor 100 moved chars - was 333!
CMOVEUP:							@ 0.78c-0.96c both aligned - both unaligned per char moved{
		prolog
		ldr r1,[dts], #4			@ r1 is naar-address
		ldr r0,[dts], #4			@ r0 is van-address

		cmp top, #4					@ als kleiner dan 4
		blt	2f						@ dan naar byte-move
		sub	top, top, #4
		add r0, r0, top
		add r1, r1, top
		add top, top, #4
	1:	ldr r2,[r0],#-4
		str r2,[r1],#-4
		sub top, top, #4
		cmp top, #4
        bge 1b

    2:	cmp top, #0
		beq	4f						@ klaar
		sub	top, top, #1
		add r0, r0, top
		add r1, r1, top
		add top, top, #1
	3:	ldrb r2,[r0],#-1
		strb r2,[r1],#-1
		subs top, top, #1
        bne 3b

	4:	ldr top,[dts], #4			@ top is nu weer up to date
next		@ checked}

codehead "cfill", 5, CMOVEUP, 7, 1	@ ( c-addr n char — ) vul vanaf c-addr n keer de char
CFILL:								@{
		ldr w, [dts], #4			@ w=aantal n
		ldr v, [dts], #4			@ v=c-addr, top is al char

	1:	subs w, w, #1				@ counter=w met 1 naar beneden
		blt 2f						@ ls counter negatief dan klaar - ook bij n=0 als inpit gelijk klaar
		strb top, [v], #1			@ zet char in top op addres in v en verhoog v met 1
		b 1b

	2:	ldr top,[dts], #4			@ top nu weer klaar
codenext  @ }#CHECK}

@  *************************  RETURN stack  ************************{
codehead ">r", 2, CFILL, 2, 1 		@ data:( n -- ) return: ( -- n ) userstack!!
TO_R:	str top,[uss, #-4]!			@ pi3:7-2c, pi2:5 - 2-3!
		ldr top,[dts], #4
codenext	@ checked

codehead "r>", 2, TO_R, 2, 1 		@ data:( -- n ) return: ( n -- ) userstack!!
FROM_R:	str top,[dts, #-4]!			@ pi3:7-2c, pi2:5 - 2-3!
		ldr top,[uss], #4
codenext	@ checked

codehead "r@", 2, FROM_R, 2, 1 		@ data:( -- n ) return: ( n -- n ) userstack!!
RFETCH:	str top,[dts, #-4]!			@ 5c - 2-3c
		ldr top,[uss]
codenext	@ #CHECK}

@  ********************  BEGIN UNTIL WHILE AGAIN  ******************{
wordhead "begin", 5, RFETCH, -1		@ ( R: -- dest )
BEGIN:	prolog
		bl	varHERE					@ address waar branch van until/again heen gaat
		bl	FETCH
		lit16 4						@ snap dit niet helemaal!!
		bl	_SUB					@ nu het correcte address op stack
		bl	TO_R					@ addres voor branch nu op r-uss
next		@ checked

wordhead "until", 5, BEGIN, -1		@ ( R: dest -- ) maak branch terug naar dest op stack - terug bij false
UNTIL:	prolog
		lit32 DOUNTIL				@ nu xt van DOUNTIL op stack
		bl	COMPILEBL				@ compile DOUNTIL as inline at HERE
		bl	FROM_R					@ address dest nu op stack
		bl	MAKEBACKBEQ				@ maak beq jump terug
		bl	COMMA					@ en zet jump op HERE
next		@ checked

codehead "(until)", 7, UNTIL, 2, 1	@ ( flag -- )
DOUNTIL:
		cmp top, #0x0				@ vergelijk flag met 0 - bij flag=false jump terug
		ldr top,[dts], #4			@ drop flag
									@ hier komt de jump:beq naar begin
codenext	@ checked

wordhead "again", 5, DOUNTIL, -1	@ maak unconditional branch terug naar dest van cs
AGAIN:	prolog
		bl	FROM_R					@ address dest nu op datastack
		bl	MAKEBACKB				@ maak branch terug
		bl	COMMA					@ en zet jump op HERE
next		@ checked!

codehead "(again)", 7, AGAIN, 0, 1
DOAGAIN:
			@ is alleen een branch terug
codenext

wordhead "while", 5, DOAGAIN, -1	@ GOED:zet address=orig onder dest op stack - en maak plek mbv nop
WHILE:	prolog
		lit32 DOWHILE				@ nu xt van DOUNTIL op stack
		bl	COMPILEBL				@ compile DOELSE (?) as inline at HERE
		bl	FROM_R					@ dest nu op datastack
		bl	varHERE					@ address waar branch achteraf geplaatst wordt
		bl	FETCH
		bl	TO_R					@ orig voor branch nu op r-uss
		bl	TO_R					@ nu dest ook weer op r-uss
		lit16 0x0
		bl	COMMA					@ lege plek van 1 word voor branch
next		@ checked

codehead "(while)", 7, WHILE, 2, 1
DOWHILE:
		cmp top, #0x0				@ vergelijk flag met 0 -> "while" is een if die een orig onder dest zet
		ldr top,[dts], #4			@ drop flag
									@hier straks de jump verder naar net na repeat bne voorwaarts
codenext	@ checked

wordhead "repeat", 6, DOWHILE, -1	@ resolve B to dest(Begin), dan resolve BEQ op orig(While) tot na Repeat
REPEAT:	prolog
			@ eerst dest resolven
		bl	FROM_R					@ address dest nu op datastack
		bl	MAKEBACKB				@ make opcode voor branch terug
		bl	COMMA					@ en zet jump op HERE

			@ nu orig resolven
		bl	FROM_R					@ op stack nu address waar nop staat
		bl	DUP						@ nu twee keer
		bl	MAKETOBEQ				@ nu de opcode voor BEQ op stack met daaronder address
			bl	DOTDBMKHEAD1
			bl	DOTDBOUTHEX

		bl	SWAP					@ nu beq addr-nop op stack
			bl	DOTDBREPEAT1

		bl	STORE					@ store op lege plek bij While
next		@ checked

codehead "dorepeat", 8, REPEAT, 0, 1	@ is alleen een richtpunt -> geen code nodig voor DOREPEAT
DOREPEAT:
		@ leegte voor de storm
codenext  @ #CHECK}

@  **************************  IF endif else  **********************{
wordhead "if", 2, DOREPEAT, -1		@ ( R: -- dest_if )
IF:		prolog						@{
		lit32 DOIF					@ nu xt van DOIF op stack
		bl	COMPILEBL				@ compile DOIF as inline at HERE
		bl varHERE					@ address waar branch achteraf geplaatst wordt
		bl	FETCH
		bl	TO_R					@ addres voor branch nu op r-uss
		lit16 0x0
		bl	COMMA					@ lege plek van 1 word voor branch
next		@ checked}

codehead "(if)", 4, IF, 2, 1		@ ( f -- ) jump naar endif als flag is false
DOIF:	cmp top, #0x0				@ is flag null? (beq){
		ldr top, [dts], #4			@ drop flag
									@ <= hier straks de jump, nl: beq naar EndIf
codenext	@}

wordhead "endif", 5, DOIF, -1		@ ( R: dest -- )
ENDIF:	prolog						@ endif lost één voorwaartse sprong op, OOK van else en while!
		bl	FROM_R					@ op stack nu dest

		popdts r0					@ r0 is dest
		tst r0, #0x1				@ kijk naar bit(0) van dest en zet flags
		bne 1f						@ dan B ipb BEQ (else B, IF en While BEQ) scheelt 1 cycle bij else - YEAH!!

		pushdts r0
		bl	DUP
		bl	MAKETOBEQ				@ nu de opcode voor BEQ op stack met daaronder address
		b 2f

	1:	ldv32 r2, 0xfffffffe
		and r0, r0, r2				@ r0 nu weer zonder B-flag
		pushdts r0
		bl	DUP
		bl	MAKETOB

	2:	bl	DOTDBMKHEAD1
		bl	SWAP					@ nu beq addr-nop op stack

		bl	DOTDBREPEAT1
		bl	STORE					@ store op plek NOP
next		@ checked - functions!

codehead "(endif)", 7, ENDIF, 0, 1	@ endif is niets meer als een richtpunt, er is geen execution-code
DOENDIF:
		@een punt is oneindig klein, een richtpunt dus ook...
codenext	@ checked

wordhead "else", 4, DOENDIF, -1		@ ( R: orig1 -- orig2 )
ELSE:	prolog
		bl	FROM_R					@ haal orig1 naar datastack
		bl	DUP						@ nu twee keer address op stack
		bl	MAKETOBEQ				@ nu de opcode voor BEQ op stack met daaronder address

		lit16 1						@ uitzoeken waarom deze 1 erbij moet!!
		bl	_ADD

			bl	DOTDBMKHEAD1
			bl	DOTDBOUTHEX

		bl	SWAP					@ nu beq addr-nop op stack

			bl	DOTDBREPEAT1

		bl	STORE					@ store op plek NOP van DOIF

		bl varHERE					@ address waar branch achteraf geplaatst wordt
		bl	FETCH
		lit16 1						@ signal to endif that a B is needed instead of a BEQ
		bl	_ADD
		bl	TO_R					@ addres voor branch nu op r-uss

		lit16 0x0
		bl	COMMA					@ lege plek van 1 word voor branch
next		@ checked}

@  ****************************  DOTQUOTE  *************************{
wordhead ".\"", 2, ELSE, -1			@ immediate - state-smart - print een string
DOTQUOTE:
		prolog
		lit16 34					@ 34 is double quote
		bl	F83WORD					@ nu address van text tot " op stack of een 0
		cmp top, #0x0
		bne 1f

		bl	DROP					@ drop de 0 die aangeeft dat er geen text is
		b 2f						@ en klaar ( empty stack )

	1:	bl	GETSTATE				@ ( addr-string state-flag )
		cmp top, #0x0
		bne 3f						@ naar compiling

		bl	DROP					@ drop de 0 van interpreting ( addr-string )
		bl	PR_CST					@ print woord op address wat nog steeds op stack staat
		b 2f						@ en klaar ( empty stack )

	3:	bl	DROP					@ drop de 1 van compiling ( addr-string )

		popdts r0					@ r0 nu addres van string ( lege stack )
		ldr r1, [r0]				@ r1 nu lengte string
		add r0, r0, #4				@ r0 nu start characters van string

	@ maak nu de code om te printen, plek voor de branch en zet de string in de definitie

		lit32 DODOTQUOTE			@ nu xt van DODOTQUOTE op stack
		bl	COMPILEBL				@ compile op HERE
		bl	varHERE					@ address waar branch achteraf geplaatst wordt
		bl	FETCH
		bl	TO_R					@ zet address op uss-R

		lit16 0x0
		bl	COMMA					@ lege plek van 1 word voor branch over string heen

		pushdts r1					@ lengte string op stack
		bl	COMMA					@ lengte nu in definitie

	4:	pushdts r0					@ pointer naar string op stack
		bl	CFETCH					@ get char
		bl	CCOMMA					@ zet char in definitie

		add r0, r0, #1				@ verhoog pointer met 1
		subs r1, r1, #1				@ verlaag lengte met 1
		bne 4b						@ tot lengte gelijk is aan nul

		bl	ALIGN					@ align HERE
		bl	FROM_R					@ op stack nu dest

		bl	DUP						@ ( dest dest )
		bl	MAKETOB					@ ( dest branch-opcode )

			bl	DOTDBMKHEAD1
			bl	DOTDBOUTHEX

		bl	SWAP					@ ( branch-opcode dest )

			bl	DOTDBREPEAT1

		bl	STORE					@ store branch opcode op dest ( empty stack )
	2:
next		@ checked

wordhead "dodotquote", 10, DOTQUOTE, 1 @ ( -- )
DODOTQUOTE:
		prolog
		mov r0, lr					@ terugkom address als basis voor wat te printen
		add r0, r0, #4				@ begin te printen vanaf 1 word na terugkom addres
		pushdts r0
		bl	PR_CST					@ print counted string
next		@ checked}

@  ************************ jump calculations **********************{
wordhead "calcjump2does", 13, DODOTQUOTE, 1	@ ( r14-doesto orig -- 24 upper offset-bits )
CALCJUMP2DOES:
		prolog						@ bereken de offset voor de verschillende branches
		popdts r0					@ r0 is nu orig, top is nu r14 (?) Waarom r14?

		@ ************************
		mov r1, top					@ r1 is nu r14-doesto van stack, top is scratch
		@ ************************

		sub r2, r1, r0				@ r2 is r14 minus orig -> spring naar r14
		sub r2, r2, #8				@ en nog 8 extra eraf door pipeline effect -> r2 nu de benodigde offset voor jump
		ldv32 r3, 0x03ffffff
		and r2, r2, r3				@ maak ofset geschikt voor opcode - 26 bits jump
		lsr top, r2, #2				@ shift logically 2 bits to right, nu goed voor opcode - 24 upper bits van jump
next		@ checked

wordhead "calcjump2me", 11, CALCJUMP2DOES, 1 @ ( orig -- 24 offset bits )
CALCJUMP2ME:
		prolog						@ bereken de offset voor de verschillende branches
		mov r0, top					@ r0 is nu orig
		bl	varHERE
		bl	FETCH					@ ( xt varHERE:value -- )
		popdts r1					@ r1 is HERE:value

		sub r2, r1, r0				@ r2 is HERE minus orig -> spring naar mij
		sub r2, r2, #8				@ en nog 8 extra eraf door pipeline effect -> r2 nu de benodigde offset voor BL
		ldv32 r3, 0x03ffffff
		and r2, r2, r3
		lsr top, r2, #2				@ shift logically 2 bits to right
next

wordhead "calcjumpback", 12, CALCJUMP2ME, 1	@ ( dest -- 24 offset bits )
CALCJUMPBACK:
		prolog						@ bereken de offset voor de verschillende branches
		mov r0, top					@ r0 is nu dest
		
		getvalvar r1, varHERE
		@bl varHERE
		@bl	FETCH					@ ( xt varHERE:value -- )
		@popdts r1					@ r1 is HERE:value

		sub r2, r0, r1				@ r2 is dest minus HERE -> spring weg van mij
		sub r2, r2, #4				@ en nog 8 extra eraf door pipeline effect + 4 omdat HERE nog niet is aangepast -> r2 nu de benodigde offset voor BL
		ldv32 r3, 0x03ffffff		@ lijkt overbodig maar zeker
		and r2, r2, r3
		lsr top, r2, #2				@ shift logically 2 bits to right
next

wordhead "make>beq", 8, CALCJUMPBACK, 1	@ ( source -- opcode van BEQ van source naar varHERE )
MAKETOBEQ:
		prolog						@ opcode = 0x0A met dan 24 bits voor sprong
		bl	CALCJUMP2ME
		mov r0, top
		ldv32 top, 0x0A000000		@ top vullen met opcode, met nullen voor de sprong 0A=beq, 1A=bne
		orr top, top, r0			@ voeg r0 toe aan top, top nu opcode van BEQ
next		@ checked

wordhead "make>does", 9, MAKETOBEQ, 1	@ ( r14-doesto source -- opcode van B van source naar varHERE )
MAKETODOES:
		prolog						@ opcode = 0xEA met dan 24 bits voor sprong
		bl	CALCJUMP2DOES
		mov r0, top
		ldv32 top, 0xEA000000		@ top vullen met opcode, met nullen voor de sprong 0A=beq, 1A=bne, EA=b, EB=bl
		orr top, top, r0			@ voeg r0 toe aan top, top nu opcode van B
next		@ checked

wordhead "make>b", 6, MAKETODOES, 1	@ ( source -- opcode van B van source naar varHERE )
MAKETOB:
		prolog						@ opcode = 0xEA met dan 24 bits voor sprong
		bl	CALCJUMP2ME
		mov r0, top
		ldv32 top, 0xEA000000		@ top vullen met opcode, met nullen voor de sprong 0A=beq, 1A=bne, EA=b
		orr top, top, r0			@ voeg r0 toe aan top, top nu opcode van B
next		@ checked

wordhead "make<beq", 8, MAKETOB, 1	@ ( dest -- opcode van BNE van varHERE naar dest )
MAKEBACKBEQ:
		prolog						@ opcode = 0x0A met dan 24 bits voor sprong
		bl	CALCJUMPBACK
		mov r0, top
		ldv32 top, 0x0A000000		@ top vullen met opcode, met nullen voor de sprong 1A=bne 0A=beq
		orr top, top, r0			@ voeg r0 toe aan top, top nu opcode van BEQ
next		@ checked

wordhead "make<b", 6, MAKEBACKBEQ, 1	@ ( dest -- opcode met B van varHERE naar dest )
MAKEBACKB:
		prolog						@ opcode = 0xEA met dan 24 bits voor sprong
		bl	CALCJUMPBACK			@ nu the offset op stack
		mov r0, top					@ r0 is nu dest-address (van nop die door bne vervangen wordt)
		ldv32 top, 0xEA000000		@ top vullen met opcode, met nullen voor de sprong EA=branch EB=branch link
		orr top, top, r0			@ voeg r2 toe aan top, top nu opcode van B
next		@ checked}

@  ***************************  comparisons  ***********************{
codehead "within", 6, MAKEBACKB, 0, 1 @ ( g m n -- true als g tussen m en n, anders false ) GEEN inlinen -> doet niets!!
WITHIN:								@ pi3: 13-15 - nvt  pi3 beter:13-14 - 9-10   NU NOG beter: 10 - 10
		ldr w, [dts], #4			@ #ERROR r4 gebruikt in codehead!!!!!!
		pushuss r4					@ save r4 tijdelijk #CHECK
		
		cmp w, top					@ vergelijk m en n
		movgt w, top				@ als w groter dan top=n naar w=m
		ldrgt top, [dts, #-4]		@ 2 opcode swap!

		ldr v, [dts], #4			@ v nu getal
		sub r4, w, top				@ r4 is negatieve verschil tussen m en n

		subs top, w, v				@ top=temp is m-getal -> negatief is goed

		cmple r4, top				@ r4=diff(-) moet kleiner zijn dan temp(-)
		popuss r4					@ en haal oude r4 weer terug

		movge top, #0				@ top is false
        mvnlt top, #0				@ anders true
      								@ nog maar 10 opcodes en volledig voorspelbaar qua tijd
codenext	@ checked

codehead ">", 1, WITHIN, 4, 1
GREATERTHAN:
		ldr w, [dts], #4
        cmp w, top
        mvngt top, #0
        movle top, #0
codenext	@ #checked

codehead "<", 1, GREATERTHAN, 4, 1
LESSTHAN:
		ldr w, [dts], #4
        cmp w, top
        mvnlt top, #0
        movge top, #0
codenext	@ checked

codehead "u<", 2, LESSTHAN, 4, 1
ULESSTHAN:
		ldr w, [dts], #4
        cmp w, top
        mvnlo top, #0
        movhs top, #0
codenext	@ checked

codehead "<>", 2, ULESSTHAN, 4, 1		@ ( n m -- flag )
NOTEQUAL:
		ldr w, [dts], #4
        cmp w, top
        mvnne top, #0
        moveq top, #0
codenext

codehead "=", 1, NOTEQUAL, 4, 1
EQUALS:
		ldr w, [dts], #4
        cmp w, top
        mvneq top, #0
        movne top, #0
codenext

codehead "<=", 2, EQUALS, 4, 1
SMEQUAL:
		ldr w, [dts], #4
        cmp w, top
        mvnle top, #0
        movgt top, #0
codenext

codehead ">=", 2, SMEQUAL, 4, 1
GREQUAL:
		ldr w, [dts], #4
        cmp w, top
        mvnge top, #0
        movlt top, #0
codenext

codehead "0=", 2, GREQUAL, 3, 1		@ ( m -- true if m=0 )
ZEROEQUAL:							@ pi3:7-2c
		cmp top, #0x0
		mvneq top, #0
		movne top, #0
codenext

codehead "d0=", 3, ZEROEQUAL, 4, 1	@ ( d -- true if d=0 )
DZEROEQUAL:
		ldr w, [dts], #4
        orrs w, top					@ geeft een equal flag als beide 0 zijn
        mvneq top, #0
        movne top, #0
codenext	@ CHECK!!

codehead "0<", 2, DZEROEQUAL, 4, 1	@ ( m -- true if m<0 )
ZEROSMALLER:
		cmp top, #0x0
		mvnlt top, #0
		movge top, #0
codenext

codehead "d0<", 3, ZEROSMALLER, 3, 1 @ ( d -- true if d<0 )
DZEROSMALLER:
		add dts, dts, #4			@ verlaag dts met 4 door op te tellen!
		cmp top, #0x0				@ verbazingwekkenderwijze werkt dit! Elke negatief getal heeft een negatieve top!!
		mvnlt top, #0
		movge top, #0
codenext	@ CHECK!!

codehead "0>", 2, DZEROSMALLER, 3, 1 @ ( m -- true if m>0 )
ZEROGREATER:
		cmp top, #0x0
		mvngt top, #0
		movle top, #0
codenext

codehead "even?", 5, ZEROGREATER, 3, 1	@ ( m -- true if even )
_EVEN:	ands top, #1				@ if odd than nu top=1
		mvneq top, #0
		movne top, #0
codenext

codehead "odd?", 4, _EVEN, 3, 1		@ ( m -- true if odd )
_ODD:	ands top, #1				@ if odd than nu top=1
		mvnne top, #0
		moveq top, #0
codenext		@ }

@  *********************** stack manipulations  ********************{
codehead "dup", 3, _ODD, 1, 1		@ ( n -- n n )
DUP:								@ 5-7c - 1-2c inline (zo een gave waarde voor bl versie!!!)
		str top, [dts, #-4]!		@ pop top op datastack
codenext	@ checked

codehead "?dup", 4, DUP, 2, 1		@ ( n -- nil/n n )
QDUP:								@ 6-8c - 2-3c inline
		cmp top, #0x0				@ 0 op stack?
		strne top, [dts, #-4]!		@ als geen nul, dan pop top op datastack
codenext	@ checked

codehead "?2dup", 5, QDUP, 4, 1		@ ( d -- nil/d d )
QTWODUP:							@ #CORE dit isgeen core maar wel leuk -> blijft
		ldr w, [dts]
        orrs w, top					@ geeft een equal flag als beide 0 zijn!!
        strne top, [dts, #-4]!		@ copy top op stack
		strne w, [dts, #-4]!		@ en copy w op stack, top blijft gewoon hetzelfde
codenext	@ CHECK!!

codehead "2dup", 4, QTWODUP, 3, 1		@ ( n m -- n m n m )
TWODUP:								@ 6-8c - 3-4c inline
		ldr w, [dts]				@ haal waarde van pos 2
		str top, [dts, #-4]!		@ copy top op stack
		str w, [dts, #-4]!			@ en copy w op stack, top blijft gewoon hetzelfde
codenext	@ CHECK!!

codehead "3dup", 4, TWODUP, 3, 1	@ ( n m o -- n m o n m o )
THREEDUP:							@ 8c - 6c inline
		ldmfd dts,{v, w}			@ haal waarde van pos 2 en 3 (geen ! na dts!!)
		str top, [dts, #-4]!		@ copy top op stack
		stmfd dts!,{v, w}			@ en copy waardes van pos 2 en 3 op stack
codenext	@ checked

codehead "4dup", 4, THREEDUP, 3, 1	@ ( n m o p -- n m o p n m o p )
FOURDUP:							@ ?c - ?c inline
		pushuss r4					@ save r4 temporary
		ldmfd dts,{r4, v, w}		@ haal waarde van pos 2, 3 en 4 (geen ! na dts!!)
		str top, [dts, #-4]!		@ copy top op stack
		stmfd dts!,{r4, v, w}		@ en copy waardes van pos 2, 3 en 4 op stack, top blijft hetzelfde
		popuss r4					@ restore r4
codenext	@ CHECK!!

codehead "drop", 4, FOURDUP, 1, 1	@ ( n -- )
DROP:	ldr top,[dts], #4			@ 5-8c - 1-2c
codenext	@ checked

codehead "2drop", 5, DROP, 2, 1		@ ( n m -- )
TWODROP:							@ 4-8c - 2c
		ldr top,[dts, #4]
		add dts, dts, #8
codenext	@ checked

codehead "3drop", 5, TWODROP, 2, 1	@ ( n m o -- )
THREEDROP:							@ 4-8c - 2c
		ldr top,[dts, #8]
		add dts, dts, #12
codenext	@ checked

codehead "4drop", 5, THREEDROP, 2, 1 @ ( n m o -- )
FOURDROP:							@ 4-8c - 2c
		ldr top,[dts, #12]
		add dts, dts, #16
codenext	@ checked

codehead "swap", 4, FOURDROP, 3, 1	@ ( n m -- m n )
SWAP:								@ 6c bl - 2!-3c
		mov w, top
		ldr top, [dts]				@ laad top met een onder de top
		str w, [dts]				@ en zet w daar terug
codenext	@ checked

codehead "2swap", 5, SWAP, 5, 1		@ ( n m o p -- o p n m )
TWOSWAP:							@ pi3:8-5c! very nice! (pi2:11-8c!) #ERROR r4 veranderd!!! push/popuss as workaround!
		pushuss r4					@ om r4 te bewaren
		ldmia dts, {r4, v, w}
		str top, [dts, #4]
		str w, [dts]
		mov top, v
		str r4, [dts, #8]
		popuss r4					@ om r4 weer te herstellen
		@@ldr v, [dts]				@ o
		@@ldr w, [dts, #4]			@ m
		@ldmia dt, {v, w}
		@str top, [dts, #4]			@ p to old m
		@ldr top, [dts, #8]			@ n
		@str v, [dts,#8]			@ o to old n
		@str top, [dts]				@ n to old o
		@mov top, w					@ m to old p
codenext	@ checked

codehead "over", 4, TWOSWAP, 2, 1	@ ( a b -- a b a )
OVER:								@ pi3:6 - 2c (pi2:9 - 4c)
		str top, [dts, #-4]!		@ zet top op stack en move dts een positie omhoog
		ldr top, [dts, #4]			@ en laad top met twee onder de top
codenext	@ checked

codehead "2over", 5, OVER, 4, 1		@ ( a b c d -- a b c d a b )
TWOOVER:							@ pi3:8-5c - voor 1c latentie geen oplossing gevonden
		str top, [dts, #-4]!		@ zet top op stack en move dts een positie omhoog
		ldr w, [dts, #12]
		str w, [dts, #-4]!
		ldr top, [dts, #12]
codenext	@ checked

codehead "overswap", 8, TWOOVER, 2, 1 @ ( n m -- n n m )
OVERSWAP:							@ pi3:5-2c! pi2:5-5c Dit spaart 4-5c ivm swap en over als aparte inlines
		ldr w, [dts]
		str w, [dts, #-4]!
codenext	@ checked - OPTIMIZED!

codehead "-rot", 4, OVERSWAP, 3, 1	@ ( a b c -- c a b ) bring top to third
MINROT:								@ pi3:7c - 4c (pi1:12c - 7c, pi2:8 - 5c) - moet nog ala ROT
		mov w, top					@ w=old top=c
		ldmia dts,{top, v}
		stmia dts,{v, w}
codenext	@ checked

codehead "rot", 3, MINROT, 5, 1		@ ( a b c -- b c a ) bring third to top
ROT:	mov w, top					@ w=old top=c pi3:8c - 3c (pi1:13-7c Pi2:7-4c)
		ldr v, [dts]				@ v=b
		ldr	top, [dts, #4]			@ top nu a
		str v, [dts, #4]			@ pos 3 nu b
		str w, [dts]				@ pos 2 nu c
codenext	@ checked!!! Yeah!!

codehead "pick", 4, ROT, 1, 1		@ kopieert de n-de waarde van onder de top naar de top
PICK:								@ is 2 cycli sneller door exTop! Nog maar 1 opcode 6-7c - 1-2c
		ldr top, [dts, top, lsl #2]
codenext	@ checked

codehead "nip", 3, PICK, 1, 1		@ ( n m -- m ) 5c - 1c bij 1 gHz
NIP:	add dts, dts, #4			@ stackpointer een pos naar beneden - top blijft as is
codenext	@ CHECK

codehead "tuck", 4, NIP, 3, 1		@ ( n m -- m n m ) pi3:5-3c (pi2:5-4c)
TUCK:	ldr w, [dts]				@ pos 2 naar w
		str top, [dts]				@ zet top op pos 2
		str w, [dts, #-4]!			@ en zet w weer op de verhoogde stack (dit is de snelste variant!)
codenext	@ checked}

@  **********************  classic Forth words  ********************{
codehead "true", 4, TUCK, 2, 1		@ ( -- flag:t ) underscore blijft
_TRUE:	str top, [dts, #-4]!		@ pi3 8-2c (pi2:4 - 2c!)
		mvn	top, #0					@ dit zijn dus 32 enen naast elkaar
codenext	@ checked

codehead "false", 5, _TRUE, 2, 1	@ ( -- flag:f ) underscore blijft
_FALSE:	str top, [dts, #-4]!		@ 4c - 2c
		mov top, #0
codenext	@ checked

codehead "cell", 4, _FALSE, 2, 1	@ ( -- 4 )
CELL:	str top, [dts, #-4]!		@ 4c - 2c
		mov top, #4
codenext	@ checked

codehead "cells", 5, CELL, 1, 1		@ ( n - n*4 ) 1 cell is 4 address units -> returns number of adres-units to hold n cells
CELLS:	mov top, top, lsl #2
codenext	@ checked

codehead "cell+", 5, CELLS, 1, 1	@ ( n - n+4 ) 1 cell is 4 address units -> add 4
CELLPLUS:							@ #CORE: kan weg maar: wel goed voor performance van data-block manipulatie!!
		add top, top, #4			@ het alternatief (cell +) is 4c bij inlining door stack-movements
codenext	@ checked

codehead "negate", 6, CELLPLUS, 1, 1	@ ( n -- -n ) 5c - 1c
NEGATE:
        rsb top, top, #0			@ arithmic negate
codenext	@ checked

codehead "dnegate", 7, NEGATE, 4, 1	@ ( d:lo do:hi -- -d:lo -d:hi ) 9?c - 4?c
DNEGATE:
		ldr v, [dts]				@ d:lo
		rsbs v, v, #0				@ v=nul-v en set carry
		rsc top, top, #0			@ top=nul-top-not(carry)
		str v, [dts]				@ d:lo
codenext	@ checked!

codehead "!", 1, DNEGATE, 3, 1		@ ( n addr -- ) top is addres
STORE:	ldr w, [dts], #4			@ n naar w
		str w, [top]				@ zet w op address in top
		ldr top, [dts], #4			@ update top met de waarde van twee onder de top en verlaag stack
codenext	@ checked

codehead "2!", 2, STORE, 3, 1		@ ( n m addr -- )
TWOSTORE:							@ pi3:6-4c (pi2:9-7c (pakweg 26c bij gewone woorden tuck, store, oneplus, store))
		ldmia dts!, {v, w}			@ strd r2, r3, [top] niet sneller als ldmia!
		stmia top, {v, w}
		ldr top, [dts, #4]!
codenext	@ checked maar meer checken

codehead "+!", 2, TWOSTORE, 5, 1	@ ( n addr -- ) verhoog waarde op addr met n
PLUSSTORE:							@ pi3:7-6c
		ldr w, [dts], #4			@ n naar w
		ldr v, [top]				@ v nu de waarde op addr
		add w, w, v					@ tel v en w op
		str w, [top]				@ zet w op address in top
		ldr top, [dts], #4			@ update top met de waarde van twee onder de top en verlaag stack
codenext	@ checked

codehead "1+!", 3, PLUSSTORE, 4, 1	@ ( a-addr -- ) verhoog waarde op addr met 1
ONEPLUSSTORE:						@ 6-4! ipv 22 als gewone Forth definitie op pi2
		ldr v, [top]				@ v nu de waarde op addr (in top)
		add v, v, #1				@ tel 1 op bij v
		str v, [top]				@ zet v op address in top
		ldr top, [dts], #4			@ update top en verlaag stack
codenext	@ checked

codehead "4+!", 3, ONEPLUSSTORE, 4, 1 @ ( a-addr -- ) verhoog waarde op addr met 4
FOURPLUSSTORE:
		ldr v, [top]				@ v nu de waarde op addr (in top)
		add v, v, #4				@ tel 4 op bij v
		str v, [top]				@ zet v op address in top
		ldr top, [dts], #4			@ update top en verlaag stack
codenext	@ checked

codehead "-!", 2, FOURPLUSSTORE, 5, 1 @ ( n a-addr -- ) verlaag waarde op addr met n
SUBSTORE:
		ldr w, [dts], #4			@ n naar w
		ldr v, [top]				@ v nu de waarde van addr
		sub w, v, w					@ trek w=n van v=value on addr af
		str w, [top]				@ zet w op address in top
		ldr top, [dts], #4			@ update top met de waarde van twee onder de top en verlaag stack
codenext	@ checked

codehead "1-!", 3, SUBSTORE, 4, 1	@ ( a-addr -- ) verlaag waarde op addr met 1
ONESUBSTORE:
		ldr v, [top]				@ v nu de waarde op addr (in top)
		sub v, v, #1				@ trek 1 af van v
		str v, [top]				@ zet v op address in top
		ldr top, [dts], #4			@ update top en verlaag stack
codenext	@ checked

codehead "4-!", 3, ONESUBSTORE, 4, 1 @ ( a-addr -- ) verlaag waarde op addr met 4
FOURSUBSTORE:
		ldr v, [top]				@ v nu de waarde op addr (in top)
		sub v, v, #4				@ trek 4 af van v
		str v, [top]				@ zet v op address in top
		ldr top, [dts], #4			@ update top en verlaag stack
codenext	@ checked

wordhead "?", 1, FOURSUBSTORE, 1	@ ( a-addr -- ) print waarde uit addres
QUESTION:
		prolog
		ldr top, [top]				@ top nu de waarde op addr (in top)
		bl	OUTNUM					@ nu stack weer goed
next		@ CHECK

codehead "@4+", 3, QUESTION, 3, 1	@ ( a-addr -- a-addr+4, n ) get value from address, keep address and add 4 to it
FETCHFOURPLUS:						@ equal to: "dup 4 + swap fetch" takes ~15c this word takes 4-5c on pi3
		add w, top, #4				@ w now addres in top plus 4
		ldr top, [top]				@ top now the value contained in addr
		str w, [dts, #-4]!			@ put addr+4 back onstack below top
codenext	@ CHECK

codehead "@", 1, FETCHFOURPLUS, 1, 1 @ ( addr -- n ) top is addres 6c - 2c
FETCH:	ldr top, [top]
codenext	@ checked

codehead "2@", 2, FETCH, 3, 1		@ ( addr - n m ) fetch m from addr, n from addr+cell
TWOFETCH:							@ pi3:7-4c pi3 pre-ldmia:8-6
		ldmia top, {v, w}
		str w, [dts, #-4]!			@ en zet n op stack die verhoogd wordt
		mov top, v					@ top=m
codenext	@ checked

codehead "c!", 2, TWOFETCH, 3, 1	@ ( char u-addr -- )
CSTORE:
		ldrb w, [dts], #4
		strb w, [top]				@ top is al adres
		ldr top, [dts], #4			@ update top
codenext	@ checked

codehead "c@", 2, CSTORE, 1, 1 		@ ( u-addr -- char )
CFETCH:
		ldrb top, [top]				@ testen of het ook met [top] gaat - JA!!!
codenext	@ checked}

@  *****************************  WORDS  ***************************{
wordhead "words", 5, CFETCH, 1		@ print alle woorden in de dictionary, inclusief xt.
WORDS:	prologmax					@ pure print, geen opslag van lijst oid
		ldv16 r4, 1
		bl	varLAST
		bl	FETCH
		popdts r0
		bl	CR
	2:	pushdts r4
		bl	OUTDEC
		lit16 4
		bl	TAB
		add r4, r4, #1
		pushdts r0
		lit16 40					@ #HEADER
		bl	_ADD
		prchar 'x'
		prchar 't'
		prchar ':'
		bl	OUTHEX
		add r2, r0, #7				@ r2 wijst nu naar het byte met de lengte van de naam van het woord
		ldrb r3, [r2], #1			@ r3 heeft nu de lengte van het woord, r2 wijst nu naar eerste char
	1:	ldrb r1, [r2], #1			@ haal character van adres r2
		pushdts r1
		bl	EMIT					@ en print char
		subs r3, r3, #1				@ verlaag counter met 1
		bne 1b						@ alle letters al gedrukt? Nee, dan verder
		bl	CR						@ nieuw woord en dus een nieuwe regel
		ldr r3, [r0]				@ r3 heeft nu het xt-addres van het volgende WORD
		sub r0, r3, #40				@ link-adres = xt-adres-40 #HEADER
		ldr r3, [r0]				@ haal nieuwe link
		cmp r3, #0					@ is de nieuwe link leeg? Zo niet dan print volgende woord
		bne 2b
nextmax		@ checked

wordhead "word#", 5, WORDS, 1		@ telt alle woorden in de dictionary ( -- aantal woorden )
WORDNO:	prolog						@ #HEADER
		str top, [dts, #-4]!
		ldr top,=0
		getvalvar r0, varLAST

	2:	add top, top, #1

		ldr r3, [r0]				@ r3 heeft nu het xt-addres van het volgende WORD
		sub r0, r3, #40				@ link-adres = xt-adres-40
		ldr r3, [r0]				@ haal nieuwe link
		cmp r3, #0					@ is de nieuwe link leeg? Zo niet haal volgende woord
		bne 2b
next		@ checked

wordhead ">number", 7, WORDNO, 1	@ ( n1 addr -- n2 e ) converts string to number - is bijna oude CONVERT van f83
TONUMBER:							@ n1 = start number meestal 0, addr waar counted string staat
									@ n2 is het resultaat, e het aantal niet verwerkte chars, en dus 0 bij succes
		prologmax

    	mov r1, top					@ r1 is nu address
    	ldr top, [r1], #4			@ top is lengte, r1 wijst nu naar start string
    	ldr r0, [dts]				@ r0 is tussen-resultaat plus n1, meestal 0

		cmp top, #0					@ Check if length is positive, lijkt onnodig
		ldrle top, =-1				@ bij lengte niet positief top wordt -1 en...
		ble 10f						@ naar het einde

		bl	varBASE					@ addres van varBASE op stack
        bl	FETCH					@ waarde base op stack
		popdts r3					@ r3 nu base

		ldrb r4, [r1], #1			@ Load first character in r4 and increment address.

		ldr r2, =0					@ r2 is flag die aangeeft of het een negatief getal (1) is of niet (0)
		cmp r4, #'-'				@ check voor trailing '-'
		bne 2f						@ Number is niet negatief

		ldr r2, =1					@ Onthoud dat number is negatief -> r2=1
        sub top, top, #1

        cmp top, #0					@ nog wat anders dan '-' in string?
        bgt 5f						@ zo ja, begin conversie

        mov top, #1					@ hierdoor een 1 op de stack -> 1 niet geconverteerde char
        b 10f

	5:	ldrb r4, [r1], #1			@ haal volgende char van address

	2:	sub r4, r4, #48 			@ r4 = r4 - '0' Convert the character into a digit.
        cmp r4, #0
        blt 9f 						@ naar einde als < 0
        cmp r4, #9
        ble 3f 						@ char tussen 0 en 9

    	sub r4, r4, #17				@ 17 = 'A' - '0'
    	cmp r4, #0					@ Test if hexadecimal character
    	blt 9f 						@ naar einde als < 'A'
        add r4, r4, #10				@ hier r4 een correcte HEX

	3:	cmp r4, r3					@ Compare to the current base.
        bge 9f						@ naar einde als > BASE

        add r0, r0, r4				@ hier is alles goed, tel digit op bij tussen-resultaat
        sub top, top, #1			@ counter 1 naar beneden
		cmp top, #0					@ verder als nog digits in string
        ble 9f
        b 7f

	7:	ldrb r4, [r1], #1			@ haal volgende char van address

		sub r4, r4, #48 			@ r4 = r4 - '0' Convert the character into a digit.
        cmp r4, #0
        blt 9f 						@ naar einde als < 0
        cmp r4, #9
        ble 3f 						@ char tussen 0 en 9

    	sub r4, r4, #17				@ 17 = 'A' - '0'
    	cmp r4, #0					@ Test if hexadecimal character
    	blt 9f 						@ naar einde als < 'A'
        add r4, r4, #10				@ hier r4 een correcte HEX

	3:	cmp r4, r3					@ Compare to the current base.
        bge 9f						@ naar einde als > BASE

		mul r0, r0, r3				@ number = nummer keer base
		add r0, r0, r4				@ hier is alles goed, tel digit op bij tussen-resultaat

        sub top, top, #1			@ counter 1 naar beneden
		cmp top, #0					@ verder als nog digits in string
        bgt 7b

	9:	cmp r2, #1					@ Negate result if we had a '-'.
        rsbeq r0, r0, #0

	10:	str r0, [dts]				@ en de waarde r0 in pos 2
nextmax		@ checked

@ zoek woord op address in dictionary, geef xt en code terug of addr en 0
@ -1 = immediate, 0 = niet gevonden, 1 wel gevonden #ERROR: immediate moet 1 zijn, en gevonden=true
wordhead "find", 4, TONUMBER, 1		@ ( addr word -- (addr, 0) of (xt, -1) of (xt, 1) )
FIND:	prologmax					@ #HEADER
		mov v, top					@ onthoud addr voor geval niets gevonden wordt
		
		getvalvar r0, varLAST

		@ldr r0, =EndDict			@ linkveld van laatste woord in dictionary
		ldr w, =0					@ index op nul

		add top, top, #4			@ top wijst nu naar de eerste character van word, lengte zit in 4 bytes daarvoor

	2:	add r2, r0, #7				@ r2 wijst naar het byte met de lengte van de naam van het woord in de dictionary
		ldrb r3, [r2], #1			@ r3 heeft nu de lengte van het woord, r2 wijst nu naar eerste char

		ldr r4, [top, #-4]			@ r4 heeft nu de lengte van het woord op address

		cmp r4, r3					@ zijn de lengtes gelijk?
		bne	3f						@ zo nee, dan de volgende link in dictionary halen vanaf 3f

	1:	ldrb r4, [r2, w]			@ haal character van adres r2, indexed met w, uit dictionary
		pushdts r4
		bl	TOCAPS
		popdts r4					@ r4 is nu altijd hoofdletter

		ldrb r1, [top, w]			@ haal character van adres top, indexed met r3 van te vinden woord
		pushdts r1
		bl	TOCAPS
		popdts r1					@ r1 is nu atijd hoofdletter

		cmp r1, r4					@ vergelijk letters
		bne	3f						@ als verschil tussen letters dan klaar met deze dictionary entry, ga naar volgende

		add w, w, #1				@ verhoog index met 1
		cmp w, r3					@ vergelijk index met lengte, al alle letters vergeleken
		bne 1b						@ alle letters al gecontroleerd? Nee, dan verder
									@ als hier aangekomen dan succes!! woord gevonden!!

		add top, r0, #40			@ zet het linkveld+40 in top, is dus nu xt van gevonden woord
		ldrb w, [r0, #5]			@ haal immediate byte - w is nu 1 als het een gewoon woord is
		cmp w, #255
		moveq w, #-1				@ maak van w -1 in 32 bit als w=255
		pushdts w					@ en zet w op stack
		b 4f						@ en klaar, met succes!

	3:	ldr w, =0					@ zet index weer op 0
		ldr r3, [r0]				@ r3 heeft nu het xt-addres van het volgende WORD
		sub r0, r3, #40				@ link-adres = xt_adres-40
		ldr r3, [r0]				@ haal nieuwe link
		cmp r3, #0					@ is de nieuwe link leeg? Zo niet dan vergelijk volgende woord uit dictionary
		bne 2b

		mov top, v					@ zet het oorspronkelijke addres terug als niets gevonden
		lit16 0x0					@ en zet een nul op de stack
	4:
nextmax		@ checked

wordhead "'", 1, FIND, -1			@ ( -- xt van woord na tick ) moet immediate
TICK:	prolog
		lit16 32					@ In f83 tick=state-smart, in ANSI tick alleen voor interpretatie, ['] voor in compilatie!
		bl	F83WORD

		cmp top, #0x0
		@bne 1f
		beq	_ABORT					@ Hier nog een ABORT-message: geen woord na TICK gevonden
		@b 2f

	1:	bl	FIND					@ hier is een woord in de TIB gevonden
		cmp top, #0x0				@ als nul dan woord onbekend
		@bne 3f
		beq	_ABORT					@ Hier nog een ABORT-message: woord na TICK onbekend
		@b 2f

	3:	bl	DROP					@ drop flag van stack en klaar!

	2:		@ CHECK verder!
next		@ moet de gevonden xt als lit in de compilatie gezet worden? NEE! TICK alleen in interpretatie
			@ voor in een definitie is ['] gedacht BRACKET-TICK

codehead ">caps", 5, TICK, 4, 1		@ ( char -- CHAR )
TOCAPS:
		cmp top, #97				@ vergelijk met 'a'
		blo 1f						@ als kleiner dan 97 zijn we al klaar
		cmp top, #122				@ vergelijk met 'z'
		subls top, top, #32			@ trek 32 af van character als het tussen 97 en 122 ligt
	1:
codenext	@checked

@ 'F83WORD' is de f83 versie
@ als word gecalled met een char on stack dan haalt WORD het volgende woord afgesloten door char op
@ uit de input buffer. Als buffer aan het einde cq. leeg dan returned word een nul

wordhead "f83word", 7, TOCAPS, 1	@ ( char -- addr waar volgende woord staat of '0')
F83WORD:	prolog
		mov r0, top					@ r0 nu 'zoek' character
		ldr r1, =0					@ r1=lengte van gevonden woord, hier nog nul
		ldr r2, =Trans				@ r2=scratch area waar woord heen geschreven wordt
		add r2, r2, #4				@ skip lengte word aan begin string -> r2 wijst nu naar begin string

	@ eerst leading delimiters weghalen en niet tellen
	1:	bl NEXTCHAR					@ top nu volgende char uit inputbuf #ERROR: alles kleiner als 32 is leading delimiter!
		popdts r3					@ char 9-13 en 32 zijn de klassieke 70r jaren Berkley leading delimiters, en dus OK!
		cmp r3, #32					@ spatie?
		beq 1b						@ dan verder zoeken
		cmp r3, #9					@ tab?
		beq 1b						@ dan ook verder zoeken
		cmp r3, #10					@ tab?
		beq 1b						@ dan ook verder zoeken
		cmp r3, #11					@ tab?
		beq 1b						@ dan ook verder zoeken
		cmp r3, #12					@ tab?
		beq 1b						@ dan ook verder zoeken
		cmp r3, #13					@ tab?
		beq 1b						@ dan ook verder zoeken

	@ dan kijken of buffer (verder) leeg
		cmp r3, #0					@ nul? dan buffer verder leeg
		beq	2f						@ als hier buffer al leeg dan dus geen volgende woord gevonden

	6:	strb r3, [r2, r1]			@ sla char op in Trans met lengte woord als pointer

		cmp r3, r0					@ is r3 gelijk aan zoek character?
		beq	3f						@ zo ja, zoeken klaar, lengte (kan 0 zijn!) moet nog aan begin string gezet

		add r1, r1, #1				@ hier aangekomen is er dus een woord van minstens 1 character gevonden

		bl	NEXTCHAR				@ haal volgende letter
		popdts r3
		cmp r3, #0					@ als hier een nul dan is de buffer leeg voordat de delimiter is gevonden
		beq 3f						@ stoppen - maar wel woord opslaan

		b	6b						@ en anders gaan we verder

	3:	cmp r1, #0
		beq	2f						@ als r1=lengte nul is dan een nul teruggeven via top

		ldr top, =Trans				@ top is nu weer up to date!
		str r1, [top]				@ en zet lengte op de juiste plek

		b 5f

	2:	ldr top, =0					@ zet een nul op de stack en klaar!
	5:
next		@ checked

@ nextchar haalt het volgende character uit de input buffer of 0x0 als buffer aan het einde
@ NEXTCHAR reset de InputBuffer niet! Dat doen routines die de buffer vullen.
@ NEXTCHAR past wel to_in aan door die te verhogen per char.

wordhead "nextchar", 8, F83WORD, 1	@ ( -- char / 0x0 )
NEXTCHAR:
		prolog
        str top, [dts, #-4]!		@ zet top alvast op stack, dan kan top verder als scratch dienen

        getvalvar r0, varTOIN		@ r0 nu de waarde in to_in = ofset van volgende char in TIB=text input buffer
		
		ldr r1, =TIB				@ r1 is start TIB #UPDATE voor interpret text ipv alleen TIB
									@ bijvoorbeeld met variabele die aangeeft waar ohm geinterpreteerd wordt.
									@ varTOIN blijft zoals hij is
        add r3, r1, r0				@ tel index op bij addres TIB

  		ldr r2, =TIBLim				@ r2 is TIB-limit #UPDATE voor interpret text ipv alleen TIB
  									@ bijvoorbeeld door nul terminated ipv limiet of lengte
        cmp r2, r3					@ if TIB+TIBpointer gelijk aan TIBLim
        beq 1f                  	@ dan jump naar einde en return 0x0

        ldrb top, [r3]				@ haal volgende char uit r3=TIB+ofset

        add r0, r0, #1				@ verhoog ofset met 1
        pushdts r0
        bl	varTOIN
        bl	STORE					@ en zet verhoogde offset terug in >in

		b 2f						@ en klaar

	1:	ldr top, =0x0				@ als hier dan buffer is aan het einde, return 0x0 en terug
	2:
next		@ checked}

@  ***************************  TYPE etc  **************************{
codehead "count", 5, NEXTCHAR, 3, 1 @ ( a-addr -- a-addr+4, lengte )
COUNT:
		ldr v, [top], #4			@ v nu lengte, van address top, top=top+4
		str top, [dts, #-4]!
		mov top, v					@ en v naar top
codenext	@ CHECK!!

wordhead "type", 4, COUNT, 1		@ ( address start string, lengte string (bytes) -- )
TYPE:	prolog						@ print het character op address, en de volgende characters tot lengte					
		ldr r0, [dts], #4			@ haal address naar r0 en verlaag dts
		mov r2, top					@ r2 is nu lengte
		ldr top, [dts, #4]!			@ update top alvast
		cmp r2, #0					@ als lengte 0 dan gelijk klaar
		beq 1f
		
	2:	ldrb r1, [r0], #1			@ haal character van address
		pushdts r1
		bl EMIT
		subs r2, r2, #1
		bne 2b
	1:
next		@ checked

wordhead ".c$", 3, TYPE, 1			@ ( address -- )
PR_CST:	prolog						@ print een string die vooraf gegaan wordt door een lengte word (32bit!!!)
		popdts r3					@ r3 is nu address, top is up-to-date
		@cmp r3, #0					@ als address = 0 #ERROR onnodige check
		@beq 1f						@ dan gelijk klaar
		ldr r0, [r3]				@ r0 nu lengte
		cmp r0, #0					@ als lengte = 0
		beq 1f						@ dan gelijk klaar

		add r1, r3, #4				@ r1 nu start string

	2:	ldrb r2, [r1], #1
		pushdts r2
		bl EMIT
		subs r0, r0, #1
		bne 2b
	1:
next		@ checked}

@  ***************************  TASKBLOCK  *************************{
codehead "cleartaskblock", 14, PR_CST, 0, 1 @ ( -- ) zet alles in taskblock op nul - alleen vanuit boot oproepen ivm  #DEBUG
CLEARTASKBLOCK:						@ verkeerde regs (r0-r2 ipv v,w) - kan hier geen stack gebruiken!!{
		ldv32 r0, TASK_BLOCK		@ en dus geen wordhead maar codehead!!
		ldv32 r1, NO_CACHE_MEM
		ldv16 r2, 0

	1:	str r2, [r0]				@ loop door alle memory positions tussen TASK_BLOCK en NO_CACHE_MEM
		add r0, r0, #4				@ en clear alle posities door te schrijven met '0'
		cmp r0, r1
		blo 1b
codenext	@ }

codehead "inittaskblock", 13, CLEARTASKBLOCK, 0, 1 @ codehead, dus geen bl's vanuit deze routine (zonder eerst de sp te zetten...)!
INITTASKBLOCK:						@ Zet per core (gecalled vanuit die core) de stackpointers - wordt gecalled vanuit elke core-init{
									@ de 'tasktable' vullen zou ook hier kunnen!
	@ there used to be a primitive cache-clean here, now done at sys-init

	@ set core-id					@ core-id hoeft niet meer gehaald!
		@mrc p15, 0, r0, c0, c0, 5	@ haal id van core waar deze routine loopt - inlinen want geen bl in codehead!
		@and r0, r0, #0b11			@ r0=core_id - 00, 01, 10 of 11

		ldv16 r0, 0
		lsl r1, r0, #TaskSize		@ core# * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een core -> r1 = offset in TASK_BLOCK
									@ dit werkt ip ook voor IRQ en FIQ en SMC
									
	@ zet de gewone stackpointers voor de correcte core - core# equals task# voor gewone tasks
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt
		add sp, r2, r1				@ r1=offset in TASK_BLOCK naar goede core
		
		ldv32 r2, TASK_BLOCK+TaskDTS @ =DatSt
		add dts, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskUSS @ =UseSt
		add uss, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskFPS @ =FloSt
		add fps, r2, r1
				
	@ zet IRQ stackpointer voor een core #CHECK: zou nu voor alle cores moeten werken!
		
		ldv16 r0, TaskNoIRQ0		@ r0=tasknummer van IRQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoIRQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoIRQn -> ie nu correct voor core#
									@ r1 is nog tseeds de offset door het core#
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3				@ r2 nu pointer naar waar de sp van de irq van deze task moet komen
		msr SP_irq, r2				@ zet stackpointer irq op address in TASK_BLOCK
		
	@ zet FIQ stackpointer voor een core
		ldv16 r0, TaskNoFIQ0		@ r0=tasknummer van FIQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoFIQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoFIQn -> ie nu correct voor core#
		
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3
		msr SP_fiq, r2				@ zet stackpointer fiq op address in TASK_BLOCK
		
	@ what about the SMC stackpointer for a core #THINK
		
@ codenext is 'bx lr' -> geen sp nodig -> sp kan dus hier gedefinieerd worden naar locatie in TASK_BLOCK
codenext	@}

codehead "inittaskblockc1", 15, INITTASKBLOCK, 0, 1 @ codehead, dus geen bl's vanuit deze routine (zonder eerst de sp te zetten...)!
INITTASKBLOCKC1:					@ Zet per core (gecalled vanuit die core) de stackpointers - wordt gecalled vanuit elke core-init{
									@ de 'tasktable' vullen zou ook hier kunnen!

	@ set core-id					@ #UPDATE kan weg want routines nu core-specifiek!!
		@mrc p15, 0, r0, c0, c0, 5	@ haal id van core waar deze routine loopt - inlinen want geen bl in codehead!
		@and r0, r0, #0b11			@ r0=core_id - 00, 01, 10 of 11

		ldv16 r0, 1					@ dit is core1
		lsl r1, r0, #TaskSize		@ core# * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een core -> r1 = offset in TASK_BLOCK
									@ dit werkt ip ook voor IRQ en FIQ en SMC
									
	@ zet de gewone stackpointers voor de correcte core - core# equals task# voor gewone tasks
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt
		add sp, r2, r1				@ r1=offset in TASK_BLOCK naar goede core
		
		ldv32 r2, TASK_BLOCK+TaskDTS @ =DatSt
		add dts, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskUSS @ =UseSt
		add uss, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskFPS @ =FloSt
		add fps, r2, r1
				
	@ zet IRQ stackpointer voor een core #CHECK: zou nu voor alle cores moeten werken!
		
		ldv16 r0, TaskNoIRQ0		@ r0=tasknummer van IRQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoIRQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoIRQn -> ie nu correct voor core#
									@ r1 is nog tseeds de offset door het core#
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3				@ r2 nu pointer naar waar de sp van de irq van deze task moet komen
		msr SP_irq, r2				@ zet stackpointer irq op address in TASK_BLOCK
		
	@ zet FIQ stackpointer voor een core
		ldv16 r0, TaskNoFIQ0		@ r0=tasknummer van FIQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoFIQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoFIQn -> ie nu correct voor core#
		
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3
		msr SP_fiq, r2				@ zet stackpointer fiq op address in TASK_BLOCK
		
	@ what about the SMC stackpointer for a core #THINK
		
@ codenext is 'bx lr' -> geen sp nodig -> sp kan dus hier gedefinieerd worden naar locatie in TASK_BLOCK
codenext	@}

codehead "inittaskblockc2", 15, INITTASKBLOCKC1, 0, 1 @ codehead, dus geen bl's vanuit deze routine (zonder eerst de sp te zetten...)!
INITTASKBLOCKC2:					@ Zet per core (gecalled vanuit die core) de stackpointers - wordt gecalled vanuit elke core-init{
									@ de 'tasktable' vullen zou ook hier kunnen!

	@ set core-id					@ #UPDATE kan weg want routines nu core-specifiek!!
		@mrc p15, 0, r0, c0, c0, 5	@ haal id van core waar deze routine loopt - inlinen want geen bl in codehead!
		@and r0, r0, #0b11			@ r0=core_id - 00, 01, 10 of 11

		ldv16 r0, 2					@ dit is core2
		lsl r1, r0, #TaskSize		@ core# * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een core -> r1 = offset in TASK_BLOCK
									@ dit werkt ip ook voor IRQ en FIQ en SMC
									
	@ zet de gewone stackpointers voor de correcte core - core# equals task# voor gewone tasks
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt
		add sp, r2, r1				@ r1=offset in TASK_BLOCK naar goede core
		
		ldv32 r2, TASK_BLOCK+TaskDTS @ =DatSt
		add dts, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskUSS @ =UseSt
		add uss, r2, r1
		
		ldv32 r2, TASK_BLOCK+TaskFPS @ =FloSt
		add fps, r2, r1
				
	@ zet IRQ stackpointer voor een core #CHECK: zou nu voor alle cores moeten werken!
		
		ldv16 r0, TaskNoIRQ0		@ r0=tasknummer van IRQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoIRQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoIRQn -> ie nu correct voor core#
									@ r1 is nog tseeds de offset door het core#
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3				@ r2 nu pointer naar waar de sp van de irq van deze task moet komen
		msr SP_irq, r2				@ zet stackpointer irq op address in TASK_BLOCK
		
	@ zet FIQ stackpointer voor een core
		ldv16 r0, TaskNoFIQ0		@ r0=tasknummer van FIQ van core0 - moet nog keer 2^TaskSize
		lsl r3, r0, #TaskSize		@ task#irq * TaskSize (de n in 2^n) is offset in TASK_BLOCK voor een task
									@ -> r3 = offset naar TaskNoFIQ0 in TASK_BLOCK
		add r3, r3, r1				@ r3 = offset naar TaskNoFIQn -> ie nu correct voor core#
		
		ldv32 r2, TASK_BLOCK+TaskRTS @ =RetSt dit is de sp in ARM-terminologie
		add r2, r2, r3
		msr SP_fiq, r2				@ zet stackpointer fiq op address in TASK_BLOCK
		
	@ what about the SMC stackpointer for a core #THINK
		
@ codenext is 'bx lr' -> geen sp nodig -> sp kan dus hier gedefinieerd worden naar locatie in TASK_BLOCK
codenext	@}

wordhead "inittasktable", 13, INITTASKBLOCKC2, 1 @ ( -- ) reinigt en vult tasktable met default voor de standard tasks
INITTASKTABLE: prolog				@ keeps nothing - tabel shared by all cores -> core0 does this once{

		ldv32 r0, TaskTable			@ r0 is base Tasktable
		ldv32 r2, 0x00000000		@ priocount en prioreset bij opzetten TaskTable beide altijd op 0
		ldv32 r3, 0xFFFF0000
		
		ldv16 r1, MaxTask
		
	@ clean eerste hele tabel, met 0xFF op core en status en 0x0000 in de rest
	1:	str r3, [r0], #4			
		str r2, [r0], #4
		subs r1, r1, #1
		bne 1b						@ en doe dit evenvaak als er maximaal tasken zijn
		
	@ vul nu specifiek de rest met core en taak specifieke data
		ldv32 r0, TaskTable			@ r0 is base Tasktable

		ldv32 r1, 0x00000000		@ TaskNoCore0=0
		str r1, [r0], #8			@ core0 alles op 0

		ldv32 r1, 0x01000000		@ TaskNoCore1=1
		str r1, [r0], #8			@ core1

		ldv32 r1, 0x02000000		@ TaskNoCore2=2
		str r1, [r0], #8			@ core2

		ldv32 r1, 0x03000000		@ TaskNoCore3=3
		str r1, [r0], #8			@ core3

		ldv32 r1, 0x00020100		@ TaskNoPIXEL=4
		str r1, [r0], #8			@ core 0

		ldv32 r1, 0x03020100		@ TaskNoSOUND=5
		str r1, [r0], #8			@ core3

	@ jump over free rows
		ldv16 r3, 116
		ldv32 r0, TaskTable			@ r0 is base Tasktable
		add r0, r0, r3, lsl #3 

		ldv32 r1, 0x00010000		@ TaskNoIRQ0=MaxTask-12=116	-> dit is de task-manager van core0 - hoofdtaskmanager
		str r1, [r0], #8

		ldv32 r1, 0x01010000		@ TaskNoIRQ1=MaxTask-11=117	-> dit is de task-manager van core1
		str r1, [r0], #8

		ldv32 r1, 0x02010000		@ TaskNoIRQ2=MaxTask-10=118 ->	dit is de task-manager van core2
		str r1, [r0], #8

		ldv32 r1, 0x03010000		@ TaskNoIRQ3=MaxTask-9=119 -> dit is de task-manager van core3 - cave sound-manager
		str r1, [r0], #8

		ldv32 r1, 0x00010000		@ TaskNoFIQ0=MaxTask-8=120
		str r1, [r0], #8

		ldv32 r1, 0x01010000		@ TaskNoFIQ1=MaxTask-7=121
		str r1, [r0], #8

		ldv32 r1, 0x02010000		@ TaskNoFIQ2=MaxTask-6=122
		str r1, [r0], #8

		ldv32 r1, 0x03010000		@ TaskNoFIQ3=MaxTask-5=123 -> dit is de sound-manager - core3 - gaat voor de task-manager
		str r1, [r0], #8

		ldv32 r1, 0x00010000		@ TaskNoSMC0=MaxTask-4=124
		str r1, [r0], #8

		ldv32 r1, 0x01010000		@ TaskNoSMC1=MaxTask-3=125
		str r1, [r0], #8

		ldv32 r1, 0x02010000		@ TaskNoSMC2=MaxTask-2=126
		str r1, [r0], #8

		ldv32 r1, 0x03010000		@ TaskNoSMC3=MaxTask-1=127
		str r1, [r0], #8
next		@ #UPDATE blijf aan werken}
@}

@  **********************  interrupt handlers  *********************{

codehead "handlereset", 11, INITTASKTABLE, 0, 1
HANDLERESET:						@{
		@ UPDATE: hier nog verder met een watchdog full reset spelen
		stmfd r13!, {r0, r1}		@ r0 en 1 veiligstellen
		
		ldv32 r1, sivhandlereset	@ tel hoe vaak een handlereset gezien wordt
 		ldr r0, [r1]				@ moet uiteraard 0 zijn en blijven
 		add r0, r0, #1
 		str r0, [r1]

		ldmfd r13!, {r0, r1}		@ en weer terugzetten
		subs pc, lr, #0				@ - #0 => ga verder met de volgende opdracht (who cares!!)
		
		@b HANDLERESET
codenext		@}

codehead "undefinstr", 10, HANDLERESET, 0, 1
UNDEFINSTR:							@{
		stmfd r13!, {r0, r1}		@ r0 en 1 veiligstellen
		
		ldv32 r1, sivundefined		@ tel hoe vaak een undefined instructie gezien wordt
 		ldr r0, [r1]				@ moet 0 zijn en blijven, maar is dat niet!! #ERROR
 		add r0, r0, #1
 		@ldv32 r0, 0x555
 		str r0, [r1]
 		
 		ldv32 r1, sivundefinedadr
 		mov r0, lr
		str r0, [r1]

		ldmfd r13!, {r0, r1}		@ en weer terugzetten
		subs pc, lr, #0				@ - #0 => ga verder met de volgende opdracht (who cares!!)
codenext	@}

codehead "monitor", 7,UNDEFINSTR, 0, 1 @ alleen tijdens boot! MONITOR is EL3
MONITOR:							@{
	@ hier evt eerst nog non-secure mode	

	@ naar secure mode	
		ldv16 v, 0x0130				@ 0x0130 ideaal for CrayonForth, frees as much as technically possible - met NS=0
		dmb
		mcr p15, 0, v, c1, c1, 0	@ functions!!! =SCR register - 0x0130 zet alles in een keer goed voor mij
		isb

	@ set coherency - secure version - only possible in EL3 met SCR.NS=0
		MRRC p15, 1, r0, r1, c15
		dmb
		orr r0, r0, #64				@ set bit [6] op 1 #CHECK
		MCRR p15, 1, r0, r1, c15
		isb

	@ set secure version of TTBCR - only possible in EL3 met SCR.NS=0
		mov	r0, #0x0				@ us only TTBR0 (die bedoeld is voor applicaties en switchen van context)
		mcr	p15, 0, r0, c2, c0, 2	@ Write to TTBCR => de TTBCR wordt simpelweg in zijn geheel op 0 gezet
		isb

	@ set secure version of TTRB0 - only possible in EL3 met SCR.NS=0	
		ldv32 r1, 0x406A 			@ 0000 0000 0000 0000 0100 0000 0110 1010 shared outer en inner write back, alocate on write
									@ kan gelijk schrijven in TTRB0, heeft geen kritische waardes om te onthouden
 		ldv32 r3, 0x3014			@ debug-register			
		str r1, [r3]
		dmb
		mcr	p15, 0, r1, c2, c0, 0	@ Write to TTBR0[31:0] - this is de secure version
		isb
		
	@ en zet sysvariabele
		ldv32 r1, sivmonitor		@ count number of calls to the monitor
 		ldr r0, [r1]				@ should eb 3, and stay 3
 		add r0, r0, #1
 		str r0, [r1]

	@ einde monitor call
		subs pc, lr, #0				@ Return to secure SVC mode	- was eerst movs pc, lr - sub of subs?? subs!!
codenext							@ }

codehead "prefetchabort", 13, MONITOR, 0, 1
PREFETCHABORT:						@{
		stmfd r13!, {r0, r1}		@ r0 en 1 veiligstellen
		
		ldv32 r1, sivprefetchabort	@ tel hoe vaak een prefetchabort gebeurd
 		ldr r0, [r1]				@ moet uiteraard 0 zijn en blijven
 		add r0, r0, #1
 		str r0, [r1]

		ldmfd r13!, {r0, r1}		@ en weer terugzetten
		subs pc, lr, #0				@ - #0 => ga verder met de volgende opdracht (who cares!!)
codenext	@}

codehead "dataabort", 9, PREFETCHABORT, 0, 1
DATAABORT:							@{
		stmfd r13!, {r0, r1}		@ r0 en 1 veiligstellen
		
		ldv32 r1, sivdataabort		@ tel hoe vaak een dataabort gebeurd
 		ldr r0, [r1]				@ moet uiteraard 0 zijn en blijven
 		add r0, r0, #1
 		str r0, [r1]
 			
 		mov r0, lr
 		ldv32 r1, sivdataabortadr	@ laat zien welk address de fout gaf!
 		str r0, [r1]
 		
 		ldmfd r13!, {r0, r1}		@ en weer terugzetten
		subs pc, lr, #0				@ - #0 => ga verder met de volgende opdracht (who cares!!)
codenext	@}

codehead "reserved", 8, DATAABORT, 0, 1
RESERVED:							@{
		b RESERVED
codenext	@}

codehead "irqhandler", 10, RESERVED, 0, 1 @ ( -- ) base for multitasking forth - YEAH!!!
IRQHANDLER:							@ {
		cpsid ifa					@ needed? I think not, no more interrupt calls during interrupt possible
		stmfd r13!, {r0-r12, lr}	@ pc saved by irq, sp en lr not visible during irq, but lr_irq sp_irq are visible 
									@ -> no need to save sp en lr (maar lr MOET hier dus WEL gesaved...)
									@ IRQ op dit moment 30Hz gecalled - kan omhoog naar 1000 of zo, maar PIXEL
									@ moet op 30Hz blijven
									
	@ stack-pointer=sp_irq is defined in INITTASKBLOCK
									
		bl LTIRQCLEAR				@ mandatory!! otherwise irq storm by the local timer!!

		bl	COREID					@ haal id van core waar dit op loopt
		popdts r0
		cmp r0, #0
		bne 9f						@ als geen core0 => gelijk klaar

	@ start core0 specifiek deel van IRQ **********************
		valtocore0task TaskNoIRQ0, r0, r1 @ zet task nummer van IRQ0 in core0task #CORE1
		
		dsb
		sev							@ this activates all and every waiting core due to wfe
									@ core1 does PIXEL (no more crashing!) and than waits again.

	@ update sysvar *****************
		ldv32 r1, sivirq			@ count number of IRQ-calls done
 		ldr r0, [r1]				@ should be around 25 Hz presently
 		add r0, r0, #1
 		str r0, [r1]
 		
		valtocore0task 0, r2, r3	@ en zet task nummer weer op 0
		
	9:	ldmfd r13!, {r0-r12, lr}
		cpsie ifa
		subs pc, lr, #4				@ return persé uit irq met subs, maar codenext nodig voor .text
									@ subs ipv sub is essential to update cpsr from spsr!
codenext	@ CHECK!}

codehead "fiqhandler", 10, IRQHANDLER, 0, 1
FIQHANDLER:							@ een fast irq heeft zijn eigen regs (r8-r15){
		stmfd r13!, {r0-r7, lr}		@ pc saved by irq, sp en lr not visible during irq, but lr_irq sp_irq are visible 
									@ -> no need to save sp en lr (maar lr MOET hier dus WEL gesaved...)
									@ FIQ op dit moment 30Hz gecalled - kan omhoog naar 1000 of zo, maar PIXEL
		
		bl LTIRQCLEAR				@ mandatory!! otherwise irq storm by the local timer!!
		
	@ update sysvar *****************
		ldv32 r1, sivfiq			@ tel hoe vaak een FIQ gecalled wordt
 		ldr r0, [r1]				@ moet 0 zijn o.h.m.
 		add r0, r0, #1
 		str r0, [r1]

		ldmfd r13!, {r0-r7, lr}
		subs pc, lr, #4
codenext							@ return uit fiq met subs maar codenext nodig voor .text}

@  ***********************************************************
SECUREVECTORS:						@{
		ldr pc, [pc, #24]			@ Reset Handler
 		ldr pc, [pc, #24]			@ Undefined Instruction Handler
		ldr pc, [pc, #24]			@ Secure Monitor Handler
 		ldr pc, [pc, #24]			@ Prefetch Abort Handler
		ldr pc, [pc, #24]			@ Data Abort Handler
		ldr pc, [pc, #24]			@ Reserved Handler
		ldr pc, [pc, #24]			@ IRQ (Interrupt Request) Handler
		ldr pc, [pc, #24]			@ FIQ (Fast Interrupt Request) Handler

handleresetlink:
	.word HANDLERESET				@ kan misschien gebruikt worden om een core te resetten
undefinedInstructionlink:
	.word UNDEFINSTR
monitorlink:
	.word MONITOR        			@ zet CPR goed, oa secure flag = EL3!
prefetchabortlink:
	.word PREFETCHABORT
dataabortlink:
	.word DATAABORT
reservedlink:
	.word RESERVED
irqlink:
	.word IRQHANDLER
fiqlink:
	.word FIQHANDLER				@}
@}

@  *************************  system setup  ************************{

codehead "initsys", 7, FIQHANDLER, 0, 1	@ only for core0
INITSYS:							@{
	@ ***********  here stuff for hyp-mode can be done  ************
		ldv32 r13, 0x1000000		@ temp stackpointer - to make booting more reliable

		mrs r0, cpsr
		
		ldv32 r3, sivisc0CPSR		@ put value in siv
		str r0, [r3]				@ dit is 0x600001DA for core0 and 0x200001DA for the others

		cpsid ifa					@ we don't want no surprises
		
		@ hier HCR lezen (is HypConfRegister)
		@ MRC p15,4,<Rt>,c1,c1,0 ; Read HCR into Rt MCR p15,4,<Rt>,c1,c1,0 ; Write Rt to HCR

	@ ***************  disable caches  *****************************
		ldv32 r1, 0xC50838			@ disables MMU, I-cache, D-cache
		
		ldv32 r2, sivisc0SCTLR		@ put value in siv 
		str r1, [r2]
		
		MCR p15, 0, r1, c1, c0, 0	@ 0xC50838 is always what is written -> no read-change-write cycle needed
		isb
		
	@ ************** invalidate caches  *****************************
		ResetDatacache				@ ResetDataCache purges AND invalidates data-cache						
		dsb
		isb

		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ =BPIALL - invalidate all branch-prediction
		isb
		
		MOV r0, #0
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb en isb moeten na een invalidate TLB
		isb

	@ ********* Invalidate complete Data cache  ********************
		@ResetDataCache
		@barrier
		
	@ *** test and switch from HYP *********************************

		mrs r0, cpsr
		dsb
		isb

		and r1, r0, #0x1F
		cmp r1, #0x1A				@ is core in hyp-mode>

		bne	2f						@ als geen hyp-mode than ook geen switch nodig!

		bic	r0, r0, #0x1F			@ clear mode bits
		orr r0, r0, #0x1F			@ and set SVC mode - #CHECK: why not system-mode, which we do later
									@ modes => 111110=0x1F:system, 10000:user, 10110:monitor, 10011=0x13:supervisor

		ldv32 r3, sivisc0SPSRhyp	@ put value in siv
		str r0, [r3]

		msr SPSR_hyp, r0			@ put adapted CPSR in SPSR_hyp for return - spsr suffices but spsr_hyp is clearer
		isb							@ this SPSR is what after hyp is available in cpsr!!
		
		ldv32 r1, 2f
		
		ldv32 r3, sivisc0ELRhyp		@ put value in siv
		str r1, [r3]
		
		msr ELR_hyp, r1				@ put address 2f in ELR_hyp
		eret						@ and return from hyp to that address

	2:	nop
		nop
		barrier
		
		ldv32 r13, 0x1000000		@ temp sys-stackpointer - to make booting more reliable
@  *****************************  CAVE  ****************************
		
@  SPSRsys instead of SPSRhyp from here!
@  SPRS is saved program state register and is filled when an exception is taken 
@  it has a copy of CPSR, and puts this back comming out of the state.
@  SYS and USR share the SPSR, SVC has its own SPRS	
		
@  *************** nu in non-secure system mode  ******************* 

@ Get the vector base address from the System Control register. Every core uses this vector-base, presently
		mrc p15, 0, r0, c12, c0, 0	@ haal r0 uit VBAR -> de huidige vector locatie
									@ is this always the correct location?? -> checked that r0=0
									@ Is there always a correct location in VBAR!?

		ldv32 r1, SECUREVECTORS		@ Get the address of the secure vectors in r1

		ldmia r1!, {r2-r5}			@ Copy the secure vector table van r1 naar oorspronkelijk
		stmia r0!, {r2-r5}			@ dit werkt omdat de vectoren absolute spring-addressen zijn van WORDS
		ldmia r1!, {r2-r5}
		stmia r0!, {r2-r5}
		ldmia r1!, {r2-r5}
		stmia r0!, {r2-r5}
		ldmia r1!, {r2-r5}
		stmia r0!, {r2-r5}
		barrier

@  *******************  nu caches ed cleanen  ******************
@ cache clean hoeft niet bij total reset - wel bij soft-restart - helpt niet tegen slechte boot
 		
 		ResetDataCache				@ deze volgorde is plicht - anders start de boel niet!!!
 		barrier
 		
 		mcr p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
 		barrier
 		
 		MCR p15, 0, r0, c7, c5, 6	@ =BPIALL - invalidate all branch-prediction
		isb
		
		MOV r0, #0
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb en isb moeten na een invalidate TLB
		isb

 		smc #0						@ Perform a secure monitor call - #0 is mandatory !!
		isb
		
@  ******************* disable Data throttle  ******************
		@ a non-recomended feature of ARMv8 - makes screen-writing routines much faster
		@ but does not work with Coherency together
		
@  ******************* stay in system mode  ********************
		@ Hyp-mode is now left in 0x1F=system-mode ipv 0x13 (0b10011:supervisor)
		@ supervisor has its own r13, r14 en r15 (!!) and has to be left via
		@ a subs, just as all other exceptions except hyp
		@ on an ARMv8 cps cannot be used for this!!
		@ if EL3 uses Aarch32 than EL1 does not exist and all privileged modes, like system, run in EL3!
		
		@cps	#0x1F				@ #ERROR: now in system mode - #0x1F (0b11111:system)
		@isb						

@  ******************* enable FPU en SIMD  *********************
		
		ldv32 r0, 0xf00000			@ make cp10 en 11 fully accesible - pi3 - see page 4272

		ldv32 r3, sivisc0CPACR		@ put value in siv
		str r0, [r3]

		mcr p15, 0, r0, c1, c0, 2	@ Write r0 to CPACR
		isb

		mov r1, #0x40000000			@ Set FPEXC_EN bit to enable the FPU
		vmsr FPEXC, r1				@ SUCCESS!!!!!!
		isb

@  ******************  enable coherency  pi3 *******************
		MRRC p15, 1, r0, r1, c15
		orr r0, r0, #64				@ set bit [6] op 1 - this is the non-secure version!!!!
									@ the secure versie can only be done in MON=el3
									
		ldv32 r3, sivisc0ACTLRns	@ put values in siv
		str r1, [r3], #4			@ =bit 63:32 r1 should be 0x0
		str r0, [r3]				@ =bit 31:0	r0 should be 0x40						
									
		MCRR p15, 1, r0, r1, c15	@ Write CPU Extended Control Register = ACTLR = implementation defined :(
		isb

@  ******  make system clocks available to PL0 level  **********
		ldr r1, =0x303				@ 11 0000 0011
		mrc p15, 0, r0, c14, c1, 0	@ read CNTKCTL=counter-timer kernel control register
		orr r0, r0, r1

		ldv32 r3, sivisc0CNTKCTL	@ put value in siv
		str r0, [r3]
		
		mcr p15, 0, r0, c14, c1, 0	@ update CNTKCTL
		isb
codenext		@}

codehead "initsysc1", 9, INITSYS, 0, 1 @ to initiate core1
INITSYSC1:							@{
	@  ****** hier dingen/statussen die alleen in hyp kunnen  ******
	@  *************************************************************
	
		ldv32 r13, 0x1100000		@ temp hyp-stackpointer - to make booting more reliable - this one is essential

		mrs r0, cpsr
		dsb
		isb
		
		ldv32 r3, sivisc1CPSR		@ put present value cpsr in siv
		str r0, [r3]				@ 0x200001DA for core1
	
		cpsid ifa					@ we don't want no surprises
		
	@ ************ disable caches  *********************************
	
		ldv32 r1, 0xC50838			@ =disables MMU, I-cache, D-cache
		
		ldv32 r2, sivisc1SCTLR		@ zet gelezen waarde in siv
		str r1, [r2]
		
		MCR p15, 0, r1, c1, c0, 0	@ actual disabeling
		isb

	@ ********* Invalidate caches  *********************************		
		ResetDataCache
		dsb
		isb

		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb & isb mandatory after invalidate TLB
		isb

	@ *** test and switch from HYP *********************************

		mrs r0, cpsr
		dsb
		isb

		and r1, r0, #0x1F
		cmp r1, #0x1A				@ is core in hyp-mode?

		bne	2f						@ als geen hyp-mode than ook geen switch nodig!

		bic	r0, r0, #0x1F			@ clear mode bits
		orr r0, r0, #0x1F			@ and set SYS mode
									@ modes => 11111=0x1F:system, 10000:user, 10110:monitor, 10011=0x13:supervisor

		ldv32 r3, sivisc1SPSRhyp	@ put value in siv
		str r0, [r3]

		msr SPSR_hyp, r0			@ put adapted CPSR in SPSR_hyp for return - spsr suffices but spsr_hyp is clearer
		isb							@ this SPSR is what after hyp is available in cpsr!!
		
		ldv32 r1, 2f
		
		ldv32 r3, sivisc1ELRhyp		@ put value in siv
		str r1, [r3]
		
		msr ELR_hyp, r1				@ put address 2f in ELR_hyp
		eret						@ and return from hyp to that address
		
	2:	nop
		nop
		barrier
		
		@ldv32 r13, 0x1100000		@ temp sys-stackpointer #DEBUG: seems not essential
	
 	@  *************** now in non-secure system mode  ************** 

	@ core0 sets vectors, other cores also use these vectors and thus don't need to reset them
	@ later individual vectors for each core. #UPDATE

	@  *********************  clean caches etc  ********************
	@ not strictly necessary
 		
 		ResetDataCache
		dsb
		isb

		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb & isb mandatory after invalidate TLB
		isb

 		smc #0						@ Perform a secure monitor call - #0 is mandatory !!
 		isb

	@  ******************* ga NIET in system mode  *****************

		@cps	#0x1F				@ switch to system mode - crasht bij core1
		@dmb						@ #CHECK waarom switch niet werkt
		@isb

	@  ******************* enable FPU en SIMD  *********************
		
		ldv32 r0, 0xf00000			@ make cp10 en 11 fully accesible - pi3 - see page 4272
		
		ldv32 r3, sivisc1CPACR		@ put value in siv
		str r0, [r3]
		
		mcr p15, 0, r0, c1, c0, 2	@ Write Rt to CPACR
		isb

		mov r1, #0x40000000			@ Set FPEXC_EN bit to enable the FPU
		vmsr FPEXC, r1
		isb
		
	@  ******************  enable coherency  pi3 *******************
		
		MRRC p15, 1, r0, r1, c15	@ r0=bit 31:0 r1=bit 63:32
		orr r0, r0, #64				@ set bit [6] op 1 - this is the non-secure version!!!!
									@ the secure versie can only be done in MON=el3
		
		ldv32 r3, sivisc1ACTLRns	@ put values in siv
		str r1, [r3], #4			@ =bit 63:32
		str r0, [r3]				@ =bit 31:0	
		
		MCRR p15, 1, r0, r1, c15	@ Write CPU Extended Control Register = ACTLR = implementation defined :(
		isb
		
	@  ******  make system clocks available to PL0 level  **********
		
		ldr r1, =0x303				@ 11 0000 0011 - deze wel op CORE-nivo, staat op 0
		mrc p15, 0, r0, c14, c1, 0	@ read CNTKCTL
		isb
		dsb
		orr r0, r0, r1
		
		ldv32 r3, sivisc1CNTKCTL	@ put value in siv
		str r0, [r3]
		
		mcr p15, 0, r0, c14, c1, 0	@ update CNTKCTL
		isb
codenext		@}

codehead "initsysc2", 9, INITSYSC1, 0, 1 @ to initiate core2
INITSYSC2:								@{
	@  ****** hier dingen/statussen die alleen in hyp kunnen  ******
	@  *************************************************************
	@ CAVE: isb is beperkt tot de core die hem called, en zorgt dus niet voor coherency
	@ tussen verschillende CORES als in gebruik met invalidate i-cache, omdat isb per core werkt

		ldv32 r13, 0x1200000		@ temp hyp-stackpointer #DEBUG: this one essential for reliable boot
		
		mrs r0, cpsr
		dsb
		isb
		
		ldv32 r3, sivisc2CPSR		@ put present value cpsr in siv
		str r0, [r3]				@ 0x200001DA for core2
	
		cpsid ifa					@ we don't want no surprises
		
	@ Eerst de caches disabelen - #TEST #DEBUG works => leave for now
		
		ldv32 r1, 0xC50838			@ disables MMU, I-cache, D-cache
		
		ldv32 r2, sivisc2SCTLR		@ zet gelezen waarde in siv
		str r1, [r2]
		
		MCR p15, 0, r1, c1, c0, 0
		isb

	@ ********* Invalidate caches  *********************************		
		ResetDataCache
		dsb
		isb

		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb & isb mandatory after invalidate TLB
		isb

	@ *** test and switch from HYP *********************************

		mrs r0, cpsr

		and r1, r0, #0x1F
		cmp r1, #0x1A				@ is core in hyp-mode?

		bne	2f						@ als geen hyp-mode than ook geen switch nodig!

		bic	r0, r0, #0x1F			@ clear mode bits
		orr r0, r0, #0x1F			@ and set SVC mode - #CHECK: why not system-mode, which we do later
									@ modes => 111110=0x1F:system, 10000:user, 10110:monitor, 10011=0x13:supervisor

		ldv32 r3, sivisc2SPSRhyp	@ put value in siv
		str r0, [r3]

		msr SPSR_hyp, r0			@ put adapted CPSR in SPSR_hyp for return
		isb							@ this SPSR is what after hyp is available in cpsr!!
		
		ldv32 r1, 2f
		
		ldv32 r3, sivisc2ELRhyp		@ put value in siv
		str r1, [r3]
		
		msr ELR_hyp, r1				@ put address 2f in ELR_hyp
		eret						@ and return from hyp to that address
		
	2:	nop
		nop
		barrier
		
		@ldv32 r13, 0x1200000		@ temp sys-stackpointer #DEBUG: seems not essential
		
 	@  *************** now in non-secure system mode  ************** 

	@ bij core0 hier het zetten van de vectoren, bij de andere cores niet nodig want die gebruiken dezelfde
	@ vectoren. Later iedere core zijn eigen vectoren. #UPDATE

	@  *********************  clean caches etc  ********************
	@ cache clean hoeft niet bij total reset - wel bij soft-restart - helpt niet tegen slechte boot
 		
 		ResetDataCache				@ deze volgorde is plicht - anders start de boel niet!!!
 		barrier
 		mcr p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
 		dsb
 		isb

 		smc #0						@ Perform a secure monitor call - #0 is mandatory !!
 		isb

	@  ******************* ga NIET in system mode  *****************

	@  ******************* enable FPU en SIMD  *********************
		
		ldv32 r0, 0xf00000			@ make cp10 en 11 fully accesible - pi3 - see page 4272
		
		ldv32 r3, sivisc2CPACR		@ put value in siv
		str r0, [r3]
		
		mcr p15, 0, r0, c1, c0, 2	@ Write Rt to CPACR
		isb

		mov r1, #0x40000000			@ Set FPEXC_EN bit to enable the FPU
		vmsr FPEXC, r1
		isb
		
	@  ******************  enable coherency  pi3 *******************
		
		@MRRC p15, 1, r0, r1, c15	@ r0=bit 31:0 r1=bit 63:32
		@orr r0, r0, #64			@ set bit [6] op 1 - this is the non-secure version!!!!
									@ the secure versie can only be done in MON=el3
									
		ldv16 r0, 0x40
		ldv16 r1, 0x0
									
		ldv32 r3, sivisc2ACTLRns	@ put values in siv
		str r1, [r3], #4			@ =bit 63:32 r1 moet 0x0 zijn
		str r0, [r3]				@ =bit 31:0 r0 moet 0x40 zijn
		
		MCRR p15, 1, r0, r1, c15	@ Write CPU Extended Control Register = ACTLR = implementation defined :(
		isb
		
	@  ******  make system clocks available to PL0 level  **********
		
		ldr r1, =0x303				@ 11 0000 0011 - deze wel op CORE-nivo, staat op 0
		mrc p15, 0, r0, c14, c1, 0	@ read CNTKCTL
		orr r0, r0, r1
		
		ldv32 r3, sivisc2CNTKCTL	@ put value in siv
		str r0, [r3]
		
		mcr p15, 0, r0, c14, c1, 0	@ update CNTKCTL
		isb
codenext		@}

wordhead "initpagetable", 13, INITSYSC2, 1
INITPAGE:							@ set page table tot 1024-16-64-16=928 op cached, secure, shared{
		prolog
		ldr r1, =0					@ r1 = base
		ldr r2, =PageTableAdr		@ address van PageTable
									@ voor TEX zie page 4063
									@ was 0x90C0E, nu 0x94C06 => allocate on write essential for coherency!!
									@ flags fast TEX 100 CB 01, inner-cached, sector, full access, non-secure
		ldr	r3, =0x94C06			@ was 0x10C0E is goed, 0x14C0E very slow (tex 4->geen cache of buffer) 0x50C0E is met supersections
1:		mov r0, r1, lsl #20			@ r0 is base 20 bit naar links geschift
		orr	r0, r0, r3				@ or r0 met flags in r3
		str r0, [r2], #4			@ zet de flags in de eerste positie van de pagetable
		add r1, r1, #1
		cmp r1, #1					@ limit vullen deel 1 PageTable RasPi3 -> 1024 min 16 min 64 min 16 MByte=928
		bne 1b
		
		dsb
		
		mov r0, #0
		mcr p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb en isb moeten na een invalidate TLB
		isb

		ldr	r3, =0x94C06			@ was 0x10C0E is goed, 0x14C0E very slow (tex 4->geen cache of buffer) 0x50C0E is met supersections
5:		mov r0, r1, lsl #20			@ r0 is base 20 bit naar links geschift
		orr	r0, r0, r3				@ or r0 met flags in r3
		str r0, [r2], #4			@ zet de flags in de eerste 928 posities van de pagetable
		add r1, r1, #1
		cmp r1, #928				@ limit vullen deel 1 PageTable RasPi3 -> 1024 min 16 min 64 min 16 MByte=928
		bne 5b		

		ldr r3, =0x90C02			@ NOCACHE-deel, 16Mb no-cache, no-buffer geheugen - TEX 000 (device) en 001 (normal memory) werkt beide
2:		mov r0, r1, lsl #20			@ r0 is base 20 bit naar links geschift 
		orr	r0, r0, r3				@ or r0 met flags in r3 MOET geshared, anders onregelmatige crashes bij initiatie
		str r0, [r2], #4
		add r1, r1, #1
		cmp r1, #944				@ einde non-cached memory-blok
		bne 2b
									@ 0x40C02>449ms (maar error -> crasht soms) 0x50C0A>842ms voor het verticaal bewegen van een window
									@ 0x50C02 en 0x50C06 both function, met data-throttle (default) is ...06 sneller
									@ =0xD0C06 of 0x10C02
		ldr r3, =0x94C06			@ SCREEN-deel: cave: 0x50C0A (cachable, no-buffer) => SLOWER!!
3:		mov r0, r1, lsl #20			@ r0 is base 20 bit naar links geschift
		orr	r0, r0, r3				@ or r0 met flags in r3
		str r0, [r2], #4			@ zet de flags in de rest van de posities van de pagetable
		add r1, r1, #1
		cmp r1, #1008				@ einde 64MB screen-memory
		bne 3b

		ldr r3, =0x90C06			@ flags voor de rest van de pagetable, device-memory
4:		mov r0, r1, lsl #20			@ r0 is base 20 bit naar links geschift
		orr	r0, r0, r3				@ or r0 met flags in r3
		str r0, [r2], #4			@ zet de flags in de rest van de posities van de pagetable
		add r1, r1, #1
		cmp r1, #4096				@ rest van pagetabel
		bne 4b
		
		dsb
		
		mov r0, #0
		mcr p15, 0, r0, c8, c7, 0	@ Invalidate entire Unified Main TLB
		dsb							@ dsb en isb moeten na een invalidate TLB
		isb
next		@ checked}

wordhead "cleancache", 10, INITPAGE, 1
CLEANCACHE:							@ cleans cache - mandatory for pi3 after each new definition of a word!!{
		prolog						@ cave: 1 DCCMVAC cleans 1 cache line of 64 bytes!!!! als een woord langer is
									@ als 64 bytes dan moeten er meerdere lines gecleaned worden!!! #UPDATE #ERROR
	#DEBUG
		@bl	varHERE
		@bl	FETCH					@ nu HERE op stack
		@popdts r0					@ r0 nu addres net na het laatste deel van dictionary

		@MCR p15, 0, r0, c7, c11, 1	@ DCCMVAU operation - was eerst DCCMVAC - dit doet maar 1 cache-lijn=64 bytes!!
	
		FlushDataCache				@ werkt! - cleans whole datacache, en werkt dus ook voor lange woorden!!
	#ENDDEBUG	
		dsb							@ werkt - hoeft niet voor pi2!!!!!
		mcr p15, 0, r0, c7, c5, 0	@ =ICIALLU operation, ignoring the value in R0

		dsb
		isb
next		@ checked}

wordhead "cleandcblock", 12, CLEANCACHE, 1 @ ( van-addres tot-addres -- )
CLEANDCBLOCK:	prolog				@ cleans datacache for a block of memory - cleans all{
									@ voor de zekerheid van en tot aligned
		popdts r0					@ r0=tot
		popdts r1					@ r1=van
		@ldv16 r2, 64				@ 64 is lengte cache-line
		
	@ align r1 to closest lower 4byte boundery	
		@sub r1, r1, #1				@ #DEBUG werkt goed zonder sub
        and r1, r1, #~3				@ align to last 4 byte boundary (ie. the closest aligned boundery lower than r1)
        
	1:	MCR p15, 0, r1, c7, c11, 1	@ DCCMVAU operation => write cache line to memory if dirty
		add r1, r1, #64				@ verhoog r1 met 64
		cmp r1, r0					@ einde block al bereikt?
		blo 1b
		
		MCR p15, 0, r1, c7, c11, 1	@ doe nog 1 clean om zeker te zijn dat alles gecleaned is!!
		@DSB @ISH					@ wacht tot alles klaar is
		@ISB
		barrier
next		@ #CHECK!}

wordhead "cleancachecomma", 15, CLEANDCBLOCK, 1 @ ( addres -- )
CLEANCACHECOMMA:					@ cleans cache - mandatory for pi3 after each write of opcode{
		prolog
		popdts r0					@ r0 nu addres van stack

		mcr p15, 0, r0, c7, c11, 1	@ DCCMVAU operation - was eerst DCCMVAC
		isb
		dsb							@ werkt - hoeft niet voor pi2

		mcr p15,0,r0,c7,c5,0		@ =ICIALLU operation, ignoring the value in R0
		isb
		mcr p15, 0, r0, c7, c5, 6	@ Flush Branch Target Cache
		isb
next		@ checked}

wordhead "reboot", 6, CLEANCACHECOMMA, 1 @ underscores tegen foutief gebruik -> en denk aan: ldv32 r4, 0xDEADD0D0
_REBOOT_:							@ functions!! laat RasPi opnieuw starten vanaf kernel image{
		prolog						@ nodig als r4 niet de goede inhoud heeft!
		
		bl	CR
		bl	CR
		prstr goreset
		bl	CR
		bl	CR
		
		@ldv32 r0, 0xDEADD0D0		@ Nu even uitgeschakeld.
		@cmp r4, r0					@ kijk of r4 gelijkis aan r0 -> om foutief resetten te voorkomen
		@bne 2f
		
		ldv32 r0, 0x3F100024		@ watchdog timer
		ldv32 r1, 0x5A000000		@ arm-password (= 5A lsl 24)
		ldv32 r3, 0xFFFF			@ tijd van watchdog timer 0xFFFF = precies 1 sec
		
		orr r2, r1, r3				@ set watchdogtimer tijd in r3
		str r2, [r0]
		
		ldv32 r0, 0x3F10001C		@ start watchdog address
		@orr r2, r1, #0x10
		orr r2, r1, #0x20			@ en wel met met een full reset als uitgelopen = 0x20
		str r2, [r0]
		
		wfi							@ en wacht tot full reset gebeurd....... 'wfe' werkt hier niet
	2:		
next		@ checked}

wordhead "initmmu", 7, _REBOOT_, 1
INITMMU:							@{
		prolog
		bl	DISMMU
		incrmem sivc0init
		bl	INITPAGE				@ setup pagetable
		incrmem sivc0init
		bl	MMUNU					@ set translationtable en enable MMU
		incrmem sivc0init
next		@ checked}

wordhead "dismmu", 6, INITMMU, 1	@ #core0
DISMMU:	prolog						@ dis MMU/caches, clean TLT/caches/branche prediction - inc databarier{

	@   ********* disable MMU and caches ***************************

		MRC p15, 0, r1, c1, c0, 0
		BIC r1, r1, #1				@ disable MMU
		BIC r1, r1, #4096			@ disable I cache
		@BIC r1, r1, #2048			@ disable branche prediction - niet voor PI3
		BIC r1, r1, #4				@ disable D cache
		MCR p15, 0, r1, c1, c0, 0
		isb
		
		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		ResetDataCache

		MOV r0, #0
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Main TLB
		isb
next		@}

wordhead "dismmuc1", 8, DISMMU, 1	@ #core1
DISMMUC1:	prolog					@ dis MMU/caches, clean TLT/caches/branche prediction - inc databarier{

	@   ********* disable MMU and caches ***************************
		MRC p15, 0, r1, c1, c0, 0
		BIC r1, r1, #1				@ disable MMU
		BIC r1, r1, #4096			@ disable I cache
		BIC r1, r1, #4				@ disable D cache
		MCR p15, 0, r1, c1, c0, 0
		isb
		
		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		ResetDataCache

		MOV r0, #0
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Main TLB
		isb
next		@}

wordhead "dismmuc2", 8, DISMMUC1, 1	@ #core2
DISMMUC2:	prolog					@ dis MMU/caches, clean TLT/caches/branche prediction - inc databarier{

	@   ********* disable MMU and caches ***************************

		MRC p15, 0, r1, c1, c0, 0
		BIC r1, r1, #1				@ disable MMU
		BIC r1, r1, #4096			@ disable I cache
		BIC r1, r1, #4				@ disable D cache
		MCR p15, 0, r1, c1, c0, 0
		isb
		
		MCR p15, 0, r0, c7, c5, 0	@ Invalidate Instruction Cache
		isb
		
		MCR p15, 0, r0, c7, c5, 6	@ BPIALL - invalidate all branch-prediction
		isb
		
		ResetDataCache

		MOV r0, #0
		MCR p15, 0, r0, c8, c7, 0	@ Invalidate entire Main TLB
		isb
next		@}

wordhead "mmunu", 5, DISMMUC2, 1	@ feitelijke start MMU en caches en TTRB0 etc voor @core0
MMUNU:	prolog						@{
		mov	r0, #0x0				@ us only TTBR0 (die bedoeld is voor applicaties en switchen van context)
		mcr	p15, 0, r0, c2, c0, 2	@ Write to TTBCR => de TTBCR wordt simpelweg in zijn geheel op 0 gezet
		isb

		ldr r1, =0x406A 			@ 0000 0000 0000 0000 0100 0000 0110 1010 shared inner, write back, alocate on write

		mcr	p15, 0, r1, c2, c0, 0	@ Write to TTBR0[31:0] - #UPDATE: dit is de non-secure versie!!
		isb

@		ldr	r0, =0x55555555			@ maak alle access domains client
		ldr r0, =0xFFFFFFFF			@ maak alle access domains manager #DEBUG: geeft geen verschil
		mcr	p15, 0, r0, c3, c0, 0	@ Write to DACR
		isb

	@  ********************** start caches  ************************

		mrc p15, 0, r0, c1, c0, 0
		ldv32 r1, 0xfffffffd
		and r0, r0, r1				@ maak unaligned acces possible
		orr r0, r0, #4096			@ instruction cache enabled
		orr r0, r0, #4				@ data cache enabled
		mcr p15, 0, r0, c1, c0, 0
		isb

	@  ****************** start MMU  *******************************

		mrc	p15, 0, r0, c1, c0, 0
		orr	r0, r0, #1				@ enables MMU
		mcr	p15, 0, r0, c1, c0, 0
		isb
next		@checked }

wordhead "mmunuc1", 7, MMUNU, 1		@ feitelijke start MMU en caches en TTRB0 etc voor #core1
MMUNUC1: prolog						@{
		mov	r0, #0x0				@ us only TTBR0 (die bedoeld is voor applicaties en switchen van context)
		mcr	p15, 0, r0, c2, c0, 2	@ Write to TTBCR => de TTBCR wordt simpelweg in zijn geheel op 0 gezet
		isb

		ldr r1, =0x406A 			@ 0000 0000 0000 0000 0100 0000 0110 1010 shared inner, write back, alocate on write

		mcr	p15, 0, r1, c2, c0, 0	@ Write to TTBR0[31:0] - #UPDATE: dit is de non-secure versie!!
		
		isb

		ldr	r0, =0x55555555			@ maak alle access domains client
@		ldr r0, =0xFFFFFFFF			@ maak alle access domains manager #DEBUG
		mcr	p15, 0, r0, c3, c0, 0	@ Write to DACR
		isb

	@  ********************** start caches  ************************

		mrc p15, 0, r0, c1, c0, 0
		ldv32 r1, 0xfffffffd
		and r0, r0, r1				@ maak unaligned acces possible
		orr r0, r0, #4096			@ instruction cache enabled
		orr r0, r0, #4				@ data cache enabled
		mcr p15, 0, r0, c1, c0, 0
		isb

	@  ****************** start MMU  *******************************

		mrc	p15, 0, r0, c1, c0, 0
		orr	r0, r0, #1				@ enables MMU
		mcr	p15, 0, r0, c1, c0, 0
		isb
next		@checked }

wordhead "mmunuc2", 7, MMUNUC1, 1	@ feitelijke start MMU en caches en TTRB0 etc voor #core2
MMUNUC2: prolog						@{
		mov	r0, #0x0				@ us only TTBR0 (die bedoeld is voor applicaties en switchen van context)
		mcr	p15, 0, r0, c2, c0, 2	@ Write to TTBCR => de TTBCR wordt simpelweg in zijn geheel op 0 gezet
		isb

		ldr r1, =0x406A 			@ 0000 0000 0000 0000 0100 0000 0110 1010 shared inner, write back, alocate on write

		mcr	p15, 0, r1, c2, c0, 0	@ Write to TTBR0[31:0] - #UPDATE: dit is de non-secure versie!!
		isb

@		ldr	r0, =0x55555555			@ maak alle access domains client
		ldr r0, =0xFFFFFFFF			@ maak alle access domains manager #DEBUG
		mcr	p15, 0, r0, c3, c0, 0	@ Write to DACR
		isb

	@  ********************** start caches  ************************
		mrc p15, 0, r0, c1, c0, 0
		ldv32 r1, 0xfffffffd
		and r0, r0, r1				@ maak unaligned acces possible
		orr r0, r0, #4096			@ instruction cache enabled
		orr r0, r0, #4				@ data cache enabled
		mcr p15, 0, r0, c1, c0, 0
		isb

	@  ****************** start MMU  *******************************
		mrc	p15, 0, r0, c1, c0, 0
		orr	r0, r0, #1				@ enables MMU
		mcr	p15, 0, r0, c1, c0, 0
		isb
next		@checked }
@}

@  ********************** TIMERS en IRQ control ********************{

wordhead "initpixel", 9, MMUNUC2, 1	@ ( -- ) (her)start het pixel irq systeem
INITPIXEL: prolog
		@lit32 1266667				@ deling van 38 miljoen door n geeft freq -> 30Hz is een deler door 1266667 - werkt!
		@lit32 3800000
		@lit32 633333				@ =60 Hz => is net kritisch bij 2 windows, geeft iets flikkeren
		lit32 760000				@ =50 Hz => gaat wel goed bij 2 windows maar op het randje bij 3
		@lit32 380000000
		
		bl	TOLTRELOADVAL
		
		lit16 0						@ Local timer router - 4=naar FIQ van core 0 - 0=IRQ van core 0 - werkt
		@lit16 4
		bl	TOLTROUTER				@ #CORE1: moet 1 worden, werkt nog niet...
		
		bl	_TRUE
		bl	TOLTENABLE
		
		bl	_TRUE
		bl	TOLTIRQEN
next

wordhead "coretimer>", 10, INITPIXEL, 1	@ ( -- l0-timer hi-timer ) geeft de huidige waarde van de core timer
CORETIMERTO: prolog					@ pi3: >700c!!! -> meestal zinloos, physical of virtual timer gebruiken!!!!
		ldv32 r0, 0x4000001C		@ addres bits 31:0 van counter
		ldv32 r1, 0x40000020		@ addres bits 63:32 van counter

		ldr r2, [r0]				@ eerst low bits lezen!
		str top, [dts, #-4]!		@ move top naar stack
		str r2, [dts, #-4]!			@ zet low bits op stack
		ldr top, [r1]				@ zet hi bits in top
next		@ functions!!

wordhead ">ctprsc", 7, CORETIMERTO, 1 @ ( factor -- ) zet prescalefactor van core-timer - 0 stopt de clock, max =2^31
TOCTPRSC: prolog					@ pi3: ?c
		ldv32 r0, 0x40000008		@ addres prescaler coretimer
		popdts r1					@ prescaler factor van coretimer in r1
		str r1, [r0]				@ zet prescaler op het addres
next		@ functions!!

wordhead "ctprsc>", 7, TOCTPRSC, 1	@ ( -- factor ) haalt prescalefactor van core-timer - 0 stopt de clock, max =2^31
CTPRSCTO: prolog					@ pi3: ?c
		str top, [dts, #-4]!		@ move top naar stack
		ldv32 r0, 0x40000008		@ addres prescaler coretimer
		ldr top, [r0]				@ haal prescaler van het addres naar top
next		@ functions!!

wordhead ">ltirqen", 8, CTPRSCTO, 1	@ ( IRQ (true/false) -- ) enables/disables IRQ werking van local timer -> set/clear bit29
TOLTIRQEN: prolog					@ pi3: ?c
		ldv32 r0, 0x40000034		@ addres control and status of localtimer -
		ldv32 r3, 0x20000000		@ is een 1 op bit29
		popdts r1					@ wanted action voor local timer in r1
		ldr r2, [r0]				@ huidige waarde in local timer control register

		cmp r1, #0					@ vergelijk wanted action met 0
		orrne r2, r2, r3			@ set flag als wanted action is not-0
		biceq r2, r2, r3			@ anders clear flag

		str r2, [r0]				@ zet oorspronkelijke waarde met correcte IRQ-flag terug op het addres
next		@ functions!!

wordhead ">ltenable", 9, TOLTIRQEN, 1 @ ( timerenable-flag (true/false) -- ) enable/disables the timer van local timer -> set/clear bit28
TOLTENABLE: prolog					@ pi3: 441-471c
		ldv32 r0, 0x40000034		@ addres control and status of localtimer -
		ldv32 r3, 0x10000000		@ is een 1 op bit28
		popdts r1					@ wanted action voor local timer in r1
		ldr r2, [r0]				@ huidige waarde in local timer control register

		cmp r1, #0					@ vergelijk wanted action met 0
		orrne r2, r2, r3			@ set flag als wanted action is niet 0
		biceq r2, r2, r3			@ anders clear flag

		str r2, [r0]				@ zet oorspronkelijke waarde met correcte timer-enable-flag terug op het addres
next		@ functions!!

wordhead "ltirqflag>", 10, TOLTENABLE, 1 @ ( -- IRQflag (true/flase) ) haal de IRQ-flag van local timer naar stack
LTIRQFLAGTO: prolog					@ pi3: ?c
		str top, [dts, #-4]!
		ldv32 r0, 0x40000034		@ addres control and status of localtimer -
		ldr r1, [r0]				@ control en reload register van local-timer in r1
		ldv32 r2, 0x80000000
		and r1, r1, r2				@ bit 31 is de IRQ-flag

		cmp r1, #0					@ als r1
		mvnne top, #0				@ is niet nul dan zet 'true'-flag op stack
		moveq top, #0				@ anders een 'false' op stack
next		@ check!!

wordhead ">ltreloadval", 12, LTIRQFLAGTO, 1	@ ( reload-value -- ) zet nieuwe reload value in local timer control
TOLTRELOADVAL: prolog				@ pi3: 443c (bij alleen 28 bit schrijven zonder eerst lezen maar 35c!!
		ldv32 r0, 0x40000034		@ addres control and status of localtimer
		ldr r1, [r0]				@ huidige waarde in local timer control register - werkt ook zonder
		ldv32 r3, 0xF0000000		@ is een 1 op bit31:28 - en is sneller
		and r1, r1, r3				@ nu alleen de bovenste 4 bits - etc.

		popdts r2					@ wanted action voor local timer in r2
		ldv32 r3, 0x0FFFFFFF		@ is een 1 op bit27:0
		and r2, r2, r3				@ nu alleen de laagste 28 bits van r2 in r2
		orr r2, r2, r1				@ nu r1 en r2 gecombineerd
		str r2, [r0]				@ zet oorspronkelijke waarde met correcte reload-value terug op het addres
next		@ functions!!

wordhead "ltreloadval>", 12, TOLTRELOADVAL, 1 @ ( -- reload-value ) copieer reload value uit local timer control naar stack
LTRELOADVALTO: prolog				@ pi3: 230-413c (zeer variabel dus...)
		str top, [dts, #-4]!
		ldv32 r0, 0x40000034		@ addres control and status of localtimer
		ldr r1, [r0]				@ huidige waarde in local timer control register
		ldv32 r3, 0x0FFFFFFF
		and top, r1, r3				@ alleen de laagste 28 bits van r1 in top
next		@ functions!!

codehead "ltirqclear", 10, LTRELOADVALTO, 5, 1 @ ( -- ) clear irq van local timer - hierdoor een volgende interrupt mogelijk
LTIRQCLEAR:							@ pi3: 33.2!-31.7c (maw met bl sneller!) clear must be done during each IRQ -> fast enough for multitasking?
		ldv32 v, 0x40000038			@ addres control and status of localtimer
		ldv32 w, 0x80000000
		str w, [v]					@ write 1 to bit 31 to clear interrupt
codenext	@ functions!!

codehead "ltreload", 8, LTIRQCLEAR, 5, 1 @ ( -- ) reload local timer ZONDER een IRQ te genereren!
LTRELOAD:							@ pi3: 30.1c
		ldv32 v, 0x40000038			@ addres control and status of localtimer
		ldv32 w, 0x40000000
		str w, [v]					@ zet op addres - write 1 to bit 30 to reload timer
codenext	@ functions!!

wordhead ">ltrouter", 9, LTRELOAD, 1 @ ( routing-code -- ) determines which core (0-3) gets what interrupt (irq or fiq)
TOLTROUTER: prolog					@ pi3: ?c
		popdts r1					@ routing-code in r1
		ldv32 r0, 0x40000024		@ addres local timer interrupt routing register
		ldv16 r2, 0x7
		and r1, r1, r2				@ alleen bit 2:0 interresant
		str r1, [r0]				@ write routing code to memory-mapped register.
next		@ functions!!

codehead "cntpct>", 7, TOLTROUTER, 0, 1	@ ( -- l0-timer hi-timer ) haal de huidige waarde van de physical counter (=CNTPCT)
CNTPCTTO:							@ pi3: 10!-10c -> inlinen zinloos (17c als word)
		str top, [dts, #-4]!		@ move top naar stack
		mrrc p15, 0, v, top, c14	@ lees Physical counter - CNTPCT
		str v, [dts, #-4]!			@ zet low bits op stack
codenext	@ functions!!

codehead ">cntvct", 7, CNTPCTTO, 0, 1 @ ( -- l0-timer hi-timer ) haal de huidige waarde van de virtual counter (=CNTVCT)
TOCNTVCT:							@ pi3: 10!-10c -> inlinen zinloos (17c als word)
		str top, [dts, #-4]!		@ move top naar stack
		mrrc p15, 1, v, top, c14	@ lees Physical counter - CNTPCT
		str v, [dts, #-4]!			@ zet low bits op stack
codenext	@ functions!!

codehead "cntvcval>", 9, TOCNTVCT, 0, 1	@ ( -- l0-comp_val hi-comp_val ) haal de compare value van de virtual counter (=CNTV_CVAL)
CNTVCVALTO:							@ pi3: 10!-10c -> inlinen zinloos (17c als word)
		str top, [dts, #-4]!		@ move top naar stack
		mrrc p15, 3, v, top, c14	@ lees Virtual counter - CNTV_CVAL
		str v, [dts, #-4]!			@ zet low bits op stack
codenext	@ check!

codehead ">cntvcval", 9, CNTVCVALTO, 0, 1	@ ( l0-comp_val hi-comp_val -- ) schrijf nieuwe compare value van de virtual counter (=CNTV_CVAL)
TOCNTVCVAL:							@ pi3: 12!-12c -> inlinen zinloos

		ldr v, [dts], #4			@ v is pos 2 van stack
		mcrr p15, 3, v, top, c14	@ schrijf compare value virtual counter -> CNTV_CVAL
		ldr top, [dts], #4			@ en herstel top
codenext	@ check!}

@  **************************  Pause words  ************************{
wordhead "wait", 4, TOCNTVCVAL, 1	@ wacht exact 1s (+/- 0.000001s) bij 1200 Mhz RasPi3 - Inlinen zinloos
WAIT:	prolog
		ldr	 r0, =1199999780		@ hihihi yeah!! 1.2 miljard cycles voor 1 sec!!
	1:	subs r0, r0, #1				@ 1 cycle
		bne	 1b						@ zou 4c moeten zijn, is 1c
next		@ checked

wordhead "blink", 5, WAIT, 1		@ wacht 0.1s bij 1200 Mhz RasPi3 - Inlinen zinloos
BLINK:	prolog
		ldv32 r0, 119999780			@ 220 cycles minder dan ideaal
	1:	subs r0, r0, #1
		bne	1b
next		@ checked

wordhead "xmics", 5, BLINK, 1		@ ( n -- ) wait n times 0.000001s - 1200 Mhz Raspi3 - inlining useless
XMICS: prolog
		popdts r1
		cmp r1, #1					@ #NONCRITICAL 
		movlt r1, #1				@ never less than 1 loop, as than very long wait!!
		
	2:	ldv32 r0, 1199				@ #UPDATE - measure exact time!!
	1:	subs r0, r0, #1
		bne	1b
		subs r1, r1, #1
		bne 2b
next		@ checked}

@  *************************  randoroutines  ***********************{
codehead "fastrndm", 8, XMICS, 0, 1 @ ( -- rndm# ) snelle maar onnauwkeurige rando function - inlinen brengt weinig!
FASTRNDM:							@ pi3: 11c - 10c
		str top, [dts, #-4]!		@ save top en maak top vrij voor gebruik
		ldv32 v, fastrndmseed		@ address fastrndmseed in
		ldr top, [v]				@ waarde van fastrndmseed in top
		ldv32 w, 0x91E6D6A5

		mla top, w, top, w			@ hierna w vrij te gebruiken, v is nieuwe seed
		ldv16 w, 0x100				@ w=range =0-0x100-1
		str top, [v]				@ vernieuw seed

		umull v, top, top, w		@ hierna top het nieuwe randgetal
codenext	@ checked

wordhead "rndm", 4, FASTRNDM, 1		@ zet een nummer tussen 0 en 255 op de stack - correct verdeeld
RNDM:	prolog						@ 60c (was 139c pfff) 48 moet mogelijk zijn...
									@ pi3:was 35, nu 33 (pi2:36c) inc bl en inline drop
		str top,[dts, #-4]!
		ldv32 top, varMW+16
		ldr r0, [top]				@ r0 nu waarde varMW

		str top,[dts, #-4]!
		ldv32 top, varMZ+16
		ldr r1, [top]				@ r1 nu waarde varMZ

		ldv16 w, 65535
		ldv16 r4, 36969

		and r3, r1, w
		mov r2, r1, lsr #16
		mul	r1, r3, r4
		add r1, r1, r2
		str r1, [top]				@ top is nog steeds varMZ

		mov r2, r0, lsr #16
		ldv16 r4, 18000
		and r3, r0, w
		ldr top, [dts], #4			@ top is nu weer addres varMW
									@ ldr top niet direct voor str r0, [top] spaart 3 cycli!!
		mul r0, r3, r4
		mov r4, r1, lsl #16			@ verplaatst, spaart 1 cycle
		add r0, r0, r2
		str r0, [top]				@ zet r0 op address in top

		add r4, r4, r0
		mov r4, r4, lsr #24
		mov top, r4					@ nu stack weer correct
next		@ checked

wordhead "initrndm", 8, RNDM, 1		@ seed de rando nummers, uiteindelijk uit TRNG
INITRNDM:
		prolog
		lit32 37
		bl	varMW
		bl	STORE
		lit32 124173
		bl	varMZ
		bl	STORE
next		@ checked

wordhead "inittrng", 8, INITRNDM, 1	@ INIT a true random# generator
INITTRNG:
		prolog
									@ start cycli veranderen doet niets
		ldv32 r0, 0x3F104000		@ RNG enable reg
		ldr r1, =0x1
		str r1, [r0]				@ RNG enabled

		ldv32 r0, 0x3F104010		@ RNG Interrupt control reg
		ldr r1, =0x1
		str r1, [r0]				@ RNG interrupt disabled
next

codehead "trng", 4, INITTRNG, 4, 1	@ ( -- rndm# ) VERY SLOW! 406 cycli!!
TRNG:	@ldv32 v, 0x3F104004		@ bit 31:24 zijn fifo info -> aantal getallen in fifo
	@1:	ldr w, [v]					@ haal aantal # in fifo trng buffer
		@lsr w, w, #24				@ shift 24 bit naar rechts om getal te krijgen
		@cmp w, #0x0
		@beq	1b					@ haal alleen als minstens 1 getal in fifo

		@ werkt prima zonder fifo check -> als 0 terug dan niet valide werkt net zo goed!!

		str top, [dts, #-4]!		@ save top op datastack
		ldv32 v, 0x3F104008
		ldr top, [v]				@ en maak top gelijk aan rando getal
codenext	@ checked}

@  ********************* stack printing and DEPTH  *****************{
codehead "depth", 5, TRNG, 6, 1		@ pop de diepte van de datastack op de datastack voordat de diepte zelf word gepusht
DEPTH:								@ van nature core-aware? (Nee!, DatSt is core0!)
		mov w, dts					@ stand dts voor toevoegen van de waarde van DEPTH
		str top, [dts, #-4]!
		ldv32 v, DatSt				@ Dit werkt ook in TaskTable environment!!
		sub top, v, w				@ v (= DatSt) is altijd groter of gelijk aan dts
		asr top, top, #2			@ diepte in cells, not in bytes
codenext	@ checked

codehead "lrdepth", 7, DEPTH, 0, 1	@ pop de diepte van de returnstack op de datastack
LRDEPTH:							@ #core0 - inlinen lijkt zinloos anders 5 lang
		str top, [dts, #-4]!
		ldv32 v, RetSt				@ geen ldr v, =RetSt -> slecht inlinebaar!!
		sub top, v, sp				@ v (= RetSt) is altijd groter of gelijk aan sp
		asr top, top, #2
codenext @ #CHECK

codehead "usdepth", 7, LRDEPTH, 0, 1 @ pop de diepte van de user-stack (uss) op de datastack
USDEPTH:							@ #core0
		str top, [dts, #-4]!		@ push top on stack
		ldv32 v, UseSt				@ UseSt=core0
		sub top, v, uss				@ v (= UseSt) is altijd groter of gelijk aan uss
		asr top, top, #2			@ diepte in cells, not in bytes
codenext

wordhead ".s", 2, USDEPTH, 1		@ ( -- )
PRINTS:	prolog
		@bl		CR					@ te printen zijn top plus de rest van de stack
		prstr stack
		bl		DEPTH				@ top is nu de diepte
		popdts	r0					@ r0 is nu de diepte, top is nu weer in begin stand
		cmp		r0, #0				@ als depth=nul
		beq		1f					@ dan print EMPTY en return

		lit16	10
		bl		TAB
		bl		DUP					@ eerst top hanteren -> top op stack
		bl		DUP
		bl		OUTNUM				@ en print de top, stack is weer onveranderd
		lit16	18
		bl		TAB
		prchar 'x'
		bl		OUTHEX
		bl		CR
		cmp		r0, #1				@ als diepte is 1 dan nu klaar
		beq		2f
									@ Als hier dan de stack nog te printen
		mov		r2, dts				@ r2 = address van het eerst te printen getal
	3:	subs	r0, r0, #1			@ heb al top=1 geprint, dus de diepte nu al met een verlagen
		beq		2f					@ als nul dan klaar
		ldr		r1, [r2], #4		@ haal de waarde van [r2], en pas r2 daarna aan
		pushdts	r1
		bl		DUP
		lit16	10
		bl		TAB
		bl		OUTNUM				@ hierna de stack weer oorspronkelijk
		lit16	18
		bl		TAB
		prchar 'x'
		bl		OUTHEX
		bl		CR
		b		3b

	1:	prstr stackempty
	2:
next		@ checked}

@  **********************  Classic Maths n Logic  ******************{
codehead "ror", 3, PRINTS, 2, 1		@ ( n m -- ror(n) met m bits )
_ROR:								@ pi3: 6-3c
		ldr w, [dts], #4			@ haal getal van 1 onder de top en dts een naar beneden
		mov	top, w, ror top			@ en rotate met het aantal bits van de top
codenext

codehead "rol", 3, _ROR, 3, 1		@ ( n m -- rol(n) met m bits )
_ROL:								@ pi3: 7-3c
		ldr w, [dts], #4			@ haal getal van 1 onder de top en dts een naar beneden
		rsb top, top, #32			@ top=32-top -> omdat er geen rol in de ARM zit
		mov	top, w, ror top			@ en rotate met het aantal bits van de top
codenext

codehead "rshift", 6, _ROL, 2, 1	@ ( m n -- o ) logically shift m met n bits right
RSHIFT:
		ldr w, [dts], #4			@ haal getal van 1 onder de top en dts een naar beneden
		mov	top, w, lsr top
codenext	@ checked

codehead "lshift", 6, RSHIFT, 2, 1	@ ( m n -- n) logically shift m met n bits left
LSHIFT:
		ldr w, [dts], #4			@ haal getal van 1 onder de top
		mov	top, w, lsl top
codenext	@ checked

codehead "2*", 2, LSHIFT 1, 1		@ ( n -- n*2 ) logically shift m met 1 bit left=arithmetic bij 2-complement
TWOSTAR:
		lsl top, top, #1			@ shift logically left is equivalent met asl voor 2 complement
codenext	@ CHECK

codehead "2/", 2, TWOSTAR, 1, 1		@ ( n -- n/2 ) arithmetic shift right m met 1 bit
TWOSLASH:
		asr	top, top, #1			@ arithmetic shift right top
codenext	@ CHECK

codehead "or", 2, TWOSLASH, 2, 1 	@ ( m n -- m OR n )
_OR:	ldr w, [dts], #4			@ haal getal van 1 onder de top
		orr	top, top, w
codenext	@ checked

codehead "and", 3, _OR, 2, 1 		@ ( m n -- m AND n )
_AND:	ldr w, [dts], #4			@ haal getal van 1 onder de top
		and	top, top, w
codenext	@ checked

codehead "xor", 3, _AND, 2, 1		@ ( m n -- m EOR n )
_XOR:	ldr w, [dts], #4			@ haal getal van 1 onder de top
		eor	top, top, w
codenext

codehead "nor", 3, _XOR, 3, 1		@ ( m n -- m NOR n )
_NOR:	ldr w, [dts], #4			@ haal getal van 1 onder de top
		orr	top, top, w
		mvn top, top
codenext	@ why not ORN zonder de mvn????? #ERROR

codehead "nand", 4, _NOR, 2, 1		@ ( m n -- m NAND n )
_NAND:	ldr w, [dts], #4			@ haal getal van 1 onder de top
		bic	top, top, w
codenext	@ CHECKEN!!

codehead "abs", 3, _NAND, 2, 1		@ ( n -- abs(n) )
_ABS:	movs w, top
		rsblt top, w, #0
codenext	@ checked

codehead "not", 3, _ABS, 1, 1		@ ( m -- NOT m ) ANSI: "invert"
_NOT:
		mvn top, top				@ logical invert every bit
codenext	@ werkt - toggles flag van "false" naar "true" en vv}

@  **************************  Microsecs  **************************{
codehead "mcs", 3, _NOT, 5, 1 		@ ( -- mcs ) wordhead is 4-8c langzamer (80c ipv 72-76cc inlined)
MCS:	str	top, [dts, #-4]!
		ldv32 v, ARM_TIMER_CNT		@ ldr v, =ARM_TIMER_CNT crasht bij inlinen!
		ldr	top, [v]
		asr top, top, #1			@ divide by 2 nodig bij 400Mhz core freq
codenext		@ checked

wordhead "initmcs", 7, MCS, 1		@ ( -- )
INITMCS:
		prolog
		ldv32 r0, ARM_TIMER_CTL
		@ldv32 r1, 0x00F90202		@ F9=249+1 is de deel factor, bit 9 op 1 enables de free running counter

		ldv32 r1, 0x00C70202		@ C7=199+1 is de deel factor, bit 9 op 1 enables de free running counter

		str	r1, [r0]
next		@ checked - 249+1 bij 250Mhz 199+1 bij 400Mhz}

@  **************************  Space TAB  **************************{
wordhead "space", 5, INITMCS, 1 	@ ( -- ) #NAME 'bl' is sympatieker
SPACE:	prolog
		lit16 32
		BL	EMIT
next		@ checked

wordhead "spaces", 6, SPACE, 1		@ ( n -- )
SPACES: prolog
		mov r1, top
		ldr top, [dts], #4			@ update top
		cmp r1, #0
		ble	1f						@ als negatief of nul dan klaar
	2:	bl	SPACE
		sub r1, r1, #1
		cmp r1, #0
		bne	2b
	1:
next		@ checked

wordhead "tab", 3, SPACES, 1		@ ( positie -- ) #UPDATE - moet nog window-aware!
TAB:	prolog

		
		bl	varOUT
		bl	FETCH
		bl	_SUB
		bl	SPACES					@ gebruik spaties om de leegte tot de tab op te vullen
next

wordhead "pad", 3, TAB, 1			@ UNDERSCORE laten!! Pad is ALLEEN voor de gebruiker!!!
_PAD:	prolog						@ ( -- address van core-correct pad )
									@ _PAD is core-aware zodat iedere core altijd address van eigen pad krijgt
		bl	COREID					@ _PAD moet task-aware worden, zodat iedere task altijd een eigen pad krijgt
		popdts r0					@ r0 nu id-no van core waarop dit loopt

		cmp r0, #0x1
		bne 1f
		lit32 PadC1
		b 4f

	1:	cmp r0, #0x2
		bne 2f
		lit32 PadC2
		b 4f

	2:	cmp r0, #0x3
		bne 3f						@ spring naar 3f als niet core 1-3
		lit32 PadC3
		b 4f

	3:	lit32 Pad					@ als niet core 1-3 dan core 0.
	4:
next		@ CHECK!!}

@  *****************************  UART  ****************************{
wordhead "inituart", 8, _PAD, 1
INITUART:
		prolog
		mov r1, #1					@ Enable UART in AUX_ENABLES
		ldr r0, =AUX_ENABLES
		str r1, [r0]
		mov r1, #0					@ disable interrupts in AUX_MU_IER_REG
		ldr r0, =AUX_MU_IER_REG
		str r1, [r0]
		mov r1, #0					@ disable transmit/receive in AUX_MU_CNTL_REG
		ldr r0, =AUX_MU_CNTL_REG
		str r1, [r0]
		mov r1, #3					@ set 8 bits communication in AUX_MU_LCR_REG
		ldr r0, =AUX_MU_LCR_REG
		str r1, [r0]
		@mov r1, #0					@ set the RTS line high in AUX_MU_LCR_REG
		@ldr r0, =AUX_MU_MCR_REG	@ is onnodig als de zender dit niet gebruikt!
		@str r1, [r0]
		mov r1, #6					@ clear the fifo buffers - 6 werkt!
		@mov r1, #198 				@ clear the input and output buffers in AUX_MU_IIR_REG
		ldr r0, =AUX_MU_IIR_REG
		str r1, [r0]

		@ldv32 r1, 271				@ if core-clock is 250 Mhz
		ldv32 r1, 434				@ if core-clock is 400 Mhz functions!!
		@ldv32 r1, 488				@ if core-clock is 450 Mhz
		@ldv32 r1, 541				@ if core-clock is 500 Mhz
		ldr r0, =AUX_MU_BAUD_REG
		str r1, [r0]

		@ ***************************
		ldr r0, =GPFSEL1			@ Set GPIO line 14 for transmission (TXD) in GPFSEL1

		ldr r1, [r0]				@ #CHECK: de GPIO worden gezet, maar nooit gereset bij 
		bic r3, r1, #28672			@ een herstart. Dit lijkt niet goed
		orr r3, r3, #8192
		str r3, [r0]

		ldr r1, [r0]				@ Set GPIO line 15 for receiving (RXD) ook in GPFSEL1
		bic r3, r1, #229376
		orr r3, r3, #65536
		str r3, [r0]

		mov r1, #0					@ disable GPIO pull-up/down in GPPUD
		ldr r0, =GPPUD
		str r1, [r0]
		mov r1, #0					@ wait for 8192 cycles
	1:	add r1, r1, #1
		cmp r1, #8192
		bne 1b
		mov r1, #16384  @ 1<<14		@ Assert clock lines (14 & 15) in GPPUDCLK0
		ldr r0, =GPPUDCLK0
		str r1, [r0]
		mov r1, #32768 @ 1<<15
		str r1, [r0]
		mov r1, #0					@ wait for 8192 (was 150) cycles
	2:	add r1, r1, #1
		cmp r1, #8192
		bne 2b
		mov r1, #0					@ clear clock lines in GPCLR0
		ldr r0, =GPCLR0
		str r1, [r0]
		mov r1, #3					@ enable bits 0 and 1 in AUX_MU_CNTL_REG
		ldr r0, =AUX_MU_CNTL_REG	@ en enable daarmee zend en receive
		str r1, [r0]
next		@ }

@  *****************************  SPI  *****************************{
wordhead "initspi", 7, INITUART, 1	@ ( -- )
INITSPI:
		prolog

		ldr r0, =AUX_ENABLES
		ldr r2, [r0]
		orr r2, r2, #2				@ Enable spi0 in AUX_ENABLES
		str r2, [r0]

		lit16 4						@ =alt func 0!
		lit16 7						@ voor gpio7
		bl SETFUNCGPIO

		lit16 4						@ =alt func 0!
		lit16 8						@ voor gpio8
		bl SETFUNCGPIO

		lit16 4						@ =alt func 0!
		lit16 9						@ voor gpio9
		bl SETFUNCGPIO

		lit16 4						@ =alt func 0!
		lit16 10					@ voor gpio10
		bl SETFUNCGPIO

		lit16 4						@ =alt func 0!
		lit16 11					@ voor gpio11
		bl SETFUNCGPIO

		ldr r0, =AUX_SPI0_CS
		mov r1, #0x30				@ zet alles op reset-waards en clear de fifo buffers voor trx and rcv
		str r1, [r0]				@ oa geen DMA, polling-mode, geen interrupts set, etc. etc.

		ldr r0, =AUX_SPI0_CLK
		mov r1, #0x0				@ 0x0 geeft -> divider = 2^16 Dit is wel èrrug langzaam!!
		str r1, [r0]
next

wordhead "word>spi", 8, INITSPI, 1	@ ( data-in -- data-out )
WORDTOSPI:
		prolog

		ldv32 r0, 0x004000B0
		ldv32 r1, AUX_SPI0_CS
		str r0, [r1]				@ cs spi0 active -> TA=1

		ldv32 r2, 1<<18
	1:	ldr r0, [r1]				@ check status bit 18 van AUX_SPI0_CS
		ands r0, r2
		beq 1b

		ldv32 r3, AUX_SPI0_FIFO

		uxtb r2, top, ror #24		@ extract byte van bit[31:24] uit top in r2
		str r2, [r3]
		uxtb r2, top, ror #16		@ extract byte van bit[23:16] uit top in r2
		str r2, [r3]
		uxtb r2, top, ror #8		@ extract byte van bit[15:8] uit top in r2
		str r2, [r3]
		uxtb r2, top, ror #0		@ extract byte van bit[7:0] uit top in r2
		str r2, [r3]

		ldv32 r2, 1<<16

	2:	ldr r0, [r1]				@ wacht tot bit 16 1 is
		and r0, r0, r2
		cmp r0, #0
		beq 2b


		ldv32 r2, 1<<17
	3:	ldr r0, [r1]				@ wacht tot bit 17 1 is
		and r0, r0, r2
		cmp r0, #0
		beq 3b

		ldr r0, [r3]				@ en haal een byte uit de fifo
		bfi top, r0, #24, #8

		ldv32 r2, 1<<17
	4:	ldr r0, [r1]				@ wacht tot bit 17 1 is
		and r0, r0, r2
		cmp r0, #0
		beq 4b

    	ldr r0, [r3]				@ en haal een byte uit de fifo
		bfi top, r0, #16, #8

		ldv32 r2, 1<<17
	5:	ldr r0, [r1]				@ wacht tot bit 17 1 is
		and r0, r0, r2
		cmp r0, #0
		beq 5b

    	ldr r0, [r3]				@ en haal een byte uit de fifo
		bfi top, r0, #8, #8

		ldv32 r2, 1<<17
	6:	ldr r0, [r1]				@ wacht tot bit 17 1 is
		and r0, r0, r2
		cmp r0, #0
		beq 6b

    	ldr r0, [r3]				@ en haal een byte uit de fifo
		bfi top, r0, #0, #8

		ldv32 r0, 0x00400000
		str r0, [r1]				@ cs spi0 non-active -> TA=0
next		@ #CHECK fully untested}

@  *****************************  WAVE  ****************************{
wordhead "initwave", 8, WORDTOSPI, 1 @ ( -- ) initialises PWM for sound generation - now follows book
INITWAVE: prolog					@{
	@ Set phone-jack (GPIO 40 & 45) to PWM function = alternate function 0
		ldv32 r0, GPFSEL4			@ = bank 4 of alternate function selection for GPIO
		ldv32 r1, 0x20004			@ select alt func 0 for GPIO 40 & 45 (phone-jack connected to 40&45)
		str r1, [r0]				@ #UPDATE: tbd via alt func WORD!!
		
		incrmem wvcounter
		
	@ stop PWM-clock
		ldv32 r0, CM_PWMCTL
		ldv32 r1, 0x5A000020		@ set kill-bit PWMCTL (=bit 5 from CM_PWMCTL)
		str r1, [r0]				@ 
		
		incrmem wvcounter
	
	@ wait till PWM-clock is stopped => busy-bit=0
	1:	@incrmem wvcounter
		ldr r1, [r0]				@ load CM_PWMCTL to check busy-bit = bit 7
		ands r1, r1, #128			@ isolate bit 7
		cmp r1, #0					@ bit 7 = busy bit still set? (bit 7 geset betekend dat oscillator is still active)
		bne 1b						@ wait till busy bit clears	

		incrmem wvcounter
		
	@ set clock-divider for PWM-freq - (must be below 25Mhz! => 'sampling' multiplied with 'range') 
		ldv32 r0, CM_PWMDIV
		ldv32 r1, 0x5A035257		@ deelfactor 53.146 geeft 22050 bij 1.2 Ghz en 10bits range #checked!!
		str r1, [r0]

		incrmem wvcounter		

	@ set clock-source and MASH level: here 650Mhz PLLC and MASH=2 - do NOT enable in same step!!! #ERROR
		ldv32 r0, CM_PWMCTL
		ldv32 r1, 0x5A000405		@ MASH-level (bit 10:9)=2, use ??2400??Mhz PLLD (=6)  clock as oscillator - 5 aan einde is clock_id (=PLLC)
		str r1, [r0]
		
		incrmem wvcounter

	@ start PWM-clock
		ldv32 r0, CM_PWMCTL
		ldv32 r1, 0x5A000415		@ set enable-bit PWMCTL (=bit 4 from CM_PWMCTL)
		str r1, [r0]
		
		incrmem wvcounter
		
	@ wait till PWM-clock has started => busy-bit=1
	2:	ldr r1, [r0]				@ load CM_PWMCTL to check busy-bit = bit 7
		and r1, r1, #128			@ isolate bit 7
		cmp r1, #128				@ bit 7 = busy bit set? (bit 7 geset betekend dat oscillator is still active)
		bne 2b						@ wait till busy bit sets
		
		incrmem wvcounter

	@ initiate PWM to stereo with 10 bit sampling @ 22.05kHz
		ldv32 r1, 1024				@ =10 bit depth - to be tested with 9 bits and 44.1kHz to find optimal sound
		ldv32 r0, PWM_RNG1			@ channel 1
		str r1, [r0]
		ldv32 r0, PWM_RNG2			@ channel 2
		str r1, [r0]
		
		incrmem wvcounter
		
		ldv32 r1, 0x2161			@ enable ch 1&2, use fifo for ch 1&2, clear fifo
		ldv32 r0, PWM_CTL			@ 0x0010 0001 0110 0001 = ok for now: but silence bits & continue should be considered
		str r1, [r0]
next		@}

codehead "noise8910", 9, INITWAVE, 0, 1 @ ( addr-var -- 0/1 ) random generator voor 8910 sound generator
NOISE8910:							@ pi3: ~11 - 6-10 geen inlinen nodig, scheelt maar 1c in reallife{
		pushuss r4					@ save r4
		ldr v, [top]				@ de variabele 'noise' moet voor 'noise8910' gezet worden in de sourcecode
		mov r4, top					@ onthoud address in r4
		and w, v, #8				@ w nu bit 3
		and top, v, #1				@ top nu bit 0
		eors top, top, w, lsr #3	@ dit is dus een eor van beide bits op pos 0
		orreq v, v, #1<<16
		lsr v, v, #1
		str v, [r4]
		popuss r4					@ herstel r4
codenext	@ checked - met meerdere variabelen zijn meerdere noise-streams mogelijk}
@}

@  ****************************  PIXEL  ****************************{

wordhead "initscreen", 10, NOISE8910, 1 @ set mn buffers en wat data van win0 - eenmalig bij starten systeem
INITSCREEN:							@{
		prolog
		ldv32 r0, WinTable			@ base-addres Windows definitions table - win0

		ldv16 r2, 1024				@ zo laten tot PIXEL ook aangepast is - die is nog op 1024 geaseerd
		ldv16 r1, pxmaxx
		str r2, [r0, r1]			@ pxmaxx van win0 - kan tot 1280 zijn

		ldv16 r3, 768
		ldv16 r1, pxmaxy
		str r3, [r0, r1]			@ pxmaxy van win0 - kan tot 1024 zijn

		mul r2, r2, r3				@ r2 nu maxx*maxy: is size buffer in pixels - 4 bytes per pixel! #32
		lsl r2, r2, #2				@ r2 nu size buffer in bytes - de buffer is groot genoeg voor de maxsize

		ldv16 r1, pxsizebuf
		str r2, [r0, r1]			@ zet size buffer in bytes in pxsizebuf
		ldv16 r1, pxsizemask
		str r2, [r0, r1]			@ zet size maskbuffer in bytes in pxsizemask - heeft zelfde maat als buffer

		pushdts r2					@ allocate memory winbuffer for win0 in initscreen -> INITSCREEN maar een keer gebruiken
		bl	ALLOCATE				@ reserveer winbuffer in bytes in MEMORY ( needed_memory_space -- winbuffer_address )
		popdts r3					@ r3 nu het address van buffer (gemaakt door ALLOCATE)
		ldv16 r1, pxbuffer
		str r3, [r0, r1]			@ zet base-address buffer in pxbuffer van win0

		pushdts r2
		bl	ALLOCATE				@ reserveer buffer in bytes in MEMORY ( needed_memory_space -- maskbuffer_address )
		popdts	r3					@ pointer to base-address van mask
		ldv16 r1, pxmask
		str r3, [r0, r1]			@ r3 nu het address van maskbuffer (gemaakt door ALLOCATE)
next		@}

wordhead "setwin0", 7, INITSCREEN, 1 @ ( x y -- ) zet zowel win0 als het scherm naar nieuwe resolutie - nooit groter dan maxx en maxy
SETWINZERO:							@ dit kan nog niet zo vaak als gewenst{
		prolog						@ set GPU en win0 op x y, de max van win0 is in INITSCREEN gezet
									@ geheugen voor buffers eenmalig in initscreen!

		bl	TWODUP					@ ( x y -- x y x y )

		ldv32 r0, WinTable			@ r0 is base-addres WinTable

		popdts r2
		ldv16 r1, pxsizey
		str r2, [r0, r1]			@ pxsizey van win0 nu set

		popdts r2					@ ( -- x y )
		ldv16 r1, pxsizex
		str r2, [r0, r1]			@ pxsizex van win0 nu set

		bl	SETGPU					@ ( x y -- ) setup van GPU met 32 bit depth; en vult varSCRPITCH, varSCRSIZE en varSCRBASE
									@ SETGPU kan kennelijk maar een keer??? Caching probleem? Weet nog niet...
		getvalvar r2, varSCRPITCH
		ldv16 r1, pxpitch
		str r2, [r0, r1]			@ zet pitch van win0

		getvalvar r2, varINK
		ldv16 r1, pxink
		str r2, [r0, r1]			@ set ink van win0 gelijk aan varINK

		getvalvar r2, varCANVAS
		ldv16 r1, pxcanvas
		str r2, [r0, r1]			@ set canvas van win0 gelijk aan varCANVAS

		ldv16 r2, 0

		ldv16 r1, pxrand
		str r2, [r0, r1]			@ rand van win0 is standaard 0 breed
		ldv16 r1, pxcurx
		str r2, [r0, r1]			@ clear pxcurx van win0
		ldv16 r1, pxcury
		str r2, [r0, r1]			@ clear pxcury van win0
		ldv16 r1, pxorigx
		str r2, [r0, r1]			@ clear pxorigx van win0
		ldv16 r1, pxorigy
		str r2, [r0, r1]			@ clear pxorigy van win0
		ldv16 r1, pxflags
		str r2, [r0, r1]			@ clear pxflags van win0
		
		ldv32 r2, MyNeoForth		@ default font voor een window
		ldv16 r1, pxfont
		str r2, [r0, r1]

		ldv16 r2, 2
		ldv16 r1, pxspacingx		@ default spacing voor een char in x richting - kan reeel 0-3 zijn, max tot breedte window
		str r2, [r0, r1]
		ldv16 r1, pxspacingy		@ default spacing voor een char in y richting - zie boven
		str r2, [r0, r1]
		
		@ nu zeker stellen dat task0 op win@ print - een beetje oneigenlijke plek
		ldv32 r0, TASK_BLOCK		@ r0 is base-addres TASK_BLOCK
		
		ldv16 r2, 0
		ldv16 r1, TaskWinNo			@ task0 print default op win0 - #ERROR - moet bij creeren van nieuwe task, niet bij win0
		str r2, [r0, r1]
		
		ldv16 r1, TaskUARTen		@ task0 print default niet op UART - #ERROR moet niet hier maar bij nieuwe task
		str r2, [r0, r1]
		@ einde task0 win0 deel

		@bl	CLEARSCREEN				@ moet er een clearscreen zijn na setwin0?? Ja!, maar werkt niet!!

		lit16 0
		bl	WINWRITEON				@ ( 0=win# -- )
		
		lit16 0
		bl	CLEARWIN
next		@}

wordhead "makewin", 7, SETWINZERO, 1 @ ( maxx maxy win# -- ) definieert nieuw window in tabel, en zet buffer pointers
MAKEWIN: prologmax					@ niet voor win0 gebruiken want die kan hopelijk ooit herhaalt worden, en deze routine nooit niet!{
									@ moet nog zo dat er een volgende window gemaakt wordt! nu nog erg primitief!
									@ nieuw window uiteindelijk definieren via FORTH mbv deze primitieve en create-does
									@ ink en canvas worden van de algemene canvas en inkt overgenomen

	@ check nummer nieuwe window tegen het maximum
		mov r2, top					@ r2 nu nummer nieuw window en nog steeds -> ( maxx maxy win# -- )
		ldv16 r3, max_win			@ r3 nu maximaal aantal windows

		cmp r3, r2					@ vergelijk max_win met win#
		bls 2f						@ als max_win gelijk aan of kleiner dan nieuw window -> fout en dus klaar

		bl	MINROT					@ ( win# maxx maxy -- )

	@ *** maak correcte offset naar regel in WinTable
		pushdts r2					@ r2=win#
		bl	WINADRTO
		popdts r0					@ r0 = base-address WinTable - KEEP
		
	@ *** get pxmaxy en store in WinTable - bij nieuw window zowel voor maxy, voor sizey en als basis voor pxcharx
		popdts r2					@ r2 nu maxy ( win# maxx -- )
		ldv16 r1, pxmaxy
		str r2, [r0, r1]
		ldv16 r1, pxsizey
		str r2, [r0, r1]

		ldv16 r4, 20				@ #UPDATE dit is een vaste waarde die eigenlijk variabel moet zijn
		udiv r4, r2, r4
		ldv16 r1, pxchary
		str r4, [r0, r1]			@ sla het aantal text-regels op in pxchary

	@ *** get pxmaxx en store in WinTable - stack nu clear
		popdts r3					@ r3 nu maxx ( win# -- )
		ldv16 r1, pxmaxx
		str r3, [r0, r1]
		ldv16 r1, pxsizex
		str r3, [r0, r1]

		ldv16 r4, 10				@ #UPDATE dit is een vaste waarde die eigenlijk variabel moet zijn
		udiv r4, r3, r4
		ldv16 r1, pxcharx
		str r4, [r0, r1]			@ sla het aantal chars op een text-regel op in pxcharx

	@ *** calc pxsizebuf voor nieuw window en zet in WinTable
		mul r2, r2, r3				@ r2 nu maxx*maxy: is size buffer in pixels
		lsl r2, r2, #2				@ r2 nu size buffer in bytes	#UPDATE checken of dit wel genoeg is!! Buffer

		ldv16 r1, pxsizebuf
		str r2, [r0, r1]			@ zet size buffer in bytes in pxsizemask
		ldv16 r1, pxsizemask
		str r2, [r0, r1]			@ zet size maskbuffer in bytes in pxsizemask

	@ *** reserveer geheugen voor buffer en mask-buffer voor nieuwe window en zet beide in WinTable
		add r2, r2, #512			@ maak een buffertje van 512 bytes #DEBUG dit buffertje
		@pushdts r2					@ #DEBUG
		@bl	ALLOCATE				@ reserveer mask-buffer in bytes in MEMORY - in RAM

		pushdts r2
		bl	ALLOCATE				@ reserveer buffer in bytes in MEMORY - in RAM

		@popdts r2					@ pointer to mask-buffer
		@ldv16 r1, pxmask
		@str r2, [r0, r1]			@ zet r2 op r0, indexed met r1

		popdts	r2					@ r2 nu base-address buffer
		ldv16 r1, pxbuffer
		str r2, [r0, r1]			@ zet r2 op r0, indexed met r1

	@ bereken pitch window en zet in WinTable - zo kan de window gelijk gebruikt worden
		lsl r2, r3, #2				@ r2 nu pixel per regel keer 4 maakt pitch - r3 was nog in tact
		ldv16 r1, pxpitch
		str r2, [r0, r1]			@ pitch win# gezet in WinTable

	@ zet spul op nul
		ldv16 r2, 0

		ldv16 r1, pxrand
		str r2, [r0, r1]			@ rand van nieuw window is 0 breed
		ldv16 r1, pxcurx
		str r2, [r0, r1]			@ clear pxcurx van nieuw window
		ldv16 r1, pxcury
		str r2, [r0, r1]			@ clear pxcury van nieuw window
		ldv16 r1, pxorigx
		str r2, [r0, r1]			@ clear pxorigx van nieuw window
		ldv16 r1, pxorigy
		str r2, [r0, r1]			@ clear pxorigy van nieuw window
		ldv16 r1, pxflags
		str r2, [r0, r1]			@ clear pxflags van nieuw window

		ldv32 r2, MyNeoForth		@ default font voor een window
		ldv16 r1, pxfont
		str r2, [r0, r1]

		ldv16 r2, 2
		ldv16 r1, pxspacingx		@ default spacing voor een char in x richting - kan reeel 0-3 zijn, max tot breedte window
		str r2, [r0, r1]
		ldv16 r1, pxspacingy		@ default spacing voor een char in y richting - zie boven
		str r2, [r0, r1]

	@ zet inkt en canvas uit vars in tabel
		getvalvar r2, varINK
		ldv16 r1, pxink
		str r2, [r0, r1]			@ set ink

		getvalvar r2, varCANVAS
		ldv16 r1, pxcanvas
		str r2, [r0, r1]			@ set canvas

	@ hoog aantal op - maar alleen als het een nieuw en hoger nummer is! #UPDATE #DEBUG
		bl	varWINNO
		bl	ONEPLUSSTORE			@ verhoog win# met 1

	@ en update writeon pre-calculated variables!
		bl	WINWRITEON				@ win# stond nog op stack

		b 1f						@ en klaar!!

	2:	ldr top,[dts, #8]			@ clear stack bij fout
		add dts, dts, #12			@ =3drop					
	1:
nextmax		@}

wordhead "winadr>", 7, MAKEWIN, 1	@ ( win# -- addres of correct regel in WinTable for win# )
WINADRTO:	prolog					@{
		ldv32 r0, WinTable
		ldv16 r1, bytes_win			@ r1 nu aantal bytes per regel in WinTable
		mul r2, top, r1				@ r2 nu top(=win#)*aantal bytes per regel - biv bij win#=1 nu 104 in r2
		add top, r0, r2				@ top nu addres van correcte regel in WinTable
next		@ ok}

wordhead "winsize>", 8, WINADRTO, 1	@ ( addres van regel van window in WinTable -- sizex-win, sizey-win )
WINSIZETO:	prolog					@{
		mov r0, top
		ldv16 r1, pxsizex
		ldr r2, [r0, r1]
		mov top, r2					@ ( -- sizex-win )
		ldv16 r1, pxsizey
		ldr r2, [r0, r1]
		pushdts r2					@ ( -- sizex-win sizey-win )
next		@ check!}

wordhead "winvisible", 10, WINSIZETO, 1 @ ( addres-regel-win, sizex-win, sizey-win -- xmin xmax ymin ymax )
WINVISIBLE: prolog					@ als window niet visible, dan 0, 0, 0, 0 terug - de laatste nul is genoeg voor een check!{
		bl	TO_R					@ zet y-waarde even opzij

	@ eerste xmin doen
		bl	SWAP					@ ( sizex_win, adres -- )
		popdts r0					@ ( sizex_win -- ) r0=adres-regel!
		ldv16 r1, pxorigx
		ldr r2, [r0, r1]			@ r2 nu waarde pxorigx
		pushdts r2					@ ( sizex_win, origx -- )
		bl	DUP						@ ( sizex_win, origx, origx -- )
		bl	TO_R
		lit16 0						@ ( sizex_win, origx, 0 )
		bl	MIN						@ ( sizex_win, origx/0 )
		bl	_ABS					@ ( sizex_win, xmin ) na ABS xmin gecreëerd!!

		@ => als xmin groter dan sizex_win dan window not visible!
		ldr r1, [dts]				@ r1 nu sizex_win
		cmp top, r1					@ vergelijk xmin en size_win
		blt	1f						@ ga verder met routine als xmin kleiner dan size_win

		@ als hier dan niet tekenen, stack opruimen en terug
		bl	FROM_R					@ return stack schoon ( sizex_win, xmin, origx )
		ldr top,[dts, #8]			@ threedrop inline
		add dts, dts, #12
		lit16 0
		lit16 0
		lit16 0
		lit16 0
		b 9f						@ en exit

	1:	bl	SWAP					@ ( xmin, sizex_win )

	@ nu xmax doen	- size scherm is gelijk aan size win0! = eerste regel in WinTable!
		ldv32 r1, WinTable
		ldv16 r2, pxsizex
		ldr r3, [r1, r2]			@ r3 nu sizex-scherm! heb nog origx nodig, die zit al op R-stack!
		pushdts r3					@ ( xmin, sizex_win, sizex_scherm -- )
		bl	FROM_R					@ ( xmin, sizex_win, sizex_scherm, origx -- )
		bl	_SUB					@ ( xmin, sizex_win, sizex_scherm minus origx -- )
		bl	MIN						@ ( xmin, xmax -- ) nu x klaar, verder met y - r0 nog steeds address goede regel in WinTable

		@ => als xmax negatief dan scherm niet zichtbaar
		cmp top, #0
		bgt 2f						@ als top groter als 0 -> verder met routine

		@ als hier dan niet tekenen, stack opruimen en terug
		ldr top,[dts, #4]			@ twodrop inline
		add dts, dts, #8
		lit16 0
		lit16 0
		lit16 0
		lit16 0
		b 9f						@ en exit

	@ nu ymin
	2:	bl	FROM_R					@ ( xmin, xmax, sizey_win -- )
		ldv16 r1, pxorigy
		ldr r2, [r0, r1]			@ r2 nu waarde pxorigy
		pushdts r2					@ ( xmin, xmax, sizey_win, origy -- )
		bl	DUP						@ ( xmin, xmax, sizey_win, origy, origy -- )
		bl	TO_R
		lit16 0						@ ( xmin, xmax, sizey_win, origy, 0 -- )
		bl	MIN						@ ( xmin, xmax, sizey_win, origy/0 -- )
		bl	_ABS					@ ( xmin, xmax, sizey_win, ymin -- ) na ABS ymin gecreëerd!!

		@ => als ymin groter dan sizey_win dan window not visible!
		ldr r1, [dts]				@ r1 nu sizey_win
		cmp top, r1					@ vergelijk ymin en sizey_win
		blt	3f						@ ga verder met routine als ymin kleiner dan sizey_win

		@ als hier dan niet tekenen, stack opruimen en terug
		bl	FROM_R					@ return stack schoon ( sizex_win, xmin, sizey_win, ymin, origx )
		ldr top,[dts, #16]			@ fivedrop inline
		add dts, dts, #20
		lit16 0
		lit16 0
		lit16 0
		lit16 0
		b 9f						@ en exit

	3:	bl	SWAP					@ ( xmin, xmax, ymin, sizey_win -- )
									@ => als ymin groter dan sizey_win dan window not visible!

	@ nu ymax doen	- size scherm is gelijk aan size win0! = eerste regel in WinTable!
		ldv32 r1, WinTable
		ldv16 r2, pxsizey
		ldr r3, [r1, r2]			@ -> r3 nu sizey-scherm
		pushdts r3					@ ( xmin, xmax, ymin, sizey_win, sizey_scherm -- )
		bl	FROM_R					@ ( xmin, xmax, ymin, sizey_win, sizey_scherm, origy -- )
		bl	_SUB					@ ( xmin, xmax, ymin, sizey_win, sizey_scherm minus origy -- )
		bl	MIN						@ ( xmin, xmax, ymin, ymax -- ) klaar!

		@ => als ymax negatief dan scherm niet zichtbaar
		cmp top, #0
		bgt 9f						@ als top groter als 0 -> klaar met routine

	@ als hier dan niet tekenen, stack opruimen en terug
		ldr top,[dts, #12]			@ fourdrop inline
		add dts, dts, #16
		lit16 0
		lit16 0
		lit16 0
		lit16 0
	9:
next		@}

wordhead ">winsize", 8, WINVISIBLE, 1 @ ( x y win# -- ) set size van window en calculate winpitch en do WINWRITEON
TOWINSIZE: prologmax				@ check tegen maxx en maxy!{
		popdts r2					@ r2=win#
		popdts r3					@ r3=y
		popdts r4					@ r4=x - stack nu leeg

		getvalvar r0, varWINNO

		cmp r2, r0
		bhs	1f						@ als opgegeven window niet bestaat dan stop

		pushdts r2					@ ( -- win# )

		bl	DUP						@ ( -- win# win# )
		bl	WINADRTO				@ ( -- win# address )
		popdts r0					@ ( -- win# )
		@ldv32 r0, WinTable			@ #DEBUG
		@ldv16 r1, bytes_win		@ r1 nu aantal bytes per regel in WinTable
		@mul r2, r2, r1				@ r2 nu r2*aantal bytes per regel
		@add r0, r0, r2				@ r0 nu baseoffset naar correcte regel in WinTable

		ldv16 r1, pxmaxy
		ldr r2, [r0, r1]			@ r2 nu maxy van win#
		cmp r3, r2					@ check of r3 niet te groot of te klein is
		bhi 1f						@ als r3 unsigned groter dan r2 dan stop

		ldv16 r1, pxmaxx
		ldr r2, [r0, r1]			@ r2 nu maxx van win#
		cmp r4, r2					@ check of r4=x niet te groot of te klein
		bhi 1f						@ als r4 unsigned groter dan r2 dan stop

	@ als hier dan x en y binnen grenzen

		ldv16 r1, pxsizey
		str r3, [r0, r1]			@ pxsizey van win# nu gelijk aan r3=y

		ldv16 r1, pxsizex
		str r4, [r0, r1]			@ pxsizex van win# nu gelijk aan r4=x

		lsl r4, r4, #2				@ r2 nu pixel per regel keer 4 maakt pitch
		ldv16 r1, pxpitch
		str r4, [r0, r1]			@ pitch win# gezet in WinTable

		bl	WINWRITEON				@ ( win# -- ) win# stond nog op stack
	1:
nextmax		@}

wordhead ">winorig", 8, TOWINSIZE, 1 @ ( x y win# -- ) set origin van window
TOWINORIG: prologmax				@ geen checks nodig{
		poppopdts r2, r3			@ r2 nu win#, r3 nu y 
		popdts r4					@ r1 nu x - stack nu leeg

		getvalvar r0, varWINNO
		cmp r2, r0
		bhs	1f						@ als opgegeven window niet bestaat dan stop

	@ als hier dan x en y binnen grenzen

		ldv32 r0, WinTable
		ldv16 r1, bytes_win			@ r1 nu aantal bytes per regel in WinTable
		mul r2, r2, r1				@ r2 nu r2*aantal bytes per regel
		add r0, r0, r2				@ r0 nu baseoffset naar correcte regel in WinTable

		ldv16 r1, pxorigy
		str r3, [r0, r1]			@ pxsizey van win# nu gelijk aan r3=y

		ldv16 r1, pxorigx
		str r4, [r0, r1]			@ pxsizex van win# nu gelijk aan r4=x
	1:
nextmax		@}

wordhead ">winrand", 8, TOWINORIG, 1 @ ( nieuwerand win# -- )
TOWINRAND: prolog					@{
		poppopdts r2, r3			@ r2 nu nummer window, r3 nu nieuwe size rand 

		getvalvar r0, varWINNO
		cmp r2, r0
		bhs	1f						@ als opgegeven window niet bestaat dan stop

		pushdts r2					@ ( -- win# )

		ldv16 r1, 32				@ maximale dikte rand
		cmp r3, r1
		bhs 1f						@ als rand dikker dan 32 dan klaar

		ldv16 r1, bytes_win			@ r1 nu aantal bytes per regel in WinTable
		mul r2, r2, r1				@ r2 nu r2*aantal bytes per regel

		ldv32 r0, WinTable
		add r0, r0, r2				@ r0 nu baseoffset naar correcte regel in WinTable

		ldv16 r1, pxrand
		str r3, [r0, r1]			@ pxrand van win# nu gelijk aan r3

		bl	WINWRITEON				@ ( win# -- ) win# stond nog op de stack
	1:
next		@}

wordhead ">wincanvas", 10, TOWINRAND, 1 @ ( canvas win# -- ){
TOWINCANVAS:	prolog
		poppopdts r2, r3			@ r2 nu nummer nieuw window, r3 nu size rand

		getvalvar r0, varWINNO
		cmp r2, r0
		bhs	1f						@ als opgegeven window niet bestaat dan stop

		ldv16 r1, bytes_win			@ r1 nu aantal bytes per regel in WinTable
		mul r2, r2, r1				@ r2 nu r2*aantal bytes per regel

		ldv32 r0, WinTable
		add r0, r0, r2				@ r0 nu baseoffset naar correcte regel in WinTable

		ldv16 r1, pxcanvas
		str r3, [r0, r1]			@ pxrand van win# nu gelijk aan r3
	1:
next		@}

wordhead ">winink", 7, TOWINCANVAS, 1 @ ( ink win# -- ){
TOWININK:	prolog
		poppopdts r2, r3

		getvalvar r0, varWINNO		@ nu het aantal windows				

		cmp r2, r0
		bhs	1f						@ als opgegeven window niet bestaat dan stop

		ldv16 r1, bytes_win			@ r1 nu aantal bytes per regel in WinTable
		mul r2, r2, r1				@ r2 nu r2*aantal bytes per regel

		ldv32 r0, WinTable
		add r0, r0, r2				@ r0 nu baseoffset naar correcte regel in WinTable

		ldv16 r1, pxink
		str r3, [r0, r1]			@ pxrand van win# nu gelijk aan r3
	1:
next		@}

wordhead ">winnoscroll", 12, TOWININK, 1 @ ( scrolldisable-flag win# -- ) true disables scrollen in window
TOWINNOSCROLL:	prolog				@{
		poppopdts r0, r2			@ r0=win#, r2=flag scroll-disable
		
	@ check tegen aantal windows: te groot -> klaar				
		getvalvar r3, varWINNO
		cmp r0, r3
		bhs 9f						@ als n (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT
		
	@ maak van r0 het base-address van WinTable
		pushdts r0					@ ( -- win# )
		bl	WINADRTO				@ ( -- address van correcte regel in WinTable ) #MACRO
		popdts r0					@ r0=base-address WinTable - keep in reg
		ldv16 r1, pxflags

		ldr r3, [r0, r1]			@ r3=pxflags van win#
		
		cmp r2, #0					@ als flag=0
		biceq r3, r3, #pxnoscroll_f	@ als flag=0 -> clear bit van flag
		andne r3, r3, #pxnoscroll_f	@ als flag<>0 -> set bit van flag -> no more scroll of window
		str r3, [r0, r1]			@ en sla weer op
	9:
next		@}

wordhead "winnoscroll>", 12, TOWINNOSCROLL, 1 @ ( win# -- winnoscroll_flag (t/f) ) true -> no scrollen in window
WINNOSCROLLTO:	prolog				@{
		mov r0, top					@ r0=win#
		
	@ check tegen aantal windows: te groot -> klaar				
		ldv32 r1, varWINNO+16		@ dit is een snelle assembler manier om een getal uit een variabele te halen #COOL
		ldr r3, [r1]				@ r3=max_windows
		cmp r0, r3
		bhs 9f						@ als n (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT
		
	@ maak van r0 het base-address van WinTable
		pushdts r0					@ ( -- win# )
		bl	WINADRTO				@ ( -- address van correcte regel in WinTable ) #MACRO
		popdts r0					@ r0=base-address WinTable - keep in reg
		ldv16 r1, pxflags

		ldr r3, [r0, r1]			@ r3=pxflags van win#
		and r3, r3, #pxnoscroll_f	@ isoleer flag
		
		cmp r3, #0					@ als flag=0
		movge top, #0				@ als flag=0 -> return false
		mvnne top, #0			 	@ als flag<>0 -> return true
	9:
next		@}

wordhead "win#>task#", 10, WINNOSCROLLTO, 1 @ ( win# task# -- ) set on which win# task prints
WINNOTOTASKNO:	prolog				@ #CHECK{
		poppopdts r0, r1
		
		ldv32 r2, TASK_BLOCK		@ begin TASK_BLOCK
		add r3, r2, r0, lsl #16		@ r3 = goede regel in TASK_BLOCK - r0 * 2^16 = pointer naar regel in TASK_BLOCK
		
		ldv16 r2, TaskWinNo
		str r1, [r3, r2]
next		@}
	
wordhead "uart>task#", 10, WINNOTOTASKNO, 1 @ ( flag task# -- ) set UART-printing flag for a task
UARTTOTASKNO:	prolog				@ #CHECK{
		poppopdts r0, r1
		
		ldv32 r2, TASK_BLOCK		@ begin TASK_BLOCK
		add r3, r2, r0, lsl #16		@ r3 = goede regel in TASK_BLOCK - r0 * 2^16 = pointer naar regel in TASK_BLOCK
		
		ldv16 r2, TaskUARTen
		str r1, [r3, r2]
next		@}

wordhead "win>scr", 7, UARTTOTASKNO, 1 @ ( xmin, xmax, ymin, ymax, win# -- )
WINTOSCR:	prologmax				@ kopieert window win# (tussen xmin, xmax, ymin en ymax) naar screenaddress{
									@ dit is de enige plek waar naar het scherm geschreven mag worden!
	
	@haal address juiste window in WinTable and keep
		bl	WINADRTO				@ ( xmin, xmax, ymin, ymax, address van correcte regel in WinTable )
		popdts r0					@ r0 is address correcte regel - KEEP tot loop
									@ ( xmin, xmax, ymin, ymax )
	@haal schermadres
		getvalvar r4, varSCRBASE	@ r4 nu screenaddress KEEP  - r4 wordt straks naar-adres

	@haal origy - als negatief maak dan 0
		ldv16 r1, pxorigy
		ldr r3, [r0, r1]			@ r3 nu value pxorigy
		cmp r3, #0
		movlt r3, #0				@ maak r3 gelijk aan 0 als r3 kleiner als 0
	
	@haal schermpitch
		getvalvar r2, varSCRPITCH	@ r2 nu screenpitch - KEEP voor in loop

	@bereken eerste naar-adres -> = schermadres+(origx*4)+(origy*4*schermpitch)
		mul r3, r3, r2				@ r3=origy maal schermpitch (is al in bytes!)
		add r4, r4, r3				@ r4=tussen_naar-adres = scherm-adres plus tussenwaarde

	@haal origx - als negatief maak dan 0
		ldv16 r1, pxorigx
		ldr r2, [r0, r1]			@ r2 nu value pxorigx
		cmp r2, #0
		movlt r2, #0				@ maak r2 gelijk aan 0 als r2 kleiner als 0

		add r4, r4, r2, lsl #2		@ r4=naar_adres => tussen_naar-adres plus (origx maal 4) - KEEP voor in loop #32

	@ haal pitchwin (voor volgende taak en voor in loop)
		ldv16 r1, pxpitch
		ldr r3, [r0, r1]			@ r3 nu value winpitch

	@bereken eerste van-adres -> = winbufferaddres+xmin*4+ymin*4*winpitch
		ldv16 r1, pxbuffer
		ldr r0, [r0, r1]			@ r0 nu pointer to window-buffer

		ldr r1, [dts]				@ r1 = ymin (staat op stack 1 pos onder top)
		mul r1, r1, r3				@ r1 = ymin*winpitch => tussenwaarde voor van-adres (r0)
		add r0, r0, r1				@ tel dit op bij van-adres

		ldr r1, [dts, #8]			@ r1 nu xmin (staat op stack 3 pos onder top)
		add r0, r0, r1, lsl #2		@ tel xmin*4 op bij r0 - r0 nu eerste van addres!! KEEP voor in loop #32

	@bereken x-counter (r1) -> = xmax minus xmin ( xmin, xmax, ymin, ymax )
		ldr w, [dts, #4]			@ w nu xmax - r1 is nog xmin!
		sub r1, w, r1				@ r1 nu x-counter => xmax minus xmin - KEEP

	@bereken y-counter -> = ymax-ymin
		ldr r7, [dts]
		sub r7, top, r7				@ r7 nu y-counter => ymax=top minus ymin=r7

	@***************************************************************
	@hier de checks inbouwen - om te zien of window wel zichtbaar is
	@***************************************************************

	@haal schermpitch weer - clobbered in eerder deel
		getvalvar r2, varSCRPITCH	@ r2 nu screenpitch - KEEP voor in loop

	@yloop:	zet van, naar en x-counter (r1) op stack -> call MOVE
	1:	pushdts r0					@ van-adres			#UPDATE: geen multiple pushdts!
		pushdts r4					@ naar-adres
		pushdts r1					@ x-counter - in words, niet in bytes!

		bl	MOVE					@ SCRMOVE gebruikt geen NEON-regs - #TEST

		add r0, r0, r3				@ add winpitch (r3) to van address (r0)
		add r4, r4, r2				@ add schermpitch (r2) to naar address (r4)
		subs r7, r7, #1				@ trek een af van y-counter (v)
		bne 1b						@ als y-counter nog niet nul -> ga verder in loop:

		ldr top,[dts, #12]			@ inline fourdrop - clear stack
		add dts, dts, #16
nextmax		@}

wordhead "updatewin", 9, WINTOSCR, 1 @ ( win# -- ) copy win# uit win#-buffer to screen
UPDATEWIN: prologmax				@ Masking niet meer nodig - double bufferen ipv maskeren{
	@ check tegen aantal windows
		popdts r0					@ r0 is nu win#
		getvalvar r1, varWINNO		@ r1 is nu aantal windows dat nu in het systeem gedefinieerd is
		cmp r0, r1
		bhs 9f						@ als n (unsigned) groter of gelijk aan aantal windows dan gelijk klaar

		@ updatewin kopieert window inclusief rand! MAW de hele window wordt gekopieerd.
		@ window kan geheel of gedeeltelijk buiten het scherm liggen, dit moet dus gechecked worden!!
		@ dus nu als eerste: bepaal de rechthoek van het te tekenen deel van het window
		@ xmin=abs(min(0, origx)) -> als origx=-80 -> xmin=80 -> eerste teken pixel op het scherm = origx+xmin=0
		@ xmax=min(sizex-win,(sizex-scherm minus origx))
		@ ymin en ymax idem

	@ r0 is win# en al gechecked
		pushdts r0					@ ( -- win# )
		bl	WINADRTO				@ ( -- address van correcte regel in WinTable ) - kan ENTRY bl WINTOBUFFER zijn #UPDATE
		bl	DUP						@ ( -- address, address )
		bl	WINSIZETO				@ ( -- address, sizex-win, sizey-win ) baseren op win# ipv address maakt het complexer!
		bl	WINVISIBLE				@ ( -- xmin, xmax, ymin, ymax )
		
		cmp top, #0					@ als top equals 0, than niets te tekenen #DEBUG
									@ is ymax altijd 0 als een window invisible is?
		beq 8f
		
		pushdts r0					@ ( -- xmin, xmax, ymin, ymax, win# ) - r0 is nog steeds win#

		bl	WINTOSCR				@ ( -- )
		b 9f

	8:	ldr top,[dts, #12]			@ inline fourdrop - clear stack
		add dts, dts, #16			@ double-checked
		
	9:	FlushDataCache				@ #UPDATE: zou weg moeten kunnen!
nextmax		@}

wordhead "win>buffer", 10, UPDATEWIN, 1 @ ( -- ) copy all windows from win-buffers to screenbuffer
WINTOBUFFER: prolog					@ uiteindelijk met overslaan van niet actieve windows{
									@ en met een instelbare volgorde van windows -> windows kunnen omhoog en naar beneden etc.
									@ maar nu eerst simpel
		mov r0, #0
		getvalvar r1, varWINNO		@ r1 is aantal windows dat nu in het systeem gedefinieerd is
	
		ldv32 r2, 0x3000
		str r1, [r2]

	1:	pushdts r0					@ win# wat naar de buffer moet
		bl	UPDATEWIN
		add r0, r0, #1				@ en het volgende window
		cmp r0, r1
		blo 1b						@ zodra r0 gelijk aan aantal windows in systeem -> klaar
		
		@dmb osh @isb @dmb ish
		@FlushDataCache				@ #CHECK - ok, nog niet alles weg
		@isb
		@dmb osh
next		@}

wordhead ">scroffset", 10, WINTOBUFFER, 1 @ ( n -- werkelijke offset ) n=verticale offset van screen in screen-buffer in pixels
TOSCROFFSET: prolog					@{
		popdts r2					@ r2 nu wanted offset
		ldv16 r3, 600				@ r3 is max offset voor debugging #DEBUG
		
		cmp r2, r3
		movhs r2, r3				@ zet r2 op 600 als te grote offset - normale offset = 0 of 480 of 544 of 600
		
	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x48009			@ 0x48009 = set virtual offset van screenbuffer
		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 20				@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 8					@ de 8 is de lengte van het value-field van de tag
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x0
		str r1, [r0, #20]			@ x-offset=0
		
		str r2, [r0, #24]			@ gewenst y-offset op positie 24 in MAIL_DATA

	@ end-tag
		str r1, [r0, #28]			@ end-tag van de buffer aan het begin op positie 24 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu werkelijk offset zoals gezet door GPU
		
	9:	pushdts r1
next		@}

wordhead "clearwin", 8, TOSCROFFSET, 1 @ ( win# -- ) clear win# met canvas uit die window
CLEARWIN:	prologmax				@ clear alleen het deel tussen de randen - alle data zit al in WinTable! (via winwriteon){

	@ check tegen aantal windows
		popdts r0					@ r0 is nu win#
		getvalvar r1, varWINNO		@ r1 is nu aantal windows dat maximaal toegestaan is in het systeem
		cmp r0, r1
		bhs 9f

	@ calculate baseoffset van regel in Wintable
		pushdts r0					@ win# op stack
		bl	WINADRTO
		popdts r0					@ r0 nu address van goede regel in WinTable
		
	@ check y richting
		ldv16 r4, px_wohiy
		ldr r2, [r0, r4]			@ r2 is px_wohiy = aantal regels in y-richting - KEEP
		cmp r2, #0x0				@ kijk of y geen 0 is, dan gelijk klaar
		beq 9f

	@ check x richting
		ldv16 r4, px_wohix
		ldr r3, [r0, r4]			@ r3 is pxwohix = aantal regels in x-richting - KEEP
		cmp r3, #0x0				@ kijk of x geen 0 is, dan gelijk klaar
		beq 9f

	@ get vars
		ldv16 r4, px_wobase
		ldr w, [r0, r4]				@ w is base-address van wo-window - KEEP

		ldv16 r4, pxcanvas
		ldr r1, [r0, r4]			@ r1 is canvas - KEEP

		ldv16 r4, pxpitch
		ldr v, [r0, r4]				@ v is pitch van window - KEEP

	@ loop y richting
	1:	ldv16 r4, px_wohix
		ldr r3, [r0, r4]			@ r3 is max x -> terugtellen tot 0 voor 1 regel
		mov r4, w					@ r4=base-addres uit w - w wordt per y-loop verhoogd met pitch van window

	@ loop x richting				is binnenste loop -> alleen regs!!
									@ de loop zou kunnen worden geoptimaliseerd a là MOVE!
	2:	str r1, [r4], #4			@ schrijf r1=canvas in r4=window-buffer, en verhoog buffer met 4
		subs r3, r3, #1				@ verlaag x-counter met 1
		bne 2b						@ als r3 nog geen 0, dan volgende waarde schrijven in regel

		add w, w, v					@ v=pitch window hierdoor naar volgende regel
		subs r2, r2, #1				@ verlaag y-counter met 1
		bne 1b						@ als y nog geen 0, dan verder met volgende regel
		
		FlushDataCache
	9:	
nextmax 	@ CHECK! clear-deel lijkt te werken, cache-cleaning nog testen}

wordhead "home", 4, CLEARWIN, 1		@ ( win# -- ) zet text cursor op 0, 0
HOME:	prolog						@{
		
		mov r0, top					@ r0=win# - wordt base addres WinTabel -> keep in reg	
		ldr top, [dts], #4			@ en update top	-> stack weer neutraal			

 	@ check tegen aantal windows				
		getvalvar r3, varWINNO
		cmp r0, r3					@ compare win# met aantal aanwezige windows
		bhs 9f						@ als r0=win# (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT

	@ ******************  einde checks - zet cursor op 0 ************************
	@ get base_addres
		pushdts r0					@ address win# op stack-  #OPTIMIZE door macro te maken, scheelt een push/pop en een bl
		bl	WINADRTO				@ nu base-address correcte regel in WinTable op stack
		popdts r1					@ r1 nu base-address correcte regel

	@ get vars vanuit WinTable
		ldv16 r2, 0x0
		ldv16 r3, pxvarout
		str r2, [r1, r3]			@ r3=varOUT van win# = x-pos cursor, nu op 0
		ldv16 r3, pxvarrow
		str r2, [r1, r3]			@ r3=varROW van win# = y-pos cursor, nu op 0
	9:
next		@ #CHECK!! }

wordhead "winwriteon", 10, HOME, 1 @ ( win# -- ) update px_wobase, px_wohix, px_wohiy van window win#
WINWRITEON:	prologmax				@ wordt alleen vanuit andere win-routines gecalled -> geen eigen check op win#{

	@ bereken base address van de juiste regel in WinTabel **
		bl	WINADRTO
		popdts r1

	@ set px_wobase - is daar waar de eerste pixel links boven getekend kan worden!! inclusief randen
		ldv16 r3, pxrand
		ldr r4, [r1, r3]			@ r4=pxrand
		lsl r0, r4, #2				@ r0=rand keer 4 = x-richting

		ldv16 r3, pxbuffer
		ldr r2, [r1, r3]			@ r2=pxbuffer
		add r2, r2, r0				@ r2 nu start buffer inclusief rand in x-richting

		ldv16 r3, pxpitch
		ldr r0, [r1, r3]			@ r0=pxpitch

		mul r3, r4, r0				@ r3=rand keer pitch = y-richting want r0 is nog pxwopitch
		add r2, r2, r3				@ r2 nu start buffer plus rand in x en y richting

		ldv16 r4, px_wobase
		str r2, [r1, r4]			@ zet start buffer in pxwobase - inclusief x en y van rand - maw daar waar getekend kan worden

	@ px_wohiy
		ldv16 r4, pxsizey
		ldr r2, [r1, r4]			@ r2=pxsizey

		ldv16 r4, pxrand
		ldr r3, [r1, r4]			@ r3=pxrand

		sub r2, r2, r3				@ dit kan ook in 1 keer met een lsl #1 na r3!
		sub r2, r2, r3				@ trek rand 2x af van y - dit kan subs zijn en dan zonder de volgende cmp
		cmp r2, #0x0
		movlt r2, #0x0				@ als r2 kleiner dan 0, maak r2 gelijk aan 0x0 - overwegen om een min van 10 ofzo in te voeren
		ldv16 r4, px_wohiy
		str r2, [r1, r4]			@ zet grootte window minus 2*rand in px_wohiy - maar nooit kleiner als nul
		
		ldv16 r4, 20
		udiv r4, r2, r4
		ldv16 r3, pxchary
		str r4, [r1, r3]			@ sla het aantal text-regels op in pxchary

	@ px_wohix
		ldv16 r4, pxsizex
		ldr r2, [r1, r4]			@ r2=pxsizex

		ldv16 r4, pxrand
		ldr r3, [r1, r4]			@ r3=pxrand

		sub r2, r2, r3
		sub r2, r2, r3				@ trek rand 2x af van x
		cmp r2, #0x0
		movlt r2, #0x0				@ als r2 kleiner dan 0, maak r2 gelijk aan 0x0
		ldv16 r4, px_wohix
		str r2, [r1, r4]			@ zet grootte window minus 2*rand in px_wohix - maar nooit kleiner als nul
		
		ldv16 r4, 10
		udiv r4, r2, r4
		ldv16 r3, pxcharx
		str r4, [r1, r3]			@ sla het aantal text-regels op in pxcharx
nextmax		@}

wordhead "setgpu", 6, WINWRITEON, 1 @ ( width heigth -- ) Screen altijd met 32 bit diepte
SETGPU:	prolog						@ initieert GPU en vult variabelen varSCRPITCH, varSCRSIZE en varSCRBASE{
		
		ldv32 r0, FB_INFO			@ address fb_info in r0

		ldv16 r1, 32				@ 32 bit pixel depth
		str r1, [r0, #20]			@ bit depth (1, 4, 8, 16, 24, 32) 1, 4 en 8 werken (nog?) niet - missende palet-tabel?

		popdts r1					@ r1 nu height
		add r2, r1, r1
		str r2, [r0, #12]			@ virtual height van framebuffer = 2*heigth screen
		str r1, [r0, #4]			@ height van screen
		
		popdts r1					@ r1 nu width
		str r1, [r0, #8]			@ virtual width van framebuffer
		str r1, [r0, #0]			@ width van screen

	1:	add r1, r0, #0xc0000000		@ en dus niet 0x40000000!!! 0xc0000000 en 0x80000000 werken beide goed, 0x0 werkt niet!
		pushdts r1					@ message (=0xc0000000+address_infobuffer+channel#)
		lit16 1						@ mailbox-channel
		bl	WRITEMAILBOX			@ write message in mailbox ( message-code channel# -- )

		lit16 1						@ mailbox-channel
		bl	READMAILBOX				@ en lees de feedback van de mailbox

		popdts r1
		cmp r1, #0x0				@ r1 is nu antwoord uit READMAILBOX -> nul is succes
		bne 1b						@ als niet succevol, dan opnieuw proberen

		ldv32 r0, FB_INFO			@ address fb_info in r0

		ldr r1, [r0, #16]			@ pitch=bytes per line in het geheugen
		putvalvar r1, r3, varSCRPITCH

		ldr r1, [r0, #36]			@ size van screen-memory
		putvalvar r1, r3, varSCRSIZE

		lsr r1, r1, #1				@ deel de size van het screen door 2
		putvalvar r1, r3, varSCROFFSET

		ldr r1, [r0, #32]			@ pointer naar base screen-memory
		and r1, r1, #0x3fffffff		@ Anders werkt het niet! #UPDATE proberen of met een extra or #0x8000000 oid aan cache voorbij kan
		str r1, [r0, #32]			@ schrijf het getal na 'and' terug in de FB_INFO->spaart 1 cyclus per pixel in PIXEL

		putvalvar r1, r3, varSCRBASE @ wordt in variabele scrbase gezet - deze wisselt met 30 Hz tussen de twee schermen
		putvalvar r1, r3, varSCR0BASE @ wordt ook in variabele scr0base gezet - deze is stabiel over tijd
next		@}

wordhead "switchscr", 9, SETGPU, 1	@ switched welk van de 2 schermen zichtbaar is en op welke getekend wordt
SWITCHSCR: prolog					@ ~39c of ~36c per call (inc bl), 30 Hz{

		cpsid if					@ #DEBUG
		
		getvalvar r0, varSCRVISIBLE	@ r0 is old visible screen
		cmp r0, #0
		beq 1f						@ if visible screen equals 0
	
	@ here old visible screen equaled 1 => set screen to 0, set offset SCRBASE to 0, set offset to 480
		mov r0, #0
		putvalvar r0, r3, varSCRVISIBLE
		getvalvar r1, varSCR0BASE
		putvalvar r1, r3, varSCRBASE
		
		lit16 480					@ #UPDATE hier nog een variabele afgeleid van de screen-settings inbouwen
		bl TOSCROFFSET				@ #UPDATE hier nog een counted loop inbouwen zodat het setten van de offset zekerder is
		bl DROP						@ drop return value of TOSCROFFSET
		b 9f						@ nadenken over macro 'no_text_next' #TOBEDONE (zodat je ook uit het midden
									@ van een woord kunt terugspringen ipv eerst vooruit springen om dan
									@ te returnen uit het woord)
		
	1:	mov r0, #1					@ ~22c instead of 80c by way of macros (which avoid stack-traffic)
		putvalvar r0, r3, varSCRVISIBLE
		getvalvar r1, varSCROFFSET
		getvalvar r2, varSCR0BASE
		add r1, r1, r2
		putvalvar r1, r3, varSCRBASE @ varSCRBASE alleen in de routine zitten waar de windows naar de screenbuffer worden gekopieerd
									@ alle andere teken routines alleen naar window-buffers!! 
									@ muv debug-printing routines die rechtstreeks naar varSCRBASE mogen printen
		lit16 0
		bl TOSCROFFSET				@ #UPDATE hier nog een counted loop inbouwen zodat het setten van de offset zekerder is
		bl DROP						@ drop return value of TOSCROFFSET							
		
	9:	cpsie if					@ #DEBUG
		isb
		dsb
next		@ #CHECK}

codehead "pixel", 5, SWITCHSCR, 14, 1 @ ( color x y -- ) ( we gaan hier nog uit van 1024 pixel brede lijn en 32 bits diep )
PIXEL:								@ pi3: ?-12c zonder str in scherm -> str in scherm geheugen duurt 22-26c!!{
									@ #CORE: kan weg zodra WinPixels werkt
		ldr v, [dts], #4			@ v=x-pos    Inc maxima check nu 56.4 Mpixel/s => 23c/pixel (was ooit 34, zonder check!)
									@ altijd als het gaat horizontaal tekenen ivm caching en pre-loading!!

		cmp v, #720					@ hier check op grenzen!! nog echte scherm-coordinaten inbouwen #TODO
		cmplo top, #540
		addhs dts, dts, #4			@ DROP als buiten grenzen
		bhs 1f						@ top of v buiten grenzen -> dan pixel niet tekenen

		lsl top, top, #12			@ top=y-pos maal 4096 ( is aantal bytes per lijn ) #32 niet echt maar ook niet goed
		add top, top, v, lsl #2		@ tel x*4 bij top op - top nu offset vanaf start scherm #32

		ldv32 w, FB_INFO			@ address fb_info in w
		ldr v, [w, #32]				@ v nu pointer naar base screen-memory - and met 0x3fffffff is al gedaan

		add top, top, v				@ top nu address waar color geschreven moet worden

		ldr w, [dts], #4			@ w nu color
		str w, [top]				@ schrijf pixel - duurt 18c bij gecached scherm geheugen! #32

	1:	ldr top, [dts], #4			@ en update top weer
codenext	@}

wordhead "winpixel", 8, PIXEL, 1	@ ( x y win# -- ) tekent een pixel in op window win#
WINPIXEL:							@ #TODO{
		prologmax					@ pi3: ?c
		popdts r2					@ r2=y  bytes_win
		popdts r3					@ r3=x

		@ eerst checken op tekenen binnen window size

		bl	varWRITEON				@ het deel van het berekenen van het base-adres moet hieruit
		bl	FETCH					@ en naar MAKEWINACTIVE en centrale variabelen
		popdts r1					@ is actieve window

		ldv16 r4, bytes_win			@ aantal bytes in WinTable per window
		mul r1, r1, r4				@ r1 nu offset in WinTable

		ldv32 r0, WinTable			@ base-address WinTable
		add r0, r0, r1				@ r0 nu base-address in WinTable

		ldv16 r1, pxsizey
		ldr r1, [r0, r1]			@ r1=pxsizey

		cmp r2, r1					@ vergelijk met varWINHIX!!! gebruiken voor vergelijk

		@ hier verder met checks op grenzen!!
		
		
nextmax		@}

wordhead "win0pitch>", 10, WINPIXEL, 1 @ ( -- pitch (in bytes))
WINZEROPITCHTO:	prolog				@{
		str top, [dts, #-4]!
		ldv32 r0, WinTable
		ldv16 r1, pxpitch
		ldr top, [r0, r1]
next		@}

wordhead "scrchar", 7, WINZEROPITCHTO, 1 @ ( char x y -- ) write ascii @ x-y (pix) - Nu met correctie van smalle chars als 'i'
SCRCHAR:	prologmax				@{
									@ #CORE - voor debug - normaal via PIXEL
		popdts r0					@ r0=y
		popdts r1					@ r1=x
		popdts r2					@ r2=ascii en top up-to-date

		cmp r2, #32					@ vergelijk ascii met 32
 		blt	1f						@ als kleiner dan 32->klaaer

 		cmp r2, #127				@ vergelijk ascii met 127
 		bgt 1f						@ als groter dan klaar

 		bl	varSCRPITCH
 		bl	FETCH
 		popdts r3					@ r3 nu pitch screen=win0 in

 		mul r0, r0, r3				@ =pitch*y

		add r1, r0, r1, lsl #2		@ tel r1=x*4 bij r0 op - r1 nu offset vanaf start scherm en r0 weer vrij #32

		bl	varSCRBASE
		bl	FETCH
		popdts r3					@ r3 nu base van scherm

 		add r1, r1, r3				@ r1 nu scherm address voor character

 		bl	conNEOFORTH
 		popdts r3					@ r3 nu base address van font NeoForth

 		sub r2, r2, #32				@ haal 32 af van ascii -> tabel start op 0 voor een spatie
 		ldv16 v, 20
 		mul r2, r2, v				@ r2 nu positie in ascii tabel (20 bytes per char)

		add r2, r2, #1				@ r2 wijst nu naar eerste byte van het character pos0=nummer, pos1=reedte
 		add r2, r2, r3				@ r2 nu start een pos voor character in tabel en r3 weer vrij

 		ldrb r8, [r2], #1			@ r8 nu breedte character
		rsb r8, r8, #8				@ trek breedte char af van 8 -> bijv als breedte=6, dan nu r8=2
		lsr r8, #1					@ deel wat over is door 2 -> bijv als breedte=6, dan nu r8=1 dit is de shift factor

 		bl	varINK
 		bl	FETCH					@ ink nu op stack
 		popdts r0					@ r0 nu inkt om mee te schrijven

 		bl	varCANVAS
 		bl	FETCH
 		popdts r4					@ r4=canvas color

 		ldv16 v, 0					@ v=regelcounter=y-counter
 	3:	ldv16 w, 0					@ w=bitcounter=x-counter

 		ldrb r3, [r2], #1			@ r3 nu eerste byte van font en r2=ascii-tabel 1 byte opgehoogd

 		lsl r3, r3, r8				@ r3 left-shifted met waarde in r8 - WERKT!

 	2:	tst r3, #0x1				@ kijk naar bit 0
 		strne r0, [r1], #4			@ als bit 0 is gezet, dan vul bit met r0=ink #32
 		streq r4, [r1], #4			@ anders met r4=canvas	#32

 		lsr r3, r3, #1				@ schuif r3 1 bit naar rechts
 		add w, w, #1				@ verhoog w=bitcounter met 1
 		cmp w, #8					@ als w=bitcounter nog geen 8, dan verder
 		blo 2b

 		bl	varSCRPITCH
 		bl	FETCH
 		popdts r7					@ r7=screen pitch
 		add r1, r1, r7

 		sub r1, r1, #32				@ en daar weer 8x4 bytes (=8 bits) terug ( om weer an het begin van de volgende 8 pixels te zitten) #32
 		add v, v, #1				@ verhoog v=regelcounter met 1
 		cmp v, #18					@ als nog niet alle regels
 		blo 3b						@ dan volgende regel
 	1:								@ anders klaar!
nextmax		@}

wordhead "winchar", 7, SCRCHAR, 1	@ ( char x y win# -- ) write ascii character at pos x-y of active window in pixels!
WINCHAR:	prologmax				@{
@ 97 ops in totaal					@ primitive #CORE - gecalled door nieuwe, task-aware, EMIT -> checks vooralsnog nodig
		
		mov v, top					@ v=win# - wordt base addres WinTabel -> keep in reg	
		ldr r0, [dts], #4			@ r0=y -> keep in reg
		ldr r1, [dts], #4			@ r1=x -> keep in reg
		ldr r2, [dts], #4			@ r2=ascii en top up-to-date -> keep in reg
		ldr top, [dts], #4			@ en update top				

	@ check of char==ascii 			Het kan zijn dat deze check overbodig is - evt control chars, deels, ook printen
		@cmp r2, #32					@ vergelijk ascii met 32
 		@blt	9f						@ als kleiner dan 32->klaar #NONCRIT

 		@cmp r2, #127				@ vergelijk ascii met 127
 		@bgt 9f						@ als groter dan klaar #NONCRIT

 	@ check tegen aantal windows					
		ldv32 r4, varWINNO+16		@ dit is een snelle manier om een getal uit een variabele te halen #COOL
		ldr r3, [r4]				@ r1 is nu aantal windows dat maximaal toegestaan is in het systeem
		cmp v, r3
		bhs 9f						@ als n (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT

	@ maak van v het base-address van WinTable
		pushdts v					@ ( -- win# )
		bl	WINADRTO				@ ( -- address van correcte regel in WinTable )
		popdts v					@ v nu base-address WinTable - keep in reg

	@ check x=r1 en y=r0 - x en y kunnen legitiem negatief zijn - char kan nl half zichtbaar zijn aan de zijkant
		ldv16 r4, px_wohix
		ldr r3, [v, r4]				@ r3 nu maximale grootte van x

		cmp r1, r3
		bgt 9f						@ als x groter als px_wohix dan char niet zichtbaar -> klaar

		ldv16 r4, px_wohiy
		ldr r3, [v, r4]				@ r3 nu maximale grootte van y
		cmp r0, r3
		bgt 9f						@ als y groter als px_wohiy dan char niet zichtbaar -> klaar

		ldv32 r4, -7				@ (breedte font min 1) - #TODO nog afhankelijk maken van real-time fontsize!
		cmp r1, r4					@ als x=r1 kleiner als -7 -> char niet zichtbaar -> klaar
		blt 9f

		ldv32 r4, -17				@ (hoogte font min 1) - #TODO nog afhankelijk maken van real-time fontsize!
		cmp r0, r4					@ als y=r0 kleiner als -17 -> char niet zichtbaar -> klaar
		blt 9f

	@ ***********               ********************************
	@ Al hier dan input correct en char printable - x=r1 en y=r0
    
    @ save x=r1 voor gebruik in y-loop
    	pushuss r1

	@ get pxpitch - push op uss en bewaar r4 voor volgende step
 		ldv16 r4, pxpitch
		ldr r4, [v, r4]				@ r4 nu pitch win#
		pushuss r4					@ naar uss -> DONE - r4=pitch win# nodig in volgende stap

	@ maak scherm address voor char
		mul r6, r0, r4				@ r6: pitch=r4 * y=r0
		add r6, r6, r1, lsl #2		@ tel r1=x*4 bij r0 op - r6 nu offset in window

		ldv16 r3, px_wobase
		ldr w, [v, r3]				@ w nu base-address win# waar getekend kan worden
 		add w, r6, w				@ w nu scherm address voor character -> hier is het goed

 	@ maak shift factor voor centreren font					
 		ldv32 r3, MyNeoForth		@ r3 nu base address van font NeoForth

 		sub r2, r2, #32				@ haal 32 af van ascii -> tabel start op 0 voor een spatie
 		ldv16 r6, 20
 		mul r2, r2, r6				@ r2 nu positie in ascii tabel (20 bytes per char)

		add r2, r2, #1				@ r2 wijst nu naar eerste byte van het character pos0=nummer, pos1=reedte
 		add r2, r2, r3				@ r2 nu start een pos voor character in tabel en r3 weer vrij
 		pushuss r2					@ start addres char in font-tabel

 		ldrb r8, [r2], #1			@ r8 nu breedte character
		rsb r8, r8, #8				@ trek breedte char af van 8 -> bijv als breedte=6, dan nu r8=2
		lsr r8, #1					@ deel wat over is door 2 -> bijv als breedte=6, dan nu r8=1 dit is de shift factor
		pushuss r8					@ shift factor voor char printing

	@ push y op uss
		pushuss r0

	@ push px_wohiy
		ldv16 r3, px_wohiy
		ldr r0, [v, r3]
		pushuss r0

	@ nu de x-loop registers  *********************************
		ldv16 r0, pxink
		ldr r3, [v, r0]				@ r3=ink

		ldv16 r0, pxcanvas
		ldr r4, [v, r0]				@ r4=canvas

		ldv16 r0, px_wohix
		ldr r8, [v, r0]				@ r8=px_wohix

		ldv16 v, 0					@ y-counter=0

		@		y-loop:	counter-y = regelcount	-> v om bij te houden hoeveel regels er al geweest zijn
		@		r0		px_wohiy				-> uss0 om te kijken of y buiten window valt aan onderkant
		@		r6
		@				y						-> uss1 pixel positie op window - om te kijken of y negatief of te groot is
		@				shift factor			-> uss2 om char in het midden te printen
		@				start-addres char		-> uss3 daar waar het char eigenlijk begint
		@				pitch_win#				-> uss4 om bij address op te tellen bij elke regel van char
		@				x						-> uss5 om voor elke x-loop x weer op oorsprong te zetten!!
		@				adres regel Wintabel	-> zodra y-loop begint want px_wohiy zit op uss

		@		x-loop:	deze 7 alle 7 in regs want binnensten loop
		@				x						->r1 om te kijken of x negatief is of te groot want dan naar volgende regel
		@				counter-x = bitcounter	->r7 om alle bits van een regel langs te gaan
		@				px_wohix				->r8 om te kijken of x buiten valt aan rechter zijde
		@				address waar pixel komt	->w plek om te schrijven (kan nog buiten window liggen, of komen!)
		@				1 regel/byte uit char	->r2 om een voor een de bits uit te halen
		@				canvas					->r4 om te schrijven als bit=0 - in window telt transparency nog niet
		@				ink						->r3 om te schrijven als bit=1


	@ start y-loop
 	3:	ldr r6, [uss, #4]			@ r6=y
 		cmp r6, #0
 		blt 6f						@ hop-y-loop als y=negatief - volgende y proberen, misschien is die wel positief!

		ldr r0, [uss]				@ r0=px_wohiy
		cmp r6, r0
		bhi	9f						@ als y>hiy -> printen klaar (wel nog uss opruimen!!)

 		ldv16 r7, 0					@ r7=bitcounter

 		ldr r6, [uss, #12]			@ r6 nu address eerste char
 		ldrb r2, [r6, #1]!			@ r2 nu eerste byte van font en r6=ascii-tabel 1 byte opgehoogd
 		str r6, [uss, #12]			@ en save verhoogde r6 voor volgende loop

 		ldr r6, [uss, #8]			@ r6 nu shift factor
 		lsl r2, r2, r6				@ r2 left-shifted met waarde in r6
 		
		ldr r1, [uss, #20]			@ zet x weer op oorsprong (kan ook na x-loop...)
		
 	@ start x-loop
 	2:	cmp r1, #0
 		blt 5f						@ hop-x-draw

 		cmp r1, r8					@ r1=x r8=px_wohix
 		bhi 6f						@ als x te groot voor scherm -> geen verdere bits van regel tekenen -> hop-y-loop

 		tst r2, #0x1				@ kijk naar bit 0
 		strne r3, [w, r7, lsl #2]	@ als bit 0 is gezet, dan vul bit met r3=ink - r7=bitcounter
 		streq r4, [w, r7, lsl #2]	@ anders met r4=canvas

 	5:	lsr r2, r2, #1				@ schuif r2=byte_van_char 1 bit naar rechts
 		add r7, r7, #1				@ verhoog r7=bitcounter met 1
 		add r1, r1, #1				@ verhoog r1=x met 1
 		cmp r7, #8					@ als r7=bitcounter nog geen 8, dan verder
 		blo 2b
 
 	@ einde x-loop
 	@ verhoog schrijfaddress met winpitch
 	6:	ldr r0, [uss, #16]			@ r0=winpitch
 		add w, w, r0				@ en verhoog w=schrijfaddress-pixel met winpitch

 	@ verhoog y met 1
 		ldr r0, [uss, #4]			@ r0=y
 		add r0, r0, #1
 		str r0, [uss, #4]

	@ verhoog regel counter-y met 1 en check op einde bereikt van char
 		add v, v, #1				@ verhoog v=regelcounter-y met 1
 		cmp v, #18					@ als nog niet alle regels
 		blo 3b						@ dan volgende regel van char

 	@ einde y-loop
 	9:	add uss, uss, #24			@ tel 24 op bij counter uss om die weer te clearen
nextmax		@}

wordhead "clearscreen", 11, WINCHAR, 1 @ ( -- ) clear scherm met canvas - #CORE - voor debugging, normaal via win0!
CLEARSCREEN:						@ =2.2 keer langzamer als een window-copy, die met MOVE gaat{
		prolog						@ #DEBUG - kan uiteindelijk weg (of als deel van win0-sonderbehandlung)
		getvalvar r0, varCANVAS		@ r0=background-color			
		getvalvar r3, varSCRBASE	@ r1 is screenbase			
		getvalvar r2, varSCRSIZE	@ r2 is screensize				
		
		lsr r2, r2, #2				@ deel r2 door 4 -> aantal pixels

	2:	cmp r2, #0
		beq 1f

		sub r2, r2, #1				@ optimaliseren met w strs achter elkaar helpt niets!
		str r0, [r3], #4			@ evt. een geoptimaliseerde MOVE ombouwen, inc cache-hints
		b 2b
	1:
next		@}

wordhead "scrollup", 8, CLEARSCREEN, 1	@ ( n -- ) scroll scherm n bits up - (move=: van, naar, aantal words )
SCROLLUP:	prolog					@ deze zo maken dat hij zijn gegevens van de GPU haalt! #COOL {
		popdts r0					@ r0 nu aantal bits up, top up-to-date ( -- )
									@ #CORE
		ldv16 r2, 767
		cmp r0, r2
		bhi 1f						@ als groter dan 767 of kleiner dan nul -> stop

		getvalvar r2, varSCRPITCH	@ r2 nu aantal bytes per regel				
		mul r2, r2, r0				@ r2 nu aantal bytes*bits omhoog

		bl	varSCRBASE
		bl	FETCH
		bl	DUP						@ = naar adres

		popdts r1					@ r1 nu base van scherm
		add r1, r1, r2				@ r1 nu startadres voor move = van!!!
		pushdts r1					@ is van

		bl	SWAP					@ van nu onder naar!

		getvalvar r1, varSCRSIZE	@ r1 is screensize					
		sub r1, r1, r2				@ aantal bytes wat gemoved moet worden is (screensize - PITCH*bits omhoog)
		lsr r1, r1, #2				@ deel door 4 om aantal woorden te maken

		pushdts r1					@ aantal words ( van naar aantal -- )

		bl	MOVE					@ doe de feitelijke upscroll mbv MOVE
	1:
next		@}

wordhead "winscroll", 9, SCROLLUP, 1 @ ( pixel#, win# -- ) scrollt window pixel# omhoog en cleart onderste deel
WINSCROLL: prologmax				@ winscroll kan VEEEEL beter met een schuifende offset bij het lezen vanuit{
									@ de winbuffer. Dan gaat scroll pakweg 10keer zo snel!
		popdts r0					@ r0=win#
		popdts r1					@ r1=pixel#
							
	@ check tegen aantal windows: te groot -> klaar				
		ldv32 r4, varWINNO+16		@ dit is een snelle manier om een getal uit een variabele te halen #COOL
		ldr r3, [r4]				@ r3 is nu aantal windows bekend in het systeem
		cmp r0, r3
		bhs 9f						@ als n (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT
		
	@ maak van v het base-address van WinTable
		pushdts r0					@ ( -- win# )
		bl	WINADRTO				@ ( -- address van correcte regel in WinTable )
		popdts r2					@ r2=base-address WinTable - keep in reg
		
	@ check pixel# tegen 0: als nul dan geen scrollen en dus gelijk klaar
		cmp r1, #0
		beq 9f
		
	@ check px_wohiy en px_wohix op 0 -> dan gelijk klaar
		ldv16 r4, px_wohix
		ldr r4, [r2, r4]			@ r4 nu px_wohiy
		cmp r4, #0					@ als px_wohiy=0 -> klaar
		beq 9f
		
		ldv16 r4, px_wohiy
		ldr r4, [r2, r4]			@ r4 nu px_wohiy
		cmp r4, #0
		beq 9f
		
	@ check pixel# tegen px_wohiy: groter of gelijk als px_wohiy -> clearwindow			
		cmp r1, r4					@ r4 nog px_wohiy
		blo 1f						@ alleen verder als unsigned pixel# kleiner als px_wohiy - later nog negatieve scroll!
		
	@ als hier dan aantal pixels groter/gelijk als hoogte window -> clear van window en verder niets
		pushdts r0					@ r0=win# op stack
		bl	CLEARWIN				@ scherm leeg - UPDATEWIN vindt buiten dit woord plaats
		dsb
		isb
		b 9f						@ en klaar	
	
	@ als hier dan input OK en scroll kan plaatsvinden
	@ prep shift window ********* 8 regs + work=w
		@ keep r2=WinTabel
		@ keep r1=pixel#
		@ keep r0=win#					
		@ get/keep y-counter=r4 = px_wohiy minus pixel#
		@ get/keep naar=r6 = px_wobase
		@ get/keep winpitch=r3
		@ get/temp diff=w = pixel# * winpitch
		@ get/keep van=r8 = naar + diff
		@ get/keep words#=r7 to be moved per line = px_wohix
		
	@ maak temp/diff
	1:	ldv16 w, pxpitch
		ldr r3, [r2, w]				@ r3 nu pitch - KEEP
		mul w, r3, r1				@ w nu temp/diff (=pitch*pixel#)
	
	@ maak keep/naar
		ldv16 r4, px_wobase
		ldr r6, [r2, r4]			@ r6=naar
		
	@ maak keep/van
		add r8, r6, w				@ r8=van
		
	@ maak keep/words
		ldv16 w, px_wohix
		ldr r7, [r2, w]				@ r7=words#
		
	@ maak y-counter
		ldv16 r4, px_wohiy
		ldr r4, [r2, r4]			@ r4=px_wohiy
		sub r0, r4, r1				@ r0 nu y-counter
	
	@ y-loop	
	2:	pushdts r8					@ van
		pushdts r6					@ naar
		pushdts r7					@ words
		
		bl	MOVE
		
		add r8, r8, r3				@ van=r8+pitch=r3
		add r6, r6, r3				@ naar=r6+pitch=r3
		subs r0, r0, #1				@ y-counter=r4-1
		bne 2b						@ loop verder als nog niet klaar
		
	@ ****** clear empty line  ***** 7 regs + work r0  *************************
		@ have/keep WinTabl=r2
		@ have/keep winpitch=r3
		@ get/temp px_wobase=w
		@ have/keep px_wohiy=r4
		@ have/keep pixels#=r1 - wordt y-counter
		@ get/keep startaddress=r6 = (( px_wohiy - pixels# ) * winpitch ) + px_wobase
		@ get/keep wincanvas=r7
		@ get/keep x-counter=v = px_wohix
		@ have/keep y-counte=r1 - was pixel#
		
	@ get/temp px_wobase
		ldv16 r0, px_wobase
		ldr w, [r2, r0]				@ w is base-address van wo-window - temp
		
	@ get/keep startaddress
		sub r6, r4, r1				@ r6=px_wohiy-pixel#
		mul r6, r6, r3				@ r6=(px_wohiy-pixels#)*winpitch
		add r6, r6, w				@ r6=startaddres - KEEP

	@ get/keep canvas	
		ldv16 r0, pxcanvas
		ldr r7, [r2, r0]			@ r7 is canvas - KEEP

	@ loop y richting
	3:	ldv16 r0, px_wohix
		ldr v, [r2, r0]				@ v is hix -> terugtellen tot 0 voor schrijven van 1 regel
		mov r4, r6					@ r4=base-addres uit r6 - r6 wordt per y-loop verhoogd met pitch van window

	@ loop x richting				
	4:	subs v, v, #1				@ verlaag v=x-counter met 1
		str r7, [r4], #4			@ schrijf r1=canvas in r4=window-buffer, en verhoog buffer met 4
		bne 4b						@ als v=x-counter nog geen 0, dan volgende waarde schrijven in regel

		subs r1, r1, #1				@ verlaag y-counter met 1
		add r6, r6, r3				@ r3=pitch window hierdoor naar volgende regel
		bne 3b						@ als y nog geen 0, dan verder met volgende regel
		
	@ hier CLEANDCBLOCK ingebouwd #CHECK waarschijnlijk belangrijker als in CLEARWIN!
	@ en scheelt niks!!! vwb cachelines...
		ldv16 r0, px_wobase
		ldr v, [r2, r0]
		pushdts v					@ zet als van-address op stack voor CLEANDCBLOCK
		
		ldv16 r0, px_wohiy
		ldr w, [r2, r0]
		
		ldv16 r0, pxpitch
		ldr r1, [r2, r0]			@ r1 nu pitch
		
		mul w, w, r1				@ w nu aantal pixels
		lsl w, #2					@ keer 4 voor aantal bytes
		add w, w, v					@ w nu eind van gescrollde geheugen-block
		pushdts w
		
		bl	CLEANDCBLOCK
		
	9:	isb
		dsb							@ #DEBUG niet nodig na CLEANDCBLOCK
nextmax		@}

wordhead "drawline", 8, WINSCROLL, 1 @ ( color x0 y0 x1 y1 -- )
DRAWLINE:							@{
		prologmax					@ #UPDATE STREAMS zonder UART-deel
		popdts r0					@ r0=y1
		popdts r1					@ r1=x1
		popdts r2					@ r2=y0
		popdts r3					@ r3=x0
		popdts w					@ w=color -> top is up to date
									@ r4=dx
									@ r6=dyn
									@ r7=sx
									@ r8=sy
									@ v=error
		cmp r3, r1
		subgt r4, r3, r1
		movgt r7, #-1
		suble r4, r1, r3
		movle r7, #1

		cmp r2, r0
		subgt r6, r0, r2
		movgt r8, #-1
		suble r6, r2, r0
		movle r8, #1

		add v, r4, r6
		add r1, r7
		add r0, r8

	1:	teq r3, r1
		teqne r2, r0
		beq 9f

		str top, [dts, #-4]!		@ save oude top op stack
		str w, [dts, #-4]!			@ zet w op stack
		str r3, [dts, #-4]!			@ zet r3 op stack
		mov top, r2					@ en zet r2 als nieuwe top van stack

		STMFD r13!,{r11, r12}		@ r4 niet saven, kost te veel tijd
		bl PIXEL					@ PIXEL is code met clobbering van v en w! daarom v en w gepushed en gepopt
		LDMFD r13!, {r11, r12}		@ alle regs worden in de loop gebruikt

		cmp r6, v, lsl #1
		addle v, r6
		addle r3, r7

		cmp r4, v, lsl #1
		addge v, r4
		addge r2, r8
		b 1b
	9:	
nextmax		@}						
@ }

@  ***********************  MAILbox routines  **********************{
wordhead "writemailbox", 12, DRAWLINE, 1 @ ( fb_info_addr channel -- )
WRITEMAILBOX:	prolog				@{
		ldv32 r0, 0x3F00B880		@ r0=mailbox base-address pi2 en pi3
									@ +0x00 mailbox read register   -> 0x3F00B880
									@ +0x1C mailbox config register -> 0x3F00B89C
									@ +0x18 mailbox status register -> 0x3F00B898
									@ +0x20 mailbox write register  -> 0x3F00B8A0

	1:	ldr r1, [r0, #0x18]			@ 0x18=mailbox status register
		and r1, r1, #0x80000000		@ zet flags as if and r1 en 0x80000000 -> kijk of mailbox leeg is om te schrijven
		cmp r1, #0x0
		bne 1b						@ blijf checken tot bit is 0 -> tot mailbox ruimte heeft om te schrijven

		ldr r1, [dts], #4			@ r1 nu fb_info_addr
		add r1, r1, top				@ add fb_info_addr en channel

		ldv32 r0, 0x3F00B880
		str r1, [r0, #0x20]			@ en zet de optelsom van beide in de mailbox

    	ldr top, [dts], #4			@ top weer up to date
next		@}

wordhead "readmailbox", 11, WRITEMAILBOX, 1 @ ( int-channel -- int-ra )
READMAILBOX:						@ {
		prolog
		@ top = channel en wordt ra (whatever dat is...)
		ldv32 r0, 0x3F00B880		@ v=mailbox base-address pi2 en pi3
									@ +0x00 mailbox read register   -> 0x3F00B880
									@ +0x1C mailbox config register -> 0x3F00B89C
									@ +0x18 mailbox status register -> 0x3F00B898
									@ +0x20 mailbox write register  -> 0x3F00B8A0
	2:
	1:	ldr r1, [r0, #0x18]			@ 0x18=mailbox status register

		and r1, r1, #0x40000000		@ zet flags as if an AND tussen w en operand
		cmp r1, #0x0
		bne 1b						@ als niet nul dan weer lezen -> bit 30 is 0 als er iets in mailbox zit

		ldr r3, [r0]				@ nu iets te lezen uit de maibox (anders nog in loop hiervoor)

		and r2, r3, #0b1111			@ isoleer onderste 4 bits van mailbox-reply
		teq r2, top					@ =set flags as if "eor van 'r2' en 'top'"
		bne 2b						@ begin opnieuw als r2 niet gelijk aan channel in top

		and top, r3, #0xfffffff0	@ isoleer bovenste 28 bits en zet in top -> =return boodschap van mailbox
next		@}

@  *************************  MAILBOX TAGS  ************************

wordhead "mailtest", 8, READMAILBOX, 1 @ ( -- feedback addres van MAIL_DATA ) maw een fetch na mailtest geeft gelijk de eerste waarde!
MAILTEST: prolog					@ WERKT!!! Het probleem was caching!!!! op het moment meestal 44-45 graden{

	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x30002			@ 30006=temp 30002=getfreq 38002=setfreq 38009=set turbo
		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 20					@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x4
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x0				@ 0=turbo id
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
		ldv32 r1, 0x3				@ =nieuwe cpu freq 
		str r1, [r0, #24]			@ nu op positie 24 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #28]			@ end-tag op positie 32 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		lit32 MAIL_DATA				@ ( -- MAIL_DATA-addres )
next		@}

wordhead ">cpufreq", 8, MAILTEST, 1		@ ( wanted cpu-freq (in Mhz) -- werkelijke cpu-freq (in Hz) ) maw een fetch na mailtest geeft gelijk de eerste waarde!
TOCPUFREQ: prolog					@ WERKT bijna - nog probleem met turbo-mode - alleen freqs onder 1201 kunnen gezet worden{

		popdts r2					@ r2 nu wanted freq
		ldv16 r3, 1401				@ r3 is max freq
		
		cmp r2, r3
		movhs r1, #0				@ zet r1 op 0 als te grote freq
		bhs 9f						@ als hoger als 1400 of kleiner als 0 dan gelijk klaar
		
		moveq r2, #1				@ nooit lager als 1
		
		ldv32 r3, 1000000			@ Mhz
		mul r2, r2, r3				@ r2 nu hz ipv Mhz
		
	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x38002			@ 30006=temp 30002=getfreq 38002=setfreq
		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 24				@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x12				@ de 12 is de lengte van het value-field van de tag
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x3				@ 0=temp id 0x3=ARM-freq
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
		@ldv32 r1, 1200000000		@ =nieuwe ARM-freq 8 Mhz lukt=>erg langzaam, 1400000000 wil niet, wordt geignoreerd.
									@ r2 is nog de gewenste freq
		str r2, [r0, #24]			@ nu op positie 24 in MAIL_DATA
		
		ldv32 r1, 0x1				@ 0=also do turbo when warranted - 1 werkt wel, 0 niet (??) - samenhang met turbo-setting??
		str r1, [r0, #28]			@ nu op positie 28 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #32]			@ end-tag van de buffer aan het begin op positie 32 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu freq in Hz na zetten - geeft de werkelijke freq aan na setten - kan freq clampen
		
	9:	pushdts r1
next		@ CHECKED!!}

wordhead ">corefreq", 9, TOCPUFREQ, 1	@ ( wanted core-freq (in Mhz) -- werkelijke cpu-freq (in Hz) ) maw een fetch na mailtest geeft gelijk de eerste waarde!
TOCOREFREQ: prolog					@ WERKT bijna - nog probleem met turbo-mode - alleen freqs onder 1201 kunnen gezet worden{

		popdts r2					@ r2 nu wanted freq
		ldv16 r3, 601				@ r3 is max freq
		
		cmp r2, r3
		movhs r1, #0				@ zet r1 op 0 als te grote freq
		bhs 9f						@ als hoger als 600 of kleiner als 0 dan gelijk klaar
		
		moveq r2, #1				@ nooit lager als 1
		
		ldv32 r3, 1000000			@ Mhz
		mul r2, r2, r3				@ r2 nu hz ipv Mhz
		
	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x38002			@ 30006=temp 30002=getfreq 38002=setfreq
		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 24				@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x12				@ de 12 is de lengte van het value-field van de tag
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x4				@ 0=temp id 0x4=CORE-freq
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
		@ldv32 r2, 400000000		@ =nieuwe CORE-freq grenzen?
									@ r2 is nog de gewenste freq
		str r2, [r0, #24]			@ nu op positie 24 in MAIL_DATA
		
		ldv32 r1, 0x1				@ 0=do turbo when warranted - 1 werkt wel, 0 niet (??) - samenhang met turbo-setting??
		str r1, [r0, #28]			@ nu op positie 28 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #32]			@ end-tag van de buffer aan het begin op positie 32 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu freq in Hz na zetten - geeft de werkelijke freq aan na setten - kan freq clampen
		
	9:	pushdts r1
next		@ CHECKED!!}

wordhead "cpufreq>", 8, TOCOREFREQ, 1	@ ( -- CPU-freq (in Hz) )
CPUFREQTO: prolog					@{

	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x30002			@ 30006=temp 30002=freq!

		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 8					@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x4				@ de 4 is de lengte van value
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x3				@ 0=temp id 0x3=ARM-freq
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #24]			@ response-indicator nu op positie 16 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu freq in Hz
		
		pushdts r1
next		@ CHECKED}

wordhead "corefreq>", 9, CPUFREQTO, 1	@ ( -- CPU-freq (in Hz) )
COREFREQTO: prolog					@{

	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 1024				@ geeft aan hoe groot de totale buffer-space is - maw de ruimte die de GPU
									@ heeft voor reply - te grote buffer OK, te klein geeft 0x80000001 terug (=parsen incompleet)
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (?) - per channel natuurlijk (?)
		ldv32 r1, 0x30002			@ 30006=temp 30002=freq!

		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 8					@ of max voor TAG incl antwoord, of de lengte van de aanvraag van de TAG
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x4				@ de 4 is de lengte van value
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x4				@ 0=temp id 0x4=CORE-freq
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #24]			@ response-indicator nu op positie 16 in MAIL_DATA

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP

		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu freq in Hz
		
		pushdts r1
next		@ CHECKED}

wordhead "cputemp>", 8, COREFREQTO, 1	@ ( -- temp (in Celcius) )
CPUTEMPTO: prolog					@{ 

	1:	ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		
	@ start buffer voor MAILbox request	
		ldv16 r1, 32
		str r1, [r0, #0]			@ buffer-lengte (in bytes) op positie 0 in MAIL_DATA
		
		ldv16 r1, 0
		str r1, [r0, #4]			@ process-request=0 op positie 4 in MAIL_DATA
									@ hier komt de response door de GPU: 0x8000000=goed 0x80000001=incomplete 0x0=fout

	@ nu eerste TAG - dit kunnen er meerdere zijn (maar hoe?) - per channel natuurlijk (?)
		ldv32 r1, 0x30006
		str r1, [r0, #8]			@ TAG id nu op positie 8 in MAIL_DATA

		ldv32 r1, 8
		str r1, [r0, #12]			@ value-buffer-size van TAG nu op positie 12 in MAIL_DATA

		ldv32 r1, 0x4
		str r1, [r0, #16]			@ response-indicator nu op positie 16 in MAIL_DATA

		ldv32 r1, 0x0
		str r1, [r0, #20]			@ temp-ID=0 (dit is de value) nu op positie 20 in MAIL_DATA
		
	@ end-tag
		ldv32 r1, 0x0
		str r1, [r0, #24]			@ end-TAG nu op positie 24 in MAIL_DATA - hier komt straks het antwoord!

		pushdts r0					@ mail TAG (address_infobuffer+channel#) - channel# wordt door WRITEMAILBOX gedaan
					
		lit16 8						@ channel#
		bl	WRITEMAILBOX			@ write mail in mailbox
				
		lit16 8						@ channel#
		bl	READMAILBOX				@ en lees de feedback van de mailbox van channel# ( -- ?? )
		bl	DROP
		
		ldv32 r0, MAIL_DATA			@ r0=address MAIL_DATA
		ldr r1, [r0, #24]			@ r1 nu temp
		
		ldv16 r2, 500
		add r1, r1, r2				@ tel 500 (=een halve graad) op bij r1
		
		ldv16 r2, 1000
		udiv r1, r1, r2				@ en deel r1 door 1000
		
		pushdts r1
next		@ CHECKED!!}}

@  *********************  basic IN-OUT routines  *******************{
wordhead ".char", 5, CPUTEMPTO, 1 @ ( char -- )
WRITECHAR:							@ #TOBEDONE mist nog time-out{
		prolog

	1:	ldr r1, =AUX_MU_LSR_REG
		ldrb r0, [r1]
		tst r0, #0x20
		beq 1b						@ loop totdat LSR_REG bit 1 is
		
		cpsid if
		ldr r0, =AUX_MU_IO_REG		@ om dan een byte te schrijven in IO_REG
		popdts r1					@ ( -- )
		strb r1, [r0]				@ print een byte
		cpsie if
next		@ #CHECK}

wordhead "key", 3, WRITECHAR, 1		@ ( -- char/-1 ) nu met experimentele time-out counter!!!
KEY:	prolog						@ #UPDATE - time-out ingebouwd - wel een energie-verkwistende - try wfi!{

		str top, [dts, #-4]!  		@ save top op stack
		ldv32 r2, 10000000			@ clear r2=time-out counter #CHECK - hoe hoog?? 10 miljoen = pakweg 1 sec

	1:	subs r2, r2, #1				@ time-out counter 1 naar beneden
		mvneq top, #0				@ -1 bij geen char!! #CHECK
		beq 2f						@ #UNCRITICAL

		ldr r1, =AUX_MU_LSR_REG
		ldrb r0, [r1]
		tst r0, #1
		beq 1b						@ wacht tot key ingetypt
		
		cpsid if					@ helpt niet tegen dropped chars!! (bij ASCII transfer)
		ldr r1,=AUX_MU_IO_REG
		ldrb top, [r1]
		cpsie if					@ #CHECK
	2:
next		@ #CHECK}

wordhead "timedkey", 8, KEY, 1		@ ( n -- byte/-1) wacht n sec tot time-out
TIMEDKEY:	prolog					@ #UPDATE - time-out ingebouwd - wel een energie-verkwistende - try wfi!{

		ldv32 r2, 10000000			@ clear r2=time-out counter #CHECK - hoe hoog?? 10 miljoen = pakweg 1 sec
		cmp top, #1					@ top groter of gelijk aan 1
		bge 3f						@ ga dan verder
		
		ldv16 top, 1				@ anders top=1 -> nooit een kortere time-out als 1 sec
		
	3:	mul r2, r2, top

	1:	cpsie if
		
		subs r2, r2, #1				@ time-out counter 1 naar beneden
		mvneq top, #0				@ -1 bij geen char!! #CHECK
		beq 2f						@ #UNCRITICAL
		
		cpsid if					@ helpt niet tegen dropped chars!! (bij ASCII transfer)
		
		ldr r1, =AUX_MU_LSR_REG
		ldrb r0, [r1]
		tst r0, #1
		beq 1b						@ wacht tot key ingetypt

		ldr r1,=AUX_MU_IO_REG
		ldrb top, [r1]
		
	2:	cpsie if					@ #CHECK
next		@ #CHECK}

wordhead "receive", 7, TIMEDKEY, 1		@ ( n=max-lengte, address -- number of chars received ) 
RECEIVE:							@ ontvangt max n chars ASCII van de UART op address - returns number of received chars{
		prologmax					@ zet een 'nul' aan het einde van de ontvangen uncounted string

		popdts r1					@ r1=address
		popdts r2					@ r2=max-lengte
		ldv16 r0, 0					@ zet offset (=count chars) op nul

		cmp r2, #2					@ als max lengte kleiner als 2 (voor 1 char en 'nul') dan klaar
		blt	3f						@ signed lower (want alle getallen zijn ip signed) #UNCRITICAL

		sub r2, r2, #1				@ aantal te ontvangen chars is 1 minder door de nul aan het einde

	4:	mov r4, #10					@ herhaal lees key 10 keer bij een time-out
	1:	bl KEY						@ hierna een char op de stack
		popdts r3					@ r3=received char

	@ 4=end of transmission, 0='nul', 23=end of transmission block, 25=end of medium, 24=cancel
	@ #CHECK kijken hoe minicom een ascii-transmissie afsluit
		cmp r3, #4					@ =end of transmission
		beq 2f						@ dan sluit file af

		cmp r3, #0					@ ='null'
		beq 2f						@ dan sluit file af #TOBEDONE: dit werkt alleen bij ASCII
		
		cmp r3, #-1					@ als time-out flag: 10 retries
		bne 5f
		
	@ als hier dan een time-out
		subs r4, r4, #1				@ verlaag timeout-counter met 1
		bne 1b						@ als nog niet 10 keer geprobeerd, dan nog een keer proberen
	
		b 2f						@ als na 10 keer proberen geen succes: sluit file af

	5:	add1valvar v, w, varCHARSIN	@ verhoog varCHARSIN met 1 - clobbers v en w - ongoing count van received chars
									@ @CHECK
		strb r3, [r1, r0]			@ zet character in address plus r0
		add r0, r0, #1				@ verhoog counter
		cmp r0, r2					@ maximale lengte bereikt?
		bne 4b

	2:	ldv16 r3, 0
		strb r3, [r1, r0]			@ zet 'nul' aan het einde van de string
		@add r0, r0, #1				@ ik tel de 'nul' niet mee in de count - #UPDATE ? eventueel beter van wel?

	3:	pushdts r0					@ zet de count van het aantal ontvangen chars op de stack
nextmax		@ #CHECK}

wordhead "expect", 6, RECEIVE, 1	@ ( max lengte -- lengte string in TIB )
EXPECT:								@ ontvangt max n chars van keyboard via UART in TIB - returns number of received chars{
		prolog						@ expect max n characters of een return
		bl	CLEARTIB

		ldr r0, =0					@ en zet offset op nul
		cmp top, #0					@ als max lengte is leeg dan gelijk klaar
		beq	5f						@ lengte terug is r0 en dus hier 0

		ldr r1, =TIB				@ hier gaan alle getypete chars heen

		mov r2, top
		cmp r2, #256
		blo 1f
		ldr r2, =255				@ zet lengte in r2 op 255 als gevraagde lengte is groter dan 255

	1:	bl KEY						@ hierna een char op de stack of timeout flag
		popdts r3
		
		cmp r3, #-1					@ time-out flag hier niet relevant -> afvangen en opnieuw proberen
		beq 1b
		
		cmp r3, #13					@ =return dan dus klaar met typen
		beq 2f

		cmp r3, #8					@ =backspace
		beq 3f

		cmp r3, #27					@ =escape
		beq 4f

		pushdts r3					@ als hier dan normaal ( -- )
		bl EMIT
		strb r3, [r1, r0]			@ zet character in TIB plus r0
		add r0, r0, #1
		cmp r0, r2					@ maximale lengte bereikt?
		bne 1b
		
		b 5f

	3:	cmp r0, #0
		bhi 6f						@ als buffer > 0 dan verder met cursor handling
		bl	DROP					@ anders backspace weg van stack
		b 1b						@ en verder met input

	6:	bl EMIT
		lit16 32
		bl EMIT
		lit16 8
		bl EMIT
		sub r0, r0, #1				@ en zet pointer een terug
		b 1b

	4:	ldr r0, =0					@ hier als escape, return 0 als lengte
	2:	bl DROP						@ haal overbodige char van stack en
	5:	mov top, r0					@ en update top
next		@ checked, en nou echt!}
@}

@  *************************  STREAMS EMIT  ************************{
wordhead "streams", 7, EXPECT, 1	@ ( -- win#/-1, Uart-true/false ) returns core-aware where to print for the calling task
STREAMS:	prologmax				@ win# or -1 & disable print op UART (1 = no print, 0 = print on UART){
									@ kijkt welke core wil printen, dan welke taak loopt op die core
									@ om dan in het task-block kijken voor die taak of en op welk window
									@ en of ook op de UART geprint moet worden
		bl	COREID					@ haal id van core waar dit op loopt
		popdts r0					@ r0 nu core#

		lsl r0, r0, #2				@ r0*4 om correcte index in mini-tabel te hebben
		ldv32 r1, TaskOnCore0		@ begin mini-tabel waar voor elke core staat welke task daar momenteel draait
		ldr r2, [r1, r0]			@ r2 nu taak die de EMIT (waarin WINEMIT weer gecalled wordt) heeft gecalled

		@ #DEBUG
			@ldv32 r4, 0x3000
			@str r1, [r4]
		@ #DEBUG

		ldv32 r1, TASK_BLOCK		@ r1=base-address TASK_BLOCK
		lsl r2, r2, #TaskSize		@ Tasksize = 16 o.h.m.
		add r3, r1, r2				@ r3 = goede regel in TASK_BLOCK - r2 (=taak#) * 2^16 = pointer naar regel in TASK_BLOCK

		@ #DEBUG
			@ldv32 r4, 0x3004
			@str r3, [r4]
		@ #DEBUG

		ldv16 r1, TaskWinNo
		ldr r0, [r3, r1]
		pushdts r0					@ push op welk window geprint moet worden voor de taak waar het over gaat

		ldv16 r1, TaskUARTen
		ldr r0, [r3, r1]
		pushdts r0					@ push of ook op de UART geprint moet worden
nextmax		@ checked}

wordhead "winemit", 7, STREAMS, 1	@ ( char win# -- ) is een soort text-terminal voor een window
WINEMIT: prologmax					@ ontvangt char en hanteert scroll en pxvarout ed voor win#{
		mov r7, top					@ r7=win# - wordt base addres WinTabel -> keep in reg	
		ldr r6, [dts], #4			@ r6=char -> keep in reg
		ldr top, [dts], #4			@ en update top	-> stack weer neutraal			

	@ overbodig bij call vanuit EMIT, wel nodig bij call vanuit user-program
		and r6, r6, #0xff			@ r6=char maximaal 8 bit groot

 	@ check tegen aantal windows				
		getvalvar r3, varWINNO
		cmp r7, r3					@ compare win# met aantal aanwezige windows
		bhs 9f						@ als r7=win# (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT

	@ ******************  einde checks - start printen  ************************
	@ get base_addres
		pushdts r7					@ address win# op stack-  #OPTIMIZE door macro te maken, scheelt een push/pop en een bl
		bl	WINADRTO				@ nu base-address correcte regel in WinTable op stack
		popdts r8					@ r8 nu base-address correcte regel

	@ get vars vanuit WinTable
		ldv16 r4, pxvarout
		ldr r1, [r8, r4]			@ r1=varOUT van win#
		ldv16 r4, pxcharx
		ldr r2, [r8, r4]			@ r2=regellengte van win#
		ldv16 r4, pxvarrow
		ldr r3, [r8, r4]			@ r3=varROW van win#

	@ check of char
		cmp r6, #8					@ als backspace dan: pxvarout 1 naar beneden ipv 1 omhoog
		bne 6f

	@ handle backspace	( 5c! ipv 40c ofzo in EMIT! ) **************************
		cmp r1, #0					@ mits r1=pxvarout geen nul...
		subeq r1, r1, #1			@ trek 1 af van r1=pxvarout

		ldv16 r4, pxvarout
		str r1, [r8, r4]			@ varOUT van window nu weer op 0
		b 9f						@ hoeft een backspace niet te printen, alleen de cursor terug zetten

	@ als hier, eerst het char printen *****************************************
	6:	pushdts r6					@ zet char op stack

		ldv16 r4, pxvarout
		ldr r1, [r8, r4]			@ r1 nu pxvarOUT van window
		ldv16 r4, 10				@ #UPDATE de 10 is fixed
		mul r1, r1, r4				@ r1 nu x_pos in pixels voor next char
		pushdts r1					@ zet x-pos op stack

		ldv16 r4, pxvarrow
		ldr r1, [r8, r4]			@ v nu pxvarrow
		ldv16 r4, 20				@ #UPDATE de 20 is fixed
		mul r1, r1, r4				@ r1 nu y_pos in pixels voor next char
		pushdts r1					@ zet y_pos char in pixels op stack
		
		pushdts r7					@ zet win# op stack
		bl	WINCHAR					@ ( char x y win# -- )

	@ **************************************************************************
	@ hier gewone update van pxvarout ( ie verhoog met 1 - zet op 0 if: > regellengte inc CR )
		ldv16 r4, pxvarout
		ldr r1, [r8, r4]
		add r1, r1, #1				@ verhoog r1=pxvarout met 1
		str r1, [r8, r4]			@ varOUT van win# up-gedate

		cmp r1, r2					@ vergelijk pxvarout met r2=pxcharx (=regellengte win#)
		blo 9f						@ als kleiner dan regellengte -> klaar

	@ hier carriage return omdat varout >= max regellengte voor win#
		@ hier check inbouwen op wel/niet scrollen #UPDATE

		pushdts r7					@ zet win# op stack
		bl	WINCR					@ scroll window omhoog met 1 regel (=20 pixels #UPDATE)
	9:
nextmax		@}

wordhead "wincr", 5, WINEMIT, 1		@ ( win# --  ) cr in win#, incl scroll if needed
WINCR:	prologmax					@{
	@ check op correct aantal win#
		popdts r0					@ r0=win#		
 		getvalvar r3, varWINNO		@ r3 is nu aantal windows gedefinieerd in het systeem		
		cmp r0, r3
		bhs 9f						@ als r0=win# (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT

	@ pxvarout resetten (=carriage return)
	@ nog check inbouwen op wel/niet scrollen #UPDATE
	@ get base address win#
		pushdts r0					@ address win# op stack-  #OPTIMIZE door macro te maken, scheelt een push/pop en een bl
		bl	WINADRTO				@ nu base-address correcte regel in WinTable op stack
		popdts r8					@ r8 nu base-address correcte regel

		ldv16 r4, pxvarout
		ldv16 r1, 0
		str r1, [r8, r4]			@ varOUT van window nu op 0

		ldv16 r4, pxvarrow
		ldr r3, [r8, r4]
		add r3, r3, #1				@ verhoog r3=varROW=positie text-cursor met 1
		str r3, [r8, r4]			@ varROW nu 1 hoger (kan te hoog zijn!)

		ldv16 r4, pxchary
		ldr v, [r8, r4]				@ v=pxchary=aantal regels in window

		cmp r3, v
		blo 9f						@ als r3 lower than aantal regels -> klaar

	@ als hier dan scherm vol -> scroll omhoog en correct pxvarrow
		sub r3, r3, #1
		ldv16 r4, pxvarrow
		str r3, [r8, r4]			@ varROW nu weer een terug als hij een te hoog was

		lit16 20
		pushdts r0
		bl	WINSCROLL				@ scroll window omhoog met 1 regel (=20 pixels #UPDATE)
	9:
nextmax		@}

wordhead "emit", 4, WINCR, 1		@ ( char -- ) is nu volledig IRQ resistent!!!
EMIT: 	prologmax					@{
		cpsid if					@ is dit nodig? JA!!! Als een interrupt ook print

	@ varOUT wordt upgedate als op UART geprint wordt
	@ Voor UART is regellengte vast 80, voor window wordt regellengte ed geregeld door WINEMIT

	@ eerst char beperken to 8 bits
		popdts r1					@ r1=char

		bl	STREAMS					@ ( -- win#/-1 UARTdis_flag )
		popdts r0					@ r0=UARTdis_flag: 0=print op UART, 1=disable print op UART
		cmp r0, #0					@ is UART flag ongelijk aan 0 (maw: true) -> dan geen printen op UART
		bne 4f
	
	@ *******************************		
	@ hier wel UART-printing
		cmp r1, #8					@ r1=char - als backspace (=8) dan: varOUT 1 naar beneden ipv 1 omhoog
		beq 1f

	@ hier update varOUT	
		lit16 1
		bl	varOUT
		bl	PLUSSTORE				@ verhoog varOUT met 1

		getvalvar r3, varOUT		@ ( -- char varOUT )
		cmp r3, #80					@ vergelijk varOUT met regellengte 
		bls 2f						@ print char als varOUT kleiner of gelijk aan 80

		bl	UARTCR					@ anders UARTCR ( die de reset van varOUT doet ) - cave UARTCR ipv CR
		b 2f						@ en print dan char

	@ handle backspace - alleen vwb varOUT!! Niet vwb printen van een space!
	1:	bl	varOUT					@ ( varOUT )
		bl	DUP						@ ( varOUT varOUT )
		bl	FETCH					@ ( varOUT waarde-varOUT )
		bl	ONESUB					@ verlaag top met 1
		lit16 0
		bl	MAX						@ nu nog steeds OUT op stack maar nooit lager als 0
		bl	SWAP					@ draai addres en waarde om
		bl	STORE					@ en zet waarde terug op varOUT en dan printen char

	2:	pushdts r1					@ zet char weer op stack
		bl 	WRITECHAR				@ ( char -- ) en print op UART

	@ *******************************
	@ NU win# checken
	4:	popdts r2					@ r2=win# print flag: -1=disable print op window
		cmp r2, #-1					@ -1=geen printen op window
		beq 9f						@ gelijk klaar
		
		pushdts r1					@ zet char op stack
		pushdts r2					@ zet win# op stack
		bl	WINEMIT					@ 14c voordat hier aangekomen... - kan 3-5c sneller - maar dat helpt niet echt veel!
		
	9:	cpsie if					@ nodig!! voor printen door interrupt op window waar
									@ een taak ook print - anders ook??
nextmax		@ #CHECK}
@}

@  ***************************  DIV routines  **********************{
codehead "/mod", 4, EMIT, 7, 1		@ ( a b -- r q ) inc bl>13-22c pakweg 3-4 keer sneller als originele DIVMOD
DIVMOD:								@ pi3: 9 - 6-9 = pakweg 6-10 keer sneller als pi1!!{
		mov w, top					@ get b van datastack - denom
		ldr v, [dts]				@ get a van de datastack - numer
		sdiv top, v, w				@ top - quotient
		cmp w, #0
		mlsne v, top, w, v			@ dit om de remainder te berekenen
		strne v, [dts]
		streq top, [dts]	 		@ als hier dan 0 op stack - zero division
codenext	@ CHECKED}

codehead "u/mod", 5, DIVMOD, 7, 1	@ ( a b -- r q )
UDIVMOD:							@ pi3: 9 - 6-9{
		mov w, top					@ get b van datastack - denom
		ldr v, [dts]				@ get a van de datastack - numer
		udiv top, v, w				@ top - quotient  --->>>  moet sdiv zijn!!!!
		cmp w, #0
		mlsne v, top, w, v			@ dit om de remainder te berekenen
		strne v, [dts]
		streq top, [dts]	 		@ als hier dan 0 op stack - zero division
codenext	@checked}

codehead "u/", 2, UDIVMOD, 2, 1		@ ( a b -- q (0 bij zero division)) bl>7-16c 6-15c inline - pakweg 5-6 keer sneller
_UDIV:	ldr w, [dts], #4			@ get a van de datastack - numer - stack naar beneden{
		udiv top, w, top			@ top - quotient
codenext	@ checked}

codehead "/", 1, _UDIV, 2, 1		@ ( a b -- q (0 bij zero division)) bl>7-16c 6-15c inline - pakweg 5-6 keer sneller
_DIV:	ldr w, [dts], #4			@ pi3 6-9c - 5-8c -> tot 88% sneller voor groot gedeeld door klein{
		sdiv top, w, top			@ top - quotient
codenext	@ checked}

wordhead "ud/mod", 6, _DIV, 1		@ ( lo-n hi-n lo-m hi-m -- n/m=lo-r hi-r lo-q hi-q ) remainder gevolgd door quotient
UDDIVMOD:							@ unsigned double (=64bit) deel routine met double quotient en double reminder{
									@ pi3 25c - 831c ( alle power 2s zijn 26c! )
		STMFD	r13!,{r0-r4, r6-r8, r11, r12, lr}	@ = bigprolog

		mov		r3, top
		ldr		r2, [dts]
		ldr		r1, [dts, #4]
		ldr		r0, [dts, #8]

		orrs	v, r2, r3			@ Test if B == 0
		beq	L_div_by_0

		@subs	r4, r2, #1
		@sbc		r5, r3, #0
		@tst		r4, r2
		@tsteq	r3, r5
		@beq	L_pow2				@ Test if B is power of 2: (B & (B - 1)) == 0

		orrs	v, r1, r3			@ leuke check om te zien of twee regs beide nul zijn!!
		beq L_div_32_32				@ dan gewone 32bit divide

	L_div_64_64:
		mov		r4, #1
		mov		r6, #0

    	teq		r1, #0
		clz		r7, r1
		clzeq	v, r0
		addeq	r7, r7, v			@ r7 = clz A

		teq		r3, #0
		clz		r8, r3
		clzeq	v, r2
		addeq	r8, r8, v			@ r8 = clz B

		subs	r7, r8, r7			@ if clz B - clz A > 0
		bls	L_done_shift			@ branch unsigned lower or same

		subs	r8, r7, #32
		rsb		v, r7, #32
		movmi	r3, r3, lsl r7
		orrmi	r3, r3, r2, lsr v
		movpl	r3, r2, lsl r8
		mov		r2, r2, lsl r7		@ B <<= (clz B - clz A)

		movmi	r6, r6, lsl r7
		orrmi	r6, r6, r4, lsr v
		movpl	r6, r4, lsl r8
		mov		r4, r4, lsl r7		@ C = 1 << (clz B - clz A)

	L_done_shift:
		mov		r7, #0				@ C: current bit; D: result
		mov		r8, #0

	L_subtract:
		cmp		r1, r3
		cmpeq	r0, r2
		bcc	L_update				@ if A >= B

		subs	r0, r0, r2
		sbc		r1, r1, r3			@ A -= B

		orr		r7, r7, r4
		orr		r8, r8, r6			@ D |= C

	L_update:
		orrs	v, r1, r0			@ if A == 0 -> klaar
		beq	L_exit

		movs	r6, r6, lsr #1		@ C >>= 1 :: movs ivm rrx in volgende opcode!

	@ *****************
		mov		r4, r4, rrx			@ rrx is rotate rigth with extend :: waarom hier een movs ipv een mov?? nu mov gedaan
	@ *****************

		orrs	v, r6, r4			@ if C == 0 -> klaar
		beq	L_exit

		movs	r3, r3, lsr #1		@ B >>= 1
		mov		r2, r2, rrx			@ zie boven
		b	L_subtract

	L_div_32_32:
		udiv 	r7, r0, r2			@ r7 - lo-quotient
		mls		r0, r7, r2, r0		@ dit om de lo-remainder te berekenen - check op div by zero al gebeurd

		str 	r0, [dts, #8]		@ lo-r
		ldv16	r1, 0
		str		r1, [dts, #4]		@ hi-r=0
		mov		top, r1				@ hi-q=0
		str		r7, [dts]			@ lo-q

		LDMFD	r13!, {r0-r4, r6-r8, r11, r12, pc}	@ big next

	L_pow2:
    	and		r4, r0, r4
		and		r5, r1, r5			@ R = A & (B - 1)

    	clz		r6, r2
		add		r7, r6, #1
		rsbs	r6, r6, #31
		bpl	L_1
		clz		r6, r3
		rsb		r6, r6, #31
		mov		r0, r1, lsr r6
		add		r6, r6, #32			@ Q = A >> log2(B)

	L_1:
		movpl	r0, r0, lsr r6
		orrpl	r0, r0, r1, lsl r7	@ lo-q
		mov		r1, r1, lsr r6		@ hi-q

		str		r4, [dts, #8]		@ lo-r
		str		r5, [dts, #4]		@ hi-r
		str		r0, [dts]			@ lo-q
		mov		top, r1				@ hi-q

		LDMFD 	r13!, {r0-r4, r6-r8, r11, r12, pc}	@ big next

	L_exit:
		str		r0, [dts, #8]		@ lo-r
		str		r1, [dts, #4]		@ hi-r
		str		r7, [dts]			@ lo-q
		mov		top, r8				@ hi-q

		LDMFD 	r13!, {r0-r4, r6-r8, r11, r12, pc}	@ big next

	L_div_by_0:						@ bij deling door nul 2 double nullen terug rapporteren
		ldv16	r1, 0
		str		r1, [dts, #8]
		str		r1, [dts, #4]
		str		r1, [dts]
		mov		top, r1

	1:	LDMFD 	r13!, {r0-r4, r6-r8, r11, r12, pc}	@ big next r12 doet en kost niets
.ltorg
.text	@}

wordhead "d/mod", 5, UDDIVMOD, 1	@ ( lo-n hi-n lo-m hi-m -- n/m=lo-r hi-r lo-q hi-q ) remainder gevolgd door quotient
DDIVMOD:							@ signed double (=64bit) deel routine met double quotient en double reminder{
		prolog

		ldr r1, [dts, #4]			@ r1=hi-n
		cmp r1, #0
		bge 1f						@ branch if r1=hi-n positief

		@ negatief n ****************

		cmp top, #0
		bge 2f						@ branch if top=hi-m positief

		@ neg n -- neg m ************

		ldr r2, [dts, #8]
		rsbs r2, r2, #0				@ r2 is lo-n maak r1=lo-n positief
		str r2, [dts, #8]
		ldr r2, [dts, #4]			@ r2 nu hi-n
		rsc r2, r2, #0				@ maak r2=hi-n negatief
		str r2, [dts, #4]

		ldr r2, [dts]				@ r1 = lo-m
		rsbs r2, r2, #0				@ maak top=hi-m positief
		rsc top, top, #0			@ maak r1=lo-m positief
		str r2, [dts]

		bl	UDDIVMOD				@ min*min=pos maar wel een negatieve r!!

		@ correct negs --> r negatief maken

		ldr r2, [dts, #8]
		rsbs r2, r2, #0				@ r2 is lo-r maak r1=lo-r positief
		str r2, [dts, #8]
		ldr r2, [dts, #4]			@ r2 nu hi-r
		rsc r2, r2, #0				@ maak r2=hi-r negatief
		str r2, [dts, #4]

		LDMFD r13!, {r0-r3, pc}		@ next
		b 99f

		@ neg n -- pos m ************

	2:	ldr r2, [dts, #8]
		rsbs r2, r2, #0				@ r2 is lo-n maak r1=lo-n positief
		str r2, [dts, #8]
		ldr r2, [dts, #4]			@ r2 nu hi-n
		rsc r2, r2, #0				@ maak r2=hi-n negatief
		str r2, [dts, #4]

		bl	UDDIVMOD

		@ correct negs --> r en q negatief maken

		ldr r2, [dts, #8]
		rsbs r2, r2, #0				@ r2 is lo-r maak r1=lo-r positief
		str r2, [dts, #8]
		ldr r2, [dts, #4]			@ r2 nu hi-r
		rsc r2, r2, #0				@ maak r2=hi-r negatief
		str r2, [dts, #4]

		ldr r2, [dts]				@ r1 = lo-q
		rsbs r2, r2, #0				@ maak top=hi-q positief
		rsc top, top, #0			@ maak r1=lo-q positief
		str r2, [dts]

		LDMFD r13!, {r0-r3, pc}		@ next
		b 99f

		@ positieve n ***************

	1:	cmp top, #0
		bge 4f						@ branch if top=hi-m positief

		@ pos n -- neg m ************

		ldr r2, [dts]				@ r1 = lo-m
		rsbs r2, r2, #0				@ maak top=hi-m positief
		rsc top, top, #0			@ maak r1=lo-m positief
		str r2, [dts]

		bl	UDDIVMOD

		@ correct negs --> maak q neg maar houdt r pos!!

		ldr r2, [dts]				@ r2 = lo-q
		rsbs r2, r2, #0				@ maak r2=lo-q
		rsc top, top, #0			@ maak top=hi-q positief
		str r2, [dts]

		LDMFD r13!, {r0-r3, pc}		@ next
		b 99f

		@ pos n -- pos m ************
	4:	bl	UDDIVMOD				@ en klaar!
	99:
next		@ checked!!!!!}

wordhead "newud/mod", 9, DDIVMOD, 1	@ ( dnum dden -- dr dq )
NEWUDDIVMOD:						@ udiv_64by64 Newton!{
		STMFD r13!, {r0-r4, r7, r8, lr}		@ = bigprolog - pi3: schatting 25-114c inc bl!!! KLOPT!!! Met pakweg 5-10c optimerings-kans

@ *********************************** routing - pakweg 7c

		cmp top, #0					@ top=hi_den
		bne naar_uddiv64			@ hi_den = geen nul, en dus gelijk naar eerste deel deling
		
		ldr r1, [dts]				@ r1=lo_den - als hier dan top=hi_den=0
		cmp r1, #2					@ vergelijk met 2 hiermee deling door 1 en 0 afgevangen
		blo uddiv_by_0_or_1			@ overwegen of hier ook niet een deling door 2 ook wordt afgevangen als optimering
		
		ldr r0, [dts, #4]			@ r0=hi_num
		orrs r0, top				@ als r0=hi_num en top=hi_den beide nul dan nu flag zero
		beq uddiv_32_by_32			@ gewone udiv doen!
		
@ *********************************** clz denom	- pakweg 7c

naar_uddiv64:						@ zero_div al gechecked
		@ top=hi_den [dts]=lo_den [dts, #4]=hi_num [dts, #8]=lo_num
		@ r2=leading zeros
		
		clz		r2, top				@ r2=aantal nullen hi_den - s niet mogelijk achter clz!
		cmp		r2, #0
		moveq	r7, top
		beq		uddiv_clz_done		@ als nul dan klaar - nog uitrekenen of deze check zinnig is!!

		cmp		r2, #32				@ als minder dan 32 nullen dan gelijk klaar met tellen
		beq		uddiv_more_as_31_zeros

		mov		r7, top, lsl r2		@ r7=shift top=hiden to left met r2
		
		rsb		r8, r2, #32			@ r8=32-r2
		ldr		r1, [dts]			@ r1=loden
		mov		r1, r1, lsr r8		@ shift r1loden ro right met 32-r2 - dit kan in een keer eigenlijk, dit hier ter debug
		
		orr		r7, r7, r1			@ orr hiden met right-shifted loden
		b		uddiv_clz_done			

uddiv_more_as_31_zeros:						
		clz		r3, r1				@ tell aantal van r1=loden
		
		add		r2, r2, r3			@ r2 nu totaal aantal nullen
		mov		r7, r1, lsl r3		@ r1=loden

uddiv_clz_done:
	@ r7 is nu 32 upper bits of normalized denominator - functions!!
	@ r2 is aantal zeros voor latere denormalisation - functions!!
		
@ *********************************** estimate - pakweg 18c (eerst zien!) omdat 1 Newton ~28c duurt is dit beter dan een tabel!

		mov r8, r7, LSR #4			@ neem hoogste 28 bits van normalized denominator
		vmov.u32 s5, r8				@ move r8 to vfp s5 - werkt - OPTIMIZE met LSR uit vorige opcode kan niet!

		vcvt.f64.u32 d1, s5			@ d1 nu float64 versie van r8=normalized denominator - werkt - OPTIMIZE met f32
		
		ldv32 r0, 0xfffffffc
		ldv32 r1, 0x436fffff 		@ -> samen 2^56 in float64 =2*28 bits - OPTIMIZE door deze 3 in de FORTH-setup te doen
		vmov d0, r0, r1				@ en zet constante float64 in d0 - straks evt. al bij setup van systeem - scheelt 5-6c!
		
		vdiv.f64 d2, d0, d1			@ deel 2^56 in d0 door float(r8) in d1
		
		vcvt.u32.f64 s5, d2			@ convert resultaat in d2 terug naar 64 bit integer in d3 - OPTIMIZE naar vcvt (rounding naar 0)
									@ was vcvtr lijkt zomgoed te werken zonder de sub verderop
		vmov r1, s5					@ en move estimate naar r1 - ongeveer 28 bits acuratesse, is (zou) genoeg (moeten zijn)!!
									@ functions!!!!! r1 (na links shiften met  ldz-28) is de goede schatting!!
															
		@sub r1, r1, #1				@ workaround om tevoorkomen dat verderop een 64 bits overflow voorkomt
									@ -> schatting liever iets te laag! functions! - OPTIMIZE zie vcvtr boven!

@ *********************************** denorm estimate - pakweg 9c
@ hier moet de schatting in r0 weer met de ldz-28 factor gecorigeerd worden door een shift left
@ hierna r0 = hischat r1 = loschat	
@ Ik ga uit van 28 bits! (=7*2*2) eerste factor 2 door 64b ipv 32b, tweede factor 2 nodig door maar een newton-loop
	
		subs r2, r2, #28			@ 
		bmi large_den				@ large_den MOET NOG - OVERWEGEN om large den op de ouderwetse manier te doen
									@ en al aan het begin af te vangen!! Dat scheelt tijd
		
		mov r0, #0					@ r0=hi_schat op nul
		cmp r2, #0					@ if leading zeros = 0;
		beq ready_denorm			@ dan gelijk klaar -> OPTIMIZE: kijken of nuttig
		
		cmp r2, #31					@ als r2 groter dan 31 dan alleen lo naar hi deel shiften en lo nul maken
		bgt	only_lo_to_hi
		
	@ als hier dan een shiftfactor tussen 1 en 31 -> beide regs moeten schuiven
		rsb r3, r2, #32
		orr r0, r0, r1, lsr r3		@ orr r0 met (32-r2) naar rechts geschifte r1
		mov r1, r1, lsl r2			@ shift r1 met r2 naar links

		b ready_denorm
		
	only_lo_to_hi:
		sub r2, r2, #32
		mov r0, r1, lsl r2
		mov r1, #0					@ en klaar

	ready_denorm:					@ r0=hi_schat, r1 =lo_schat - functions!

@ *********************************** step 1 Newton - pakweg 10c
@ top is hi_den, [dts]=lo_den, r0=hi_schat, r1=lo_schat
@ nodig is een 64bit multiply van schatting r0hi, r1lo met denominator tophi, r2lo

	@ 1st Newton iteration follows: dit kan 64bits daar schatting * denom niet groter  als 2^64 kan zijn
	@ STEP 1 = schatting keer denominator - niet negatief maken
			
		ldr r2, [dts]				@ ESSENTIEEL HIER!!!
	
		umull	r4, r3, r2, r1		@ low*low r4=lotussen r3=hitussen r2=loden r1=loschat
		mla		r3, r2, r0, r3		@ low*hi r3=hitussen r2=loden, r0=hischat r3=hitussen
		mla		r3, top, r1, r3		@ hi*lo  r3=hi top=hiden r1=loschat  r3=hitussen
									@ functions! maar 64x64=64bit is niet goed genoeg! moet 64*64=65bits zijn (dus de volgende routine)
									@ dit voor nu opgelost door verlagen van schatting met 1
			
@ ************************************** step 2 Newton - pakweg 18c		
@ hier schatting * resultaat  uit vorige resultaat in 128 bit resultaat! r7,r8,v,w
@ shift dat 64 bits to right door midhi en hi te nemen!! 
@ trek dat af van oorspronkelijke schatting*2 (r0 r1)
@ r0r1 hilo schatting r3r4 hilo tussenwaarde r2,v,w,r7,r8 vrij
		
		@ a_0	r0 ->v  -> eindresultaat
		@ a_1	r1 ->w	-> eindresultaat
		@ a_2	r2 ->r7	-> eindresultaat
		@ a_3	r5 ->r8	-> eindresultaat

		@ b_0	r3 ->r4 -> resultaat uit vorige stap
		@ b_1	r4 ->r3	-> resultaat uit vorige stap
		@ c_0	r11->r1 -> oude schatting
		@ c_1	r0 ->r0 -> oude schatting

		umull	v, w, r4, r1 		@ r0 r1 r3 r11 :: low*low->v w r4 r1
		mov		r7, #0				@ r2->r7
		umlal	w, r7, r4, r0	 	@ r1 r2 r3 r0 :: low*high->w r7 r4 r0
		mov		r8, #0				@ top=r5->r8
		umlal	w, r8, r3, r1		@ r1 top r4 r11 ::  high*low->w r8 r3 r1
		mov		r4, #0				@ r3->r4
		adds	r7, r7, r8			@ r2 r2 top->r7 r7 r8
		adc		r8, r4, #0			@ top r3->r8 r4
		umlal	r7, r8, r3, r0 		@ r2 top r4 r0	:: high*high-> r7 r8 r3 r0
									@ v w r7 r8 is resultaat 64x64=128bit van lo naar hi
									@ r7_lo r8_hi is eindresultaat van q*q*d>>64
									@ trek dit 64bit resultaat af vorige schatting van r1 (lo) en r0 (is hi)
									@ v en w nu vrij
						
		subs 	v, r1, r7			@ lo minus lo met flags
		sbc 	w, r0, r8			@ nu r1 (lo) en r0 (hi) best mogelijke nieuwe schatting (0-2 bits afrondingsfout mogelijk)
		
		adds	r1, r1, v
		adc		r0, r0, w

@ klaar 1ste Newton

@ *********************************** eigenlijke deling - pakweg 18c
@ Nu de eigenlijke deling doen en dan de voorlopige remainder berekenen
@ =(schatting*nummer)>>64 bits 
	
		@ a_0	->v  -> eindresultaat - kan straks weg
		@ a_1	->w	 -> eindresultaat - kan straks weg
		@ a_2	->r7 -> eindresultaat
		@ a_3	->r8 -> eindresultaat

		@ b_0	->r4 -> nummer lo
		@ b_1	->r3 -> nummer hi
		@ c_0	->r1 -> newton schatting lo
		@ c_1	->r0 -> newton schatting hi
		
		ldr r3, [dts, #4]
		ldr r4, [dts, #8]

		umull	v, w, r4, r1 		@ r0 r1 r3 r11 :: low*low->v w r4 r1
		mov		r7, #0				@ r2->r7
		umlal	w, r7, r4, r0	 	@ r1 r2 r3 r0 :: low*high->w r7 r4 r0
		mov		r8, #0				@ top=r5->r8
		umlal	w, r8, r3, r1		@ r1 top r4 r11 ::  high*low->w r8 r3 r1
		mov		r4, #0				@ r3->r4
		adds	r7, r7, r8			@ r2 r2 top->r7 r7 r8
		adc		r8, r4, #0			@ top r3->r8 r4
		umlal	r7, r8, r3, r0 		@ r2 top r4 r0	:: high*high-> r7 r8 r3 r0
									@ v w r7 r8 is resultaat 64x64=128bit van lo naar hi
									@ r7_lo r8_hi is eindresultaat van q*q*d>>64
									@ trek dit 64bit resultaat af vorige schatting van r1 (lo) en r0 (is hi) -> functions!!!
									@ v en w nu weer vrij
			
@ ********************************** calc remainder - pakweg 20c
			
	@ bereken remainder -> remainder = nummer-(antwoord*denominator)
	@ antwoord*denominator zou altijdkleiner als 2^64 moeten zijn, 64x64=64bit is voldoende
	
		@ r7 lo_antwoord
		@ r8 hi antwoord
		@ r2 lo_den
		@ top hi_den
		@ r0 wordt lo_remainder
		@ r1 wordt hi_remainder
			
		ldr r3, [dts, #4]			@ r3=hi_num
		ldr r4, [dts, #8]			@ r4=lo_num

		umull	v, w, r2, r7		@ low*low v=lo_tussen w=hi_tussen r2=lo_den r7=lo_antw
		mla		w, r2, r8, w		@ low*hi w=hitussen r2=lo_den, r8=hi_antw w=hi_tussen
		mla		w, top, r7, w		@ hi*lo  w=hi_tussen top=hi_den r7=lo_antw  w=hitussen
		
		subs 	r0, r4, v			@ lo_remainder = lo_num minus lo_tussen met flags
		sbc 	r1, r3, w			@ hi_remainder = hi_num minus hi_tussen met carry

@ ************************************* correct for rounding - pakweg 8c
@ Correct remainder for rounding errors:
@ top=hi_den, r2=lo_den, r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
@ nog checken of remainder ook negatief kan zijn!!!
	
	correct_remainder:
		cmp top, r1					@ vergelijk hi_den en hi_rem
		bhi	3f						@ als top groter dan gelijk klaar met correctie - TENZIJ remainder negatief is!!
		blo	4f						@ als top kleiner dan remainder dan correctie modig
									@ als hier dan hi_den en hi_rem gelijk -> de lows comparen
									
		cmp r2, r0					@ vergelijk lo_den en lo_rem
		blo 4f						@ als lo_den groter dan correctie nodig
		
		b 3f						@ als hier dan geen afrondings fout gevonden
	
	4:	@ Doe een rondje correctie van afrondings-fouten en test dan opnieuw
		@ top=hi_den, r2=lo_den, r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
		@ verhoog antwoord met 1
		@ verlaag remainder met denominator
		
		subs 	r0, r0, r2			@ lo_remainder = lo_rem - lo_den inc flags
		sbc 	r1, r0, top			@ hi_remainder = hi_rem - hi_den inc carry
		
		adds	r7, r7, #1			@ lo_ant = lo_ant plus 1 inc flags
		adc		r8, r8, #0			@ hi_ant = hi_ant plus carry
		
		b correct_remainder			@ en kijk of het nu goed is	
	
@ ********************************** return - pakweg 5c
@ RETURN WRAPPER
@ r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
		
	3:	str r7, [dts]
		str r1, [dts, #4]
		str r0, [dts, #8]
		mov top, r8
		
		LDMFD r13!, {r0-r4, r7,r8, pc}	@ big next - KLAAR!

@ ********************************** pakweg 8c
	uddiv_by_0_or_1:				@ top=hi_den r1=lo_den
		ldv16	r3, 0
		cmp 	r1, #0
		beq 	uddiv_by_0			@ rem en quot op nul
		
	@ hier deling door 1			@ maak rem=0 en quotient=num
		ldr 	top, [dts, #4]		@ nu top=hi_num
		ldr		r0, [dts, #8]		@ r0=lo_num
		str		r0, [dts]
		str		r3, [dts, #4]		@ hi_rem=0
		str		r3, [dts, #8]		@ lo_rem=0

		LDMFD 	r13!, {r0-r4, r7,r8, pc}	@ big next - KLAAR!
		
	uddiv_by_0:						@ zet antwoorden op 0
		str 	r3, [dts]
		str		r3, [dts, #4]
		str		r3, [dts, #8]
		mov		top, r3
		
		LDMFD 	r13!, {r0-r4, r7,r8, pc}	@ big next - KLAAR!
	
@ *********************************** pakweg 15c

	uddiv_32_by_32:					@ r1=lo_den
		ldr 	r2, [dts, #8]		@ r2=lo_num
		udiv 	r7, r2, r1			@ r7 - lo-quotient
		mls		r3, r7, r2, r1		@ dit om de r3=lo-remainder te berekenen - check op div by zero al gebeurd
		
		str 	r3, [dts, #8]		@ lo-r
		ldv16	r3, 0					
		str		r3, [dts, #4]		@ hi-r=0
		mov		top, r3				@ hi-q=0
		str		r7, [dts]			@ lo-q
		
		LDMFD r13!, {r0-r4, r7,r8, pc}	@ big next - KLAAR!

@ *********************************** large den - nog uitzoeken!
	large_den: 
	
		LDMFD r13!, {r0-r4, r7,r8, pc}	@ big next
.ltorg
.text		@}

wordhead "myud/mod", 8, NEWUDDIVMOD, 1	@ ( dnum dden -- dr dq )
MYUDDIVMOD:							@ udiv_64by64 Newton! - optimized met classic en div32by32 en div_by0 en div_by1{
		STMFD r13!, {r0-r4, r7, r8, lr}		@ = bigprolog - pi3: schatting 25-111c (ipv 25-831c for classic div) inc bl
									@ 198 opcodes
	@ ******************************* routing - pakweg 7c
		cmp 	top, #0				@ top=hi_den
		beq 	myuddiv_top0

		clz 	r2, top
		cmp 	r2, #4				@ als minder als 4 leading zeros dan...
		blo		myuddiv_classic		@ doe de gewone deling (en er is in ieder geval geen deling door 0)
									@ en anders...
		b 		myuddiv_newton		@ in 6c naar newton deling (die ~100c duurt)

	myuddiv_top0:
		ldr 	r1, [dts, #4]		@ r1=hi_num
		cmp 	r1, #0				@ als hi_num=0 en top=0 dan... 
		beq 	myuddiv_32by32		@ ...doe de cpu-interne 32door32 en laat die opcode ook evt 0-deling hanteren
									@ in 5c naar 32by32 (die ~16c duurt)

		ldr 	r0, [dts, #8]		@ r0=lo_num
		cmp 	r0, #1
		bhi 	myuddiv_newton		@ in 8c naar newton deling (die ~100c lang is)

		b	 	myuddiv_divby0or1	@ hanteer een deling door 0 of 1 - in 9c naar deling door 0of1
									@ deling door 2 (en evt 4?) nog overwegen!
	@ ****** start classic **********
	myuddiv_classic:

		STMFD r13!, {r6, r11, r12}	@ r11 en r12 hoeven niet echt gesaved, tijdelijk wel tot na debuggen

		mov		r3, top				@ -> hi_den
		ldr		r2, [dts]			@ -> lo_den
		ldr		r1, [dts, #4]		@ -> hi_num
		ldr		r0, [dts, #8]		@ -> lo_num

		mov		r4, #1
		mov		r6, #0

    	teq		r1, #0
		clz		r7, r1
		clzeq	v, r0
		addeq	r7, r7, v			@ r7 = clz A

		teq		r3, #0
		clz		r8, r3
		clzeq	v, r2
		addeq	r8, r8, v			@ r8 = clz B

		subs	r7, r8, r7			@ if clz B - clz A > 0
		bls 	myuddiv_shiftdone	@ L_done_shift - branch unsigned lower or same

		subs	r8, r7, #32			
		rsb		v, r7, #32
		movmi	r3, r3, lsl r7
		orrmi	r3, r3, r2, lsr v
		movpl	r3, r2, lsl r8
		mov		r2, r2, lsl r7		@ B <<= (clz B - clz A)

		movmi	r6, r6, lsl r7		
		orrmi	r6, r6, r4, lsr v
		movpl	r6, r4, lsl r8
		mov		r4, r4, lsl r7		@ C = 1 << (clz B - clz A)

	@L_done_shift:
	myuddiv_shiftdone:
		mov		r7, #0				@ C: current bit; D: result
		mov		r8, #0

	@L_subtract:
	myuddiv_substract:
		cmp		r1, r3				
		cmpeq	r0, r2
		bcc		myuddiv_update		@ L_update - if A >= B

		subs	r0, r0, r2			
		sbc		r1, r1, r3			@ A -= B

		orr		r7, r7, r4			 
		orr		r8, r8, r6			@ D |= C

	@L_update:
	myuddiv_update:
		orrs	v, r1, r0			@ if A == 0 -> klaar
		beq		myuddiv_classicexit	@ L_exit

		movs	r6, r6, lsr #1		@ C >>= 1 :: movs ivm rrx in volgende opcode!
		movs	r4, r4, rrx			@ rrx is rotate rigth with extend :: waarom hier een movs ipv een mov?? nu mov gedaan
		orrs	v, r6, r4			@ if C == 0 -> klaar
		beq		myuddiv_classicexit	@ L_exit

		movs	r3, r3, lsr #1		@ B >>= 1
		movs	r2, r2, rrx			@ zie boven
		b 		myuddiv_substract

	@L_exit:
	myuddiv_classicexit:
		str		r0, [dts, #8]		@ lo-r
		str		r1, [dts, #4]		@ hi-r
		str		r7, [dts]			@ lo-q
		mov		top, r8				@ hi-q
		LDMFD 	r13!, {r6, r11, r12}	@ big next
		LDMFD 	r13!, {r0-r4, r7, r8, pc}	@ big next

	@ ***** einde classic routine *******************************

	myuddiv_newton:
		@ top=hi_den [dts]=lo_den [dts, #4]=hi_num [dts, #8]=lo_num
		@ r2=leading zeros

	@ ******************************* clz denom	- pakweg 7c

		ldr		r1, [dts]			@ r1=lo_den
		clz		r2, top				@ r2=aantal nullen hi_den - s niet mogelijk achter clz! is al gevuld
		cmp		r2, #0
		moveq	r7, top				@ r7=normalized denom
		beq		myuddiv_clz_done	@ als nul dan klaar - nog uitrekenen of deze check zinnig is!!

		cmp		r2, #32				@ als minder dan 32 nullen dan gelijk klaar met tellen
		beq		myuddiv_more_as_31_zeros

		mov		r7, top, lsl r2		@ r7=shift top=hi_den to left met r2
		rsb		r8, r2, #32			@ r8=32-r2
		orr		r7, r7, r1, lsr r8	@ orr hi_den met right-shifted loden
		b		myuddiv_clz_done			

	myuddiv_more_as_31_zeros:					
		clz		r3, r1				@ tell aantal van r1=loden - r1 is al gevuld aan begin routine
		add		r2, r2, r3			@ r2 nu totaal aantal nullen
		mov		r7, r1, lsl r3		@ r7=normalized denom, r1=lo_den

	myuddiv_clz_done:
	@ r7 is nu 32 upper bits of normalized denominator - r2 is aantal zeros voor latere denormalisation

	@ ******************************* estimate - pakweg 16c, 1 Newton-cycle duurt ~28c -> sneller en preciezer dan een tabel!
		vmov.u32 s5, r7
		vcvt.f64.u32 d1, s5			@ d1 nu float64 versie van r8=normalized denominator - deze conversie is langzaam!
									@ daarom al voor de volgende ldvs
		ldv32 r0, 0xfffffffc		@ ldv32 r1, 0x43b00000 en ldv16 r0, 0x0 zou ook goed moeten zijn en misschien 1c sparen
		ldv32 r1, 0x43afffff		@ -> samen 2^60 -> hierdoor geen shift meer nodig van de normalized denom met 4 bits
		vmov d0, r0, r1				@ en zet constante float64 in d0 - dit hoeft niet in system-setup door langzame vcvt hiervoor!

		vdiv.f64 d2, d0, d1			@ deel 2^60 in d0 door float(r8) in d1
		vcvt.u32.f64 s5, d2			@ convert resultaat in d2 terug naar 64 bit integer in d3 - OPTIMIZE naar vcvt (rounding naar 0)
		vmov.u32 r1, s5				@ en move estimate naar r1 - ongeveer 31-32 bits accuratesse, is ruim genoeg

	@ ******************************* denorm estimate - pakweg 9c
	@ hier moet de schatting in r1 weer met de ldz-28 factor gecorigeerd worden door een shift LEFT
	@ hierna r0 = hischat r1 = loschat
		sub r2, r2, #28 			@ echt grote denominators worden bij routing al afgevangen
		bmi myuddiv_large_den
		
		mov r0, #0					@ r0=hi_schat op nul
		
		@cmp r2, #0					@ if leading zeros = 0;
		@beq myuddiv_ready_denorm	@ dan gelijk klaar -> OPTIMIZE: kijken of nuttig
		
		cmp r2, #31					@ als r2 groter dan 31 dan alleen lo naar hi deel shiften en lo nul maken
		bgt	myuddiv_only_lo_to_hi
		
	@ als hier dan een shiftfactor tussen 1 en 31 -> beide regs moeten schuiven
		rsb r3, r2, #32
		orr r0, r0, r1, lsr r3		@ orr r0 met (32-r2) naar rechts geschifte r1
		mov r1, r1, lsl r2			@ shift r1 met r2 naar links

		b myuddiv_ready_denorm
		
	myuddiv_only_lo_to_hi:
		sub r2, r2, #32
		mov r0, r1, lsl r2
		mov r1, #0					@ en klaar

	myuddiv_ready_denorm:			@ nu r0=hi_schat, r1 =lo_schat - functions!

	@ ******************************* step 1 Newton - pakweg 10c
	@ top is hi_den, [dts]=lo_den, r0=hi_schat, r1=lo_schat
	@ nodig is een 64bit multiply van schatting r0hi, r1lo met denominator tophi, r2lo

	@ 1st Newton iteration follows: dit kan 64bits daar schatting * denom niet groter als 2^64 kan zijn
	@ STEP 1 = schatting keer denominator
			
		ldr r2, [dts]
			
		umull	r4, r3, r2, r1		@ low*low r4=lotussen r3=hitussen r2=loden r1=loschat
		mla		r3, r2, r0, r3		@ low*hi r3=hitussen r2=loden, r0=hischat r3=hitussen
		mla		r3, top, r1, r3		@ hi*lo  r3=hi top=hiden r1=loschat  r3=hitussen
									@ functions! maar 64x64=64bit is niet goed genoeg! moet 64*64=65bits zijn (dus de volgende routine)
									@ dit voor nu opgelost door verlagen van schatting met 1
			
	@ ******************************* step 2 Newton - pakweg 18c		
	@ hier schatting * resultaat  uit vorige resultaat in 128 bit resultaat! r7,r8,v,w
	@ shift dat 64 bits to right door midhi en hi te nemen!! 
	@ trek dat af van oorspronkelijke schatting*2 (r0 r1)
	@ r0r1 hilo schatting r3r4 hilo tussenwaarde r2,v,w,r7,r8 vrij

		umull	v, w, r4, r1 		@ low*low->v w r4 r1
		mov		r7, #0
		umlal	w, r7, r4, r0	 	@ low*high->w r7 r4 r0
		mov		r8, #0
		umlal	w, r8, r3, r1		@ high*low->w r8 r3 r1
		mov		r4, #0
		adds	r7, r7, r8
		adc		r8, r4, #0
		umlal	r7, r8, r3, r0 		@ high*high-> r7 r8 r3 r0
									@ v w r7 r8 is resultaat 64x64=128bit van lo naar hi
									@ r7_lo r8_hi is eindresultaat van q*q*d>>64
									@ trek dit 64bit resultaat af vorige schatting van r1 (lo) en r0 (is hi)
									@ v en w nu vrij
									
		subs 	v, r1, r7			@ lo minus lo met flags
		sbc 	w, r0, r8			@ hi minus hi met carry
		
		adds	r1, r1, v
		adc		r0, r0, w			@ r1_lo r0_hi = newton schatting 1 (0-1 bits afrondingsfout mogelijk)
			
	@ klaar 1ste Newton

	@ ******************************* eigenlijke deling - pakweg 18c
	@ quotient=(schatting*nummer)>>64 bits 
		
		ldr r3, [dts, #4]
		ldr r4, [dts, #8]
		
		umull	v, w, r4, r1 		@ low*low->v w r4 r1
		mov		r7, #0
		umlal	w, r7, r4, r0	 	@ low*high->w r7 r4 r0
		mov		r8, #0
		umlal	w, r8, r3, r1		@ high*low->w r8 r3 r1
		mov		r4, #0
		adds	r7, r7, r8
		adc		r8, r4, #0
		umlal	r7, r8, r3, r0 		@ high*high-> r7 r8 r3 r0
									@ v w r7 r8 is resultaat 64x64=128bit van lo naar hi
									@ r7_lo r8_hi is eindresultaat van de deling
			
	@ ******************************* calc remainder - pakweg 20c
	@ bereken remainder -> remainder = nummer-(antwoord*denominator)
	@ antwoord*denominator zou altijd kleiner als 2^64 moeten zijn, 64x64=64bit is voldoende
			
		ldr r3, [dts, #4]			@ r3=hi_num - MOET DIT HIER NOG EEN KEER? Nee, maar scheelt ook geen tijd
		ldr r4, [dts, #8]			@ r4=lo_num - JA! wordt veranderd in routine hierboven

		umull	v, w, r2, r7		@ low*low v=lo_tussen w=hi_tussen r2=lo_den r7=lo_antw
		mla		w, r2, r8, w		@ low*hi w=hitussen r2=lo_den, r8=hi_antw w=hi_tussen
		mla		w, top, r7, w		@ hi*lo  w=hi_tussen top=hi_den r7=lo_antw  w=hitussen
		
		subs 	r0, r4, v			@ lo_remainder = lo_num minus lo_tussen met flags
		sbc 	r1, r3, w			@ hi_remainder = hi_num minus hi_tussen met carry

	@******************************** correct for rounding - pakweg 8c
	@ Correct remainder for rounding errors:
	@ top=hi_den, r2=lo_den, r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
	@ nog checken of het zou kunnen dat antwoord te hoog is!

	myuddiv_correct_remainder:
		ldr r2, [dts]				@ r2=lo_den
	
		cmp top, r1					@ vergelijk hi_den en hi_rem
		bhi	3f						@ als top groter dan gelijk klaar met correctie - (TENZIJ antwoord te hoog is!!)
		blo	4f						@ als top kleiner dan remainder dan correctie modig
		
	@ als hier aangekomen dan hi_den en hi_rem gelijk -> de lows comparen

		cmp r2, r0					@ vergelijk lo_den en lo_rem
		blo 4f						@ als lo_den groter dan correctie nodig

		b 3f						@ als hier dan geen afrondings fout gevonden

	4:	@ Doe een rondje correctie van afrondings-fouten en test dan opnieuw
		@ top=hi_den, r2=lo_den, r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
		@ als afrondings-fout -> verhoog antwoord met 1 en verlaag remainder met denominator
		
		subs 	r0, r0, r2			@ lo_remainder = lo_rem - lo_den inc flags
		sbc 	r1, r1, top			@ hi_remainder = hi_rem - hi_den inc carry
		
		adds	r7, r7, #1			@ lo_ant = lo_ant plus 1 inc flags
		adc		r8, r8, #0			@ hi_ant = hi_ant plus carry
		
		b myuddiv_correct_remainder	@ en kijk of het nu al wel goed is	
	
	@ ******************************* exit Newton - pakweg 5c
	@ r8=hi_ant, r7=lo_ant, r1=hi_rem, r0=lo_rem
		
	3:	str r7, [dts]
		str r1, [dts, #4]
		str r0, [dts, #8]
		mov top, r8
		
		LDMFD 	r13!, {r0-r4, r7, r8, pc}	@ big next

	@ ******************************* div by 0 or 1 - pakweg 8c - hier evt ook div by 2, 3 en 4 hanteren!
	myuddiv_divby0or1:
		ldv16	r3, 0
		
		cmp 	r1, #0
		beq 	myuddiv_by0			@ rem en quot op nul
		
	@ hier deling door 1			@ maak rem=0 en quotient=num
		ldr 	top, [dts, #4]		@ nu top=hi_num
		ldr		r0, [dts, #8]		@ r0=lo_num
		str		r0, [dts]
		str		r3, [dts, #4]		@ hi_rem=0
		str		r3, [dts, #8]		@ lo_rem=0

		LDMFD 	r13!, {r0-r4, r7, r8, pc}	@ big next
		
	myuddiv_by0:					@ zet antwoorden op 0
		str 	r3, [dts]
		str		r3, [dts, #4]
		str		r3, [dts, #8]
		mov		top, r3
		
		LDMFD 	r13!, {r0-r4, r7, r8, pc}	@ big next
	
	@ ******************************* pakweg 15c+5c+bl! (meten)

	myuddiv_32by32:					
		ldr		r1, [dts]			@ r1=lo_den
		ldr 	r2, [dts, #8]		@ r2=lo_num
		
		udiv 	r0, r2, r1			@ r0 - lo-quotient -  hanteert zelf div_by0
		ldv16	r3, 0
		str		r3, [dts, #4]		@ hi-r=0
		mov		top, r3				@ hi-q=0
		mls		r4, r0, r2, r1		@ dit om de r4=lo-remainder te berekenen
		str		r0, [dts]			@ lo-q
		str 	r4, [dts, #8]		@ lo-r

		LDMFD 	r13!, {r0-r4, r7, r8, pc}	@ big next
		
	@ *******************************
	@ r0=hi_schat, r1=lo_schat, top=hi_den, r2=leading zeros-28 (en hier altijd negatief!)

	myuddiv_large_den:				@ als hier dan grote deler, en geen newton nodig! - scheelt ~20c, 87-88c inc bl.
		rsb r2, r2, #0				@ s=-s -> r2=leading zeros nu weer positief!! 	

	@ shiftfactor by large_den altijd tussen 1 en 27 en verkleinen -> right shift -> alleen lo_schat moeten schuiven want hi_schat=0
		mov r1, r1, lsr r2			@ shift r1 met r2 naar rechts!!
		mov r0, #0					@ r0=hi_schat op nul - nu r0=hi_schat, r1 =lo_schat
		
	@ nu resultaat berekenen met schatting in r1, r0 en nummer in r4=lo_num en r3=hi_num
		ldr r4, [dts, #8]
		ldr r3, [dts, #4]

		umull	v, w, r4, r1 		@ low*low->v,w=r4*r1
		mov		r7, #0				@ r7=0
		umlal	w, r7, r4, r0	 	@ low*high->w,r7=(r4*r0)+w,r7
		mov		r8, #0				@ ->r8
		umlal	w, r8, r3, r1		@ high*low->w r8 r3 r1
		mov		r4, #0				@ r4
		adds	r7, r7, r8			@ ->r7 r7 r8
		adc		r8, r4, #0			@ ->r8 r4
		umlal	r7, r8, r3, r0 		@ high*high-> r7 r8 r3 r0
									@ v w r7 r8 is resultaat 64x64=128bit van lo naar hi
									@ r7_lo r8_hi is eindresultaat - resultaat 0-1 te laag, is voldoende
									
	@ ********************************** calc remainder - pakweg 20c
	@ bereken remainder -> remainder = nummer-(antwoord*denominator)
	@ antwoord*denominator zou altijd kleiner als 2^64 moeten zijn -> 64x64=64bit is voldoende

		ldr r1, [dts]				@ [dts]=lo_den - wordt hi_rem
		ldr r3, [dts, #4]			@ r3=hi_num - MOET DIT HIER NOG EEN KEER? Nee, maar spaart ook geen tijd
		ldr r4, [dts, #8]			@ r4=lo_num - wordt veranderd in routine hierboven

		umull	v, w, r1, r7		@ low*low v=lo_tussen w=hi_tussen r1=lo_den r7=lo_antw
		mla		w, r1, r8, w		@ low*hi w=hitussen r1=lo_den, r8=hi_antw w=hi_tussen
		mla		w, top, r7, w		@ hi*lo  w=hi_tussen top=hi_den r7=lo_antw  w=hitussen
		
		subs 	r0, r4, v			@ r0=lo_remainder = lo_num minus lo_tussen met flags
		sbc 	r1, r3, w			@ r1=hi_remainder = hi_num minus hi_tussen met carry - 0-1*den te hoog, is voldoende

		b myuddiv_correct_remainder
.ltorg
.text		@}

wordhead "*/", 2, MYUDDIVMOD, 1		@ ( n m o -- q ) n*m/o=q met een 64 bits tussen getal, en daardoor minder afronden!
STARSLASH:	prolog					@{
		bl	TO_R					@ save o
		ldr w, [dts]				@ haal getal van 1 onder de top
		smull w, top, w, top		@ dit is zoooo cool! w wordt low, top wordt hi
		bl	FROM_R					@ haal o terug
		bl	STOD					@ maak een double van o
		bl	DDIVMOD					@ nog niet goed!! MOET signed zijn!!
		bl	TWOSWAP
		bl	TWODROP
		bl	DTOS
next		@ CHECK!!!!}
@}

@  *********************  classic math routines  *******************{

codehead "+", 1, STARSLASH, 2, 1 	@ ( n m -- n+m ) 4c - 2c inline
_ADD:	ldr w, [dts], #4			@ haal getal van 1 onder de top en verlaag stack met 1 pos{
		add	top, top, w
codenext	@ checked}

codehead "-", 1, _ADD, 2, 1			@ ( n m -- n-m ) 4c - 2c inline
_SUB:	ldr w, [dts], #4			@ haal getal van 1 onder de top en verlaag stack met 1 pos{
		sub	top, w, top
codenext	@ checked}

codehead "d-", 2, _SUB, 6, 1		@ ( n_lo n_hi m_lo m_hi -- n+m_lo n+m_hi ) add 64b+64b naar 64b
_DSUB:								@ pi3 8c - 7c{
		ldr v, [dts], #4			@ getal van 1 onder top, en verschuif dts 1 pos
		ldr w, [dts, #4]			@ getal van 3 onder de top
		subs w, w, v
		ldr v, [dts], #4			@ getal van 2 onder de top, en verschuif dts 1 pos
		sbc top, top, v				@ top gelijk goed
		str w, [dts]				@ en sla n+m_lo 1 pos onder top op
codenext	@ TESTEN!!!!!}

codehead "d+", 2, _DSUB, 6, 1		@ ( n_lo n_hi m_lo m_hi -- n+m_lo n+m_hi ) 64b+64b->64b
_DADD:								@ pi3 8c - 7c{
		ldr v, [dts], #4			@ getal van 1 onder top, en verschuif dts 1 pos
		ldr w, [dts, #4]			@ getal van 3 onder de top
		adds w, w, v
		ldr v, [dts], #4			@ getal van 2 onder de top, en verschuif dts 1 pos
		adc top, top, v				@ top gelijk goed
		str w, [dts]				@ en sla n+m_lo 1 pos onder top op
codenext	@ TESTEN!!!!!}

codehead "*", 1, _DADD, 2, 1			@ ( n m -- n*m ) 5c - 4c inline -> invloed latentie!!
STAR:	ldr w, [dts], #4			@ haal getal van 1 onder de top en verlaag stack met 1 pos{
		mul	top, w, top
codenext	@ checked}

codehead "d*", 2, STAR, 10, 1		@ ( nlo nhi mlo mhi -- plo phi ) inlinebaar - maar scheelt maar 1c!
DSTAR:								@ pi3: 16-15c (pfft...) 64bx64b naar 64b{
		stmfd r13!, {r0-r2}			@ = specifieke prolog, kost 2x2 c!!

		ldr w, [dts], #4			@-> w=mlo
		ldr v, [dts], #4			@-> v=nhi
		ldr r2, [dts]				@-> r2=nlo
		mov r1, top					@-> r1=mhi
		
		umull r0, top, r2, w		@ plo phi nlo mlo
		mla top, r2, r1, top		@ phi nlo mhi phi
		mla top, v, w, top			@ phi nhi mlo phi - top hiermee gelijk goed
		
		str r0, [dts]				@ zet plo 1 positie onder de top vd stack
		
		ldmfd r13!, {r0-r2}			@ = specifieke next
codenext	@ lijkt goed...}

wordhead "umd*", 4, DSTAR, 1		@ ( b_0 b_1 c_0 c_1 -- a_0 a_1 a_2 a_3 )
UMDSTAR:							@ pi3: 25c u64bxu64b=u128b!!{

		@ a_0	r0  naar dts+8 kan na eerste UMULL al naar dts+8
		@ a_1	r1  naar dts+4
		@ a_2	r2  naar dts
		@ a_3	r5  =top

		@ b_0	r3  uit dts+8
		@ b_1	r4  uit dts+4
		@ c_0	r11 uit dts
		@ c_1	r0 mov uit top
	
		stmfd   sp!, {r0-r4, r11, lr}

		ldr 	r3, [dts, #8]		@ dts+8 > r3=b_0
		ldr 	r11, [dts]			@ dts   > r11=c_0

		umull	r0, r1, r3, r11 	@ r0 r1 r3 r11 :: low*low
		str 	r0, [dts, #8]		@ r0=a_0> dts+8 :: kan want dts+8 is al naar b_0=r3
		mov		r2, #0				@ r2
		mov 	r0, top				@ top > r0=c_1
	
		umlal	r1, r2, r3, r0	 	@ r1 r2 r3 r0 :: low*high
		mov		top, #0				@ top
		ldr 	r4, [dts, #4]		@ dts+4 > r4=b_1

		umlal	r1, top, r4, r11	@ r1 top r4 r11 ::  high*low
		mov		r3, #0				@ r3
		str 	r1, [dts, #4]		@ r1=a_1 > dts+4
		adds	r2, r2, top			@ r2 r2 top
		adc		top, r3, #0			@ top r3
 	
		umlal	r2, top, r4, r0 	@ r2 top r4 r0	:: high*high
		str 	r2, [dts]			@ r2=a_2 naar dts
									@ r5=top=a3 is al goed 
		ldmfd 	sp!, {r0-r4, r11, pc}
.ltorg
.text		@ CHECK - lijkt te werken}

codehead "um*", 3, UMDSTAR, 3, 1		@ ( n m -- d:n*m ) unsigned 7c - 4c! inline
UMSTAR:	ldr w, [dts]				@ haal getal van 1 onder de top{
		umull w, top, w, top		@ dit is zoooo cool! w wordt low, top wordt hi
		str w, [dts]				@ en store low word 1 onder top
codenext	@ checked - stack: low high}

codehead "m*", 2, UMSTAR, 3, 1		@ ( n m -- d:n*m ) signed 7c - 4c! inline
MSTAR:	ldr w, [dts]				@ haal getal van 1 onder de top{
		smull w, top, w, top		@ dit is zoooo cool! w wordt low, top wordt hi
		str w, [dts]				@ en store low word 1 onder top
codenext	@ checked - stack: low high}

codehead "s>d", 3, MSTAR, 4, 1		@ ( n -- d:n_lo d:n_hi ) convert single number to double
STOD:	str top, [dts, #-4]!  		@ top op stack{
		cmp top, #0
		movge top, #0
		mvnlt top, #0
codenext	@ checked}

codehead "d>s", 3, STOD, 1, 1		@ ( d:n_lo d:n_hi -- n ) convert double number to single
DTOS:								@ dit is dus gewoon een drop!!! werkt alleen bij signed 32 bits getallen!!{
		ldr top,[dts], #4
codenext	@ CHECK!}

codehead "1+", 2, DTOS, 1, 1		@ ( n -- n+1 ) 5c - 0-1c! inline -> zelfs tot 0c kort!
ONEPLUS:
		add top, top, #1			@ verhoog top met 1
codenext	@ checked				@ 2c sneller als "lit16 1 en bl _ADD" en 3c sneller als inlines

codehead "4+", 2, ONEPLUS, 1, 1		@ ( n -- n+4 ) zie boven
FOURPLUS:
		add top, top, #4			@ verhoog top met 4
codenext	@ checked

codehead "1-", 2, FOURPLUS, 1, 1	@ ( n -- n-1 ) zie boven
ONESUB:	sub top, top, #1			@ verlaag top met 1
codenext	@ checked

codehead "4-", 2, ONESUB, 1, 1		@ ( n -- n-4 ) zie boven
FOURSUB:
		sub top, top, #4			@ verlaag top met 4
codenext	@ checked

codehead "max", 3, FOURSUB, 3, 1	@ ( n m -- grootste van beide )
MAX:	ldr w, [dts], #4			@ 6-7c - 3c{
		cmp w, top
		movgt top, w				@ als w signed groter dan top dan: w naar top
codenext	@ checked}

codehead "dmax", 4, MAX, 11, 1		@ ( dn dm -- grootste van beide ) - inlinen brengt maar 11-12%!
DMAX:								@ pi3: 8-9c - 7-8 inc bl! (12-15c als word ipv code!) ik verwachtte 13-14 - 7-8 voor code{
		ldr w, [dts], #4			@ w=dm_lo - top=nog steeds dm_hi w
		ldr v, [dts], #4			@ v=dn_hi - [dts]=hierna dn_lo - stack nu goed

		teq v, top					@ als beide gelijk...
		beq 2f						@ jump naar volgende deel

		cmp v, top					@ eerst het meest relevante deel vergelijken v
		
		strlt w, [dts]				@ als v=dn_hi signed kleiner dan top=dm_hi dan mov w=dm_lo naar [dts] of...
		movgt top, v				@ als top signed groter dan v dan: v=dn_hi naar top en [dts] is al goed
		
		b 1f						@ klaar!

	@ als hier dan most-significant woord gelijk -> top is dus al goed -> check less-significant words
	2:	ldr v, [dts]				@ v=nu dn_lo

		cmp v, w					@ compare w=dm_lo met v=dn_lo
		strlt w, [dts]				@ als v=dn_lo signed kleiner dan w=dm_lo dan: w=dm_lo naar [dts] en...
	1:								@ klaar
codenext	@ CHECK!!}

codehead "min", 3, DMAX, 3, 1		@ ( n m -- kleinste van beide ) - inlinen zinvol!
MIN:	ldr w, [dts], #4			@ 6-7c - 3c{
		cmp w, top
		movlt top, w				@ als w signed kleiner dan top dan: w naar top
codenext 	@ checked}

codehead "dmin", 4, MIN, 11, 1		@ ( dn dm -- kleinste van beide ) - inlinen brengt maar 11-12%!
DMIN:								@ pi3: 8-9c - 7-8 inc bl! (12-15c als word ipv code!) ik verwachtte 13-14 - 7-8 voor code{
		ldr w, [dts], #4			@ w=dm_lo - top=nog steeds dm_hi w
		ldr v, [dts], #4			@ v=dn_hi - [dts]=hierna dn_lo - stack nu goed

		teq v, top					@ als beide gelijk...
		beq 2f						@ jump naar volgende deel

		cmp v, top					@ eerst het meest relevante deel vergelijken v
		
		strgt w, [dts]				@ als v=dn_hi signed groter dan top=dm_hi dan mov w=dm_lo naar [dts] of...
		movlt top, v				@ als top signed kleiner dan v dan: v=dn_hi naar top en [dts] is al goed
		
		b 1f						@ klaar!

	@ als hier dan most-significant woord gelijk -> top is dus al goed -> check less-significant words
	2:	ldr v, [dts]				@ v=nu dn_lo

		cmp v, w					@ compare w=dm_lo met v=dn_lo
		strgt w, [dts]				@ als v=dn_lo signed groter dan w=dm_lo dan: w=dm_lo naar [dts] en...
	1:								@ klaar
codenext	@ CHECK!!}}

@  *****************************  BASE  ****************************{
codehead "hex", 3, DMIN, 4, 1		@ zet varBASE op 16
HEX:								@ pi3:8 - 4c
		ldv32 w, varBASE
		mov v, #16
		str v, [w, #16]
codenext

codehead "decimal", 7, HEX, 4, 1	@ zet varBASE op 10
DECIMAL:							@ pi3:8 - 4c
		ldv32 w, varBASE
		mov v, #10
		str v, [w, #16]
codenext

codehead "binary", 6, DECIMAL, 4, 1	@ zet varBASE op 2
BINARY:								@ pi3:8 - 4c
		ldv32 w, varBASE
		mov v, #2
		str v, [w, #16]
codenext							@ }

@  ************************  print formatted  **********************{

wordhead "<#", 2, BINARY, 1			@ start conversie getal, onthoud sign, maak getal absoluut
LESSSHARP:							@ ( n -- true abs(n) ) sign remembered in varNEG (true is negatief){
		prolog						@ lengte string in varLEN# - opslag string op Trans

		bl	_TRUE
		bl	SWAP					@ nu een true onder het getal

		lit16 0x0
		bl	varNEG
		bl	STORE					@ varNEG start met 0

		cmp top, #0					@ als getal op stack is 0 of groter dan springen
		bge 1f

		lit32 -1
		bl	varNEG
		bl	STORE					@ als getal negatief dan  varNEG is true

	1:	bl	_ABS					@ top nu positief
next		@ checked}

wordhead "#>", 2, LESSSHARP, 1		@ einde conversie getal, zet string addr met lengte in buffer klaar voor 'TYPE'
SHARPGREAT:							@ ( -- addr n ){
		prolog
		bl	DROP					@ haal de pos:n van de stack

		ldv32 r0, Trans				@ r0 is start string
		ldv32 r1, 0					@ counter
		mvn r3, #0					@ =true

	1:	popdts r2					@ r2 nu extop stack
		cmp r2, r3
		beq 2f

		strb r2, [r0, r1]			@ zet char van stack in Trans

		add r1, r1, #1
		b	1b

	2:	pushdts r0					@ addr Trans op stack
		pushdts r1					@ en nu lengte string ook op stack
next		@ checked}

codehead "#>asc", 5, SHARPGREAT, 3, 1 @ ( 0-35 - ascii-code ) doel: conversie binnen nummer naar string
SHARPTOASC:							@{
		add top, #48
		cmp top, #57				@ 57 = '9'
		addhi top, #7
codenext	@ checked}

wordhead "#", 1, SHARPTOASC, 1		@ converteer 1 digit ( pos:n -- asc pos:n/base ) en een digid toegevoegd aan stack
SHARP:	prolog						@{
		bl	varBASE
		bl	FETCH					@ nu BASE op stack

		bl	UDIVMOD					@ deel getal door BASE - rest waarde
		bl	SWAP					@ nu de rest bovenop stack

		bl	SHARPTOASC
		bl	SWAP					@ en de rest, nu als ascii weer terug onder rest
next		@ checked}

wordhead "#s", 2, SHARP, 1			@ converteer digits totdat getal is op, minstens 1 nul
SHARPS:								@ ( pos:n -- ){
		prolog

	1:	bl SHARP
		cmp top, #0
		bne 1b
next		@ checked}

wordhead "hold", 4, SHARPS, 1		@ ( ascii -- ) zet ascii in buffer voor nummer conversie - bijv $ teken
HOLD:	prolog						@{
		bl	SWAP					@ zet ascii onder pos:n
next		@ checked}

wordhead "sign", 4, HOLD, 1			@ ( ascii -- ) zet ascii teken voor sign in buffer voor nummer conversie - plus ook?
SIGN:								@ overwegen om een plussign te hebben dat altijd een sig  geeft behalve bij waarde 0{
		prolog
		bl	varNEG
		bl	FETCH
		cmp top, #0
		beq	1f

		bl	DROP 					@ drop de 'true' die zegt negatief
		lit16 '-'
		bl	SWAP					@ en onder pos:n
		b 2f

	1:	bl	DROP					@ drop de 0 die zegt positief
		lit16 '+'
		bl	SWAP					@ en onder pos:n
	2:
next		@ checked}

wordhead "-sign", 5, SIGN, 1		@ ( ascii -- ) zet ascii teken voor sign in buffer voor nummer conversie - plus ook?
MINSIGN:	prolog					@{
		bl	varNEG
		bl	FETCH
		cmp top, #0
		beq	1f

		bl	DROP 					@ drop de 'true' die zegt negatief
		lit16 '-'
		bl	SWAP					@ en onder pos:n
		b 2f

	1:	bl	DROP					@ drop de 0 die zegt positief

	2:
next		@ checked}}

@  **************************  DOT routines  ***********************{

wordhead "u.", 2, MINSIGN, 1		@ ( n -- )
UDOT:								@ Dit is de essentieele primitive voor 32 bit printing!!!{
		prolog
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack uss:( -- #0 )
	1:	bl varBASE					@ address van BASE op stack
		bl FETCH					@ nu BASE op stack
		bl UDIVMOD					@ modulus en resultaat op datastack ( -- mod, res )
		poppopdts r0, r2			@ r0=resultaat, r2=modulus
		@popdts r0					@ resultaat bewaren in r0 ( -- mod )
		@popdts r2					@ modulus in r2 ( -- )
		cmp r2, #9
		addgt r2, r2, #7			@ tel 7 op bij r2 als r2 groter is dan 9 (maw 'A' is)
		add r2, r2, #'0'			@ maak ASCII van modulus
		pushuss r2					@ en push r2 on user stack uss:( -- #0, char )
		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel
		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling
	2:	popuss r2					@ haal chars van userstack in r2
		cmp r2, #0					@ kijk of char al NUL is
		beq 3f						@ als nul dan klaar
		pushdts r2
		bl EMIT
		b 2b						@ loopen tot getal omgezet en geprint
3:
next		@ checked}

wordhead "u.dec", 5, UDOT, 1		@ ( n -- )
OUTDEC:	prologmax					@ {
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack uss:( -- #0 )

	1:	lit16 10					@ push 10 op datastack, is de BASE ( n, 10 -- )
		bl UDIVMOD					@ modulus en resultaat op datastack ( -- mod, res )
		poppopdts r0, r2			@ r0=resultaat, r2=modulus
		@popdts r0					@ resultaat bewaren in r0 ( -- mod )
		@popdts r2					@ modulus in r2 ( -- )
		
		cmp r2, #9
		addgt r2, r2, #7			@ tel 7 op bij r2 als r2 groter is dan 9 (maw 'A' is)
		add r2, r2, #'0'			@ maak ASCII van modulus
		pushuss r2					@ en push r2 on user stack uss: ( -- #0, char )

		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel

		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling

	2:	popuss r2					@ haal chars van userstack in r2
		cmp r2, #0					@ kijk of char al NUL is
		beq 3f						@ als nul dan klaar
		pushdts r2
		bl EMIT
		b 2b						@ loopen tot getal omgezet en geprint
3:
nextmax		@ checked}

wordhead "du.dec", 6, OUTDEC, 1		@ ( dn -- )
OUTUDDEC:							@ print double getal in decimal format{
		prologmax
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack uss:( -- #0 )

	1:	lit32 10					@ push 10 op datastack, is de BASE ( dn d10 -- )
		bl STOD

		bl UDDIVMOD					@ modulus en resultaat op datastack ( -- mod_lo mod_hi res_lo res_hi )

		poppopdts r0, r3			@ r0=res_hi, r3=res_lo ( -- mod_lo mod_hi )
		@popdts r0					@ res_hi in r0
		@popdts r3					@ res_lo in r3 ( -- mod_lo mod_hi)

		poppopdts r2, r1			@ r2=mod_hi, r1=mod_lo ( -- )
		@popdts r2					@ mod_hi in r2
		@popdts r1					@ mod_lo in r1 ( -- )

		cmp r1, #9
		addgt r1, r1, #7			@ tel 7 op bij r2 als r2 groter is dan 9 (maw 'A' is)
		add r1, r1, #'0'			@ maak ASCII van modulus

		pushuss r1					@ en push r1 on user stack uss: ( -- #0, char )

		cmp r3, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel

		pushdts r3					@ res_lo op stack
		pushdts r0					@ res_hi op stack
		b 1b						@ en loop naar volgende deling

	2:	popuss r2					@ haal chars van userstack in r2
		cmp r2, #0					@ kijk of char al NUL is
		beq 3f						@ als nul dan klaar
		pushdts r2
		bl EMIT
		b 2b						@ loopen tot getal omgezet en geprint
3:
nextmax		@ checked}

wordhead ".dec", 4, OUTUDDEC, 1		@ ( signed n -- unsigned n (en dan nar OUTDEC) )
OUTSNDEC:							@{
		prologmax
		cmp top, #0
    	bge 1f
    	lit16 '-'
    	bl EMIT
		rsb top, top, #0
	1:  bl	OUTDEC
		bl	SPACE
nextmax		@ checked!!! yeah!!}

wordhead ".", 1, OUTSNDEC, 1		@ ( signed n -- )
OUTNUM:	prologmax					@{
		cmp top, #0
    	bge 1f
    	lit16 '-'
    	bl EMIT
		rsb top, top, #0
	1:  bl	UDOT
		bl	SPACE
nextmax		@ CHECK}

wordhead ".ns", 3, OUTNUM, 1		@ ( signed n -- )
OUTNUMNS:	prologmax				@{
		cmp top, #0
    	bge 1f
    	lit16 '-'
    	bl EMIT
		rsb top, top, #0
	1:  bl	UDOT
nextmax		@ CHECK}

wordhead ".ddec", 5, OUTNUMNS, 1	@ ( signed d-lo signed d-hi -- )
OUTDDEC:							@ print signed 64 bit{
		prologmax

		cmp top, #0					@ top = hi-d
    	bpl 1f						@ jump if zero or greater

    	lit16 '-'					@ print een min
    	bl EMIT						@

		bl	DNEGATE					@ flip getal

	1:  bl	OUTUDDEC				@ print getal
		bl	SPACE
nextmax		@ CHECK}

wordhead ".hex", 4, OUTDDEC, 1
OUTHEX:	prologmax					@{
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack
	1:	lit16 16					@ push 16 op datastack, is de BASE
		bl UDIVMOD					@ modulus en resultaat op datastack
		popdts r0					@ resultaat bewaren in r0
		popdts r2					@ modulus in r2
		cmp r2, #9
		addgt r2, r2, #7			@ tel 7 op bij r2 als r2 groter is dan 9 (maw 'A' is)
		add r2, r2, #'0'			@ maak er ASCII van
		pushuss r2					@ en push on user stack
		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel
		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling
	2:	popuss r1					@ haal chars van userstack in r1
		tst r1, r1					@ kijk of char NUL is
		beq 3f						@ als nul dan klaar
		pushdts r1
		bl EMIT						@ char in r1 voor WriteChar
		b 2b						@ loopen tot getal omgezet en geprint
	3:	bl	SPACE
nextmax		@ checked}

wordhead ".hexns", 6, OUTHEX, 1
OUTHEXNS:	prologmax				@{
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack
	1:	lit16 16					@ push 16 op datastack, is de BASE
		bl UDIVMOD					@ modulus en resultaat op datastack
		popdts r0					@ resultaat bewaren in r0
		popdts r2					@ modulus in r2
		cmp r2, #9
		addgt r2, r2, #7			@ tel 7 op bij r2 als r2 groter is dan 9 (maw 'A' is)
		add r2, r2, #'0'			@ maak er ASCII van
		pushuss r2					@ en push on user stack
		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel
		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling
	2:	popuss r1					@ haal chars van userstack in r1
		tst r1, r1					@ kijk of char NUL is
		beq 3f						@ als nul dan klaar
		pushdts r1
		bl EMIT
		b 2b						@ loopen tot getal omgezet en geprint
	3:
nextmax		@ checked}

wordhead ".bin", 4, OUTHEXNS, 1		@ print getal binair zonder leading zeros
OUTBIN:								@{
		prologmax
		mov	r1, #0
		pushuss	r1					@ push NULL on userstack
	1:	lit16 2						@ push 2 op datastack, is de BASE
		bl UDIVMOD					@ modulus en resultaat op datastack
		popdts r0					@ resultaat bewaren in r0
		popdts r2					@ modulus in r2
		ADD r2, r2, #'0'			@ maak er ASCII van
		pushuss r2					@ en push on user stack
		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel
		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling
	2:	popuss r1					@ haal chars van userstack in r1
		tst r1, r1					@ kijk of char NUL is
		beq 3f						@ als nul dan klaar
		pushdts r1
		bl	EMIT
		b 2b						@ loopen tot getal omgezet en geprint
3:		bl	SPACE
nextmax		@ checked}

wordhead ".32bin", 6, OUTBIN, 1		@ print 32bits getal inclusief leading zeros
OUT32BIN:	prologmax				@{
		mov	r1, #0
		mov r4, #4
		mov r3, #0					@ counter van aantal chars
		pushuss	r1					@ push NULL on userstack

	1:	lit16 2						@ push 2 op datastack, is de BASE voor bin
		bl UDIVMOD					@ modulus en resultaat op datastack
		popdts r0					@ resultaat bewaren in r0
		popdts r2					@ modulus in r2
		ADD r2, r2, #'0'			@ maak er ASCII van
		pushuss r2					@ en push on user stack
		cmp r0, #0					@ resultaat al nul?
		beq 2f						@ zo ja dan naar print-deel
		add r3, r3, #1				@ en tel het aantal chars
		pushdts r0					@ push r0 terug op stack
		b 1b						@ en loop naar volgende deling

	2:	ldr r0, =31
		sub r3, r0, r3				@ 32 - r3 leading zeros nodig

	5:	cmp r3, #0
		beq 4f
		lit16 '0'
		bl	EMIT

		subs r4, r4, #1
		bne 6f						@ als 0
		lit16 ' '
		bl	EMIT
		mov r4, #4

	6:	sub r3, r3, #1
		b 5b

	4:	popuss r1					@ haal chars van userstack in r1
		tst r1, r1					@ kijk of char NUL is
		beq 3f						@ als nul dan klaar
		pushdts r1
		bl	EMIT

		subs r4, r4, #1
		bne 4b						@ als geen nul dan gewoon verder

		lit16 ' '
		bl	EMIT
		mov r4, #4

		b 4b						@ loopen tot getal omgezet en geprint
	3:
nextmax		@ checked}

wordhead ".64bin", 6, OUT32BIN, 1	@ print 64bits getal inclusief leading zeros
OUT64BIN:	prolog					@{
		bl	OUT32BIN
		prchar ' '
		bl	OUT32BIN
next		@ CHECK}

wordhead ".ok", 3, OUT64BIN, 1
PROK:								@{
		prologmax
		bl CR
		prchar '<'
		bl DEPTH
		bl OUTNUM
		bl USDEPTH
		bl OUTNUMNS
		prchar '>'
		prchar ' '
		prchar 'o'
		prchar 'k'
		prchar ' '
nextmax		@ checked}}

@  ******************************  CLS  ****************************{
wordhead "wincls", 6, PROK, 1		@ ( win# -- ) clears win# and resets cursor to 0,0 position
WINCLS:	prolog
		mov r0, top					@ r0 nu win#, en win# nog op stack
		bl	CLEARWIN				@ window nu gecleared en stack weer ok
		
	@ check tegen aantal windows
 		getvalvar r2, varWINNO		@ r2 is nu aantal windows dat maximaal toegestaan is in het systeem
		
		cmp r0, r2					@ deze vergelijk is nodig omdat wincls ook door de gebruiker kan worden gecalled
		bhs 9f						@ als r0=win# (unsigned) groter of gelijk aan aantal windows dan klaar - #NONCRIT

	@ get base address win#
		pushdts r0					@ address win# op stack-  #OPTIMIZE door macro te maken, scheelt een push/pop en een bl
		bl	WINADRTO				@ nu base-address correcte regel in WinTable op stack
		popdts r2					@ r2 nu base-address correcte regel

	@ reset varOUT en varROW van win#
		ldv16 r3, pxvarout
		ldv16 r1, 0
		str r1, [r2, r3]			@ varOUT=x van window nu op 0

		ldv16 r3, pxvarrow
		str r1, [r2, r3]			@ varROW=y van win# nu ook op 0
	9:
next		@ #CHECK

wordhead "uartcls", 7, WINCLS, 1	@ ( -- ) clears UART terminal, resets varOUT
UARTCLS: prolog

		lit16	0x1b				@ escape char
		bl	WRITECHAR
		lit16	0x5b				@ '['
		bl	WRITECHAR
		lit16 '2'
		bl	WRITECHAR
		lit16	'J'
		bl	WRITECHAR
		
		lit16 0
		bl	varOUT					@ OUT variabele gaat omhoog bij EMIT en naar 0 bij CLS en CR
		bl	STORE					@ zet 0 in OUT
next		@ #CHECK

wordhead "cls", 3, UARTCLS, 1		@ clears a window and/or terminal-screen - nu STREAM-compatible
CLS:	prolog

		bl	STREAMS					@ ( -- win#/-1 UARTdis/0 ) - finds the correct win# and UART-flag for the calling task
		poppopdts r0, r1			@ r0=UARTen-flag: 0=print op UART, 1=disable print op UART	
									@ r1=win# enable flag: -1=disable print op window
		cmp r0, #0					@ r0=UART flag ongelijk aan 0 (=true) -> dan geen printen op UART
		bne 1f						@ #UPDATE naar 0=geen printen ipv true is geen printen

		bl	UARTCLS

	1:	cmp r1, #-1					@ -1=no printing on window
		beq 2f						@ gelijk klaar

		pushdts r1					@ address win# op stack 
		bl	WINCLS
next		@ #CHECK}

@  *************************** GPIO words **************************{
wordhead "setfuncgpio", 11, CLS, 1	@ ( function# gpio# -- )  geen code nodig, functie loopt niet erg vaak
SETFUNCGPIO:						@ ( zet de functie voor een gpio poort op function# ) pi3: 492c!!{
		prolog
		ldr r0, [dts], #4			@ r0 is function# - top is gpio#

		cmp top, #53
		cmpls r0, #7
		bhi 1f						@ return als input buiten range

		ldv32 r2, 0x3F200000		@ is start-addres gpio regs in pi2 en pi3

	2:	cmp top, #9					@ als top>9
		subhi top, top, #10			@ verlaag top met 10
		addhi r2, r2, #4			@ en verhoog gpio address met 4
		bhi 2b						@ en probeer nog een keer

		add top, top, top, lsl #1	@ top=top*3

		lsl r0, r0, top				@ shift function# left met aantal bits in top -> 0 bits voor top=0, 3 bits voor top=3 etc

		mov r3, #7					@ r3 nu 0x111 = mask
		lsl r3, r3, top				@ r3=mask nu in zelfde plek als functie in r0=function#
		mvn r3, r3					@ nu dus 000 in dezelfde positie als functie in r0

		ldr top, [r2]				@ top nu de aanwezige data op r2=gpio-address

		and top, top, r3			@ de 3 bits voor de funcite van deze gpio-pin nu gecleared
		orr r0, r0, top				@ r0=function bits ge-orred met oude data
		str r0,	[r2]				@ en zet dit terug in het register

	1:	ldr top, [dts], #4			@ is inline-drop - stack weer goed
next		@}

codehead "setgpio", 7, SETFUNCGPIO, 0, 1	@ ( true/false gpio# -- ) Speed-relevant!! inlinen irrelevant max=15Mhz
SETGPIO:							@ set or clear de gpio# - een pin-specifieke routine is korter maar NIET sneller!!{
		pushuss r4
		
		ldr v, [dts], #4			@ v=waarde - top=gpio# - w=pinbank

		cmp top, #53				@ SETGPIO testen met GPIO 4, is de tegenover middelste UART0 draden
		bhi 1f						@ bij een signed number is een unsigned test op hoger gelijk ook een test op niet kleiner dan nul

			@lsr w, top, #5				@ w=pinbank -> hier 5 bits naar rechts -> onthoudt bit 5
			@lsl w, w, #2				@ die nu bit 2 wordt (gaaf!)

		ldv32 r4, 0x3F200000		@ is start-addres gpio regs in pi2 en pi3

			@add r4, r4, w				@ r4 is start-address plus pinbank na shiften

			teq top, #32				@ is bit 5 gezet?
			addeq r4, r4, #4			@ zo ja dan het address met 4 verhogen om zo de hogere bank te gebruiken

		and top, top, #31			@ neem de onderste 5 bits van top
		mov w, #1					@ w=nu SetBit
		lsl w, w, top				@ en SetBit op zijn plek gezet via leftshift

		cmp v, #0
		streq w, [r4, #40]			@ reset pin òf...
		strne w, [r4, #28]			@ set pin
		
		popuss r4

	1:	ldr top, [dts], #4			@ is inline-drop - stack weer goed
codenext	@ CHECK - mn de selectie van de hogere bank!! Lijkt goed te zijn!}
@}

@  ******************************  CR  *****************************{
wordhead ".dbcr", 5, SETGPIO, 1
DOTDBCR:	prolog

		getvalvar r0, varDEBUG
		cmp r0, #0					@ als debug 0, dan geen debug printen
		beq	1f

		bl	CR
	1:
next		@ checked

wordhead "uartcr", 6, DOTDBCR, 1
UARTCR:		prolog
		lit16 0
		bl	varOUT					@ varOUT variabele gaat omhoog bij EMIT en terug op 0 bij CR
		bl	STORE					@ zet 0 in varOUT
	
		lit16 0x0D
		bl	WRITECHAR
		lit16 0x0A					@ line feed
		bl	WRITECHAR
next

.section .rodata
EndDict:	@ EndDict is de meest recente link in de Dictionary, zoeken naar woorden vanaf hier
.text

wordhead "cr", 2, UARTCR, 1			@ ( -- ) =core&task-aware CR - does a CR on a task-specific win# & UART
CR:		prolog
		bl	STREAMS					@ ( -- win#/-1 UARTdis_flag ) - finds the correct win# and UARdis_flag for the calling task
		popdts r0					@ r0=UARTdis_flag: 0=print op UART, 1=disable print op UART	
									
		cmp r0, #0					@ r0=UART flag ongelijk aan 0 (=true) -> dan geen printen op UART
		bne 1f						@ #UPDATE naar 0=geen printen ipv true is geen printen

		bl	UARTCR

	1:	popdts r1					@ r1=win# enable flag: -1=disable print op window
		cmp r1, #-1					@ -1=no printing on window
		beq 2f						@ gelijk klaar

		pushdts r1					@ address win# op stack 
		bl	WINCR
	2:
next		@ checked}

@  ***********************  einde source-code  *********************{
.section .rodata
FreeRam:
.section .bss	@}

@  *******************  DATA STRUCT FRAME BUFFER  ******************{
.align 4									@ bit[3:0] moeten 0 zijn voor mailbox-data gebied
FB_INFO:				.skip 40			@ bijv 1024 #00 Physical Width
											@ bijv 768  #04 Physical Height
											@ bijv 1024 #08 Virtual Width
											@ bijv 768  #12 Virtual Height
											@ 0			#16 door GPU ingevuld - Pitch=bytes per lijn
											@ bijv 16   #20 Bit Depth
											@ 0	   		#24 X-offset framebuffer naar screen, hier altijd 0
											@ 0    		#28 Y-offset framebuffer naar screen, hier altijd 0
											@ 0    		#32 door GPU ingevuld - Pointer naar buffer
											@ 0    		#36 door GPU ingevuld - Size van buffer
						.skip 64			@ buffertje voor als de gpu meer schrijft

.align 4									@ bit [3:0] moeten 0 zijn voor mailbox-data gebied
@}

@  ***********************  PRIMITIVE VARS  ************************{

varcntvoff:				.skip 8				@ store van actual offset van virtual counter - CNTVOFF - will nog niet!

@ het volgende is nu omgebouwd in WINWRITEON en kan weg
@ volgende vars worden gevuld door "writeon" - dit ter verhogen prestaties van teken-routines
@ -> gebruikt door de primitieve tekenroutines voor/op writeon window
@ "writeon" doet alle calcs en eventuele checks, zodat dit niet door de tekenroutines hoeft worden gedaan

pxwobase:				.skip 4				@ base address write on window - inclusief rand!
pxwohix:				.skip 4				@ is size x window min 2*rand - grens van tekenen
pxwohiy:				.skip 4				@ is size y window min r*rand - grens van tekenen
pxwopitch:				.skip 4				@ pitch van window - zelfde als de var maar sneller te bereiken - nodig voor tekenen in window
pxwoink:				.skip 4				@ kleur ink van het window
pxwocanvas:				.skip 4				@ kleur canvas van het window

@ voor boodschappen van en naar cores 1-3 naar core 0
postbus1_0:				.skip 8				
postbus2_0:				.skip 8
postbus3_0:				.skip 8
postbus0_1:				.skip 8				@ en omgekeerd
postbus0_2:				.skip 8
postbus0_3:				.skip 8

fastrndmseed:			.word 0x80706050	@ primitieve variabele voor fastrndm met een soort seed

pxcounter:				.word 0				@ for core1 PIXEL routine, counts number of activations by IRQ0
pxwin2buf:				.skip 4				@ flag to signal core1 to copy windows to buffer
pxwritten:				.skip 4				@ flag core1 uses to signal that windows are written
pxswitchscr:			.skip 4				@ flag to core1 to switch screens

wvcounter:				.word 0				@ counter to check/debug WAVE
wvcmpwmctl:				.word 0				@ clock manager - pwm control -> use by WAVE

@ systeem-info variabelen ************************

sysinfovars:
sivhandlereset:			.word 0
sivundefined:			.word 0
sivundefinedadr:		.word 0
sivmonitor:				.word 0
sivprefetchabort:		.word 0
sivdataabort:			.word 0
sivdataabortadr:		.word 0
sivirq:					.word 0
sivfiq:					.word 0

sivc0init:				.word 0				@ counts the # of steps done during initiation of core0
sivc1init:				.word 0				@ counts the # of steps done during initiation of core1
sivc2init:				.word 0				@ counts the # of steps done during initiation of core2
sivc3init:				.word 0				@ counts the # of steps done during initiation of core3

sivisc0SCTLR:			.word 0				@ value put in SCTLR in initsys core0
sivisc1SCTLR:			.word 0				@ value put in SCTLR in initsys core1
sivisc2SCTLR:			.word 0				@ value put in SCTLR in initsys core2
sivisc3SCTLR:			.word 0				@ value put in SCTLR in initsys core3

sivisc0CPSR:			.word 0				@ read from CPSR of core0 during initsys
sivisc1CPSR:			.word 0				@ read from CPSR of core1 during initsys
sivisc2CPSR:			.word 0				@ read from CPSR of core2 during initsys
sivisc3CPSR:			.word 0				@ read from CPSR of core3 during initsys

sivisc0SPSRhyp:			.word 0				@ value written in SPSRhyp during initsys of core0
sivisc1SPSRhyp:			.word 0				@ value written in SPSRhyp during initsys of core1
sivisc2SPSRhyp:			.word 0				@ value written in SPSRhyp during initsys of core2
sivisc3SPSRhyp:			.word 0				@ value written in SPSRhyp during initsys of core3

sivisc0ELRhyp:			.word 0 			@ ELRhyp core0 during initsys
sivisc1ELRhyp:			.word 0
sivisc2ELRhyp:			.word 0
sivisc3ELRhyp:			.word 0

sivisc0R13pre:			.word 0				@ reg 13 pre switch to system during initsys of core0
sivisc1R13pre:			.word 0
sivisc2R13pre:			.word 0
sivisc3R13pre:			.word 0

sivisc0R14pre:			.word 0				@ reg 14 pre switch to system during initsys of core0
sivisc1R14pre:			.word 0
sivisc2R14pre:			.word 0
sivisc3R14pre:			.word 0

sivisc0R15pre:			.word 0				@ reg 15 pre switch to system during initsys of core0
sivisc1R15pre:			.word 0
sivisc2R15pre:			.word 0
sivisc3R15pre:			.word 0

sivisc0R13pst:			.word 0				@ reg 13 post switch to system during initsys of core0
sivisc1R13pst:			.word 0
sivisc2R13pst:			.word 0
sivisc3R13pst:			.word 0

sivisc0R14pst:			.word 0				@ reg 14 post switch to system during initsys of core0
sivisc1R14pst:			.word 0
sivisc2R14pst:			.word 0
sivisc3R14pst:			.word 0

sivisc0R15pst:			.word 0				@ reg 15 post switch to system during initsys of core0
sivisc1R15pst:			.word 0
sivisc2R15pst:			.word 0
sivisc3R15pst:			.word 0

sivisc0CPACR:			.word 0				@ write to CPACR of core0 during initsys
sivisc1CPACR:			.word 0
sivisc2CPACR:			.word 0
sivisc3CPACR:			.word 0

sivisc0ACTLRns:			.word 0, 0			@ value written to ACTLRns (=64 bit reg) of core0 during initsys
sivisc1ACTLRns:			.word 0, 0
sivisc2ACTLRns:			.word 0, 0
sivisc3ACTLRns:			.word 0, 0

sivisc0ACTLRs:			.word 0, 0			@ value written to ACTLRs (=64 bit reg) of core0 during secure monitor call
sivisc1ACTLRs:			.word 0, 0
sivisc2ACTLRs:			.word 0, 0
sivisc3ACTLRs:			.word 0, 0

sivisc0CNTKCTL:			.word 0				@ value written to CNTKCTL of core0 during initsys
sivisc1CNTKCTL:			.word 0
sivisc2CNTKCTL:			.word 0
sivisc3CNTKCTL:			.word 0

@ entry vars
sivenc0R13s0:			.word 0				@ value r13 gedurende verschillende stappen in ENTRY
sivenc0R13s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc0R13s2:			.word 0
sivenc0R13s3:			.word 0
sivenc0R13s4:			.word 0
sivenc0R13s5:			.word 0
sivenc0R13s6:			.word 0
sivenc0R13s7:			.word 0

sivenc0R14s0:			.word 0				@ value r14 gedurende verschillende stappen in ENTRY
sivenc0R14s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc0R14s2:			.word 0
sivenc0R14s3:			.word 0
sivenc0R14s4:			.word 0
sivenc0R14s5:			.word 0
sivenc0R14s6:			.word 0
sivenc0R14s7:			.word 0

sivenc1R13s0:			.word 0				@ value r13 gedurende verschillende stappen in init van core1
sivenc1R13s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc1R13s2:			.word 0
sivenc1R13s3:			.word 0
sivenc1R13s4:			.word 0
sivenc1R13s5:			.word 0
sivenc1R13s6:			.word 0
sivenc1R13s7:			.word 0

sivenc1R14s0:			.word 0				@ value r14 gedurende verschillende stappen in init van core1
sivenc1R14s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc1R14s2:			.word 0
sivenc1R14s3:			.word 0
sivenc1R14s4:			.word 0
sivenc1R14s5:			.word 0
sivenc1R14s6:			.word 0
sivenc1R14s7:			.word 0

sivenc2R13s0:			.word 0				@ value r13 gedurende verschillende stappen in init van core2
sivenc2R13s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc2R13s2:			.word 0
sivenc2R13s3:			.word 0
sivenc2R13s4:			.word 0
sivenc2R13s5:			.word 0
sivenc2R13s6:			.word 0
sivenc2R13s7:			.word 0

sivenc2R14s0:			.word 0				@ value r14 gedurende verschillende stappen in init van core2
sivenc2R14s1:			.word 0				@ #DEBUG want er lijkt hier een probleem
sivenc2R14s2:			.word 0
sivenc2R14s3:			.word 0
sivenc2R14s4:			.word 0
sivenc2R14s5:			.word 0
sivenc2R14s6:			.word 0
sivenc2R14s7:			.word 0

@ ***  register-values in the different cores

sivc0R0:				.word 0				@ value in R0 of core0 after call to REGS
sivc0R1:				.word 0
sivc0R2:				.word 0
sivc0R3:				.word 0
sivc0R4:				.word 0
sivc0R5:				.word 0
sivc0R6:				.word 0
sivc0R7:				.word 0
sivc0R8:				.word 0
sivc0R9:				.word 0
sivc0R10:				.word 0
sivc0R11:				.word 0
sivc0R12:				.word 0
sivc0R13:				.word 0
sivc0R14:				.word 0
sivc0R15:				.word 0

sivc1R0:				.word 0				@ value in R0 of core1 after call to REGS
sivc1R1:				.word 0				@ #DEBUG of PIXEL as it crashes after 12 hours or so
sivc1R2:				.word 0				@ this is PIXEL-engine
sivc1R3:				.word 0
sivc1R4:				.word 0
sivc1R5:				.word 0
sivc1R6:				.word 0
sivc1R7:				.word 0
sivc1R8:				.word 0
sivc1R9:				.word 0
sivc1R10:				.word 0
sivc1R11:				.word 0
sivc1R12:				.word 0
sivc1R13:				.word 0
sivc1R14:				.word 0
sivc1R15:				.word 0

sivc2R0:				.word 0				@ value in R0 of core2 after call to REGS
sivc2R1:				.word 0				@ this is WAVE-engine
sivc2R2:				.word 0
sivc2R3:				.word 0
sivc2R4:				.word 0
sivc2R5:				.word 0
sivc2R6:				.word 0
sivc2R7:				.word 0
sivc2R8:				.word 0
sivc2R9:				.word 0
sivc2R10:				.word 0
sivc2R11:				.word 0
sivc2R12:				.word 0
sivc2R13:				.word 0
sivc2R14:				.word 0
sivc2R15:				.word 0

sivc3R0:				.word 0				@ value in R0 of core3 after call to REGS
sivc3R1:				.word 0
sivc3R2:				.word 0
sivc3R3:				.word 0
sivc3R4:				.word 0
sivc3R5:				.word 0
sivc3R6:				.word 0
sivc3R7:				.word 0
sivc3R8:				.word 0
sivc3R9:				.word 0
sivc3R10:				.word 0
sivc3R11:				.word 0
sivc3R12:				.word 0
sivc3R13:				.word 0
sivc3R14:				.word 0
sivc3R15:				.word 0

sysdbvars:
sivdb0:					.word 0				@ temp vars for debugging-purposes
sivdb1:					.word 0
sivdb2:					.word 0
sivdb3:					.word 0

@}

@  **************************  TASKTABLE  **************************{
@ main table to work fast thru the TASK_BLOCK
@ bij elke slice EN bij elke task die teruggeeft wordt deze tabel gebruikt om de volgende task te vinden
@ Deze tabel is daarmee extreem speed-relevant en dus .align 6 en apart van het TASK_BLOCK
@ om zo te verhinderen dat bij elke walk van the table de cache wordt verkloot.

TaskTable:
	.align 6								@ cacheline starts at 64 byte intervals #CHECK of dit werkelijk uitmaakt qua speed
		.skip (MaxTask*TaskTableByte)		@ op dit moment gelijk aan 1024 bytes want 128*16
	.skip 16								@ buffertje
		
@ primitieve variabelen om bij te houden welke taak op welke core loopt
		TaskOnCore0:		.word 0			@ Core0 begint altijd met task 0
		TaskOnCore1:		.word 1			@ Core1 doet roept Pixel - maar nu zonder IRQ!
		TaskOnCore2:		.word -1		@ nog geen task toegewezen
		TaskOnCore3:		.word -1		@ nog geen task toegewezen
	.skip 8
@}

@  ***************************  WINTABLE  **************************{
WinTable:  									@ info-table for windowing system
		.equ max_win, 		3				@ 1 betekent alleen win0, 2 betekent 1 extra window toegestaan etc.
		.equ bytes_win,		124				@ aantal bytes per regel in de window_tabel

	@ dit zijn offsets binnen een regel in de Tabel
		.equ pxmaxx,		0				@ maximale breedte window
		.equ pxmaxy,		4				@ maximale hoogte window
		.equ pxorigx,		8				@ locatie window tov scherm
		.equ pxorigy,		12				@ idem
		.equ pxsizex,		16				@ current size window - kan wel kleiner als maxx maar  nooit groter
		.equ pxsizey,		20				@ idem
		.equ pxcurx,		24				@ positie cursor x
		.equ pxcury,		28				@ idem
		.equ pxbuffer,		32				@ pointer to buffer - tb filled via ALLOCATE
		.equ pxsizebuf,		36				@ size buffer - tb filled via ALLOCATE
		.equ pxbackgr,		40				@ pointer to background buffer - tb filled via ALLOCATE - size same as buffer
		.equ pxpitch,		44				@ aantal bytes per regel vd window - nodig om snel pxwopitch te vullen
		.equ pxmask,		48				@ pointer to mask - tb filled via ALLOCATE
		.equ pxsizemask,	52				@ size mask - to be filled via ALLOCATE - size=size buffer
		.equ pxtext,		56				@ pointer to textbuffer - later - tb filled via ALLOCATE
		.equ pxsizetxt,		60				@ grootte textbuffer in bytes - later - tb filled via ALLOCATE
		.equ pxcurstxt,		64				@ positie cursor in text - later - daar waar nieuwe text heen gaat in buffer
		.equ pxrand,		68				@ size rand rond window - tekenen, schrijven, linefeed en scrollen niet op de rand
		.equ pxink,			72				@ ink for this window - start with white for win0
		.equ pxcanvas,		76				@ canvas color for this window - start with darkblue for win0
		.equ pxfont,		80				@ pointer naar default-font voor een window
		.equ pxspacingx,	84				@ horizontale spacing tussen letter
		.equ pxspacingy,	88				@ verticale spacing tussen letter
		.equ pxcharx,		92				@ aantal chars op regel - met het standard font en rand en spacing
		.equ pxchary,		96				@ aantal regels in window - met het standard font en rand en spacing
		.equ pxvarout,		100				@ varOUT voor een window - is x_pos van textcursor
		.equ pxvarrow,		104				@ which row is text cursor
		.equ px_wobase,		108				@ address eerste pixel to draw -> redo when: make new, new rand, new orig
		.equ px_wohix,		112				@ size x window min 2*rand - grenzen van tekenen -> redo: zie px_wobase
		.equ px_wohiy,		116				@ size y window min r*rand - grenzen van tekenen -> redo: zie px_wobase
		.equ pxflags,		120				@ flags window - elke bit een flag

		.equ pxhidden_f,	1				@ set=not visible
		.equ pxnoscroll_f,	2				@ set=no scroll
		.equ pxnolinef_f,	4				@ set=no linefeed
		@ cannotbemasked - doesnotmask - transparency etc
		@ nog overwegen: positie laatste spatie (voor bij linefeed en afbreken na woord), scrollbar zijkant y/n

	@ dit is win0, de eigenlijke vulling van win0 via SETWINZERO

		.align 6							@ cache-friendly
		.skip (bytes_win*max_win)
		.skip bytes_win						@ buffertje}

@  ******************** DATA STRUCT Pads en TIB ed *****************{

.align 2
@	Pad ed moet nog omgebouwd worden tot taskspecifieke Pads - #UPDATE
		.equ PadLen,		0x200			@ pad om strings te bewerken (door gebruiker, niet voor Assembler!)
		.equ TIBLen,		0x200			@ Input buffer voor Interpreter

		.equ PageTableAdr,	0x4000			@ 16 k Pagetable - direct voor het start adres op 0x8000

@	Tijdelijke stacks
		.equ RetStLen, 		0x200
		.equ DatStLen, 		0x200
		.equ UseStLen, 		0x200
		.equ FloStLen, 		0x200

	@ ******** input buffer, pads en Trans area

		Pad:				.skip PadLen
		PadLim:				.skip 0x10		@ buffer
		PadC1:				.skip PadLen 	@ deze voor core 1
		PadLimC1:			.skip 0x10		@ buffer
		PadC2:				.skip PadLen 	@ deze voor core 2
		PadLimC2:			.skip 0x10		@ buffer
		PadC3:				.skip PadLen	@ deze voor core 3
		PadLimC3:			.skip 0x10		@ buffer

		TIB:				.skip TIBLen	@ maar 1 nuttig
		TIBLim:				.skip 0x10		@ buffer

		Trans:				.skip PadLen	@ Transient area voor string bewerking door standard words
		TransLim:			.skip 0x10
		
	@ ****** stacks voor core1
	
		RetSTLimC1:			.skip RetStLen
		RetStC1:			.skip 0x100
		DatStLimC1:			.skip DatStLen
		DatStC1:			.skip 0x100
		UseStLimC1:			.skip UseStLen
		UseStC1:			.skip 0x100
		FloStLimC1:			.skip FloStLen
		FloStC1:			.skip 0x100

	.align 2
EindeStacks:
.section .text}

@  ****************************  FONTS  ****************************{
NEWFONT:
MyNeoForth: @ nu ook 18 pixels hoog!!
.byte 32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 
.byte 33,4,0,0,6,6,6,6,6,6,6,6,0,0,6,6,0,0,0,0 
.byte 34,6,0,0,54,54,36,18,0,0,0,0,0,0,0,0,0,0,0,0 
.byte 35,8,0,0,0,0,0,72,72,254,36,36,36,127,18,18,0,0,0,0 
.byte 36,8,0,0,20,62,85,21,21,62,84,148,148,148,85,62,20,0,0,0 
.byte 37,8,0,0,0,0,70,105,41,54,24,24,108,148,150,98,0,0,0,0 
.byte 38,8,0,0,0,0,28,38,38,54,28,158,115,99,115,158,0,0,0,0 
.byte 39,4,0,0,6,6,4,2,0,0,0,0,0,0,0,0,0,0,0,0 
.byte 40,4,0,0,8,4,6,2,3,3,3,3,3,3,3,2,6,4,8,0 
.byte 41,4,0,0,1,2,6,4,12,12,12,12,12,12,12,4,6,2,1,0 
.byte 42,6,0,0,0,4,21,14,14,17,0,0,0,0,0,0,0,0,0,0 
.byte 43,8,0,0,0,0,0,0,0,24,24,24,255,24,24,24,0,0,0,0 
.byte 44,4,0,0,0,0,0,0,0,0,0,0,0,0,6,6,4,2,0,0 
.byte 45,8,0,0,0,0,0,0,0,0,0,0,255,0,0,0,0,0,0,0
.byte 46,4,0,0,0,0,0,0,0,0,0,0,0,0,6,6,0,0,0,0
.byte 47,8,0,0,32,32,48,16,24,8,12,4,6,2,3,1,1,0,0,0
.byte 48,8,0,0,60,66,195,195,211,211,203,203,195,195,66,60,0,0,0,0
.byte 49,8,0,0,24,28,30,24,24,24,24,24,24,24,24,126,0,0,0,0              
.byte 50,8,0,0,62,99,193,192,192,96,48,24,12,6,131,255,0,0,0,0
.byte 51,8,0,0,127,97,96,48,24,60,96,192,192,192,99,62,0,0,0,0
.byte 52,8,0,0,104,104,100,100,98,98,97,97,255,96,96,96,0,0,0,0
.byte 53,8,0,0,127,67,3,3,3,63,96,192,192,192,99,62,0,0,0,0          
.byte 54,8,0,0,60,102,2,3,3,63,99,195,195,195,102,60,0,0,0,0
.byte 55,8,0,0,255,193,96,32,48,24,60,24,24,24,24,24,0,0,0,0
.byte 56,8,0,0,60,102,66,66,102,60,102,195,195,195,195,126,0,0,0,0
.byte 57,8,0,0,60,66,195,195,195,195,254,192,192,192,70,60,0,0,0,0
.byte 58,4,0,0,0,0,0,0,0,6,6,0,0,0,6,6,0,0,0,0        
.byte 59,4,0,0,0,0,0,0,0,6,6,0,0,0,6,6,4,2,0,0 
.byte 60,6,0,0,0,0,0,48,24,12,6,3,6,12,24,48,0,0,0,0
.byte 61,7,0,0,0,0,0,0,0,0,127,0,0,127,0,0,0,0,0,0
.byte 62,6,0,0,0,0,0,3,6,12,24,48,24,12,6,3,0,0,0,0
.byte 63,8,0,0,60,126,195,192,192,112,56,24,0,0,24,24,0,0,0,0
.byte 64,8,0,0,0,0,0,60,66,185,165,165,165,165,121,2,60,0,0,0
.byte 65,8,0,0,24,126,102,195,195,195,195,255,255,195,195,195,0,0,0,0                                                                           
.byte 66,8,0,0,31,63,99,99,99,63,127,195,195,195,127,63,0,0,0,0
.byte 67,8,0,0,56,126,198,3,3,3,3,3,3,198,126,56,0,0,0,0
.byte 68,8,0,0,31,127,99,195,195,195,195,195,195,99,127,63,0,0,0,0       
.byte 69,7,0,0,127,127,3,3,3,63,63,3,3,3,127,127,0,0,0,0                 
.byte 70,7,0,0,127,127,3,3,3,63,63,3,3,3,3,3,0,0,0,0                     
.byte 71,8,0,0,60,126,195,3,3,3,243,243,195,195,126,60,0,0,0,0           
.byte 72,8,0,0,195,195,195,195,195,195,255,255,195,195,195,195,0,0,0,0                                                                                   
.byte 73,6,0,0,30,12,12,12,12,12,12,12,12,12,12,30,0,0,0,0               
.byte 74,8,0,0,252,252,192,192,192,192,192,192,192,99,126,60,0,0,0,0                                                                                     
.byte 75,8,0,0,195,99,51,27,15,7,7,15,27,51,99,195,0,0,0,0               
.byte 76,7,0,0,3,3,3,3,3,3,3,3,3,3,127,127,0,0,0,0                       
.byte 77,8,0,0,195,195,231,231,255,219,219,195,195,195,195,195,0,0,0,0                                                                                   
.byte 78,8,0,0,195,195,199,199,203,203,211,211,227,227,195,195,0,0,0,0                                                                                   
.byte 79,8,0,0,60,126,195,195,195,195,195,195,195,195,126,60,0,0,0,0                                                                                     
.byte 80,8,0,0,63,127,195,195,195,127,63,3,3,3,3,3,0,0,0,0               
.byte 81,8,0,0,60,126,195,195,195,195,195,211,243,99,254,188,0,0,0,0                                                                                     
.byte 82,8,0,0,63,127,195,195,195,127,63,15,27,51,99,195,0,0,0,0         
.byte 83,8,0,0,60,126,195,3,3,62,124,192,192,195,126,60,0,0,0,0          
.byte 84,8,0,0,255,255,24,24,24,24,24,24,24,24,24,24,0,0,0,0             
.byte 85,8,0,0,195,195,195,195,195,195,195,195,195,195,126,60,0,0,0,0                                                                                    
.byte 86,8,0,0,195,195,195,231,102,102,102,60,60,60,24,24,0,0,0,0        
.byte 87,8,0,0,195,195,195,195,195,195,219,219,219,219,126,102,0,0,0,0                                                                                   
.byte 88,8,0,0,195,195,102,102,60,60,60,60,102,102,195,195,0,0,0,0       
.byte 89,8,0,0,195,195,195,195,195,126,60,24,24,24,24,24,0,0,0,0         
.byte 90,8,0,0,255,255,192,96,48,24,24,12,6,3,255,255,0,0,0,0            
.byte 91,4,0,0,15,3,3,3,3,3,3,3,3,3,3,3,3,3,15,0                         
.byte 92,6,0,0,1,1,3,2,6,4,12,8,24,16,48,32,32,0,0,0                     
.byte 93,4,0,0,15,12,12,12,12,12,12,12,12,12,12,12,12,12,15,0            
.byte 94,6,0,0,12,30,51,0,0,0,0,0,0,0,0,0,0,0,0,0                        
.byte 95,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,0,0                         
.byte 96,4,0,0,6,6,4,0,0,0,0,0,0,0,0,0,0,0,0,0                           
.byte 97,8,0,0,0,0,0,60,102,192,192,254,195,193,195,254,0,0,0,0          
.byte 98,8,0,0,3,3,3,63,103,195,195,195,195,195,103,59,0,0,0,0           
.byte 99,8,0,0,0,0,0,124,198,3,3,3,3,3,198,124,0,0,0,0                   
.byte 100,8,0,0,192,192,192,252,230,195,195,195,195,195,230,220,0,0,0,0                                                                                  
.byte 101,8,0,0,0,0,0,60,102,195,195,255,3,3,198,124,0,0,0,0             
.byte 102,8,0,0,124,198,6,6,31,6,6,6,6,6,6,6,0,0,0,0                     
.byte 103,8,0,0,0,0,0,252,198,195,195,195,195,195,230,220,192,192,198,124                                                                                
.byte 104,8,0,0,3,3,3,59,111,195,195,195,195,195,195,195,0,0,0,0         
.byte 105,6,0,0,12,12,0,14,12,12,12,12,12,12,12,63,0,0,0,0               
.byte 106,7,0,0,96,96,0,112,96,96,96,96,96,96,96,96,96,96,99,62          
.byte 107,8,0,0,3,3,3,99,51,27,15,15,27,51,99,195,0,0,0,0                
.byte 108,7,0,0,3,3,3,3,3,3,3,3,3,3,99,62,0,0,0,0                        
.byte 109,8,0,0,0,0,0,111,147,147,147,147,147,147,147,147,0,0,0,0        
.byte 110,7,0,0,0,0,0,59,71,199,195,195,195,195,195,195,0,0,0,0          
.byte 111,8,0,0,0,0,0,60,102,195,195,195,195,195,102,60,0,0,0,0          
.byte 112,8,0,0,0,0,0,59,103,195,195,195,195,195,103,59,3,3,3,3          
.byte 113,8,0,0,0,0,0,220,230,195,195,195,195,195,230,220,192,192,192,192                                                                                
.byte 114,8,0,0,0,0,0,119,206,6,6,6,6,6,6,6,0,0,0,0                      
.byte 115,8,0,0,0,0,0,126,195,3,3,126,192,192,195,126,0,0,0,0            
.byte 116,6,0,0,6,6,6,63,6,6,6,6,6,6,198,124,0,0,0,0                     
.byte 117,8,0,0,0,0,0,195,195,195,195,195,195,195,230,220,0,0,0,0        
.byte 118,8,0,0,0,0,0,195,195,195,66,102,36,60,24,24,0,0,0,0             
.byte 119,8,0,0,0,0,0,195,195,195,195,219,90,90,126,102,0,0,0,0          
.byte 120,8,0,0,0,0,0,99,99,34,54,28,28,54,99,99,0,0,0,0                 
.byte 121,8,0,0,0,0,0,195,195,195,195,195,195,195,230,220,192,192,198,124                                                                                
.byte 122,7,0,0,0,0,0,127,97,48,24,8,12,6,67,127,0,0,0,0                 
.byte 123,6,0,0,56,12,4,4,4,4,3,3,4,4,4,4,12,56,0,0                      
.byte 124,4,0,0,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,0           
.byte 125,6,0,0,7,12,8,8,8,8,48,48,8,8,8,8,12,7,0,0                      
.byte 126,8,0,0,0,0,0,0,0,140,90,49,0,0,0,0,0,0,0,0
.byte 127,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                     
.byte 162,8,0,0,0,128,64,124,230,35,19,19,11,11,198,126,2,2,1,0          
.byte 163,8,0,0,56,124,198,6,6,6,31,6,6,6,127,255,0,0,0,0                
.byte 164,6,0,0,0,0,0,33,30,18,18,30,33,0,0,0,0,0,0,0                    
.byte 165,8,0,0,195,195,195,102,102,36,255,60,255,24,24,24,0,0,0,0       
.byte 166,4,0,0,6,6,6,6,6,6,0,0,0,6,6,6,6,6,6,0                          
.byte 167,7,0,0,60,98,3,1,2,30,115,65,103,60,96,64,99,62,0,0             
.byte 168,6,0,0,54,54,0,0,0,0,0,0,0,0,0,0,0,0,0,0                        
.byte 169,8,0,0,0,0,0,60,102,219,165,133,165,219,102,60,0,0,0,0          
.byte 255,8,0,0,60,126,134,3,3,127,3,127,3,3,254,60,0,0,0,0

.align 2

.word 0xffffffff @}

@  ****************************  SINUS  ****************************{

wvwaveforms:
wvsinus6_10: @ dit is een 64 steps 4096 range sinus tussen 0 en 4096
.word 2048, 2249, 2448, 2643, 2832, 3013, 3186, 3347
.word 3496, 3631, 3751, 3854, 3940, 4008, 4057, 4086
.word 4096, 4086, 4057, 4008, 3940, 3854, 3751, 3631
.word 3496, 3347, 3186, 3013, 2832, 2643, 2448, 2249
.word 2048, 1847, 1648, 1453, 1264, 1083,  910,  749
.word  600,  456,  345,  242,  156,   88,   39,   10
.word    0,   10,   39,   88,  156,  242,  345,  465
.word  600,  749,  910, 1083, 1264, 1453, 1648, 1847
.word 2048 @ begin next cycle!! Hier alleen als een buffer tegen afrond-fouten bij indexen}

@  ***************************  MYSOURCE  **************************{

MYSOURCE:

mycode "0 0 uart>task#"
mycode ": dosysvar ( offset -- ) sysinfovars + dup .\" [ \" .hex .\" ] : \" @ dup .32bin .\" = 0x\" .hex cr ;"
mycode "cr .\" testtesttesttesttest \" cr ( words ) "
mycode "cr 4 dosysvar 8 dosysvar cr"
mycode ".\" core init steps (expected: 16 6 6 0): \" sivc0init @4+ . @4+ . @4+ . @4+ . drop cr cr"
mycode ": ?w @4+ .hex ;"
mycode ": .8words ( addres -- ) 16 tab ?w ?w 34 tab ?w ?w 52 tab ?w ?w 70 tab ?w ?w drop cr ;"
mycode ": .4words ( addres -- ) 16 tab ?w 25 tab ?w 34 tab ?w 43 tab ?w drop cr ;"
mycode ": 25tab ( addres -- addres+4 ) 25 tab ?w cr ;"
@
mycode "540 300 1 makewin" @ maak window 1
mycode "75 100 1 >winorig"
mycode "vlcyan 1 >wincanvas 1 clearwin 2 1 >winrand dblue 1 >winink"
mycode "150 150 2 makewin" @ maak window 2
mycode "475 210 2 >winorig"
mycode "vlred 2 >wincanvas 2 clearwin 2 2 >winrand dblue 2 >winink"
@
mycode ".\" CPSR    c0-3: \" sivisc0cpsr .4words "
mycode ".\" SPSRhyp c0-3: \" sivisc0spsrhyp .4words "
mycode ".\" ELRhyp  c0-3: \" sivisc0elrhyp .4words "
mycode ".\" CPACR   c0-3: \" sivisc0cpacr .4words "
mycode ".\" ACTLRns c0-3: \" sivisc0actlrns .8words "
mycode ".\" CNTKCTL c0-3: \" sivisc0cntkctl .4words "
mycode ": .c0r13sx ( stap -- ) 4 * sivenc0R13s0 + @ 8 tab .hex ;"
mycode ": .c1r13sx ( stap -- ) 4 * sivenc1R13s0 + @ 19 tab .hex ;"
mycode ": .c2r13sx ( stap -- ) 4 * sivenc2R13s0 + @ 30 tab .hex ;"
mycode ": regelr13 ( stap -- ) dup .c0r13sx dup .c1r13sx .c2r13sx cr ;"
mycode ": .hdr13rg0 ( -- ) .\" REG 13 - INIT - steps  \" cr ;"
mycode ": .c0r14sx ( stap -- ) 4 * sivenc0R14s0 + @ 8 tab .hex ;"
mycode ": .c1r14sx ( stap -- ) 4 * sivenc1R14s0 + @ 19 tab .hex ;"
mycode ": .c2r14sx ( stap -- ) 4 * sivenc2R14s0 + @ 30 tab .hex ;"
mycode ": regelr14 ( stap -- ) dup .c0r14sx dup .c1r14sx .c2r14sx cr ;"
mycode ": .hdr14rg0 ( -- ) .\" REG 14 - INIT - steps  \" cr ;"
mycode ": .hdrg1 ( -- ) space space .\" stap  core0      core1      core2 \" cr ;"
mycode ": .tabelr13 ( -- ) cr .hdr13rg0 .hdrg1 8 0 do 4 tab i . i regelr13 loop cr ;"
mycode ": .tabelr14 ( -- )    .hdr14rg0 .hdrg1 8 0 do 4 tab i . i regelr14 loop cr ;"
@mycode ".tabelr13  .tabelr14"
@mycode "1 0 uart>task#"
mycode ": .inwin ( -- ) 2 0 win#>task# cr . 1 0 win#>task# cr .\" wvcounter: \" wvcounter @ . 0 0 win#>task# ;"
mycode ": wvinit 5000000 0 do .tabelr13 .tabelr14 i .inwin ( blink ) loop ;"
mycode "wvinit reboot"
mycode ""

@ ******** END SOURCE ******************************************

ENDMYSOURCE:				.skip 0x10		@ buffertje}

