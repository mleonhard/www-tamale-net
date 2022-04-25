MythAsm0

// proj5.asm (Project 5 Assembly)
// Michael Leonhard (mleonhar)
// CS366 Fall 2005, Professor Khokhar
// TA: Mani Radhakrishnan
// MythSim 3.1.1 on Windows XP SP2, Java SDK 1.5.0_04
// 2005-11-30
//
// STATUS:
//   This file implements the odd-parity algorithm specified in the assignment
// description with two exceptions.  The algorithm is shown with C and N being
// initialized outside of the do {} loop.  This is incorrect.  C and N should
// be reset before each byte is processed.  Thus they must be initialized INSIDE
// the do loop.  This program implements that change.
//
// You may convert this program to a MythSim memory file by invoking:
//   python assemble.py proj5.asm >> proj5.mem
//
// TESTING:
//   The data to be processed is specified at the end of this file.  Testing was
// performed on a chunk of 33 bytes: values 1 through 32, terminated with a
// zero byte.  The data begins at location 80 in memory.  Execution of the
// program results in the bytes being shifted left, and a parity bit added, such
// that the number of 1 bits in the byte are odd.  The execution requires 30478
// clock cycles.

	// Variable Storage Memory (all initially NOP)
var:
	DATA 0 0			// Mem[var + 0] is X
					// Mem[var + 1] is C
	DATA 0 0			// Mem[var + 2] is N
					// Mem[var + 3] is M
	LI r3 var		// r3 <-- address of var
	
	// Register assignments
	// r2 is M
	// r3 points to var
	
	// Algorithm from Assignment Description
	
	LI r0 data		// r0 <-- address of data	; X := starting address for data
	SWM r0 r3 0		// r0 --> Mem[var+0]			; save X
	
mainloop:
	LI r0 0			// r0 <-- 0					; C := 0
	SWM r0 r3 1		// r0 --> Mem[var+1]			; save C
	LI r0 64			// r0 <-- 64					; N := 01000000; /* binary */
	SWM r0 r3 2		// r0 --> Mem[var+2]			; save N
	LDM r0 r3 0		// r0 <-- Mem[var+0]			; load X into r0
	LDM r2 r0 0		// r2 <-- Mem[X+0] 			; M := read 8-bit integer from X
	
	// REGS: r2=M
	// Prepare to loop 8 times					; for (I=8; I != 0; I--)
	LI r1 8			// r1 <-- 8					; I := 8
	// REGS: r1=I r2=M r3=var
for:
	LDM r0 r3 2		// r0 <-- Mem[var+2]			; load N
	
	BNZ r1 nobreak	// if r1 != 0 then j nobreak	; if I == 0 then goto break
	JI break			// goto break
nobreak:
	SUBI r1 r1 1		// r1 <-- r1 - 1				; I--
	
	// REGS: r0=N r1=I r2=M r3=var				; if (M & N) then C++
	AND r0 r0 r2		// r0 <-- r0 & r2			; T := M & N
	BZ r0 skipC		// if r0 == 0 then j noIncC	; if M&N == 0 then goto skipC
	
	LDM r0 r3 1		// r0 <-- Mem[var+1]			; load C
	ADDI r0 r0 1		// ro <-- r0 + 1				; C++
	SWM r0 r3 1		// r0 --> Mem[var+1]			; save C
skipC:

	// N := N / 2	
	LDM r0 r3 2		// r0 <-- Mem[var+2]			; load N
	DIVI r0 r0 2		// r0 <-- r0 / 2				; N := N / 2
	SWM r0 r3 2		// r0 --> Mem[var+2]			; save N
	JI for			// r7 <-- address of for		; goto head of for loop
break:
	
	// REGS: r2=M r3=var
	ADD r2 r2 r2		// r2 <-- r2 + r2			; M := M + M; /* M := M * 2 */
	
	// 											; if !(C & 1) then M++
	LDM r0 r3 1		// r0 <-- Mem[var+1]			; load C
	LI r1 1			// r1 <-- 1					; T := 1
	AND r1 r0 r1		// r1 <-- r0 & r1			; T := C & T
	BNZ r1 skipM		// if r1 != 0 then j skipM	; if T == T' then goto skipM

	ADDI r2 r2 1		// r2 <-- r2 + 1				; M := M + 1
skipM:
	
	// REGS: r2=M r3=var
	LDM r0 r3 0		// r0 <-- Mem[var+0]			; load X into r0
	SWM r2 r0 0		// r2 <-- Mem[X+0] 			; store M back into location X
	
	// REGS: r0=X r2=M r3=var
	ADDI r0 r0 1		// r0 <-- r0 + 1				; X++
	SWM r0 r3 0		// r0 --> Mem[var+0]			; save X
	
	//											; } while (M != 1)
	SUBI r0 r2 1		// r0 <-- r2 - 1				; T := M - 1
	BZ r0 end		// if r0 == 0 then goto end	; if T==0 (same as M==1) then goto end
	
	JI mainloop		// goto mainloop ; goto mainloop
end:

	HALT

data: // data to be checksummed, 32 bytes + 2 null
	DATA 1 2
	DATA 3 4
	DATA 5 6
	DATA 7 8
	DATA 9 10
	DATA 11 12
	DATA 13 14
	DATA 15 16
	DATA 17 18
	DATA 19 20
	DATA 21 22
	DATA 23 24
	DATA 25 26
	DATA 27 28
	DATA 29 30
	DATA 31 32
	DATA 0 0

