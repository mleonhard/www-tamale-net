# Myth Assembler version 0
# Michael Leonhard
# assemble.py (Project 5 Assembler)
# Michael Leonhard (mleonhar)
# CS366 Fall 2005, Professor Khokhar
# TA: Mani Radhakrishnan
# MythSim 3.1.1 on Windows XP SP2, Java SDK 1.5.0_04, Python 2.4
# 2005-11-30
#
# This Python script is an assembler that targets MythSim.  When fed an
# assembly file, it will emit a MythSim memory file.  The accompanying
# proj5.ucode file contains microcode which must be used.
#
# Usage: python assemble.py foo.asm > foo.mem
#
# ----{ Assembly Language Directives }----
#
# Comments may be included anywhere in the assembly language file.  Comments
# begin with two slash characters "//" and extend to the end of the line.
# Comments are completely ignored by the assembler.  They are not included in
# the outputted memory file because MythSim doesn't parse them properly.  Or
# rather, it parses them when it shouldn't, leading to numerous
# NumberFormatException errors.
#
# There are three kinds of parameters which may be specified to the directives:
#  1. Registers Ri, Rj, Rk may be specified as r0, r1, r2, or r3
#  2. 8-bit Signed Integer const8 may take values from -255 through 255.
#     Negative numbers are stored in 2's complement format.
#  3. 4-bit Signed Integer const4 may take values from -8 through 7.
#     Like const8, const4 is also stored in 2's complement form.
#  4. Labels, which allow easy referencing of data and jump destinations.
#
# A label must be specified on its own line.  A valid label is a single
# alphanumeric string followed by the colon ":" symbol.  The label refers to
# the directive that follows it.
#
# Directives are translated directly into pairs of bytes in the memory file.
# One directive may appear per line.  Directives may not span lines.  The format
# of this listing is according to this model:
#
# DIRECTIVE param1 param2 : logical description
#  English description.
#
# Where "DIRECTIVE" is the name of the directive.  For example, BNZ.
# Where param1 and param2 are parameters.  The logical description defines the
# behavior of the directive.  Finally, an English description is provided.
#
# NOP
#  No-Operation.  The execution of this instruction has no effect on the
#  machine except to move the program counter past it.  This instruction
#  consists of two null bytes.
#
# HALT
#  Halt the machine.  The program counter does not proceed past a halt
#  instruction.
#
# LI Ri const8 : ri <-- const8
#  Load the 8-bit signed immediate value into register Ri.
#
# ADD Ri Rj Rk : ri <- rj + rk
#  Add Rj and Rk, store the result in Ri.
#
# ADDI Ri Rj const4 : ri <- rj + const4
#  Add-Immediate.  Adds the immediate value to Rj and stores the result in Ri.
#
# SUBI Ri Rj const4 : ri <- rj - const4
#  Subtact-Immediate.  Subtract const4 from Rj and store the result in Ri.
#
# AND Ri Rj Rk : ri <- rj + rk
#  And.  Take the bitwise AND of Rj and Rk and store it in Ri.
#
# DATA const8A const8B : literal bytes const8A and const8B
#  Data.  The bytes are stored directly in the memory file.  The byte const8A
#  is stored first, followed by const8B.
#
# MOVE Ri Rj : ri <- rj
#  Copies the value in Rj into Ri.
#
# LDM Ri Rj const4 : ri <- Mem[rj + const4]
#  Load-From-Memory.  Retrieve the byte at address Rj + const4 and store it in
#  register Ri.  Do not use a label in place of const4.
#
# SWM Rk Rj const4 : rk -> Mem[rj + const4]
#  Store-To-Memory.  Store the byte in Rk to the memory location with address
#  Rj + const.  Do not use a label in place of const4.
#
# ADDM Ri Rj Rk const4 : ri <- rj + Mem[rk + const4]
#  Add-From-Memory.  This instruction behaves like a combination of LDM and ADD.
#  It retrieves the byte from memory, adds it to Rj, and stores the result in
#  Ri.
#
# SUBM Ri Rj Rk const4 : ri <- rj - Mem[rk + const4]
#  Subtract-From-Memory.  This is the same as ADDM except that it does
#  subtraction.
#
# DIV Ri Rj Rk : ri <- rj / rk
#  Divide.  Divide Rj by Rk and store the result in Ri.  Rk must be 2 or 4, or
#  the machine will halt.  This instruction uses a lookup table of length 65.
#  This means that Rj must be in the range 0 through 64.  If Rj is not in this
#  range then the result is undefined.
#
# DIVI Ri Rj const4 : ri <- rj / const4
#  Divide-Immediate.  Divide Rj by const4 and store the result in Ri.  This
#  function behaves just like DIV, except the divisor is obtained from const4
#  rather than Rk.
#
# DIVT const8 : r4 <- const8  *Do not use this instruction*
#  Division-Table.  Sets the offset of the division lookup table.
#
# LDI Ri const8 : ri <- Mem[const8]
#  Load-Memory-Immediate.  Loads the byte in memory at the specified address
#  into Ri.
#
# LDI Ri label : ri <- Mem[address of label]
#  Load-Memory-Immediate.  Loads the byte in memory at the specified label
#  into Ri.
#
# STI Rk const4 : rk -> Mem[const4]
#  Store-Immediate.  Stores the contents of Rk into memory at the specified
#  address.  Because const4 can hold only values -8 through 7, this instruction
#  is able to store only to bytes with addresses in the range 0-7 and 248-255.
#
# STI Rk label : rk -> Mem[address of label]
#  Store-Immediate.  Stores the contents of Rk into memory at the specified
#  label.  Because const4 can hold only values -8 through 7, this instruction
#  is able to store only to labels with address 0, 2, 4, 6, 248, 250, 252, 
#  and 254.
#
# J Rj : r7 <- r7 + rj
#  Add the value in Rj to the program counter register.  This has the effect of
#  jumping forward or backward in the program.  The destination position is
#  relative to the first byte of the instruction following this J instruction.
#  See also: JI
#
# JI const8 : r7 <- r7 + const8
#  Jump-Immediate.  Jump the specified number of bytes forward or backward.
#  
# JI label : r7 <- r7 + distance to destination
#  Jump directly to the specified label.
#
# BE Rj Rk const4 : if rj == rk then r7 <- r7 + 2*const4
#  Branch-If-Equal.  If Rj and Rk hold the same value then jump a number of 
#  instructions specified in const4.  The destination is calculated relative to
#  the PC, which points to the following instruction.  Thus "BE r0 r0 0" is 
#  equivalent to NOP.  Similarly "BE r0 r0 -1" would behave similarly to HALT.  
#
# BE Rj Rk label : if rj == rk then r7 <- r7 + byte distance to label
#  This is the preferred form of Branch-If-Equal.  The offset is automatically
#  calculated by the assembler.
#
# BNZ Rj const4 : if rj != 0 then r7 <- r7 + 2*const4
#  Branch-If-Not-Zero.  If Rj is not zero then jump the number of instructions
#  specified in const4.  This instruction behaves similarly to BE.
#
# BNZ Rj label : if rj != 0 then r7 <- r7 + distance to label
#  Branch-If-Not-Zero.  If Rj is not zero then jump to the specified label.
#  This instruction behaves similarly to BE.
#
# BZ Rj const4 : if rj == 0 then r7 <- r7 + 2*const4
#  Branch-If-Zero.  If Rj contains 0 then jump the number of instructions
#  specified in const4.  This instruction behaves similarly to BE.
#
# BZ Rj label : if rj == 0 then r7 <- r7 + distance to label
#  Branch-If-Zero.  If Rj contains 0 then jump to the specified label. This
#  instruction behaves similarly to BE.
##############################################################################

# TODO
#   Add offsets to label constants
#
VERSION=0

import sys, struct

OFFSET = 0 # represents the current byte offset in the outputted MEM file
Instructions = [] # list of instructions
Labels = {} # dictionary of uppercase labels and their instruction number

# constant types
LABEL = 1
CONST = 2

def err(Line, String):
	print >> sys.stderr, "%s:" % String
	print >> sys.stderr, " ", Line
	sys.exit(2)

def stripComment(Line):
	Index = Line.find("//")
	# no comment
	if Index == -1: return Line
	# return line without comment
	return Line[:Index]

def getComment(Line):
	Index = Line.find("//")
	# no comment
	if Index == -1: return ""
	# return line without comment
	return Line[Index:]

def lastbit(Num):
	if Num % 2 == 0: return "0"
	else: return "1"

def binary(Num, NBits):
	if Num < 0:
		(Num,) = struct.unpack("L", struct.pack("l", Num))
		return binary(Num, NBits)
	elif NBits == 0: return ""
	else: return binary(Num/2, NBits - 1) + lastbit(Num)

def instructionI8(Line, OpCode, Ri=0, Const8=0):
	# check
	assert OpCode > -1 and OpCode < 64
	if Ri < 0 or Ri > 3: err(Line, "Ri=%d out of range" % Ri)
	if Const8 < -255 or Const8 > 255:
		err(Line, "Const8=%d out of range" % Const8)
	# output
	global OFFSET
	print "%d: %s   // const8=%d" % (OFFSET, binary(Const8,8), Const8)
	OFFSET = OFFSET + 1
	print "%d: %s %s  // opcode=%d ri=%d" \
			% (OFFSET, binary(OpCode,6), binary(Ri,2), OpCode, Ri)
	OFFSET = OFFSET + 1
	print

def instructionIJK4(Line, OpCode, Ri=0, Rj=0, Rk=0, Const4=0):
	# check
	assert OpCode > -1 and OpCode < 64
	if Ri < 0 or Ri > 3: err(Line, "Ri=%d out of range" % Ri)
	if Rj < 0 or Rj > 3: err(Line, "Rj=%d out of range" % Rj)
	if Rk < 0 or Rk > 3: err(Line, "Ri=%d out of range" % Rk)
	if Const4 < -8 or Const4 > 7: err(Line, "Const4=%d out of range" % Const4)
	# output
	global OFFSET
	print "%d: %s %s %s // rj=%d rk=%d const4=%d" \
		% (OFFSET, binary(Rj,2), binary(Rk,2), binary(Const4,4), Rj, Rk, Const4)
	OFFSET = OFFSET + 1
	print "%d: %s %s  // opcode=%d ri=%d" \
			% (OFFSET, binary(OpCode,6), binary(Ri,2), OpCode, Ri)
	OFFSET = OFFSET + 1
	print

def prettyConstant(Tuple):
	(Type,Value) = Tuple
	if Type == LABEL: return Value.upper()
	elif Type == CONST: return Value
	else: assert false

def resolveConstant(Line, Tuple):
	(Type,Value) = Tuple
	if Type == LABEL:
		Label = Value.upper()
		if not Labels.has_key(Label):
			err(Line, "Label %s is not defined" % Label)
		else:
			InstructionNumber = Labels[Label]
			return InstructionNumber * 2
	elif Type == CONST: return Value
	else: assert false

def strToReg(Line, String):
	String = String.upper()
	if String == "R0": return 0
	elif String == "R1": return 1
	elif String == "R2": return 2
	elif String == "R3": return 3
	else: err(Line, "Expected register but got " + String)

def strToConst(Line, String):
	if validLabel(Line,String): return (LABEL,String)
	try: return (CONST,int(String))
	except ValueError: err(Line, "Expected constant but got " + String)

def validLabel(Line, Label):
	Label = Label.upper()
	return Label[0].isalpha() and Label.isalnum()

def jumpOffset4(Line, ConstTuple):
	global OFFSET
	# get
	(Type,Value) = ConstTuple
	if Type == CONST: Const4 = Value
	else:
		Const4 = (resolveConstant(Line, ConstTuple) - OFFSET - 2) / 2
		#print "OFFSET=%s %s:%s DIST=%s" \
		#	% (OFFSET, Value, resolveConstant(Line, ConstTuple), Const4)
	# check
	if Const4 < -8: err(Line, "instruction cannot jump backward more than 8 instructions")
	elif Const4 > 7: err(Line, "instruction cannot jump forward more than 7 instructions")
	else: return Const4

# 0) NOP
def inst_NOP(Line):
	print "// NOP (No Operation)"
	instructionI8(Line, 0)

# 1) LI: ri <- const8
def inst_LI(Line, Ri, ConstTuple):
	Const8 = resolveConstant(Line, ConstTuple)
	print "// LI r%d %s ; r%d <- %d (Load Immediate)" \
		% (Ri, prettyConstant(ConstTuple), Ri, Const8)
	instructionI8(Line, 1, Ri, Const8)

# 2) J: r7 <- r7 + rj
def inst_J(Line, Rj):
	print "// J r%d ; r7 < r7 + r%d (Jump)" % (Rj, Rj)
	instructionIJK4(Line, 2, 0, Rj)

# 3) ADD: ri <- rj + rk
def inst_ADD(Line, Ri, Rj, Rk):
	print "// ADD r%d r%d r%d ; r%d <- r%d + r%d (Add)" \
		% (Ri, Rj, Rk, Ri, Rj, Rk)
	instructionIJK4(Line, 3, Ri, Rj, Rk)

# 4) BE: if rj == rk then r7 <- r7 + const4
def inst_BE(Line, Rj, Rk, ConstTuple):
	Const4 = jumpOffset4(Line, ConstTuple)
	# print
	print "// BE r%d r%d %s ;" % (Rj, Rk, prettyConstant(ConstTuple)), \
		"if r%d == r%d then r7 <- r7 + %d (Branch If Equal)" % (Rj, Rk, Const4)
	instructionIJK4(Line, 4, 0, Rj, Rk, Const4)

# 5) JI: r7 <- r7 + const8
def inst_JI(Line, ConstTuple):
	global OFFSET
	# get
	(Type,Value) = ConstTuple
	if Type == CONST: Const8 = Value
	else: Const8 = resolveConstant(Line, ConstTuple) - OFFSET - 2
	# check
	if Const8 < -255: err(Line, "instruction cannot jump backwards further than 255 bytes")
	elif Const8 > 255: err(Line, "jump distance cannot be > 255 bytes")
	# print
	print "// JI %s ; r7 < r7 + %d (Jump Immediate)" \
		% (prettyConstant(ConstTuple), Const8)
	instructionI8(Line, 5, 0, Const8)

# 6) HALT
def inst_HALT(Line):
	print "// HALT (Halt)"
	instructionI8(Line, 6)

# 7) LDM: ri <- Mem[rj + const4]
def inst_LDM(Line, Ri, Rj, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// LDM r%d r%d %s ; r%d <- Mem[r%d + %d] (Load From Memory)" \
		% (Ri, Rj, prettyConstant(ConstTuple), Ri, Rj, Const4)
	instructionIJK4(Line, 7, Ri, Rj, 0, Const4)

# 8) SWM: rk -> Mem[rj + const4]
def inst_SWM(Line, Rk, Rj, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// SWM r%d r%d %s ; r%d -> Mem[r%d + %d] (Store To Memory)" \
		% (Rk, Rj, prettyConstant(ConstTuple), Rk, Rj, Const4 )
	instructionIJK4(Line, 8, 0, Rj, Rk, Const4)

# 9) ADDM: ri <- rj + Mem[rk + const4]
def inst_ADDM(Line, Ri, Rj, Rk, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print \
		"// ADDM r%d r%d r%d %s ; r%d <- r%d + Mem[r%d + %d] (Add from Memory)" \
		% (Ri, Rj, Rk, prettyConstant(ConstTuple), Ri, Rj, Rk, Const4)
	instructionIJK4(Line, 9, Ri, Rj, Rk, Const4)

#10) SUBM: ri <- rj - Mem[rk + const4]
def inst_SUBM(Line, Ri, Rj, Rk, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// SUBM r%d r%d r%d %s ;" % (Ri, Rj, Rk, prettyConstant(ConstTuple)), \
		"r%d <- r%d - Mem[r%d + %d] (Subtract from Memory)"%(Ri, Rj, Rk, Const4)
	instructionIJK4(Line, 10, Ri, Rj, Rk, Const4)

#11) ADDI: ri <- rj + const4
def inst_ADDI(Line, Ri, Rj, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// ADDI r%d r%d %s ; r%d <- r%d + %d (Add Immediate)" \
		% (Ri, Rj, prettyConstant(ConstTuple), Ri, Rj, Const4)
	instructionIJK4(Line, 11, Ri, Rj, 0, Const4)

# 12) MOVE: ri <- rj
def inst_MOVE(Line, Ri, Rj):
	print "// MOVE r%d r%d ; r%d <- r%d (Move Register)" % (Ri, Rj, Ri, Rj)
	instructionIJK4(Line, 12, Ri, Rj)

#13) DIV: ri <- rj / rk (the value in rk may be 2 or 4
def inst_DIV(Line, Ri, Rj, Rk):
	print "// DIV r%d r%d r%d ; r%d <- r%d r%d (Divide)" \
		% (Ri, Rj, Rk, Ri, Rj, Rk)
	instructionIJK4(Line, 13, Ri, Rj, Rk, 0)

#14) DIVI: ri <- rj / const4 (supports divion by 2 or 4)
def inst_DIVI(Line, Ri, Rj, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	if Const4 != 2 and Const4 != 4:
			err(Line, "DIVI supports division only by 2 or 4.")
	print "// DIVI r%d r%d %s ; r%d <- r%d / %d (Divide Immediate)" \
		% (Ri, Rj, prettyConstant(ConstTuple), Ri, Rj, Const4)
	instructionIJK4(Line, 14, Ri, Rj, 0, Const4)

#15) DIVT: r4 <- const8
def inst_DIVT(Line, ConstTuple):
	Const8 = resolveConstant(Line, ConstTuple)
	print "// DIVT %s ; r4 <- %d (Division Table)" \
		% (prettyConstant(ConstTuple), Const8)
	instructionI8(Line, 15, 0, Const8)

# 16) LDI: ri <- Mem[const8]
def inst_LDI(Line, Ri, ConstTuple):
	Const8 = resolveConstant(Line, ConstTuple)
	print "// LDI r%d %s ; r%d <- Mem[%d] (Load Immediate)" \
		% (Ri, prettyConstant(ConstTuple), Ri, Const8)
	instructionI8(Line, 16, Ri, Const8)

# 17) STI: rk -> Mem[const4]
def inst_STI(Line, Rk, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// STI r%d %s ; r%d -> Mem[%d] (Load Immediate)" \
		% (Rk, prettyConstant(ConstTuple), Rk, Const4)
	instructionIJK4(Line, 17, 0, 0, Rk, Const4)

#18) SUBI: ri <- rj - const4
def inst_SUBI(Line, Ri, Rj, ConstTuple):
	Const4 = resolveConstant(Line, ConstTuple)
	print "// SUBI r%d r%d %s ; r%d <- r%d - %d (Subtract Immediate)" \
		% (Ri, Rj, prettyConstant(ConstTuple), Ri, Rj, Const4)
	instructionIJK4(Line, 18, Ri, Rj, 0, Const4)

#19) BZ: if rj == 0 then r7 <- r7 + const4
def inst_BZ(Line, Rj, ConstTuple):
	Const4 = jumpOffset4(Line, ConstTuple)
	# print
	print "// BZ r%d %s ;" % (Rj, prettyConstant(ConstTuple)), \
		"if r%d == 0 then r7 <- r7 + %d (Branch If Zero)" % (Rj, Const4)
	instructionIJK4(Line, 19, 0, Rj, 0, Const4)

# 20) AND: ri <- rj + rk
def inst_AND(Line, Ri, Rj, Rk):
	print "// AND r%d r%d r%d ; r%d <- r%d & r%d (Bitwise AND)" \
		% (Ri, Rj, Rk, Ri, Rj, Rk)
	instructionIJK4(Line, 20, Ri, Rj, Rk)

# 21) BNZ: if rj != 0 then r7 <- r7 + const4
def inst_BNZ(Line, Rj, ConstTuple):
	Const4 = jumpOffset4(Line, ConstTuple)
	# print
	print "// BNZ r%d %s ;" % (Rj, prettyConstant(ConstTuple)), \
		"if r%d != 0 then r7 <- r7 + %d (Branch If Not Zero)" % (Rj, Const4)
	instructionIJK4(Line, 21, 0, Rj, 0, Const4)


# X) DATA: const8 const8
def inst_DATA(Line, ConstTupleA, ConstTupleB):
	# check
	Const8A = resolveConstant(Line, ConstTupleA)
	Const8B = resolveConstant(Line, ConstTupleB)
	if Const8A < -255 or Const8A > 255:
		err(Line, "Const8=%d out of range" % Const8A)
	if Const8B < -255 or Const8B > 255:
		err(Line, "Const8=%d out of range" % Const8B)
	# output
	print "// DATA %s %s (Data)" \
		% (prettyConstant(ConstTupleA), prettyConstant(ConstTupleB))
	global OFFSET
	print "%d: %s   // const8=%d" % (OFFSET, binary(Const8A,8), Const8A)
	OFFSET = OFFSET + 1
	print "%d: %s   // const8=%d" % (OFFSET, binary(Const8B,8), Const8B)
	OFFSET = OFFSET + 1
	print

def parse(Lines):
	for Line in Lines:
		Line = Line.strip()
		Tokens = stripComment(Line).split()
		#print str(len(Instructions)), "processing", Tokens
		
		# empty line
		if len(Tokens) == 0: continue
		
		# command is the first token
		Command = Tokens[0].upper()
		
		# single token
		if len(Tokens) == 1:
			
			# HALT
			if Command == "HALT": Instructions.append((inst_HALT, [Line]))
			
			# NOP
			elif Command == "NOP": Instructions.append((inst_NOP, [Line]))
			
			# Label:
			elif Command[-1] == ":" and len(Command) > 1:
				Label = Command[:-1]
				if not validLabel(Line,Label): err(Line, "Invalid label name")
				elif Label == "divtable": err(Line, "Reserved label name")
				elif Labels.has_key(Label): err(Line, "Duplicate label")
				else: Labels[Label] = len(Instructions)
			
			# unknown
			else: err(Line, "Syntax error")
		
		# two tokens
		elif len(Tokens) == 2:
			
			# J: r7 <- r7 + rj
			if Command == "J":
				Rj = strToReg(Line, Tokens[1])
				Instructions.append((inst_J, [Line,Rj]))
			
			# JI: r7 <- r7 + const8
			elif Command == "JI":
				ConstTuple = strToConst(Line, Tokens[1])
				Instructions.append((inst_JI, [Line,ConstTuple]))
			
			# DIVT: r4 <- const8
			elif Command == "DIVT":
				ConstTuple = strToConst(Line, Tokens[1])
				Instructions.append((inst_DIVT, [Line,ConstTuple]))
			
			else: err(Line, "Syntax error")
		
		# three tokens
		elif len(Tokens) == 3:
			
			# LI: ri <- const8
			if Command == "LI":
				Ri = strToReg(Line, Tokens[1])
				ConstTuple = strToConst(Line, Tokens[2])
				Instructions.append((inst_LI, [Line,Ri,ConstTuple]))
			
			# MOVE: ri <- rj
			elif Command == "MOVE":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Instructions.append((inst_MOVE, [Line,Ri,Rj]))
			
			# DATA: const8 const8
			elif Command == "DATA":
				ConstTuple1 = strToConst(Line, Tokens[1])
				ConstTuple2 = strToConst(Line, Tokens[2])
				Instructions.append((inst_DATA, [Line,ConstTuple1,ConstTuple2]))
			
			# LDI: ri <- Mem[const8]
			elif Command == "LDI":
				Ri = strToReg(Line, Tokens[1])
				ConstTuple = strToConst(Line, Tokens[2])
				Instructions.append((inst_LDI, [Line,Ri,ConstTuple]))
			
			# STI: rk -> Mem[const4]
			elif Command == "STI":
				Rk = strToReg(Line, Tokens[1])
				ConstTuple = strToConst(Line, Tokens[2])
				Instructions.append((inst_STI, [Line,Rk,ConstTuple]))
			
			# BZ: if rj == 0 then r7 <- r7 + const4
			elif Command == "BZ":
				Rj = strToReg(Line, Tokens[1])
				ConstTuple = strToConst(Line, Tokens[2])
				Instructions.append((inst_BZ, [Line,Rj,ConstTuple]))
			
			# BNZ: if rj != 0 then r7 <- r7 + const4
			elif Command == "BNZ":
				Rj = strToReg(Line, Tokens[1])
				ConstTuple = strToConst(Line, Tokens[2])
				Instructions.append((inst_BNZ, [Line,Rj,ConstTuple]))
			
			else: err(Line, "Syntax error")
		
		# four tokens
		elif len(Tokens) == 4:
			
			# ADD: ri <- rj + rk
			if Command == "ADD":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Rk = strToReg(Line, Tokens[3])
				Instructions.append((inst_ADD, [Line,Ri,Rj,Rk]))
			
			# AND: ri <- rj + rk
			elif Command == "AND":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Rk = strToReg(Line, Tokens[3])
				Instructions.append((inst_AND, [Line,Ri,Rj,Rk]))
			
			# BE: if rj == rk then r7 <- r7 + const4
			elif Command == "BE":
				Rj = strToReg(Line, Tokens[1])
				Rk = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_BE, [Line,Rj,Rk,ConstTuple]))
			
			# LDM: ri <- Mem[rj + const4]
			elif Command == "LDM":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_LDM, [Line,Ri,Rj,ConstTuple]))
			
			# SWM: rk -> Mem[rj + const4]
			elif Command == "SWM":
				Rk = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_SWM, [Line,Rk,Rj,ConstTuple]))
			
			# ADDI: ri <- rj + const4
			elif Command == "ADDI":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_ADDI, [Line,Ri,Rj,ConstTuple]))
			
			# SUBI: ri <- rj - const4
			elif Command == "SUBI":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_SUBI, [Line,Ri,Rj,ConstTuple]))
			
			# DIV: ri <- rj / rk
			elif Command == "DIV": 
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Rk = strToReg(Line, Tokens[3])
				Instructions.append((inst_DIV, [Line,Ri,Rj,Rk]))
			
			# DIVI: ri <- rj / const4 (const4 = 2 or 4)
			elif Command == "DIVI":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				ConstTuple = strToConst(Line, Tokens[3])
				Instructions.append((inst_DIVI, [Line,Ri,Rj,ConstTuple]))
		
		# five tokens
		elif len(Tokens) == 5:

			# ADDM: ri <- rj + Mem[rk + const4]
			if Command == "ADDM":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Rk = strToReg(Line, Tokens[3])
				ConstTuple = strToConst(Line, Tokens[4])
				Instructions.append((inst_ADDM, [Line,Ri,Rj,Rk,ConstTuple]))
			
			# SUBM: ri <- rj - Mem[rk + const4]
			elif Command == "SUBM":
				Ri = strToReg(Line, Tokens[1])
				Rj = strToReg(Line, Tokens[2])
				Rk = strToReg(Line, Tokens[3])
				ConstTuple = strToConst(Line, Tokens[4])
				Instructions.append((inst_SUBM, [Line,Ri,Rj,Rk,ConstTuple]))
			
			else: err(Line, "Syntax error")
		
		# more than five tokens
		else: err(Line, "Syntax error")

# open file
if len(sys.argv) != 2:
	print >> sys.stderr, "MythAssembler version %s by Michael Leonhard (http://tamale.net/)" % VERSION
	print >> sys.stderr, "Usage: assemble.py foo.asm >> foo.asm.mem"
	sys.exit(0)
F = file(sys.argv[1])

# check signature
if F.readline().rstrip() != "MythAsm0": err(sys.argv[1], "Missing MythAsm0 signature")

# division table address loader
parse(["DIVT divtable\n"])

# file
parse(F)
F.close()

# division table
DivTableData = ["divtable:"]
for A in range(0,65):
	DivTableData.append("DATA %d %d // %d/2=%d %d/4=%d" % (A/2,A/4,A,A/2,A,A/4))
parse(DivTableData)

#print "Labels ", Labels
#print "Instructions", Instructions

# output
print "// MythSim 3.1.1 memory file generated from %s by" % sys.argv[1]
print "// Myth Assembler version %d by Michael Leonhard (www.tamale.net)" % VERSION
print
print "%"
print

# program instructions
for InstTuple in Instructions:
	(Func, Parms) = InstTuple
	apply(Func,Parms)

# done!
print "// end of file"

# check for file that is too big
if OFFSET > 256:
	print >> sys.stderr, "File is too large! Mem file must be < 256 bytes.  This one is %d." % OFFSET

