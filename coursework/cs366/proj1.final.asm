##############################################################################
# Assignment 1
# Michael Leonhard
# CS 366 Prof. Ashfaq Khokhar
# TA: Mani Radhakrishnan
# 3 Oct 2005
# PC SPIM 7.2
##############################################################################
#
# This program tests users inputted integers to see if they fall within a user
# provided range.  When the program starts, it prompts the user for a minimum
# and maximum value.  Then it enters a loop where it prompts the user for a
# number and reports if the number falls inside the range.  Let the inputted 
# number be X.  The program prints "In" when minimum <= X <= maximum, otherwise
# it prints "Out".
#
# * Psuedocode
#
# llim <-- LowerLimit
# ulim <-- UpperLimit
# 
# print prompt for min
# min <-- 32bit signed integer from console
# print prompt for max
# max <-- 32bit signed integer from console
# if min = -2^31 and max = 2~31 - 1 then halt
# if min > max then print error and halt
#
# lterm <-- min - 1
# uterm <-- max + 1
# if min = -2^31 then lterm <-- uterm
# if max = 2^31 - 1 then uterm <-- lterm
#
# LOOP:
#	print prompt for number
#	num <-- 32bit signed integer from console
#	if num = lterm then halt
#	if num = uterm then halt
#	if min <= num <= max then print "In"
#	else print "Out"
#
# * Register Allocations and Variable Descriptions
#  llim$s1 lower limit for unsigned int, -2^31
#  ulim$s2 upper limit for usigned int, 2^31 - 1
#   num$s3 number to test in loop, read from user
# lterm$s4 lower termination value (set to min$s6 - 1)
# uterm$s5 upper termination value (set to max$s7 + 1)
#   min$s6 upper limit of range (inclusive)
#   max$s7 lower limit of range (inclusive)
#
# * Testing
# Testing was done manually on the following (min,max) values.  For each test,
# various values were tested and the result checked.  The last tested value
# terminates the program.
#
# (min, max): tested values
# (0, 0): -2147483648, -2000000000, -5, 0, 5, 2000000000, 2147483647, 1
# (-1, 1): -2147483648, -2000000000, -5, 0, 5, 2000000000, 2147483647, -2
# (-2147483648, 1): -2147483648, -2000000000, -5, 0, 5, 2000000000, 2147483647, 2
# (-1, 2147483647): -2147483648, -2000000000, -5, 0, 5, 2000000000, 2147483647, -2
# (100, 200): -2147483648, -2000000000, -5, 0, 5, 100, 150, 200, 2000000000, 2147483647, 99
# (-200, -100): -2147483648, -2000000000, -200, -150, -100, -5, 0, 5, 2000000000, 2147483647, -99
# Tested error conditions:
# (1, 0) -> error
# (1, -1) -> error
# (2147483647, -2147483648) -> error
# (-2147483648, 2147483647) -> halt
# 
#
# TODO: transform program to be easily programmatically testable, by changing it
#       into several functions (using the stack to save registers)
# TODO: write a program to generate a subroutine to test 9^3 cases from the
#       cartesian product of the set:
#       {-2147483648, -2147483647, -2, -1, 0, 1, 2, 2147483646, 2147483647 }
#
# * Note on Termination Conditions
# The definition of the termination condition does not specify specific
# behavior for when either min or max is at its respective limit.  Calculation
# of the associated terminal value would overflow the 32bit unsigned integer
# data type:
#
# When min = -2^31:
#	lterm <-- min -1 = -2^31 - 1 = 2^31 overflowed
# When max = 2^31 - 1:
#	uterm = max + 1 = 2^31 - 1 + 1 = 2^31 = -2^31 overflowed
# 
# This program implements an algorithm to avoid the overflow.  It tests for 
# these cases and sets both termination condition values to the non-overflowing
# value.  This means that the uncalculable termination condition is ignored:
#
# lterm <-- min - 1
# uterm <-- max + 1
# if min = -2^31 then lterm <-- uterm
# if max = 2^31 - 1 then uterm <-- lterm
#
# * Status
# Every part of the project specification is implemented.
# Testing indicates that the program functions properly.
#
#############################################################################

	.data
	.align 2
	.jumptable: .word main, limitsok, minmaxok, skiplterm1, skiputerm1, skiplterm2, skiputerm2, loop, in, out, halt, compare, noskip1, noskip2, more, less, equal
	UpperLimit: .word 2147483647
	LowerLimit: .word -2147483648
	
	# # strings for debugging messages
	# MinString: .asciiz " Min:"
	# MaxString: .asciiz " Max:"
	# LTermString: .asciiz " LTerm:"
	# UTermString: .asciiz " UTerm:"
	# EnteredString: .asciiz" EnteredValue:"
	# AString: .asciiz " A:"
	# BString: .asciiz " B:"
	# RetString: .asciiz " Ret:"
	
	# strings for prompts and feedback to user
	MinPrompt: .asciiz "Enter min: "
	MaxPrompt: .asciiz "Enter max: "
	NumPrompt: .asciiz "Enter number to test: "
	MinGreaterMsg: .asciiz "Invalid input: min must be <= max\n"
	InMsg: .asciiz "In\n"
	OutMsg: .asciiz "Out\n"

	.text

main:
	# llim$s1 <-- LowerLimit
	la $s0, LowerLimit	# load address of constant
	lw $s1, 0($s0)		# load constant
	
	# ulim$s2 <-- UpperLimit
	la $s0, UpperLimit	# load address of constant
	lw $s2, 0($s0)		# load constant
	
	# print prompt for min
	li $v0, 4			# operation = 4 (print string)
	la $a0, MinPrompt	# param = address of string
	syscall				# call O/S
	
	# min$s6 <-- 32bit signed integer from console
	li $v0, 5			# operation = 5 (read unsigned int from console)
	syscall				# call O/S
	move $s6, $v0		# store result
	
	# print prompt for max
	li $v0, 4			# operation = 4 (print string)
	la $a0, MaxPrompt	# param = address of string
	syscall				# call O/S
	
	# max$s7 <-- 32bit signed integer from console
	li $v0, 5			# operation = 5 (read unsigned int from console)
	syscall				# call O/S
	move $s7, $v0		# store result
	
	# if min = -2^31 and max = 2~31 - 1 then halt
	bne $s6, $s1, limitsok	# if min$s6 != llim$s1 then limitsok
	beq $s7, $s2, halt		# if max$s7 = ulim$s2 then halt
limitsok:
	
	# # DEBUG print MinString
	# li $v0, 4			# operation = 4 (print string)
	# la $a0, MinString	# param = address of string
	# syscall			# call O/S
	# # DEBUG print min$s6
	# li $v0, 1			# operation = 1 (print signed 32bit integer)
	# move $a0, $s6		# param = value
	# syscall			# call O/S
	# # DEBUG print MaxString
	# li $v0, 4			# operation = 4 (print string)
	# la $a0, MaxString	# param = address of string
	# syscall			# call O/S
	# # DEBUG print max$s7
	# li $v0, 1			# operation = 1 (print signed 32bit integer)
	# move $a0, $s7		# param = value
	# syscall			# call O/S
	
	# if min > max then print error and halt
	#
	#    is performed as
	#
	# ret <-- compare(min, max)
	# if ret<=0 then minmaxok
	# else print error and halt
	
	# ret <-- compare(min, max): (A<B -> -1),(A=B -> 0),(A>B -> 1)
	move $a0, $s6		# A$a0 <-- min$s6
	move $a1, $s7		# B$a1 <-- max$s7
	jal compare			# call subroutine, result is ret$v0
	
	# if ret<=0 then minmaxok
	blez $v0, minmaxok	# if ret$v0 <= 0 then goto minmaxok
	
	# else print error and halt
	li $v0, 4				# operation = 4 (print string)
	la $a0, MinGreaterMsg	# param = address of string
	syscall					# call O/S
	j halt					# halt
	
minmaxok:
	
	# lterm <-- min - 1
	# uterm <-- max + 1
	# if min = -2^31 then lterm <-- uterm
	# if max = 2^31 - 1 then uterm <-- lterm
	#
	# to avoid overflow, this is is implemented as
	#
	# if min != -2^31 then lterm <-- min - 1
	# if max != 2^31 - 1 then uterm <-- max + 1
	# if min = -2^31 then lterm <-- uterm
	# if max = 2^31 - 1 then uterm <-- lterm
	
	# if min != -2^31 then lterm <-- min - 1
	beq $s6, $s1, skiplterm1	# if min$s6 = llim$s1 skiplterm1
	addi $s4, $s6, -1		# lterm$s4 <-- min$s6 - 1
skiplterm1:
	# if max != 2^31 - 1 then uterm <-- max + 1
	beq $s7, $s2, skiputerm1	# if max$s7 = ulim$s2 skiputerm1
	addi $s5, $s7, 1			# uterm$s5 <-- max$s7 + 1
skiputerm1:
	# if min = -2^31 then lterm <-- uterm
	bne $s6, $s1, skiplterm2	# if min$s6 != llim$s1 skiplterm2
	move $s4, $s5			# lterm$s4 <-- uterm$s5
skiplterm2:
	# if max = 2^31 - 1 then uterm <-- lterm
	bne $s7, $s2, skiputerm2	# if max$s7 != ulim$s2 skiputerm2
	move $s5, $s4			# uterm$s5 <-- lterm$s4
skiputerm2:
	
	# $s1 and $s2 are free from here down (in main, not subroutines)
	
	# # DEBUG print LTermString
	# li $v0, 4				# operation = 4 (print string)
	# la $a0, LTermString	# param = address of string
	# syscall				# call O/S
	# # DEBUG print lterm$s4
	# li $v0, 1				# operation = 1 (print signed 32bit integer)
	# move $a0, $s4			# param = value
	# syscall				# call O/S
	#
	# # DEBUG print UTermString
	# li $v0, 4				# operation = 4 (print string)
	# la $a0, UTermString	# param = address of string
	# syscall				# call O/S
	# # DEBUG print uterm$s5
	# li $v0, 1				# operation = 1 (print signed 32bit integer)
	# move $a0, $s5			# param = value
	# syscall				# call O/S
	
loop:
	
	# print NumPrompt
	li $v0, 4				# operation = 4 (print string)
	la $a0, NumPrompt		# param = address of string
	syscall					# call O/S
	# num$s3 <-- 32bit signed integer from console
	li $v0, 5				# operation = 5 (read unsigned int from console)
	syscall					# call O/S
	move $s3, $v0			# store result
	
	# # DEBUG print EnteredString
	# li $v0, 4				# operation = 4 (print string)
	# la $a0, EnteredString	# param = address of string
	# syscall				# call O/S
	# # DEBUG print num$s3
	# li $v0, 1				# operation = 1 (print signed 32bit integer)
	# move $a0, $s3			# param = value
	# syscall				# call O/S
	
	# if num = lterm then halt
	beq $s3, $s4, halt		# if num$s3 = lterm$s4 halt
	# if num = uterm then halt
	beq $s3, $s5, halt		# if num$s3 = uterm$s5 halt
	
	# if min <= num <= max then print "In"
	# else print "Out"
	#
	#    is performed as
	#
	# ret <-- compare(num, min)
	# if ret<0 then out
	# ret <-- compare(max, num)
	# if ret<0 then out
	# else in
	
	# ret <-- compare(num, min): (A<B -> -1),(A=B -> 0),(A>B -> 1)
	move $a0, $s3		# A$a0 <-- num$s3
	move $a1, $s6		# B$a1 <-- min$s6
	jal compare			# call subroutine, result is ret$v0
	# if ret<0 then out
	bltz $v0, out		# if ret$v0 < 0 then goto out
	
	# ret <-- compare(max, num): (A<B -> -1),(A=B -> 0),(A>B -> 1)
	move $a0, $s7		# A$a0 <-- max$s7
	move $a1, $s3		# B$a1 <-- num$s3
	jal compare			# call subroutine, result is ret$v0
	# if ret<0 then out
	bltz $v0, out		# if ret$v0 < 0 then goto out
	# else in
in:
	# print InMsg and loop
	li $v0, 4				# operation = 4 (print string)
	la $a0, InMsg			# param = address of string
	syscall					# call O/S
	j loop					# goto loop
out:
	# print OutMsg and loop
	li $v0, 4				# operation = 4 (print string)
	la $a0, OutMsg			# param = address of string
	syscall					# call O/S
	j loop					# goto loop
halt:
	# exit program
	li $v0, 10
	syscall

compare:
	# $a0 = A
	# $a1 = B
	# $ra = return address
	# $v0 = result = -1 if A<B, 0 if A=B, and 1 if A>B
	# Method compares A and B, taking care to avoid arithmetic overflow
	# Returns value in $v0 to indicate numerical relationship of A and B.
	#
	# This method evaluates A<B as A-B<0.  This is performed in two steps: a
	# subtraction and a test.  The intermediate subtraction result, X, will
	# overflow if X<-2^31 or X>2^31 -1.  This can happen in two situations:
	#    A < 0 and 0 <= B has the potential for A-B<-2^31
	#    B < 0 and 0 <= A has the potential for A-B>2^31 -1
	# These two cases reduce to A<B and B<A, respectively.  Thus we can
	# conveniently avoid overflow by testing for these cases before subtracting.
	#
	# if A=B then equal
	# if A<0 and if B>=0 then less
	# if A>=0 and if B<0 then more
	# else if A-B<0 then less
	# else more
	
	# if A=B then equal
	beq $a0, $a1, equal		# if A$a0 = B$a1 then goto equal
	# if A<0 and if B>=0 then less
	bltz $a1, noskip1		# if B$a1 < 0 then goto noskip1
	bltz $a0, less			# if A$a0 < 0 then goto less
noskip1:
	# if A>=0 and if B<0 then more
	bltz $a0, noskip2		# if A$a0 < 0 then goto noskip2
	bltz $a1, more			# if B$a1 < 0 then goto more
noskip2:
	# else if A-B<0 then less
	sub $s0, $a0, $a1		# A$a0 - B$a1
	bltz $s0, less			# < 0 then goto less
	
	# else more
more:
	li $v0, 1	# return value is 1
	jr $ra		# return
less:
	li $v0, -1	# return value is -1
	jr $ra		# return
equal:
	li $v0, 0	# return value is 0
	jr $ra		# return

