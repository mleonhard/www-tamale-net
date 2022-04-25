###############################################################################
# Assignment 3
# Michael Leonhard
# CS 366 Prof. Ashfaq Khokhar
# TA: Mani Radhakrishnan
# 2 Nov 2005
# PC SPIM 7.2
###############################################################################
# Functional Description: a main program to test the merge_sort function.  The
# user is prompted for an array size.  Then the array is allocated on the stack
# and filled with random integers.  It prints out the array and calls the merge_sort
# to sort it.  Finally, the sorted array is printed.
###############################################################################
# Pseudocode of main program:
#
# print prompt
# read unsigned 32-bit integer from console (into $s0)
# 
# allocate space on stack
# loop for each space:
#    call random number generator
#    store resulting number in space
#
# print unsorted array heading
# loop for each space:
#   print number
#
# call merge_sort
#
# print sorted array heading
# loop for each space:
#   print number
#
# exit
#
###############################################################################
# Status:
# ???????????????????????????????????????
###############################################################################

    .data
    .align 2
    .jumptable: .word main, randloop, randoverflow, randnext, printloop1, printloop2, halt, merge_sort, mergeloop, mergesecond, finishfirst, finishsecond, copy, mergereturn
    
	# strings
	SizePrompt: .asciiz "Enter size of array: "
	UnsortedHeader: .asciiz "\nUnsorted array:\n "
	SortedHeader: .asciiz "\nSorted array:\n "
	Spacer: .asciiz "\n "
	MergeSortDebug1: .asciiz "\nmerge_sort num_entries="
	MergeSortDebug2: .asciiz " array_pointer="

    .text

main:
	# Register allocations
	# s0 size of array (in entries)
	# s1 size of array (in bytes)
	# s2 pointer to head of array
	# s3 pointer to tail of array
	# s4 pointer into array
	
	# print prompt
	li 		$v0, 4			# operation = 4 (print string)
	la 		$a0, SizePrompt	# param = address of string
	syscall					# call O/S
	
	# s0 <-- 32bit signed integer from console
	li		$v0, 5			# operation = 5 (read unsigned int from console)
	syscall					# call O/S
	move		$s0, $v0			# store result
	
	# s1 = size of array (in bytes)
	sll		$s1, $s0, 2		# s1 = s0 << 2

	# tail of array
	move		$s3, $sp
	
	# allocate space on stack
    subu		$sp, $sp, $s1
	
	# start of array
	move		$s2, $sp
	
	# initialize PRND
	li 		$t0, 33614		# A$t0 <-- seed
	li		$v0, 2147483646	# previous result doesn't exist S'$v0 <-- 2147483646\
	
	# pointer into array (at start of array)
	move		$s4, $s2
	
# loop for each space, generating a random number:
randloop:
	multu 	$v0, $t0			# HI,LO = A$t0
	mflo		$t1				# Q$t1 = bits 00..31 of A
	srl		$t1, $t1, 1		#
	mfhi		$t2 				# P$t2 = bits 32..63 of A
	addu		$v0, $t1, $t2	# result$v0 = P + Q
	bltz		$v0, randoverflow# handle overflow ( rare )
	sw      $v0, 0($s4)		# store result in stack entry
	j		randnext
	
randoverflow:
	sll		$v0, $v0, 1		# zero bit 31 of result$v0
	srl		$v0, $v0, 1		#
	addiu	$v0, $v0, 1		# increment result$v0
	sw      $v0, 0($s4)		# store result in stack entry

randnext:
	# move to the next entry
	addiu	$s4, $s4, 4		# move to next entry
	# end of array hasn't been reached, so loop
	bne		$s3, $s4, randloop

	###########################################################################
	
	# print unsorted array heading
	li 		$v0, 4				# operation = 4 (print string)
	la 		$a0, UnsortedHeader	# param = address of string
	syscall						# call O/S

	# s4 = pointer into array (at start of array)
	move		$s4, $s2
	
# loop for each space:
printloop1:
	# load the number from the array
	lw		$a0, 0($s4)
	# print the number
	li		$v0, 1			# operation = 1 (print signed 32bit integer)
	syscall					# call O/S
	# move to the next entry
	addiu	$s4, $s4, 4		# move to next entry
	# print spacer
	li 		$v0, 4			# operation = 4 (print string)
	la 		$a0, Spacer		# param = address of string
	syscall					# call O/S
	# end of array hasn't been reached, so loop
	bne		$s3, $s4, printloop1

	###########################################################################
	
	# call merge_sort
	addiu		$sp, $sp, -8		# allocate space for 2 parameters
	move			$a0, $s0			# a0 = size of array (in 4-byte entries)
	move			$a1, $s2			# a1 = pointer to head of array
    jal			merge_sort		# call the function
    addiu   		$sp, $sp, 8    	# deallocate space on the stack
    
	# print sorted array heading
	li 		$v0, 4				# operation = 4 (print string)
	la 		$a0, SortedHeader	# param = address of string
	syscall						# call O/S

	# s4 = pointer into array (at start of array)
	move		$s4, $s2
	
# loop for each space:
printloop2:
	# load the number from the array
	lw		$a0, 0($s4)
	# print the number
	li		$v0, 1			# operation = 1 (print signed 32bit integer)
	syscall					# call O/S
	# move to the next entry
	addiu	$s4, $s4, 4		# move to next entry
	# print spacer
	li 		$v0, 4			# operation = 4 (print string)
	la 		$a0, Spacer		# param = address of string
	syscall					# call O/S
	# end of array hasn't been reached, so loop
	bne		$s3, $s4, printloop2

halt:
    li      $v0, 10         # operation = 10 (halt)
    syscall                 # call O/S

    ###########################################################################
    # Function: merge_sort (num_entries, array_pointer)
    # This function uses merge sort to recursively sort the provided array of
	# unsigned integers.
	#
	# Stack space for the two parameters is allocated by the caller, but the
	# parameters are passed in registers.  Parameter num_entries is passed in
	# a0.  Parameter array_pointer is passed in a1.
	#
	# There is no return value.  The array is sorted in place.
	# 
	# If num_entries is less than 1 then the behavior of the function is
	# undefined.
    ###########################################################################
	# Pseudocode:
	# if num_entries == 1 then return
	# p2 <-- allocate array for result (size is num_entries*4)
	# n1 <-- num_entries / 2
	# n2 <-- num_entries - num_entries/2
	# call merge_sort(n1, p2)
	# call merge_sort(n2, p2 + n1*4)
	# call merge(n1, p2, n2, p2 + n1*4, array_pointer)
	# free space on stack, p2
	# return
	#
	# Register allocation
	# s0 size of array (in entries)
	# s1 size of array (in bytes)
	# s2 pointer to head of array
	# s3 pointer to temporary array
	# s4 size of first half (in entries)
	# s5 size of second half (in entries)
	# s6 pointer to second half
	# t0 size of first half (in bytes)
	# t1 dest pointer
	# t2 src1 pointer
	# t3 src2 pointer
	# t4 end of second half
    ###########################################################################
merge_sort:
	# save return pointer to stack
	addiu	$sp, $sp, -4		# allocate space for 1 value
	sw		$ra, 0($sp)
	# save regs to stack
	addiu	$sp, $sp, -32	# allocate space for 8 values
	sw		$s0, 0($sp)
	sw		$s1, 4($sp)
	sw		$s2, 8($sp)
	sw		$s3, 12($sp)
	sw		$s4, 16($sp)
	sw		$s5, 20($sp)
	sw		$s6, 24($sp)
	sw		$s7, 28($sp)
	# save parameters
	move		$s0, $a0			# size of array (in entries)
	sll		$s1, $s0, 2		# size of array (int bytes)
	move		$s2, $a1			# pointer to array
	
	# DEBUG print string 1
	li 		$v0, 4				# operation = 4 (print string)
	la 		$a0, MergeSortDebug1	# param = address of string
	syscall						# call O/S
	# DEBUG print param 1
	li		$v0, 1				# operation = 1 (print signed 32bit integer)
	move		$a0, $s0				# param = value
	syscall						# call O/S
	# DEBUG print string 2
	li 		$v0, 4				# operation = 4 (print string)
	la 		$a0, MergeSortDebug2	# param = address of string
	syscall						# call O/S
	# DEBUG print param 2
	li		$v0, 1				# operation = 1 (print signed 32bit integer)
	move		$a0, $s2				# param = value
	syscall						# call O/S

	# if size < 2 then return
	sltiu		$t0, $s0, 2		# t0 = a0 < 2
	bgtz			$t0, mergereturn	# if t0 return
	
	# s3 = allocate temporary array
	subu		$sp, $sp, $s1
	move		$s3, $sp
	# s4 = number of entries in first half
	srl			$s4, $s0, 1
	# call merge_sort on first half
	addiu		$sp, $sp, -8		# allocate space for 2 parameters
	move			$a0, $s4			# a0 = size of array (in 4-byte entries)
	move			$a1, $s3			# a1 = pointer to head of array
    jal			merge_sort		# call the function
    addiu   		$sp, $sp, 8    	# deallocate space on the stack

	# number of entries in second half, s5 = s0 - s4
	subu		$s5, $s0, $s4
	# t0 = size of first half (in bytes)
	sll		$t0, $s4, 2		# t0 = s4 << 2
	# s6 pointer to second half
	addu		$s6, $s3, $t0
	# call merge_sort on second half
	addiu		$sp, $sp, -8		# allocate space for 2 parameters
	move			$a0, $s5			# a0 = size of array (in 4-byte entries)
	move			$a1, $s6			# a1 = pointer to head of array
	jal			merge_sort		# call the function
	addiu   		$sp, $sp, 8    	# deallocate space on the stack
	
	#####Merge######
	# t0 = size of first half (in bytes)
	sll		$t0, $s4, 2		# t0 = s4 << 2
	# t1 dest pointer
	move		$t1, $s2
	# t2 src1 pointer
	move		$t2, $s3
	# t3 src2 pointer
	move		$t3, $s6
	# t4 end of second half
	addu		$t4, $s3, $s1
	
mergeloop:
	# first or second are equal to their ends
	beq		$t2, $s6, finishfirst
	beq		$t3, $t4, finishfirst
	# load entries
	lw		$t5, 0($t2)
	lw		$t6, 0($t3)
	# second entry is less than first, so skip to mergesecond
	slt		$t7, $t6, $t5
	bgtz		$t7, mergesecond
	# first entry is the smaller one
	sw		$t5, 0($t1)		# save word
	addiu	$t1, $t1, 4		# increment dest
	addiu	$t2, $t2, 4		# increment first
	j mergeloop
mergesecond:
	# second entry is the smaller one
	sw		$t6, 0($t1)		# save word
	addiu	$t1, $t1, 4		# increment dest
	addiu	$t3, $t3, 4		# increment second
	j mergeloop

finishfirst:
	beq		$t2, $s6, finishsecond
	lw		$t5, 0($t2)
	sw		$t5, 0($t1)		# save word
	addiu	$t1, $t1, 4		# increment dest
	addiu	$t2, $t2, 4		# increment first
	j finishfirst
finishsecond:
	beq		$t3, $t4, copy
	lw		$t6, 0($t3)
	sw		$t6, 0($t1)		# save word
	addiu	$t1, $t1, 4		# increment dest
	addiu	$t3, $t3, 4		# increment second
	j finishsecond
	
	# copy temp array to src
copy:
	lw		$t5, 0($s3)		# load from temp array
	sw		$t5, 0($s2)		# save word to array
	addiu	$s2, $s2, 4		# increment
	addiu	$s3, $s3, 4		# increment
	bne		$s3, $t4, copy

	# free temporary array
	addu		$sp, $sp, $s1
	
mergereturn:
	# restore saved regs from stack
	lw		$s0, 0($sp)
	lw		$s1, 4($sp)
	lw		$s2, 8($sp)
	lw		$s3, 12($sp)
	lw		$s4, 16($sp)
	lw		$s5, 20($sp)
	lw		$s6, 24($sp)
	lw		$s7, 28($sp)
	addiu	$sp, $sp, 32		# deallocate space for 8 values
	# restore return pointer from stack
	lw		$ra, 0($sp)
	addiu	$sp, $sp, 4		# deallocate space for 1 value

	# return
    jr      $ra             # return to caller

