###############################################################################
# Assignment 2
# Michael Leonhard
# CS 366 Prof. Ashfaq Khokhar
# TA: Mani Radhakrishnan
# 18 Oct 2005
# PC SPIM 7.2
###############################################################################
# Functional Description: A main program to test the ThirdDegPoly function.
# Values are passed for parameters A, B, C, D, and X.  The result of the
# function is printed to the screen.
###############################################################################
# Psuedocode of main program:
#
# initialize X
# initialize A
# initialize B
# initialize C
# initialize D
# call ThirdDegPoly(X, A, B, C, D)
# print returned value
###############################################################################
# Testing Performed:
# The program was tested with the following inputs and results were checked:
#         X         A           B           C           D       Result
#       -10      -100          50         -10           3       105103
#       -10       100          50          10           3       -95097
#         0         0           0           0           0            0
#         1         1           1           1           1            4
#        -1         1           1           1           1            0
#        -1         0           1           0           1            2
#        -1         1           0           1           0           -2
#      1000        -2          -3          -5          -7  -2003005007
#      1000         2           3           5           7   2003005007
###############################################################################
# Status:
# Every portion of the assignment description is implemented.
###############################################################################

    .data
    .align 2
    .jumptable: .word main, halt, ThirdDegPoly
    
    # parameters passed to the function
    X: .word 1000
    A: .word 2
    B: .word 3
    C: .word 5
    D: .word 7
    
    # strings
    ResultString: .asciiz "Result: "

    .text

main:
    # call function and store result
    addiu   $sp, $sp, 20    # allocate space for 5 parameters
    la      $a0, X          # load address of constant X
    lw      $a0, 0($a0)     # load constant X
    la      $a1, A          # load address of constant A
    lw      $a1, 0($a1)     # load constant A
    la      $a2, B          # load address of constant B
    lw      $a2, 0($a2)     # load constant B
    la      $a3, C          # load address of constant C
    lw      $a3, 0($a3)     # load constant C
    la      $t0, D          # load address of constant D
    lw      $t0, 0($t0)     # load constant D
    sw      $t0, 16($sp)    # store constant D at position 5, stack[4]
    jal     ThirdDegPoly    # call the function
    move    $s0, $v0        # save result
    addiu   $sp, $sp, 20    # deallocate space on the stack
    
    # print the result label
    li      $v0, 4          # operation = 4 (print string)
    la      $a0,ResultString# param = address of string
    syscall                 # call O/S
    
    # print the result value
    li      $v0, 1          # operation = 1 (print signed 32bit integer)
    move    $a0, $s0        # param = value
    syscall                 # call O/S
    
    # exit program
halt:
    li      $v0, 10         # operation = 10 (halt)
    syscall                 # call O/S

    ###########################################################################
    # Function: ThirdDegPoly (X, A, B, C, D) -> A*X^3 + B*X^2 + C*X + D
    # This function computes the value of the third degree polynomial with
    # coefficients {A, B, C, D} at the value X.  The parameters are passed
    # according to the following convention:
    #
    #    Stack space is allocated for all 5 parameters.  The first four 
    #    parameters are passed in registers a0..a3.  Subsequent parameters are
    #    passed on the stack, starting at position 4, which is 16($sp).
    #
    #    The function returns a value in v0.
    #
    # All parameters and the return value are 32 bit signed integers.
    # Overflow conditions are ignored.  If an intermediate value overflows its
    # 32bit signed integer register, the result of the function is undefined.
    ###########################################################################
    # The expression may be rewritten to contain arithmetic operations on
    # parameters or the result of another operation:
    #    A*X^3 + B*X^2 + C*X + D
    # = (A*X^2 + B*X + C)*X + D
    # = ((A*X + B)*X + C)*X + D
    # = (((A*X) + B)*X + C)*X + D
    # The evaluation of this rewritten expression is implemented in assembly:
    # Psuedocode:
    # Acc <-- A * X
    # Acc <-- Acc + B
    # Acc <-- Acc * X
    # Acc <-- Acc + C
    # Acc <-- ACC * X
    # ACC <-- Acc + D
    #
    # The register v0 register is used as the accumulator (Acc).
    ###########################################################################
ThirdDegPoly:
    mult    $a1, $a0        # HIGH|LOW <-- A * X
    mflo    $v0             # ACC <-- LOW
    addu    $v0, $v0, $a2   # Acc <-- Acc + B
    mult    $v0, $a0        # HIGH|LOW <-- Acc * X
    mflo    $v0             # ACC <-- LOW
    addu    $v0, $v0, $a3   # Acc <-- Acc + C
    mult    $v0, $a0        # HIGH|LOW <-- Acc * X
    mflo    $v0             # ACC <-- LOW
    lw      $t0, 16($sp)    # D <-- stack[4]
    addu    $v0, $v0, $t0   # ACC <-- Acc + D
    jr      $ra             # return to caller

