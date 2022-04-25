# Comment Stripper
# Michael Leonhard
# stripcomments.py (Project 5 Comment Stripper)
# Michael Leonhard (mleonhar)
# CS366 Fall 2005, Professor Khokhar
# TA: Mani Radhakrishnan
# MythSim 3.1.1 on Windows XP SP2, Java SDK 1.5.0_04, Python 2.4
# 2005-11-30
#
# Usage: python assemble.py foo.asm >> foo.mem

import sys, string

def stripComment(Line):
	Index = Line.find("//")
	# no comment
	if Index == -1: return Line
	# return line without comment
	return Line[:Index]

# print usage
if len(sys.argv) != 2:
	print >> sys.stderr, "Comment Stripper by Michael Leonhard (http://tamale.net/)"
	print >> sys.stderr, "Strip comments out of microcode files to avoid MythSim's parsing errors."
	print >> sys.stderr, "Usage: python stripcomments.py foo.ucode >> foo.ucode2"
	sys.exit(0)

# open file
F = file(sys.argv[1])

A = ""
for Line in F: A = A + string.expandtabs(stripComment(Line.strip())).strip() + " "
B = string.join(A.split())
Lines = B.split("; ")
print string.join(Lines, ";\n")

# done
F.close()
