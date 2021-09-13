
# This should be executed using the command:
#
#     sed -Ei --file ext2double.sed  *.pas
#
# where "ext2double.sed" is the name of the file containing this script

# REMEMBER to go into the sub-directories and run it there too  :-)

s/(:\s*)extended(\s*;)/\1double\2/gi
s/(:\s*)extended(\s*=)/\1double\2/gi
s/(:\s*)extended(\s*\))/\1double\2/gi
s/(:\s*)extended(\s*\{)/\1double\2/gi
s/(of \s*)extended(\s*;)/\1double\2/gi
s/(of \s*)extended(\s*=)/\1double\2/gi
s/(of \s*)extended(\s*\))/\1double\2/gi
