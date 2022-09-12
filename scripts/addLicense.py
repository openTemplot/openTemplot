licenseText = """
(*  v1
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.

    Copyright (C) 2018  OpenTemplot project contributors

    This program is free software: you may redistribute it and/or modify
    it under the terms of the GNU General Public Licence as published by
    the Free Software Foundation, either version 3 of the Licence, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public Licence for more details.

    You should have received a copy of the GNU General Public Licence
    along with this program. See the files: licence.txt or opentemplot.lpr

    Or if not, refer to the web site: https://www.gnu.org/licenses/

                >>>     NOTE TO DEVELOPERS     <<<
                     DO NOT EDIT THIS COMMENT
              It is inserted in this file by running
                  'python3 scripts/addComment.py'
         The original text lives in scripts/addComment.py.

====================================================================================
*)

"""


"""
This program walks the source tree and inserts or replaces the license text
at the beginning of each source file.


"""

from pathlib import Path
import re
from string import whitespace
import sys

wanted_dirs = ["model", "test", "t2box_reader"]

def do_file(path_object):
    print(f"====> {path_object}")           # print the file name, then ...
#    sys.stdin.read(1)                       # ... PAUSE for a key press

    with open(f"{path_object}", encoding="iso-8859-1") as file:
        lines = file.readlines()

    ix = 0

    # first discard leading blank lines .....
    while ix < len(lines) and lines[ix].strip(whitespace) == "":
        ix += 1

    # then ensure we have a comment .....
    if re.match("^\s*\(\*", lines[ix]):
        # and if so, find the end of the comment .....
        while ix < len(lines) and not re.match("\*\)", lines[ix]):
            ix += 1

        # step past the end-of-comment .....
        if ix < len(lines):
            ix += 1

        # then discard any further blank lines .....
        while ix < len(lines) and lines[ix].strip(whitespace) == "":
            ix += 1

        # and an empty comment if we have one .....
        if ix < len(lines) and re.match("^\s*{\s*}", lines[ix]):
            ix += 1


# then rewrite the file from licensetext and any remaining lines...
    with open(f"{path_object}", encoding="iso-8859-1", mode="w") as file:
        file.writelines(licenseText)
        file.writelines(lines[ix:])



# --------------------------------------------------------------#

root_directory = Path(".")
for path_object in root_directory.glob('./**/*'):
    if path_object.is_file():
        parts = path_object.parts
        if path_object.suffix == ".pas" and \
            (parts.__len__() == 1 or parts[0] in wanted_dirs):
            do_file(path_object)
