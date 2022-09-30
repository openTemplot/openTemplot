licenseText_A = """
(*
    This file is part of OpenTemplot, a computer program for the design of
    model railway track.
"""

licenseText_T2 = """
    Copyright (C) 2018  Martin Wynne.  email: martin@templot.com"""

licenseText_B = """
    Copyright (C) 2019  OpenTemplot project contributors

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


from enum import Enum
from pathlib import Path
import re
from string import whitespace
import sys

from units_list import leaveAloneUnits, leaveAloneDirs, T2Units

class srcType(Enum):
    isDir   = 0 # Directory
    doOT    = 1 # Open Templot license comment needed
    doT2    = 2 # Templot 2 license comment needed
    doNowt  = 3 # No changes needed to license comment

def fileType(path_object):
    if not path_object.is_file():
        return srcType.isDir

    parts = path_object.parts
    if parts.__len__() > 1 and parts[0] in leaveAloneDirs:
        return srcType.doNowt

    if not path_object.suffix in [".pas", ".lpr"]:
        return srcType.doNowt

    if parts[-1] in leaveAloneUnits:
        return srcType.doNowt

    if parts[-1] in T2Units:
        return srcType.doT2

    return srcType.doOT


def doFile(path_object, fType):
    print(f"====> {fType.name} : {path_object}")           # print the file name, then ...
#    sys.stdin.read(1)                       # ... PAUSE for a key press

    with open(f"{path_object}", encoding="iso-8859-1") as file:
        lines = file.readlines()

    if len(lines) == 0:  # Just in case of an empty source file ....
        return

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
        file.writelines(licenseText_A)
        if fType == srcType.doT2:
            file.writelines(licenseText_T2)
        file.writelines(licenseText_B)
        file.writelines(lines[ix:])



# --------------------------------------------------------------#

rootDirectory = Path(".")
for pathObject in rootDirectory.glob('./**/*'):
    fType = fileType(pathObject)
    if fType == srcType.isDir:
        pass
    elif fType == srcType.doNowt:
        pass
    else:
        doFile(pathObject, fType)

