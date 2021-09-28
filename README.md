
# templot3
Templot3 is an open source fork of the Templot2 program by Martin Wynne.

See README.txt for the original ReadMe by Martin.

Templot2 is still the official version of Templot. It is still under active development
and can be found at <http://templot.com>

**Warning: the box3 file format currently used by Templot3 is _not_ cross-platform compatible. In particular, a box3
file saved using a Win32 build will not work with any other platform build (Win64, Linux etc)**

## Goals of this fork

In no particular order, the goals for this fork are:
* create an open source, functionally equivalent version of Templot
* make templot3 compile and run across several platforms, including Win32, Win64, Linux (x86, amd64 and RPi).
* work on improving the code structure/design.
* add currently unsupported features from templot2

## Tools

Tools used in this project are:
* Lazarus, v2.0.10 (either Win32 or Win64)
* Lazarus packages (through Install/Uninstall Packages):
  * lazprojectgroups
* Lazarus packages via Online Package Manager:
  * HTMLViewer
  * PowerPDF
  * VirtualTreeView  
* AsciiDoctor, plus extensions:
  * AsciiDoctor Diagram
  * AsciiDoctor PDF
  
## Git Conventions

Every commit needs a commit message :-)

Commit messages should follow the following format:

```
<type>:<area>:<message>
```

Where `<type>` is one of:
* `fix`
* `feat` (ure)
* `ref` (actor)
* `doc` (umentation)

Please try to keep changes of different types in separate commits. It is easier to review a sequence of small commits, than one single, large commit.

## Documentation

Any documentation created should be stored in the repo in AsciiDoc (.adoc) format, _not_ in the wiki. Hopefully this README will be the only .md file 
in the repo :-)


