
# OpenTemplot
OpenTemplot is an open source fork of the Templot2 program by Martin Wynne.

This has now been superceeded by the open-source release of Templot5 by Martin Wynne.

See https://github.com/Martin-Wynne/Templot5 for details of Templot5

See README.txt for the original ReadMe by Martin.

This project will continue as an experimental area for a major rewrite of Templot2 that may eventually
be included into Templot5.

**Warning: This is an ongoing work-in-progress. Do not expect any files created using this 
progream to be usable in any future version**

## Goals of this fork

In no particular order, the goals for this fork are:
* create an open source, functionally equivalent version of Templot
* make OpenTemplot compile and run across several platforms, including Win32, Win64, Linux (x86, amd64 and RPi).
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
* PasDoc
  
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


