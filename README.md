# racket-logicalc
A command-line program that can solve infix Boolean expressions using variables and constants

## Features
 - Boolean expressions are parsed using an extended BNF grammar (once again thanks to [BRAG](http://docs.racket-lang.org/brag/))
 - Supports AND, OR, NOT, IF/THEN, and IF/ONLY IF
 - Supports operator precedence
 - Supports TRUE and FALSE constants
 - Supports case sensitive variable names (_letters only_)
 - Typing "q" stops the program
 - Typing "h" shows you what you read here
 
## Why do this?
Because I've always wanted. _No seriously_, I've always wanted to write something that lets me solve arbitrary boolean expressions and Racket seemed perfect for it. I blame that whole _wanting-to-double-major-in-math-and-philosophy-in-college_ thing - but if it wasn't for Gregory Chaitin's [Meta Math!: The Quest for Omega](https://www.amazon.com/Meta-Math-Quest-Gregory-Chaitin/dp/1400077974) I probably wouln't have learned about LISP either...

## How To Run This Thing
To run **racket-logicalc**, find wherever you extracted the .tgz to and the binary should be **/bin**. Once there you would type`./logicalc`. Because **racket-logicalc** was compiled for distibution on a Linux machine, it's binary is said to [only run on other Linux machines](https://docs.racket-lang.org/raco/exe.html).

## Project Outline
- |- **build** _Build files generated by DrRacket for distribution_
- |- **dist** _A .tgz of the build files generated by DrRacket_
- |- **src**
  - |- **logicalc.rtk** _Defines main program_
  - |- **logicalc-expander.rtk** _Module that evalutates a boolean expression as a parsed syntax object_
  - |- **logicalc-grammar**. _Defines grammar used by the BRAG parser_
  - |- **logicalc-reader.rtk** _Module that transforms a boolean expression string into a parsed syntax object_
  - |- **logicalc-value-set.rtk** _Generates all the combinations of truth values used to solve a boolean expression with_
- |- **README.md** _Welcome. You are here right now._
- |- **LICENSE.md** _Licensing info...because apparently that's what you're suppose to do now_
