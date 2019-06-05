# decaff-compiler-Kritikalcoder
decaff-compiler-Kritikalcoder created by GitHub Classroom  
  
Decaf Compiler - DCC  
  
## Structure
- ast.cpp: Defining constructors, IR Generation (SSA) functions & getters  
- ast.h: Header file for ast.cpp  
- constructs.cpp: Defining LLVM Constructs  
- constructs.h: Header file for constructs.cpp  
- Makefile: File for making the entire source  
- parser.ypp: Defining the rules of the decaf parser  
- scanner.l: Input rules file for scanner generator  
- test-programs/  
    - positive test files (arraysum.dcf even.dcf fact.dcf maxmin.dcf prime.dcf)  
    - negative test files (binary.dcf bubble.dcf linear.dcf merge.dcf quick.dcf)  
  
## Run
Step 1: (Remove any existing made files:)  
`make clean`  
Step 2: (Make the source)  
`make`  
Step 3: (Generate IR of an input file, Ex: test-programs/maxmin.dcf)  
`./parser test-programs/maxmin.dcf > maxmin.ll`  
Step 4: (Interpret IR generated using lli)  
`lli maxmin.ll`  
 
## Description
This project has been built on a Linux system (Ubuntu 16.04 OS), with LLVM 6.0 as the backend. By making the source and then using the generated parser on a test decaf program, we are able to emit a persistent LLVM-6.0 compatible Intermediate Representation. For example, the IR generated for test.dcf is test.ll, which can be interpreted by the LLVM Interpreter (lli).

## Features yet to be Implemented
1. Semantic Type Compatibility checks
2. Visitor Design Pattern
3. Dead code elimination & further optimizations  
4. Taking Input  
5. Comments
