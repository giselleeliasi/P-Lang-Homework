Giselle Eliasi and Jillian Hunter CMSI 3801 Homework

Description-
Throughout this course we are learning and producing code in different languages. 

## Homework 1 Function Descriptions:
Change: This function finds the least number of coins needed to make an integer in cents and returns an error if the input is not an integer or is negative.

First_then_lower_case: This function iterates through a sequence and applys a predicate function to each element which returns the first string to statisfy this predicate (in lowercase) and returns none if no element is found.

Powers_generator: This function returns powers of a given base starting at 1 up to a limit by multiplying the base until the result exceeds the limit.

Say: This function returns a word and appends it to a sentence with each word seperated by one space and when called without an argument, returns the accumulated sentence.

Quaternion: This class contains functions for addition, multiplication, string representation, and equality checks for quaternions and also gives properties for accessing coefficents, conjugatem and norm. 

Roles-


Python:
Q1:Giselle
Q2:Jillian
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian

Lua:
Q1:Giselle
Q2:Jillian
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian

Javascript:
Q1:Giselle
Q2:Jillian
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian

## Homework 2 Function Descriptions:

Change: This function finds the least number of coins needed to make an integer in cents and returns an error if the input is not an integer or is negative.

First_then_lower_case: This function iterates through a sequence and applys a predicate function to each element which returns the first string to statisfy this predicate (in lowercase) and returns none if no element is found.
MeaningfulLineCount: 

Say: This function returns a word and appends it to a sentence with each word seperated by one space and when called without an argument, returns the accumulated sentence.

MeaningfulLineCount: Each of these functions counts the number of non-blank, non-comment lines within the file and counts the lines which follow the criteria. Each function provides error handling as in Java it uses throws whereas Swift and Kotlin handles errors within a block to catch exceptions with Swift returning a Result type.

Quaternion: This class contains functions for addition, multiplication, string representation, and equality checks for quaternions and also gives properties for accessing coefficents, conjugatem and norm. 

BinarySearchTree: This class focused on two different approaches: in Swift we used an enum based approach whereas in Java and Kotlin it utilized a sealed interface approach. Broadly, all languages modeled the tree recursivly, they each returned a new tree instead of modifying the existing one, and compared the searched value to the current nodes value to decide its next move. 
Roles-


Java:
Q1:Giselle
Q2:Jillian
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian
Q6:Giselle & Jillian 

Kotlin:
Q1:Jillian
Q2:Jillian
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian
Q6:Giselle & Jillian 

Swift:
Q1:Giselle
Q2:Jillian
Q3:Giselle
Q4:Giselle
Q5:Giselle & Jillian
Q6:Giselle & Jillian 


## Homework 3 Function Descriptions:

Change: This function calculates the minimum number of coins needed to make a given amount given certain currency ( like dimes, quarters etc.)For negative amounts it either raises an error or returning an error massage.

First_then_apply: Finds the first element in a collection that satisifies a predicate and then applies a transformation function to that element. Returns the transformed result or an indictation there was no matching elelemnt.

MeaningfulLineCount: Counts the number of non empty, non comment lines in a file. Reads the file line by line, filtering out the lines that dont satisfy those condtions.

Powers: Generates an infinite sequence of powers of a given base starting form the base raised to the power of 0. It incrementally increases the exponent with each iteration.

Shape:  This intializes the shape and has two possibilities : box, or sphere

Volume: This calculates the volume of a shape which can be either a sphere or a box. It then uses the corresponding formula based on the type.

surface_area:This function calculates the surface area of a shape which is either a sphere or a box. Then it uses the corresponding formula for each type.

BinarySearchTree: Stores an element in hierarchical order with each node having at most two children. It allows for searching, insertion, and deletions.
Roles-

TypeScript:
Q1:Giselle
Q2:Giselle
Q3:Giselle
Q4:Jillian
Q5:Giselle & Jillian
Q6:Giselle & Jillian 
Q7:Jillian
Q8: Giselle

OCamel:
Q1:Giselle
Q2:Giselle
Q3:Giselle
Q4:Jillian
Q5:Jillian
Q6:Jillian
Q7:Giselle & Jillian
Q8: Jillian

Haskell:
Q1:Jillian
Q2:Jillian
Q3:Giselle
Q4:Giselle
Q5:Giselle 
Q6:Giselle & Jillian 
Q7:Giselle
Q8: Jillian


## Homework 4 Function Descriptions:
These three implementations demonstrate a Stack data structure across Rust, C++, and C, each showing their language's distinct approaches to memory management. All three use stack operations push, pop, and capacity.
Rust: Uses ownership semantics and Vec
C++: Uses smart pointers and utilizes RAII principles [see here] (https://en.cppreference.com/w/cpp/language/raii)
C: Uses manual memory management  with malloc and free

Roles-


Rust: Giselle & Jillian


C: Giselle & Jillian


C++: Giselle & Jillian


## Homework 5 Function Descriptions:
This is a program that simulates a restaurant with cooks and customer. There are 10 customers with 3 cooks and 1 waiter that can only hold 3 orders at once. The order takes a random time to complete but after 7 seconds the customer abandons the order and later returns. The program terminates when each customer finishes 5 meals and goes home.   
Roles-    
     
Go: Jillian & Giselle 
## Instructions

Fork this repo for your homework submissions. Make sure your fork has a nice, descriptive name. Leaving the name as “lmu-cmsi-3801-template” is misleading, and keeping it indicates you are not taking sufficient pride in your work. After forking, **please replace the contents of this readme** file with information about your submissions, including the name(s) of the students, and a description of each assignment (as they are turned in).

Don’t bother with notes to the graders. Such notes go into your BrightSpace submissions, not your GitHub repository.

Your homework submissions will consist of programs in the following languages. To keep things simple, there is a separate folder for each language.

- **Homework 1 (Scripting)**: Lua, Python, JavaScript
- **Homework 2 (Enterprise)**: Java, Kotlin, Swift
- **Homework 3 (Theory)**: TypeScript, OCaml, Haskell
- **Homework 4 (Systems)**: C, C++, Rust
- **Homework 5 (Concurrency)**: Go

At each homework deadline, the graders will clone your repo and run the tests. I will be inspecting the source code, grading your work on style, clarity, and appropriate use of language idioms. Do not throw away points in these areas: **use code formatters and linters**. Please consider it a moral obligation to use these tools. Not doing so is a violation of professional ethics. _You must respect the naming, capitalization, formatting, spacing, and indentation conventions of each language_.

## The Test Suites

The test files are included in the repo already. They are available for YOU! They will help you not only learn the languages and concepts covered in this course, but to help you with professional practice. You should get accustomed to writing code to make tests pass. As you grow in your profession, you will get used to writing your tests early.

The test suites are run like so (assuming you have a Unix-like shell, modify as necessary if you have Windows):

### Lua

```
lua exercises_test.lua
```

### Python

```
python3 exercises_test.py
```

### JavaScript

```
npm test
```

### Java

```
javac *.java && java ExercisesTest
```

### Kotlin

```
kotlinc *.kt -include-runtime -d test.jar && java -jar test.jar
```

### Swift

```
swiftc -o main exercises.swift main.swift && ./main
```

### TypeScript

```
npm test
```

### OCaml

```
ocamlc exercises.ml exercises_test.ml && ./a.out
```

### Haskell

```
ghc ExercisesTest.hs && ./ExercisesTest
```

### C

```
gcc string_stack.c string_stack_test.c && ./a.out
```

### C++

```
g++ -std=c++20 stack_test.cpp && ./a.out
```

### Rust

```
cargo test
```

### Go

```
go run restaurant.go
```

## Grading Notes

Your grade is a reflection not only of your ability to write code to pass existing tests, but also of your ability to construct software in a professional setting. Therefore, the following will contribute rather heavily to your score:

- **Following all submission instructions**! Pay attention to every requirement such as replacing the contents of this readme file and including the names of all participants of any group work.
- **Keeping a pristine GitHub repository**. Do not push any file that does not belong (including but not limited to that silly `DS_Store`). Make sure all generated executables, intermediate files, third-party libraries, etc. are not committed. Your repo contents should be limited to your solution files, tests, configuration files, and `.gitignore` files.
- **Adherence to naming and formatting conventions for the language you are writing in**. Inconsistent indentation, for example, has no place in professional or student software. Neither does end-of-line whitespace, huge chunks of contiguous blank lines, and other types of messy coding practices that show friends, family, colleagues, and potential employers that you don’t care about your work.
- (As always) The **readability and maintainability** of your code.
