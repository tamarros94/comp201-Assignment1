## Assignment 1 (Due: Tuesday, 26/11/19, Noon 14:00)

         - November 10, Avi Hayoun, Shahaf Shperberg, Mayer Goldbreg
- 1 General Contents
- 2 Introduction
- 3 A reader (parser for extended S-expressions (sexprs))
   - 3.1 The CFG
   - 3.2 Comments & whitespaces
      - 3.2.1 Whitespaces.
      - 3.2.2 Line comments
      - 3.2.3 Sexpr comments
   - 3.3 The concrete syntax of sexprs
      - 3.3.1 Boolean
      - 3.3.2 Number
      - 3.3.3 Symbol
      - 3.3.4 String
      - 3.3.5 Char.
      - 3.3.6 Nil
      - 3.3.7 Pair
      - 3.3.8 Quote-like forms
      - 3.3.9 Expression references.
- 4 Extending your parser without a formal specification
   - 4.1 Scientific notation
   - 4.2 Radix notation
   - 4.3 A final note regarding case sensitivity
- 5 What to submit
   - 5.1 Creating a patch file
- 6 Tests, testing, correctness, completeness
- 7 A word of advice
   - 7.1 A final checklist.


## 1 General

- You may work on this assignment alone, or with a single partner. You may not join a group
    of two or more students to work on the assignment. If you are unable to find or maintain a
    partner with whom to work on the assignment, then you will work on it alone.
- You are not to copy/reuse code from other students, students of previous years, or code you
    found on the Internet If you do, you are guilty of _academic dishonesty_. All discovered cases
    of _academic dishonesty_ will be forwarded to _va’adat mishma’at_ for disciplinary action. Please
    save yourself the trouble, and save us the effort and the inconvenience of dealing with you on
    this level. This is not how we prefer to relate to our students, but _academic dishonesty_ is a
    real problem, and if necessary, we will pursue all disciplinary venues available to us.
- You should be very careful to test your work before you submit. Testing your work means
    that all the files you require are available and usable and are included in your submission.
- Make sure your code doesn’t generate any unnecessary output: Forgotten debug statements,
    unnecessary print statements, etc., will result in your output being considered incorrect, and
    you will lose points. You will not get back these points by appealing or writing emails and
    complaints, so **please** be careful and make sure your code runs properly
- Your code should generate absolutely no warnings or error messages. If it does, you get a
    grade of zero.
- Late submissions result in a reduction of 5 points per day up to 5 days, any submission after
    more than 5 days will be not accepted. A late submission is, **by definition** , _any time_ past
    the official deadline, according to the departmental _submission system_ , i.e. submission of this
    assignment at 14:01 at the day of the deadline would consider a day late.
- Please read this document completely, from start to finish, before beginning work on the
    assignment.

## 2 Introduction

For this assignment, you shall write a _reader_ for Scheme.
There are two parts to this assignment:

1. Constructing a parser given a language specification (in the form of a CFG); and
2. Extending the gramer on your own based on a set of requirements.

You are required to complete both parts.
Your final parser will not support all of the concrete syntax supported by Chez Scheme, and
there are some non-standard extensions you will implement that are not supported by Chez Scheme
or are supported in different manner (these syntactic forms will be pointed out in the assignment
description).
You are given a template filereader.mlfor this assignment. If you miss any parts of this as-
signment, you will need to complete them by the deadline for the final project, since this assignment
is an important part of your complete compiler.


Please remember **not to change the signatures and types of the modules inreader.ml**.
Changing these signatures will make the automated tests fail and “award” you a grade of zero. No
appeals will be accepted for assignments with modified signatures or types.
In order to help you make sure that you did not make any breaking changes, we will provide
you with a “structure test”. Always run the structure test before submission. Passing the structure
tests does not mean that your assignment is correct; just that it does not break the interface we
require.

## 3 A reader (parser for extended S-expressions (sexprs))

In this part of the assignment you will write the first part of the _sexpr_ parser, which should accept
the language defined by the CFG provided below. Along with the next part of the assignment, the
result will constitute the first part of the pipeline of your Scheme compiler, the _reader_.
The _reader_ is a parser for _sexprs_ : It reads text from a string, and outputs an _abstract syntax tree_
for sexprs. You will need to use the _parsing combinators_ package (provided on the course website)
to write mutually-referential parsers for the various kinds of _sexprs_ , and use these to define two
procedures:

- read_sexpr : string -> sexpr
- read_sexprs : string -> sexpr list

The first procedure takes a string and assumes it contains one (and only one) _sexpr_ , whereas the
second procedure takes a string and assumes it contains any number of _sexprs_. Both functions
must return ASTs that correctly represent the input. Such ASTs are represented by thesexprtype
defined inreader.ml.
As you recognize different kinds of sexprs, you will need to provide a _pack_ wrapper for the
parsers, that will invoke the corresponding type constructors.
As seen in class, thesexprtype is a disjoint type of atomic and composite types:

**type** sexpr=
| **Bool of bool**
| **Nil**
| **Number of** number
| **Char of char**
| **String of string**
| **Symbol of string**
| **Pair of** sexpr* sexpr
| **TaggedSexpr of string** *sexpr
| **TagRef of string** ;;

Note that the sexpr type does not includevectoras you are not required to support vectors in
your compiler. Additionally, we shall not support the full numeric tower of Scheme. Instead, we
support only two types: integers and floating-point numbers. The type of numberencapsulates
both types, and is given inreader.ml:

**type** number =
| **Int of int**
| **Floatof float** ;;


Recall that the _reader_ is a parser for _sexprs_ (a language of data) and that some expressions are
valid _sexprs_ even though evaluating them in Scheme would lead to an error. For example,(if if
if if)and(1 2 3)are valid _sexprs_ which your _reader_ should parse successfully.

### 3.1 The CFG

The following CFG describes the first part of the language your parser should support. Following
the CFG definition is an extended discussion of the various forms of _sexpr_ which appear in this
CFG.

```
⟨ Sexpr ⟩::=⟨ Boolean ⟩ j ⟨ Char ⟩ j ⟨ Number ⟩ j ⟨ String ⟩
j ⟨ Symbol ⟩ j ⟨ List ⟩ j ⟨ DottedList ⟩ j ⟨ Quoted ⟩
j ⟨ QuasiQuoted ⟩ j ⟨ Unquoted ⟩
j ⟨ UnquoteAndSpliced ⟩ j ⟨ TaggedExpr ⟩
⟨ Boolean ⟩::=#fj#t
⟨ Char ⟩::=⟨ CharPrefix ⟩(⟨ VisibleSimpleChar ⟩
j ⟨ NamedChar ⟩)
⟨ CharPrefix ⟩::=#\
⟨ VisibleSimpleChar ⟩::= c , where c is a character that is greater than
the space character in the ASCII table
⟨ NamedChar ⟩::=newline,nul,page,return,space,tab
⟨ Digit ⟩::=( 0 j   j 9 )
⟨ Number ⟩::=⟨ Integer ⟩ j ⟨ Float ⟩
⟨ Integer ⟩::=(+j-)?⟨ Natural ⟩
⟨ Natural ⟩::=⟨ Digit ⟩+
⟨ Float ⟩::=⟨ Integer ⟩.⟨ Natural ⟩
⟨ String ⟩::="⟨ StringChar ⟩"
⟨ StringChar ⟩::=⟨ StringLiteralChar ⟩ j ⟨ StringMetaChar ⟩
⟨ StringLiteralChar ⟩::= c , where c is any character other than the
backslash character (\) or the double-quote
char (")
⟨ StringMetaChar ⟩::=\\j\"j\tj\fj\nj\r
⟨ Symbol ⟩::=⟨ SymbolChar ⟩+
⟨ SymbolChar ⟩::=( 0 j    j 9 )j(aj    jz)j(Aj    jZ)j!j$
j^j*j-j_j=j+j<j>j?j/j:
⟨ List ⟩::=(⟨ Sexpr ⟩)
⟨ DottedList ⟩::=(⟨ Sexpr ⟩+.⟨ Sexpr ⟩)
⟨ Quoted ⟩::='⟨ Sexpr ⟩
⟨ QuasiQuoted ⟩::=`⟨ Sexpr ⟩
⟨ Unquoted ⟩::=,⟨ Sexpr ⟩
⟨ UnquoteAndSpliced ⟩::=,@⟨ Sexpr ⟩
⟨ TaggedExpr ⟩::=⟨ Tag ⟩(=⟨ Sexpr ⟩)?
⟨ Tag ⟩::=#{⟨ Symbol ⟩}
```
### 3.2 Comments & whitespaces

An _sexpr_ may contain whitespace characters and comments. Your parser will have to know to skip
over these as it constructs the AST for the _sexpr_ it’s reading.


#### 3.2.1 Whitespaces.

Any character with an ASCII value less than or equal to the _space_ character, is considered a
whitespace for the purpose of this assignment. Whitespaces may appear wherever Chez Scheme
accepts them.

#### 3.2.2 Line comments

Line comments start with the semicolon character;and continue until either an _end-of-line_ or
_end-of-input_ is reached. The semicolon may appear anywhere on the line, and need not be the first
character!
This kind of comment is used to document your code.

#### 3.2.3 Sexpr comments

Scheme includes another kind of comment, not so much to document your code as to “hide” the
next _sexpr_ without actually removing it from the source file. This kind of “commenting out” is
very handy when debugging code.
_Sexpr_ comments start with the characters#;and continue to the end of the subsequent _sexpr_.
There may be no space or comment between the#and the;characters, however the _sexpr_ following
the#;characters may be separated from them with _whitespaces_. For example, the following is a
valid _sexpr_ -comment:#; a. A _sexpr_ being commented out in this way can be any valid _sexpr_ : A
number, a Boolean, a symbol, but more often than not, a deeply-nested expression with balanced
parentheses.
Please note that while removing comments is usually “easy”, _sexpr_ -comments are non-trivial,
because they rely on the ability of your parser to recognize what is a valid _sexpr_! This is your first
_recursive production_.
Note that _sexpr_ -comments may be used in succession to ‘remove’ consecutive _sexprs_. For ex-
ample, the expression: #;#;(+ 1 2) 1 "Only one left" evaluates to the string "Only one
left".

### 3.3 The concrete syntax of sexprs

#### 3.3.1 Boolean

There are two Boolean values in Scheme: #f(for _false_ ), and#t(for _true_ ). Your _reader_ should
recognize these in a case-insensitive manner, so that#tand#Tare treated the same, etc. Boolean
nodes in the AST are represented using theBooltype constructor of thesexprtype.

#### 3.3.2 Number

Scheme has a rich numerical tower. We will not be supporting the full numerical tower in our
compiler, but we do want to experience the polymorphism of aritmetic procedures in Scheme, so
we will support two kinds of numbers: integers & floating-point numbers. Number nodes in the
AST are represented using theNumbertype constructor of thesexprtype. Note that theNumber
type constructor takes values of typenumberwhich are constructed using theIntandFloattype
constructors.

1. Integers


```
Integers can be positive or negative. They may begin with any number of zeros. They may
come with an optional sign, which can either be positive or negative. Here are examples of
valid integers in our language:
```
- 1234
- 01234
- -
- -
- -
- +
- +
2. Floating-point numbers Floating point numbers are inexact representations of real values.
Like integers, floating-point numbers may begin with any number of zeros and may include
an optional sign (positive or negative). Additionally, they may include any number of trailing
zeros after the decimal point. Here are some examples of valid floating-point numbers in our
language:
- 1.
- 0005.
- 501.
- -0.
- +999.
- -102.

```
In practice, there is a limit on the number of digits following the decimal point. In the interest
of simplicity, your parser will allow an unlimited number of digits, and rely on OCaml to
truncate the extra digits.
```
#### 3.3.3 Symbol

In an interactive Scheme system, a symbol is represented internally as a _hashed string_. Our compiler,
however, is not an _interactive_ (or _online_ ) compiler, but a _batch_ (or _offline_ ) one, so the AST for
symbols will just contain the string used to represent the symbol.
In principle, a symbol in Scheme can contain any character. This is the case for symbols that
have been created _dynamically_ from strings, usingstring->symbol. Constant symbols that are
read in by the _reader_ obey a stricter syntax. The characters in a symbol may include the following
characters:

- The lowercase letters:a, ... ,z
- The uppercase letters:A, ... ,Z
- Digits: 0 , ... , 9
- Punctuation:!$^*-_=+<>/?


Scheme has traditionally been _case-insensitive_ with respect to symbols, although this has
changed in R^6 RS (the 6thversion of the Scheme standard). For the purpose of this course, you
must support case-insensitivity for symbols in the following way: Your parser should convert all
literal symbol characters to lowercase. Hence the symbolsabc,Abc,aBc, andABCare all the same
object, which is stored internally asabc.
Symbol nodes in the AST are represented using theSymboltype constructor of thesexprtype.

#### 3.3.4 String

Strings are monomorphic arrays of characters. Syntactically, strings in Scheme are delimited by
double-quote marks, may contain any character, and span across several lines. Here are some
examples of strings:

"moshe"

"a string"

"This is a very long
string that spills across
several lines."

1. String meta-chars
    Some characters cannot appear in a string without being preceded by a special _backslash_
    prefix. This is the exact same situation as in C, C++, Java, Python, and many other
    programming languages. The list of meta-characters you need to support is:

```
Char ASCII Concrete syntax
return ASCII 13 \r
newline ASCII 10 \n
tab ASCII 9 \t
page ASCII 12 \f
backslash ASCII 92 \\
double quote ASCII 34 \"
```
```
String nodes in the AST are represented using theStringtype constructor of thesexprtype.
```
#### 3.3.5 Char.

Characters are denoted by the character prefix#\. There can be no whitespace or comments
between the the#&\characters. What follows the prefix falls under one of several categories:

1. Named chars
    Some characters are denoted by their full name:


```
Char ASCII Concrete syntax
nul ASCII 0 #\nul
newline ASCII 10 #\newline
return ASCII 13 #\return
tab ASCII 9 #\tab
formfeed ASCII 12 #\page
space ASCII 32 #\space
```
```
Named characters are case insensitive, so that#\page,#\Page, and#\PAGEare all denote
the same character.
```
2. Visible chars
    Characters that are in the visible range (i.e., have an ASCII value that is larger than 32) can
    be entered “as-is”, with the character prefix. Here are some examples:
       - #\a
       - #\A
       - #\?

```
Notice that visible characters are case-sensitive, so that#\a&#\Aare distinct.
Char nodes in the AST are represented using theChartype constructor of thesexprtype.
```
#### 3.3.6 Nil

Nil, or the empty list, is simply a matching pair of parentheses:(). These may enclose whitespaces
and comments. Nil nodes in the AST are represented using theNiltype constructor of thesexpr
type.

#### 3.3.7 Pair

The concrete syntax for pairs respects the two _dot rules_ , generating _proper lists_ and _improper lists_.
The formal grammar for these is as follows:

```
⟨List⟩ ::=(⟨Sexpr⟩ )
⟨DottedList⟩ ::=(⟨Sexpr⟩+: ⟨Sexpr⟩ )
You will need to convert both proper and improper lists to nested instances ofPair.
```
#### 3.3.8 Quote-like forms

You need to support 4 quote-like forms:

```
⟨Quoted⟩ ::= ′⟨Sexpr⟩
⟨QQuoted⟩ ::= ‘⟨Sexpr⟩
⟨UnquotedSpliced⟩ ::= ;@⟨Sexpr⟩
⟨Unquoted⟩ ::= ;⟨Sexpr⟩
```

For these, you should generate thesexprs Pair(Symbol(name), Pair(sexpr, Nil()), where
nameis one ofquote,quasiquote,unquote-splicing, andunquote, respectively, andsexpris
the _sexpr_ that follows the quote-like tag.

#### 3.3.9 Expression references.

Chez Scheme provides syntax for tagging expressions in order to reference them again in the same
s-expression. The syntax for tagging an expressionsuses the form#n=, wherenis an integer, while
#n#indicates a reference to an expression tagged byn. For example, in the expression'(#0=a #0#)
the tag#0#indicates a reference to the tagged sub-expressiona. The value of this expression is(a
a).
The main usage of expression references is to support literal cyclic lists, lists in which one of
the elements is the list itself. Lexical support of expression references is the only way to support
cyclic structures at compile time (statically) in Scheme. Without using expression references, such
structures can only be defined during runtime by using side-effects, such as the functionsset-car!
andset-cdr!.
For example, we can write in Scheme the following expression:

> '#0=(a b. #0#)
#0=(a b. #0#)

```
in order to create to following improper list:
```
```
You are required to support a slightly different syntax:
```
- Instead of using integers for tags, use symbols
- Use#{symbol_name}for both tagging an expression and referencing one.

For example: the expression#{lst}=(a b. #{lst})is equivalent to#0=(a b. #0#)in Chez
Scheme.
Your _reader_ must also detect duplicate tag definitions within a single s-expression: using
the same symbol in order to tag more than one expression is forbidden. When encountering
an input which includes such a violation you should raise theX_this_should_not_happen ex-
ception. Note that the same symbol can be used for tagging in separate s-expressions. I,e.
read_sexpr "#{foo}=(#{foo}=1 2 3)"should raise theX_this_should_not_happenexception,
whileread_sexprs "#{foo}=(1 2 3) (1 #{foo}=2 #{foo})"should parse the two separate
s-expressions successfully.


Tagged expression nodes in the AST are represented using theTaggedSexprtype constructor
of thesexprtype, while references to tags are represented using theTagReftype constructor.
Note that Despite the fact that the _reader_ parses cyclic structures, it produces a **tree** (AST),
which is acyclic. For example, the expected output of your _reader_ for the input expression#{x}=(a

. #{x})isTaggedSexpr ("x", Pair (Symbol "a", TagRef "x")). As you can see, the type
used by theTagRefconstructor is string and not a reference to the memory address of the root of
the AST. Re-encoding the cycle explicitly (creating a cycle in memory) will be handled in a later
stage of your compiler (during the code-generation phase).

## 4 Extending your parser without a formal specification

### 4.1 Scientific notation

Your parser must also support numbers (both integer and floating-point) given in scientific notation.
Valid numbers given in scientific notation begin with either a valid integer or a valid floating point
number, followed by either the charactereor the characterE, followed by a valid integer. Here are
some examples of valid numbers in scientific notation in our language:

- 1e
- 1E+
- 10e-
- 3.14e+
- 3.14E-
- +000000012.3E
- -5.000000000e-
    Note that both integers and floating-point numbers may be given in scientific notation. You are
expected to use the correct type constructor for a valid number in scientific notation in the final
AST.

### 4.2 Radix notation

Arbitrary radixes from two through 36 may be specified with the prefix#nr, wherenis the radix.
Case is not significant, so#nRmay be used as well. Digit values from 10 through 35 are specified
as either lower- or upper-case alphabetic characters. Here are some examples:

- #36rZZis 35 × 36 + 35, or 1295.
- #16R11.8ais 17.
- #2r-1101is -
- #2r+1101is 13 (similar to#2r1101)
    Despite the fact that Chez scheme support mixing scientific and radix notation for bases up to
14 (inclusive), you are not required to support this behavior.
Note that both integers and floating-point numbers may be given in radix notation. You are
expected to use the correct type constructor for a valid number in scientific notation in the final
AST.


### 4.3 A final note regarding case sensitivity

Most expressions are meant to be case-insensitive, that is#tand#Tare meant to be the same, as
well as#\spaceand#\SPACE, etc. The only expressions that are case-sensitive are:

- ⟨ _VisibleSimpleChar_ ⟩
- ⟨ _StringLiteralChar_ ⟩

```
For these, the case should remain as the user has entered them, be it uppercase or lowercase.
```
## 5 What to submit

In this course, we use thegitDVCS for assignment publishing and submission. You can find more
information ongitathttps://git-scm.com/.
To begin your work, clone the assignment template from the course website:
git clone https://www.cs.bgu.ac.il/~comp201/compiler
This will create a copy of the assignment template folder, namedcompiler, in your local
directory. The template contains three (3) files:

- reader.ml(the assignment interface)
- pc.ml(the _parsing combinators_ library)
- readme.txt

The filereader.mlis the interface file for your assignment. The definitions in this file will
be used to test your code. If you make breaking changes to these definitions, we will be unable
to test and grade your assignment. Do not break the interface. Operations that are considered
interface-breaking:

- Modifying the line: #use "pc.ml"
- Modifying the module signatures and types defined in the file

Other than breaking the interface, you are allowed to add any code and/or files you like.
Among the files you are required to edit is the filereadme.txt.
The filereadme.txtshould contain

1. The names and IDs of all the people who worked on this assignment. There should be either
    your own name, or your name and that of your partner. You may only have one partner for
    this assignment.
2. The following statement:

```
I (We) assert that the work we submitted is 100% our own. We have not received any
part from any other student in the class, nor have we give parts of it for use to others.
Nor have we used code from other sources: Courses taught previously at this university,
courses taught at other universities, various bits of code found on the Internet, etc.
We realize that should our code be found to contain code from other sources, that a
formal case shall be opened against us with va’adat mishma’at , in pursuit of disciplinary
action.
```

Submissions are only allowed through thesubmission system.
You are required to submit a **patch file** of the changes you made to the assignment template.
See instructions on how to create a patch file below.
Please note that any modifications you make topc.mlwill be discarded during the testing
process, as our testers will use our version ofpc.mlfor the tests.
You are provided with astructure testin order to help you ensure that our tests are able to
run on your code properly. Make sure to run this test on your final submission.

### 5.1 Creating a patch file

Before creating the patch, review the change list and make sure it contains all the changes that
you applied and noting more. Modified files are automatically detected by git but new files must
be added explicitly with the ‘git add’ command:

```
git add -Av .; git commit –m “write a commit message”
```
```
At this point you may review all the changes you made (the patch):
```
```
git diff origin
```
Once you are ready to create a patch, simply make sure the output is redirected to the patch
file:

```
git diff origin > compiler.patch
```
After submission (but before the deadline), it is strongly recommended that you download,
apply and test your submitted patch file. Assuming you downloadassignment1.patchto your
home directory, this can be done in the following manner:

cd ~
git clone https://www.cs.bgu.ac.il/~comp201/compiler fresh_compiler
cd fresh_compiler
git apply ~/compiler.patch

Then test the result in the directoryfresh_assignment1.
Finally, remember that your work will be tested on lab computers only! We advise you to test
your code on lab computers prior to submission!

## 6 Tests, testing, correctness, completeness

Please start working on the assignment ASAP! Please keep in mind that:

- We will not (and cannot) provide you with anything close to full coverage. It’s your respon-
    sibility to test and debug and fix your own code! We’ll do our best to provide you with many
    useful tests, but this is not and cannot be all that you rely on!
- We encourage you to contribute and share tests! Please do not share OCaml code.
- This assignment can be tested using OCaml and Chez Scheme. You don’t really need more,
    beyond a large batch of tests...
- We encourage you to compile your code and test early and frequently.


## 7 A word of advice

The class is very large. We do not have the human resources to handle late submissions or late
corrections from people who do not follow instructions. By contrast, it should take you very little
effort to make sure your submission conforms to what we ask. If you fail to follow the instructions
to the letter, you will not have another chance to submit the assignment: If files your work depends
on are not included in your submission, if functions you submit don’t work as they are supposed
to, if the statement asserting authenticity of your work is not included in your submission, if your
work generates output that is not called for (e.g., because of leftover debug statements), etc., then
you’re going to have points deducted. The graders are instructed not to accept any late corrections
or re-submissions under any circumstances.

### 7.1 A final checklist.

1. You completed the module skeleton provided in thereader.mlfile
2. You did not change the module signatures
3. Your parser runs correctly under OCaml on the departmental Linux image
4. You completed the readme.txt file that contains the following information: a) Your name and
    ID b) The name and ID of your partner for this assignment, assuming you worked with a
    partner. c) A statement asserting that the code you are submitting is your own work, that
    you did not use code found on the Internet or given to you by someone other than the teaching
    staff or your partner for the assignment.
5. You submitted you work in its entirety


