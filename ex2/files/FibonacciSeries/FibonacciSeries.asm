
@1
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@THAT
M=D
@SP
M=M-1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@0
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@1
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@SP
M=M-1
@0
D=A
@ARG
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

(FibonacciSeries.MAIN_LOOP_START)

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
A=M
D=M
@FibonacciSeries.COMPUTE_ELEMENT 
D;JNE

@FibonacciSeries.END_PROGRAM       
0;JMP

(FibonacciSeries.COMPUTE_ELEMENT)

@0
D=A
@THAT
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@THAT
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@SP
M=M-1
@2
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
@THAT
M=D
@SP
M=M-1

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@SP
M=M-1
@0
D=A
@ARG
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@FibonacciSeries.MAIN_LOOP_START
0;JMP

(FibonacciSeries.END_PROGRAM)

@256
D=A
@SP
M=D
@Sys.init.ReturnAddress
D=A
@SP
A=M
M=D
@SP
M=M+1
@LCL
D=M
@SP
A=M
M=D
@SP
M=M+1
@ARG
D=M
@SP
A=M
M=D
@SP
M=M+1
@THIS
D=M
@SP
A=M
M=D
@SP
M=M+1
@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1
@SP
D=M
@5
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D

@FibonacciSeries.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@1
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@THAT
M=D
@SP
M=M-1

@0
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@0
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@1
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@SP
M=M-1
@0
D=A
@ARG
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

(FibonacciSeries.MAIN_LOOP_START)

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
A=M
D=M
@FibonacciSeries.COMPUTE_ELEMENT // if num_of_elements > 0, goto COMPUTE_ELEMENT
D;JNE

@FibonacciSeries.END_PROGRAM        // otherwise, goto END_PROGRAM
0;JMP

(FibonacciSeries.COMPUTE_ELEMENT)

@0
D=A
@THAT
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@THAT
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@SP
M=M-1
@2
D=A
@THAT
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@THAT
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
@THAT
M=D
@SP
M=M-1

@0
D=A
@ARG
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@SP
M=M-1
@0
D=A
@ARG
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@FibonacciSeries.MAIN_LOOP_START
0;JMP

(FibonacciSeries.END_PROGRAM)

