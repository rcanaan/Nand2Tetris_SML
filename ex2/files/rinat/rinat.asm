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

@rinat.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@3030
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@THIS
M=D
@SP
M=M-1

@3040
D=A
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

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@2
D=A
@THIS
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@46
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@6
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
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@2
D=A
@THIS
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
M=M-D
@SP
M=M-1

@6
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

@rinat.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@3030
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
A=M-1
D=M
@THIS
M=D
@SP
M=M-1

@3040
D=A
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

@32
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@2
D=A
@THIS
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@46
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@6
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
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

@2
D=A
@THIS
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
M=M-D
@SP
M=M-1

@6
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
