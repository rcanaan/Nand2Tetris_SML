
(NestedCall.Sys.init)
@0
D=A
@NestedCall.Sys.init.End.1
D;JEQ
(NestedCall.Sys.init.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.init.Loop.1
D=D-1
D;JNE
(NestedCall.Sys.init.End.1)

@4000
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

@5000
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

@NestedCall.Sys.main.6.ReturnAddress
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
@NestedCall.Sys.main
0;JMP
(NestedCall.Sys.main.6.ReturnAddress)


@SP
M=M-1
@1
D=A
@5
D=A+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

(Sys.LOOP)

@Sys.LOOP
0;JMP

(NestedCall.Sys.main)
@5
D=A
@NestedCall.Sys.main.End.10
D;JEQ
(NestedCall.Sys.main.Loop.10)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.main.Loop.10
D=D-1
D;JNE
(NestedCall.Sys.main.End.10)

@4001
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

@5001
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

@200
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
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@40
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
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@3
D=A
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@123
D=A
@SP
A=M
M=D
@SP
M=M+1

@NestedCall.Sys.add12.22.ReturnAddress
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
@6
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@NestedCall.Sys.add12
0;JMP
(NestedCall.Sys.add12.22.ReturnAddress)


@SP
M=M-1
@0
D=A
@5
D=A+D
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
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@4
D=A
@LCL
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
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

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
A=A-1
M=D+M
@SP
M=M-1

@LCL
D=M
@5
A=D-A
D=M
@13
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@LCL
M=M-1
A=M
D=M
@THAT
M=D
@LCL
M=M-1
A=M
D=M
@THIS
M=D
@LCL
M=M-1
A=M
D=M
@ARG
M=D
@LCL
M=M-1
A=M
D=M
@LCL
M=D
@13
A=M
0;JMP


(NestedCall.Sys.add12)
@0
D=A
@NestedCall.Sys.add12.End.34
D;JEQ
(NestedCall.Sys.add12.Loop.34)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.add12.Loop.34
D=D-1
D;JNE
(NestedCall.Sys.add12.End.34)

@4002
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

@5002
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

@12
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

@LCL
D=M
@5
A=D-A
D=M
@13
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@LCL
M=M-1
A=M
D=M
@THAT
M=D
@LCL
M=M-1
A=M
D=M
@THIS
M=D
@LCL
M=M-1
A=M
D=M
@ARG
M=D
@LCL
M=M-1
A=M
D=M
@LCL
M=D
@13
A=M
0;JMP


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

@NestedCall.Sys.init
0; JMP
(Sys.init.ReturnAddress)
(NestedCall.Sys.init)
@0
D=A
@NestedCall.Sys.init.End.1
D;JEQ
(NestedCall.Sys.init.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.init.Loop.1
D=D-1
D;JNE
(NestedCall.Sys.init.End.1)

@4000
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

@5000
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

@NestedCall.Sys.main.6.ReturnAddress
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
@NestedCall.Sys.main
0;JMP
(NestedCall.Sys.main.6.ReturnAddress)


@SP
M=M-1
@1
D=A
@5
D=A+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

(Sys.LOOP)

@Sys.LOOP
0;JMP

(NestedCall.Sys.main)
@5
D=A
@NestedCall.Sys.main.End.10
D;JEQ
(NestedCall.Sys.main.Loop.10)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.main.Loop.10
D=D-1
D;JNE
(NestedCall.Sys.main.End.10)

@4001
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

@5001
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

@200
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
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@40
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
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@3
D=A
@LCL
D=M+D
@13
M=D
@SP
A=M
D=M
@13
A=M
M=D

@123
D=A
@SP
A=M
M=D
@SP
M=M+1

@NestedCall.Sys.add12.22.ReturnAddress
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
@6
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@NestedCall.Sys.add12
0;JMP
(NestedCall.Sys.add12.22.ReturnAddress)


@SP
M=M-1
@0
D=A
@5
D=A+D
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
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@1
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@2
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@3
D=A
@LCL
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@4
D=A
@LCL
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
A=M-1
D=M
A=A-1
M=D+M
@SP
M=M-1

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
A=A-1
M=D+M
@SP
M=M-1

@LCL
D=M
@5
A=D-A
D=M
@13
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@LCL
M=M-1
A=M
D=M
@THAT
M=D
@LCL
M=M-1
A=M
D=M
@THIS
M=D
@LCL
M=M-1
A=M
D=M
@ARG
M=D
@LCL
M=M-1
A=M
D=M
@LCL
M=D
@13
A=M
0;JMP


(NestedCall.Sys.add12)
@0
D=A
@NestedCall.Sys.add12.End.34
D;JEQ
(NestedCall.Sys.add12.Loop.34)
@SP
A=M
M=0
@SP
M=M+1
@NestedCall.Sys.add12.Loop.34
D=D-1
D;JNE
(NestedCall.Sys.add12.End.34)

@4002
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

@5002
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

@12
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

@LCL
D=M
@5
A=D-A
D=M
@13
M=D
@SP
M=M-1
A=M
D=M
@ARG
A=M
M=D
@ARG
D=M
@SP
M=D+1
@LCL
M=M-1
A=M
D=M
@THAT
M=D
@LCL
M=M-1
A=M
D=M
@THIS
M=D
@LCL
M=M-1
A=M
D=M
@ARG
M=D
@LCL
M=M-1
A=M
D=M
@LCL
M=D
@13
A=M
0;JMP


