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
D=M@SP
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

@Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

@BasicTest_exOne.Sys.init
0; JMP
(Sys.init.ReturnAddress)
@10
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

@21
D=A
@SP
A=M
M=D
@SP
M=M+1

@22
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

@SP
M=M-1
@1
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

@36
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

@42
D=A
@SP
A=M
M=D
@SP
M=M+1

@45
D=A
@SP
A=M
M=D
@SP
M=M+1

@SP
M=M-1
@5
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

@510
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

@5
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
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@THIS
A=M+D
D=M
@SP
A=M
M=D
@SP
M=M+1

@6
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
M=D+M
@SP
M=M-1

@SP
A=M-1
D=M
A=A-1
M=M-D
@SP
M=M-1

@6
D=A
@5
A=A+D
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

