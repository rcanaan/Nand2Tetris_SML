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

@StaticsTest.Sys.init
0; JMP
(Sys.init.ReturnAddress)
(StaticsTest.Class1.set)
@0
D=A
@StaticsTest.Class1.set.End.1
D;JEQ
(StaticsTest.Class1.set.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class1.set.Loop.1
D=D-1
D;JNE
(StaticsTest.Class1.set.End.1)

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
A=M-1
D=M
@Class1.0
M=D
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
@Class1.1
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


(StaticsTest.Class1.get)
@0
D=A
@StaticsTest.Class1.get.End.8
D;JEQ
(StaticsTest.Class1.get.Loop.8)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class1.get.Loop.8
D=D-1
D;JNE
(StaticsTest.Class1.get.End.8)

@Class1.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@Class1.1
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


(StaticsTest.Class2.set)
@0
D=A
@StaticsTest.Class2.set.End.1
D;JEQ
(StaticsTest.Class2.set.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class2.set.Loop.1
D=D-1
D;JNE
(StaticsTest.Class2.set.End.1)

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
A=M-1
D=M
@Class2.0
M=D
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
@Class2.1
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


(StaticsTest.Class2.get)
@0
D=A
@StaticsTest.Class2.get.End.8
D;JEQ
(StaticsTest.Class2.get.Loop.8)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class2.get.Loop.8
D=D-1
D;JNE
(StaticsTest.Class2.get.End.8)

@Class2.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@Class2.1
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


(StaticsTest.Sys.init)
@0
D=A
@StaticsTest.Sys.init.End.1
D;JEQ
(StaticsTest.Sys.init.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Sys.init.Loop.1
D=D-1
D;JNE
(StaticsTest.Sys.init.End.1)

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

@StaticsTest.Class1.set.4.ReturnAddress
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
@7
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@StaticsTest.Class1.set
0;JMP
(StaticsTest.Class1.set.4.ReturnAddress)


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

@23
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@StaticsTest.Class2.set.8.ReturnAddress
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
@7
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@StaticsTest.Class2.set
0;JMP
(StaticsTest.Class2.set.8.ReturnAddress)


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

@StaticsTest.Class1.get.10.ReturnAddress
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
@StaticsTest.Class1.get
0;JMP
(StaticsTest.Class1.get.10.ReturnAddress)


@StaticsTest.Class2.get.11.ReturnAddress
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
@StaticsTest.Class2.get
0;JMP
(StaticsTest.Class2.get.11.ReturnAddress)


(Sys.WHILE)

@Sys.WHILE
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

@StaticsTest.Sys.init
0; JMP
(Sys.init.ReturnAddress)
(StaticsTest.Class1.set)
@0
D=A
@StaticsTest.Class1.set.End.1
D;JEQ
(StaticsTest.Class1.set.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class1.set.Loop.1
D=D-1
D;JNE
(StaticsTest.Class1.set.End.1)

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
A=M-1
D=M
@Class1.0
M=D
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
@Class1.1
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


(StaticsTest.Class1.get)
@0
D=A
@StaticsTest.Class1.get.End.8
D;JEQ
(StaticsTest.Class1.get.Loop.8)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class1.get.Loop.8
D=D-1
D;JNE
(StaticsTest.Class1.get.End.8)

@Class1.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@Class1.1
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

@StaticsTest.Sys.init
0; JMP
(Sys.init.ReturnAddress)
(StaticsTest.Class2.set)
@0
D=A
@StaticsTest.Class2.set.End.1
D;JEQ
(StaticsTest.Class2.set.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class2.set.Loop.1
D=D-1
D;JNE
(StaticsTest.Class2.set.End.1)

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
A=M-1
D=M
@Class2.0
M=D
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
@Class2.1
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


(StaticsTest.Class2.get)
@0
D=A
@StaticsTest.Class2.get.End.8
D;JEQ
(StaticsTest.Class2.get.Loop.8)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Class2.get.Loop.8
D=D-1
D;JNE
(StaticsTest.Class2.get.End.8)

@Class2.0
D=M
@SP
A=M
M=D
@SP
M=M+1

@Class2.1
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

@StaticsTest.Sys.init
0; JMP
(Sys.init.ReturnAddress)
(StaticsTest.Sys.init)
@0
D=A
@StaticsTest.Sys.init.End.1
D;JEQ
(StaticsTest.Sys.init.Loop.1)
@SP
A=M
M=0
@SP
M=M+1
@StaticsTest.Sys.init.Loop.1
D=D-1
D;JNE
(StaticsTest.Sys.init.End.1)

@6
D=A
@SP
A=M
M=D
@SP
M=M+1

@8
D=A
@SP
A=M
M=D
@SP
M=M+1

@StaticsTest.Class1.set.4.ReturnAddress
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
@7
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@StaticsTest.Class1.set
0;JMP
(StaticsTest.Class1.set.4.ReturnAddress)


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

@23
D=A
@SP
A=M
M=D
@SP
M=M+1

@15
D=A
@SP
A=M
M=D
@SP
M=M+1

@StaticsTest.Class2.set.8.ReturnAddress
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
@7
D=D-A
@ARG
M=D
@SP
D=M
@LCL
M=D
@StaticsTest.Class2.set
0;JMP
(StaticsTest.Class2.set.8.ReturnAddress)


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

@StaticsTest.Class1.get.10.ReturnAddress
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
@StaticsTest.Class1.get
0;JMP
(StaticsTest.Class1.get.10.ReturnAddress)


@StaticsTest.Class2.get.11.ReturnAddress
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
@StaticsTest.Class2.get
0;JMP
(StaticsTest.Class2.get.11.ReturnAddress)


(Sys.WHILE)

@Sys.WHILE
0;JMP

