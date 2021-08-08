(* Oria Segal 20933813
Rinat Canaan 207744012*)


(*********************** GENERAL FUNCTIONS ***********************)


fun likeWhile (word)= 
  let
    val r = [word]
  in
    r@r
  end
  

fun tem (num) =  
  let
    val counter = ref num
    val act = "A=A+1 \n"
    (*val b =  while !counter > 0 do (( (likeWhile(act))); counter := !counter -1)*)
    val b =  while !counter > 0 do ((print(act)); counter := !counter -1)
  in 
    b
  end
(*
fun popthis(num)=
  let
    val x = tempPopthis(num)
    (*val st = "@SP\nA=M-1\nD=M\n@THIS\nA=M\n" ^ "M=D\n@SP\nM=M-1\n" *)
  in
    x
  end;
*)


(* splits the line from the file by the first space into a pair- (first word, rest of the line) *)
fun splitCharsFirstSpace cs =
  case cs of
    [] => ([], [])
  | c :: cs' =>
      if Char.isSpace c then ([], cs')
      else let val (l, r) = splitCharsFirstSpace cs'
           in (c :: l, r)
           end


(* sends the row (as a list) to be split by the splitCharsFirstSpace function *)
fun splitFirstSpace s =
  let
    val (l, r) = splitCharsFirstSpace (String.explode s)
  in
    (String.implode l, String.implode r)
  end


(*gets string and split it by dot. *)
fun splitCharsByDot cs =
  case cs of
    [] => ([], [])
  | c :: cs' =>
      if  c = #"."
      then ([], cs')
      else let val (l, r) = splitCharsByDot cs'
           in (c :: l, r)
           end


(* get string and sends the row (as a list) to be split by the splitCharsByDot function and at final as tuple of strings ("bread","apple)* *)
fun splitByDot s =
  let
    val (l, r) = splitCharsByDot (String.explode s)
  in
    (String.implode l, String.implode r)
  end


(*********************** MEMORY ACCESS COMMANDS ***********************)


fun popLocalType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@LCL\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n\n"

fun popArgType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@ARG\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n\n" 

fun popThisType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@THIS\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n\n"

fun popThatType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@THAT\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n\n"  

fun popTempType(num) = 
  let
    val SOME n = Int.fromString(num) 
    val number = n + 5
  in
    "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@5\nD=A+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n\n"
  end 

fun popStaticType(fileName,fName,  num) = 
  "@SP\nA=M-1\nD=M\n@" ^ fName ^ "." ^ num ^ "\nM=D\n@SP\nM=M-1\n\n"

fun popPointerType(num : string) = 
  if num = "0"
  then "@SP\nA=M-1\nD=M\n@THIS\nM=D\n@SP\nM=M-1\n\n"
  else if num = "1"
    then "@SP\nA=M-1\nD=M\n@THAT\nM=D\n@SP\nM=M-1\n\n" 
    else ""


fun pushConstType(num) = 
  "@" ^ num ^ "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n" 

fun pushLocalType(num) = 
  "@" ^ num ^ "\nD=A\n@LCL\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n" 

fun pushArgType(num) = 
  "@" ^ num ^ "\nD=A\n@ARG\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n" 

fun pushThisType(num) = 
  "@" ^ num ^ "\nD=A\n@THIS\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"  

fun pushThatType(num) = 
  "@" ^ num ^ "\nD=A\n@THAT\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n" 

fun pushTempType(num) = 
  "@" ^ num ^ "\nD=A\n@5\nA=A+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"

fun pushStaticType(fileName, fName, num : string) = 
  "@" ^ fName ^ "." ^ num ^ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n" 

fun pushPointerType(num : string) = 
  if num = "0"
  then "@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  else if num = "1"
    then "@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
    else ""



(*define what segment we have*)
fun pushSegmentType (fileName,fName, sAction) = 
let 
  val listOfString = String.tokens Char.isSpace (*"constant 7" -> ["constant","7"] *)
  val ls = listOfString sAction (* sending sAction to list of string *)
  val segType = hd(ls) (* "constant"*)
  val num = tl (ls) (*["7"]*)
in
  if segType = "constant"
  then pushConstType(hd(num))
  else if segType = "local" 
    then pushLocalType(hd(num))
    else if segType = "argument" 
      then pushArgType(hd(num))
      else if segType = "this" 
        then pushThisType(hd(num))
        else if segType = "that" 
          then pushThatType(hd(num))
          else if segType = "temp" 
            then pushTempType(hd(num))
            else if segType = "static" 
                then pushStaticType(fileName,fName,  hd(num))
                else pushPointerType(hd(num))
end


(*define what segment we have*)
fun popSegmentType (fileName,fName, sAction) = 
let 
  val listOfString = String.tokens Char.isSpace (*"constant 7" -> ["constant","7"] *)
  val ls = listOfString sAction (* sending sAction to list of string *)
  val segType = hd(ls) (* "constant"*)
  val num = tl (ls) (*["7"]*)
  
in
  if segType = "local" 
    then popLocalType(hd(num))
    else if segType = "argument" 
      then popArgType(hd(num))
      else if segType = "this" 
        then popThisType(hd(num))
        else if segType = "that" 
          then popThatType(hd(num))
          else if segType = "temp" 
            then popTempType(hd(num))
            else if segType = "static" 
                then popStaticType(fileName,fName, hd(num))
                else popPointerType(hd(num))
end


(*push func*)
fun pushFunc(fileName, fName,sAction) = 
let
  val temp = fileName ^ ".asm"
  val pushCommand = pushSegmentType(fileName,fName,  sAction) (* returns the ams *)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, pushCommand)
end


(*pop func*)
fun popFunc(fileName,fName, sAction) = 
let
  val temp = fileName ^ ".asm"
  val popCommand = popSegmentType(fileName,fName, sAction) (* returns the ams *)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, popCommand)
end


(*********************** ARITHMETIC / BOOLEAN COMMANDS ***********************)


(*add func*)
fun addFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=D+M\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*sub func*)
fun subFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=M-D\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*neg func*)
fun negFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nM=-D\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*eq func*)
fun eqFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nD=D-M\n@IF_TRUE" ^ Int.toString i ^ "\nD;JEQ\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*gt func*)
fun gtFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nD=M-D\n@IF_TRUE" ^ Int.toString i ^ "\nD;JGT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*lt func*)
fun ltFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nD=D-M\n@IF_TRUE" ^ Int.toString i ^ "\nD;JGT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*and  func*)
fun andFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=D&M\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*or  func*)
fun orFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=D|M\n@SP\nM=M-1\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*not func*)
fun notFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nM=!D\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(************** PROGRAM FLOW COMMANDS **************)


(*label func*)
fun labelFunc(fileName, fName, c) = 
let
  val temp = fileName ^ ".asm"
  val content = "(" ^ fName ^ "." ^ c ^ ")\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*if-goto func*)
fun ifgotoFunc(fileName, fName, c) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nM=M-1\nA=M\nD=M\n@" ^ fName ^ "." ^ c ^ "\nD;JNE\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*goto func*) 
fun gotoFunc(fileName, fName, c) = 
let
  val temp = fileName ^ ".asm"
  val content = "@" ^ fName ^ "." ^ c ^ "\n0;JMP\n\n"   (*0;JMP  =>  1;JMP*)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*********************** FUNCTION CALLING COMMANDS ***********************)


(*function func
c= SimpleFunction.test 2 (str)*)
fun functionFunc(fileName, fName, c, counter) = 
let
  fun first (a, _) = a
  fun second (_,b) = b

  val funcNameNum = splitFirstSpace(c)(* "SimpleFunction.test 2" -> ("SimpleFunction.test","2")*)
  val funcN = first(funcNameNum)(*"SimpleFunction"*)
  val numb = second (funcNameNum)(*"2"*)
  val SOME n = Int.fromString(numb) 

  val temp = fileName ^ ".asm"
  (*val content = "(" ^ funcN ^ ")\n@" ^ numb ^"\nD=A\n@f.End\nD;JEQ\n(f.Loop)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@f.Loop\nD=D-1;JNE\n(f.End)\n\n" *)
  val content = "(" ^ fileName ^ "." ^ funcN ^ ")\n@" ^ Int.toString(n) ^ "\nD=A\n@" ^ fileName ^ "." ^ funcN ^ ".End." ^ Int.toString(counter) ^ "\nD;JEQ\n(" ^ fileName ^ "." ^ funcN ^ ".Loop." ^ Int.toString(counter) ^ ")\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@" ^ fileName ^ "." ^ funcN ^ ".Loop." ^ Int.toString(counter) ^ "\nD=D-1\nD;JNE\n(" ^ fileName ^ "." ^ funcN ^ ".End." ^ Int.toString(counter) ^ ")\n\n" 
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*call func
c= Main.fibonacci 1   (str)*)
(*dirName, vmName, arguments*)
fun callFunc(fileName, fName, c, counter) = 
let
  fun first (a, _) = a
  fun second (_,b) = b

  val funcNameNum = splitFirstSpace(c)(* "Main.fibonacci 1 " -> ("Main.fibonacci","1")*)
  val funcN = first(funcNameNum)(*"Main.fibonacci"*)
  val numb = second (funcNameNum)(*"1"*)
  (*for ARG = SP-nun_arguments-5*)
  val SOME n = Int.fromString(numb) 
  val number = n + 5
  
  val push_return_adrs = "@" ^ fileName ^ "." ^ funcN ^ "." ^ Int.toString(counter) ^ ".ReturnAddress\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"(*push return-address*)
  val pushLcl = "@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" (*push LCL*)
  val pushArg = "@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" (*push ARG*)
  val pushThis = "@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" (*push THIS*)
  val pushThat = "@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" (*push THAT*)
  val argIs_spN = "@SP\nD=M\n@" ^ Int.toString(number) ^ "\nD=D-A\n@ARG\nM=D\n"(*ARG = SP-nun_arguments-5*)
  val lcl_is_sp = "@SP\nD=M\n@LCL\nM=D\n" (*LCL = SP*)
  val goto_funName = "@" ^ fileName ^ "." ^ funcN ^ "\n0;JMP\n" (*goto g - func name*)
  val label_returnAdr = "(" ^ fileName ^ "." ^ funcN ^ "." ^ Int.toString(counter) ^ ".ReturnAddress)\n" (*label return address*)

  val temp = fileName ^ ".asm"
  val content = push_return_adrs ^ pushLcl ^ pushArg ^ pushThis ^ pushThat ^ argIs_spN ^ lcl_is_sp ^ goto_funName ^ label_returnAdr ^"\n\n"
  
  (*
  val push_return_address = "@retunAddress\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  val push_LCL = "@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  val push_ARG = "@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  val push_THIS = "@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  val push_THAT = "@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\n"
  val chg_ARG = "@" ^ Int.toString(n) ^ "\nD=A\n@5\nD=D+A\n@SP\nD=D-M\n@ARG\nM=D\n\n"
  val chg_LCL = "@SP\nD=M\n@LCL\nM=D\n\n"
  val goto_function = "@" ^ funcN ^ "\n1;JMP\n\n"
  val return_address = "(returnAddress)\n\n"

  val temp = fileName ^ ".asm"
  val content = push_return_address ^ push_LCL ^ push_ARG ^ push_THIS ^ push_THAT ^ chg_ARG ^ chg_LCL ^ goto_function ^ return_address
  *)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*return func
c = empty*)
fun returnFunc(fileName, fName, c) = 
let
  val frm_lcl = "@LCL\nD=M\n"(*FRAME = LCL*) 
  val ret = "@5\nA=D-A\nD=M\n@13\nM=D\n" (*RAM[13] = (LOCAL - 5)*) (*A=D-A*)
  val arg_pop = "@SP\nM=M-1\nA=M\nD=M\n@ARG\nA=M\nM=D\n" (* *ARG =  pop()*)
  val sp_arg = "@ARG\nD=M\n@SP\nM=D+1\n" (* SP = ARG +1 *)
  val that_frm = "@LCL\nM=M-1\nA=M\nD=M\n@THAT\nM=D\n" (* THAT = *(FRAM-1)*) 
  val this_frm = "@LCL\nM=M-1\nA=M\nD=M\n@THIS\nM=D\n" (*THIS = *(FRAM-2)*)
  val arg_frm = "@LCL\nM=M-1\nA=M\nD=M\n@ARG\nM=D\n" (*ARG = *(FRAM-3)*)
  val lcl_frm = "@LCL\nM=M-1\nA=M\nD=M\n@LCL\nM=D\n" (*LCL = *(FRAM-4)*)
  val goto_ret = "@13\nA=M\n0;JMP\n"

  val temp = fileName ^ ".asm"
  val content = frm_lcl ^ ret ^ arg_pop ^ sp_arg ^ that_frm ^ this_frm ^ arg_frm ^ lcl_frm ^ goto_ret ^"\n\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

fun returnFunc1(fileName, fName, c) = 
let
  val return_address_in_R14 = "@LCL\nD=M\n@5\nA=D-A\nD=M\n@R14\nM=D\n\n"
  val result_at_top = "@SP\nM=M-1\n@0\nD=A\n@ARG\nD=M+D\n@R13\nM=D\n@SP\nA=M\nD=M\n@R13\nA=M\nM=D\n\n"
  val restore_SP = "@ARG\nD=M+1\n@SP\nM=D\n\n"
  val restore_THAT = "@LCL\nD=M\n@1\nA=D-A\nD=M\n@THAT\nM=D\n\n" 
  val restore_THIS = "@LCL\nD=M\n@2\nA=D-A\nD=M\n@THIS\nM=D\n\n"
  val restore_ARG = "@LCL\nD=M\n@3\nA=D-A\nD=M\n@ARG\nM=D\n\n"
  val restore_LCL = "@LCL\nD=M\n@4\nA=D-A\nD=M\n@LCL\nM=D\n\n"
  val jump_to_return_address = "@R14\nA=M\n1;JMP\n\n"

  val temp = fileName ^ ".asm"
  val content = return_address_in_R14 ^ result_at_top ^ restore_SP ^ restore_THAT ^ restore_THIS ^ restore_ARG ^ restore_LCL ^ jump_to_return_address
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*********************** DIRECTORIES AND FILES FUNCTIONS ***********************)


(*get the folder's name and send the string list of it*)
fun read_dir (directory) = 
  let
    val dir = OS.FileSys.openDir directory
    fun loop dir = 
      case OS.FileSys.readDir dir of
        SOME file => file :: loop dir
      | NONE      => []
  in
    loop dir before OS.FileSys.closeDir dir
  end;


fun gethandleVMFiles(listOfTuples, dirName, fName, i)=
  let
    val streamName = dirName ^ ".asm"
    val readstream = TextIO.openAppend streamName (*open the asm file for writing*)

    val headlist = hd(listOfTuples)(*the first file  as tuple ("inputA","VM")*)
    val taillist = tl(listOfTuples)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fAction = first(headlist) (*instead of ("pop","constant 7") we will get "pop"*)
    val sAction = second(headlist)(*instead of ("pop","constant 7") we will get "constant 7"*)
  in
    if fAction = "add"
    then addFunc(dirName)
    else if fAction = "sub" 
        then subFunc(dirName)
        else if fAction = "neg" 
          then negFunc(dirName)
          else if fAction = "eq" 
            then eqFunc(dirName, i + 1)
            else if fAction = "gt" 
              then gtFunc(dirName, i + 1)
              else if fAction = "lt" 
                then ltFunc(dirName, i + 1)
                else if fAction = "and" 
                  then andFunc(dirName)
                  else if fAction = "or" 
                    then orFunc(dirName)
                    else if fAction = "not" 
                      then notFunc(dirName)
                      else if fAction = "pop" 
                        then popFunc(dirName,fName, sAction)
                        else if fAction = "push"
                          then pushFunc(dirName, fName,sAction)
                          else if fAction = "label"
                            then labelFunc(dirName, fName, sAction)
                             else if fAction = "goto" 
                              then gotoFunc(dirName, fName, sAction)
                              else if fAction = "if-goto"
                                then ifgotoFunc(dirName, fName, sAction) (**)
                                else if fAction = "call"
                                  then callFunc(dirName, fName, sAction, i + 1) (*sAction =SimpleFunction.test 2 *)
                                  else if fAction = "function"
                                    then functionFunc(dirName, fName, sAction, i + 1) (*sAction =SimpleFunction.test 2 *)
                                    else returnFunc(dirName, fName, sAction); (*sAction is empty*)

    if taillist <> []
    then gethandleVMFiles(taillist, dirName, fName, i + 1)
    else []
  end


(*skipes rows that start with //*)
fun handleVMFiles(splitedLines) = 
  let
    fun first (a, _) = a

    fun filter pred [] = []
      | filter pred (x::xs) =
          if (pred x) then
            x :: (filter pred xs)
          else
          (filter pred xs) 
  in
    filter(fn x => first(x)<> "//") splitedLines
  end

fun writeSys(fileName, content) = 
  let
    val temp = fileName ^ ".asm"
    val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
  in
    TextIO.output (writestream, content)
  end

(*gets all lines of the vm file and opens a matching asm file*)
fun openVMFiles (dirName, fName) = 
  let
    val streamName = dirName ^ ".asm"
    val readstream = TextIO.openAppend streamName (*open the asm file for writing*)
    val fileName = fName ^ ".vm"
    val file = TextIO.openIn fileName

    (*val done = writeSys(dirName,"@256\nD=A\n@SP\nM=D\n\n(Sys.init)\n@0\nD=A\n@Sys.init.End.111\nD;JEQ\n(Sys.init.Loop.111)\n@SP\nA=M\nM=0\n@SP\nM=M+1\n@Sys.init.Loop.111\nD=D-1;JNE\n(Sys.init.End.111)\n\n")*)
    val done = writeSys(dirName, "@256\nD=A\n@SP\nM=D\n@Sys.init.ReturnAddress\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n@SP\nD=M\n@5\nD=D-A\n@ARG\nM=D\n@SP\nD=M\n@LCL\nM=D\n\n@"^dirName^".Sys.init\n0; JMP\n(Sys.init.ReturnAddress)\n")
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val lines = String.tokens (fn c => c = #"\n") contents (*gets all the lines from the vm file. each line is str*)
    val splitedLines = List.map splitFirstSpace lines (*seperates the lines like tuples ("hello","bla blah") *)
  in
    gethandleVMFiles(handleVMFiles(splitedLines), dirName, fName, 0) (*only keeps the important rows of the file- skipes the ones that start with //*)
  end


(*********************** MAIN FUNCTIONS ***********************)


(*get the [("inputA","vm"),...] list *)
(*only opens the vm files, skipes the others in the folder*)
fun mainHelper (dirName, splitFileNameList) =
  let
    val headFile = hd(splitFileNameList)(*the first file  as tuple ("inputA","VM")*)
    val tailFiles = tl(splitFileNameList)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fName = first(headFile) (*instead of ("inputA","vm") we will get "inputA"*)
    val sName = second(headFile)(*instead of ("inputA","vm") we will get "VM"*)
 
  in
    if sName = "vm"
    then openVMFiles(dirName, fName) 
    else [];

    if tailFiles <> []
    then mainHelper(dirName, tailFiles)
    else []
  end;


fun main(dirName) = 
  let
    val listFiles = read_dir(dirName) (*send the directory name and gets a list with all the files*)
    val splitFileName = List.map splitByDot listFiles (*send the list of files and return list of [("inputA","vm"),...]*)
    val dir = OS.FileSys.getDir() ^ String.substring("f\\g", 1, 1) ^ dirName  (*subString- the sting, starts at index, counts chars*)
    val temp = OS.FileSys.chDir(dir)
  in
    mainHelper(dirName, splitFileName)
  end;


fun mHelper(path, splitFileName) = 
  let
    val temp = OS.FileSys.chDir(path)

    val headFile = hd(splitFileName)(*the first file  as tuple ("inputA","VM")*)
    val tailFiles = tl(splitFileName)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fName = first(headFile) (*instead of ("inputA","vm") we will get "inputA"*)
    val sName = second(headFile)(*instead of ("inputA","vm") we will get "VM"*)
 
  in
    if sName = ""
    then main(fName) 
    else [];

    if tailFiles <> []
    then mHelper(path, tailFiles)
    else []

  end;


fun m(name) =
  let
    val listFiles = read_dir(name) (*send the directory name and gets a list with all the files*)
    val splitFileName = List.map splitByDot listFiles (*send the list of files and return list of [("inputA","vm"),...]*)
    val path = OS.FileSys.getDir() ^ String.substring("f\\g", 1, 1) ^ name  (*subString- the sting, starts at index, counts chars*)
  in 
    mHelper(path, splitFileName)
  end;