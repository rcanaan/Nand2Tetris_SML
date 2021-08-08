fun likeWhile (word)= 
  let
    val r = [word]
  in
    r@r
  end
  

fun tempPopthis (num) =  
  let
    val counter = ref num
    val act = "A=A+1 \n"
    val b =  while !counter > 0 do (( (likeWhile(act))); counter := !counter -1)
  in 
    b
  end

fun popthis(num)=
  let
    val x = tempPopthis(num)
    (*val st = "@SP\nA=M-1\nD=M\n@THIS\nA=M\n" ^ "M=D\n@SP\nM=M-1\n" *)
  in
    x
  end;


fun popLocalType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@LCL\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n"

fun popArgType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@ARG\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n" 

fun popThisType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@THIS\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n"

fun popThatType(num) = 
  "@SP\nM=M-1\n@" ^ num ^ "\nD=A\n@THAT\nD=M+D\n@13\nM=D\n@SP\nA=M\nD=M\n@13\nA=M\nM=D\n"  

fun popTempType(num) = 
  let
    val SOME n = Int.fromString(num) 
    val number = n + 5
  in
    "@SP\nA=M-1\nD=M\n@" ^ Int.toString(number) ^ "\nM=D\n@SP\nM=M-1\n"
  end 

fun popStaticType(fileName, num) = 
  "@SP\nA=M-1\nD=M\n@" ^ fileName ^ "." ^ num ^ "\nM=D\n@SP\nM=M-1\n"

fun popPointerType(num : string) = 
  if num = "0"
  then "@SP\nA=M-1\nD=M\n@THIS\nM=D\n@SP\nM=M-1\n"
  else if num = "1"
    then "@SP\nA=M-1\nD=M\n@THAT\nM=D\n@SP\nM=M-1\n" 
    else ""



fun pushConstType(num) = 
  "@" ^ num ^ "\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" 

fun pushLocalType(num) = 
  "@" ^ num ^ "\nD=A\n@LCL\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" 

fun pushArgType(num) = 
  "@ARG\nD=A\n@" ^ num ^ "\nD=D+A\nA=D\n@SP\nM=D\n@SP\nM=M+1\n" 

fun pushThisType(num) = 
  "@" ^ num ^ "\nD=A\n@THIS\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"  

fun pushThatType(num) = 
  "@" ^ num ^ "\nD=A\n@THAT\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" 

fun pushTempType(num) = 
  "@" ^ num ^ "\nD=A\n@5\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"  

fun pushStaticType(fileName, num : string) = 
  "@" ^ fileName ^ "." ^ num ^ "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n" 

fun pushPointerType(num : string) = 
  if num = "0"
  then "@THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
  else if num = "1"
    then "@THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"
    else ""



(*define what segment we have*)
fun pushSegmentType (fileName, sAction) = 
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
                then pushStaticType(fileName, hd(num))
                else pushPointerType(hd(num))
end

(*define what segment we have*)
fun popSegmentType (fileName, sAction) = 
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
                then popStaticType(fileName, hd(num))
                else popPointerType(hd(num))
end

(*push func*)
fun pushFunc(fileName, sAction) = 
let
  val temp = fileName ^ ".asm"
  val pushCommand = pushSegmentType(fileName, sAction) (* returns the ams *)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, pushCommand)
end

(*pop func*)
fun popFunc(fileName, sAction) = 
let
  val temp = fileName ^ ".asm"
  val popCommand = popSegmentType(fileName, sAction) (* returns the ams *)
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, popCommand)
end

(*add func*)
fun addFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=D+M\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*sub func*)
fun subFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nA=A-1\nM=M-D\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*neg func*)
fun negFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nM=-D\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*eq func*)
fun eqFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content = "*******************counter= " ^ Int.toString i ^ "\n@SP\nA=M-1\nD=M\nA=A-1\nD=D-M\n@IF_TRUE" ^ Int.toString i ^ "\nD;JEQ\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*greater then func*)
fun gtFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content ="*******************counter= " ^ Int.toString i ^ "\n@SP\nA=M-1\nD=M\nA=A-1\nD=D-M\n@IF_TRUE" ^ Int.toString i ^ "\nD;JLT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*lt func*)
fun ltFunc(fileName, i) = 
let
  val temp = fileName ^ ".asm"
  val content = "*******************counter= " ^ Int.toString i ^ "\n@SP\nA=M-1\nD=M\nA=A-1\nD=D-M\n@IF_TRUE" ^ Int.toString i ^ "\nD;JGT\nD=0\n@SP\nA=M-1\nA=A-1\nM=D\n@IF_FALSE" ^ Int.toString i ^ "\n0;JMP\n(IF_TRUE" ^ Int.toString i ^ ")\nD=-1\n@SP\nA=M-1\nA=A-1\nM=D\n(IF_FALSE" ^ Int.toString i ^ ")\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end

(*and  func*)
fun andFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nAM=M-1\nD=M\nA=A-1\nM=D&M\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*or  func*)
fun orFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nAM=M-1\nD=M\nA=A-1\nM=D|M\n@SP\nM=M-1\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end


(*not func*)
fun notFunc(fileName) = 
let
  val temp = fileName ^ ".asm"
  val content = "@SP\nA=M-1\nD=M\nM=!D\n"
  val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
in
   TextIO.output (writestream, content)
end







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




fun gethandleVMFiles(listOfTuples, fName, i)=
  let
    val streamName = fName ^ ".asm"
    val readstream = TextIO.openAppend streamName (*open the asm file for writing*)

    val headlist = hd(listOfTuples)(*the first file  as tuple ("inputA","VM")*)
    val taillist = tl(listOfTuples)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fAction = first(headlist) (*instead of ("pop","constant 7") we will get "pop"*)
    val sAction = second(headlist)(*instead of ("pop","constant 7") we will get "constant 7"*)
  in
    if fAction = "add"
    then addFunc(fName)
    else if fAction = "sub" 
        then subFunc(fName)
        else if fAction = "neg" 
          then negFunc(fName)
          else if fAction = "eq" 
            then eqFunc(fName, i + 1)
            else if fAction = "gt" 
              then gtFunc(fName, i + 1)
              else if fAction = "lt" 
                then ltFunc(fName, i + 1)
                else if fAction = "and" 
                  then andFunc(fName)
                  else if fAction = "or" 
                    then orFunc(fName)
                    else if fAction = "not" 
                      then notFunc(fName)
                      else if fAction = "pop" 
                        then popFunc(fName, sAction)
                        else pushFunc(fName, sAction);

    if taillist <> []
    then gethandleVMFiles(taillist,fName, i + 1)
    else []
  end



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


fun openVMFiles (fName) = 
  let
    val streamName = fName ^ ".asm"
    val readstream = TextIO.openAppend streamName (*open the asm file for writing*)
    val fileName = fName ^ ".vm"
    val file = TextIO.openIn fileName
    
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val lines = String.tokens (fn c => c = #"\n") contents (*gets all the lines from the vm file. each line is str*)
    val splitedLines = List.map splitFirstSpace lines (*seperates the lines like tuples ("hello","bla blah") *)
  in
    gethandleVMFiles(handleVMFiles(splitedLines),fName, 0)
  end

(*get the [("inputA","vm"),...] list *)
fun mainHelper (splitFileNameList) =
  let
    
    val headFile = hd(splitFileNameList)(*the first file  as tuple ("inputA","VM")*)
    val tailFiles = tl(splitFileNameList)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fName = first(headFile) (*instead of ("inputA","vm") we will get "inputA"*)
    val sName = second(headFile)(*instead of ("inputA","vm") we will get "VM"*)

 
  in
    if sName = "vm"
    then openVMFiles(fName) 
    else [];

    if tailFiles <> []
    then mainHelper(tailFiles)
    else []
  end;
  


fun main(dirName) = 
  let
    val listFiles = read_dir(dirName) (*send the directory name and gets a list with all the files*)
    val splitFileName = List.map splitByDot listFiles (*send the list of files and return list of [("inputA","vm"),...]*)
    val temp =  OS.FileSys.chDir(String.substring("\\files", 1,5))
  in
    mainHelper(splitFileName)
  end;


  
