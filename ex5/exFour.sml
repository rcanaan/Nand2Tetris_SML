(* Oria Segal 20933813
Rinat Canaan 207744012*)


(*********************** GENERAL FUNCTIONS ***********************)


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





(*
fun helpTurnJackIntoString(file, string, afterCuts2) = 
  let 
    fun first (a, _) = a
    fun second (_,b) = b

    val start = first(hd(afterCuts2))
    val finish = second(hd(afterCuts2))
  in
    if afterCuts2 = []
    then writeXML("file", string)
    else helpTurnJackIntoString(file, string ^ start ^ " " ^ finish ^ " ", tl(afterCuts2))
  end
  *)


(*skipes rows that start with //*)

fun cuts(splitedLines, bool) = 
  let
    fun first (a, _) = a

    fun filter pred [] = []
      | filter pred (x::xs) =
          if (pred x) then
            x :: (filter pred xs)
          else
          (filter pred xs) 
  in
    if bool = true
    then filter(fn x => (first(x)<> "//")) splitedLines
    else filter(fn x => (first(x)<> "/**")) splitedLines
  end






(*********************** MAIN FUNCTIONS ***********************)










(*using - T*)
fun writeXML(fileName, content, number : int) = 
  let
    val temp = fileName ^ "T1.xml"
    val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
    val write = TextIO.output (writestream, content)
  in
    number
  end


(*using - P*)
fun write_P_XML(fileName, content, number : int) = 
  let
    val temp = fileName ^ "1.xml"
    val writestream = TextIO.openAppend temp (*open the asm file fot writing*)  
    val write = TextIO.output (writestream, content)
  in
    number
  end
(*using P*)
fun itsAnID_P(fName, content, ogSize, id) = 
  let
    val ID = id ^ String.substring(content, 0, 1)
    val contentSize = String.size(content)
  in
    if(Char.isAlpha(String.sub(content, 1))) orelse (String.substring(content, 1, 1) = "_") orelse (Char.isDigit(String.sub(content, 1)))
    then itsAnID_P(fName, String.substring(content, 1, (contentSize - 1)), ogSize, ID)
    else write_P_XML(fName, "<identifier> " ^ ID ^ " </identifier>\n", (ogSize - contentSize + 1))
  end
(*
(*using - P*)
fun classDecleration(fname, content) = 
  let
    val spaces = 1
  in 
    write_P_XML(fname, "<keyword> class </keyword>")
  end
  *)

(*using - T*)
fun itsAString(fName, content, ogSize, s) = 
  let
    val str = s ^ String.substring(content, 0, 1)
    val contentSize = String.size(content)
  in
    if(String.substring(content, 1, 1) <> "\"") (*) orelse ((String.substring(content, 1, 1) <> " ") andalso (String.substring(content, 2, 1) <> "\"")) *)
    then itsAString(fName, String.substring(content, 1, (contentSize - 1)), ogSize, str)
    else writeXML(fName, "<stringConstant> " ^ str ^ " </stringConstant>\n", (ogSize - contentSize + 2))
  end


(*using - T*)
fun itsANumber(fName, content, ogSize, i) = 
  let
    val integer = i ^ String.substring(content, 0, 1)
    val contentSize = String.size(content)
  in
    if(Char.isDigit(String.sub(content, 1)))
    then itsANumber(fName, String.substring(content, 1, (contentSize - 1)), ogSize, integer)
    else writeXML(fName, "<integerConstant> " ^ integer ^ " </integerConstant>\n", (ogSize - contentSize + 1))
  end


(*using - T*)
fun itsAnID(fName, content, ogSize, id) = 
  let
    val ID = id ^ String.substring(content, 0, 1)
    val contentSize = String.size(content)
  in
    if(Char.isAlpha(String.sub(content, 1))) orelse (String.substring(content, 1, 1) = "_") orelse (Char.isDigit(String.sub(content, 1)))
    then itsAnID(fName, String.substring(content, 1, (contentSize - 1)), ogSize, ID)
    else writeXML(fName, "<identifier> " ^ ID ^ " </identifier>\n", (ogSize - contentSize + 1))
  end


(*using - T*)
(*lexical element - saved keywords - 
  ['boolean', 'char', 'class', 'constructor', 'do', 'else', 'false', 'field',
  'fucntion', 'if', 'int', 'let', 'method',
  'nul', 'return', 'static', 'this', 'true', 'var', 'void', 'while']*)
fun read_Jack_file(fName, content : string) = 
  let
    val contentSize = String.size(content)
  in 
    if(String.substring(content ,0 , 1) = " ")
    then writeXML(fName, "", 1)
    else if(String.substring(content ,0 , 1) = "\n")
    then writeXML(fName, "", 1)
    else if(String.substring(content ,0 , 1) = "\t")
    then writeXML(fName, "", 1)
        else if ((String.substring(content ,0 , 1) = "b") andalso (String.substring(content ,1 , 1) = "o") andalso (String.substring(content ,2 , 1) = "o") andalso  (String.substring(content ,3 , 1) ="l") andalso (String.substring(content ,4  , 1) = "e") andalso (String.substring(content ,5  , 1) = "a") andalso (String.substring(content ,6  , 1) = "n"))
        then writeXML(fName, "<keyword> boolean </keyword>\n", 7)
        else if ((String.substring(content ,0 , 1) = "c") andalso (String.substring(content ,1 , 1) = "h") andalso (String.substring(content ,2 , 1) = "a") andalso  (String.substring(content ,3 , 1) ="r"))
        then writeXML(fName, "<keyword> char </keyword>\n", 4)
        else if ((String.substring(content ,0 , 1) = "c") andalso (String.substring(content ,1 , 1) = "l") andalso (String.substring(content ,2 , 1) = "a") andalso  (String.substring(content ,3 , 1) ="s") andalso (String.substring(content ,4  , 1) = "s"))
        then  writeXML(fName, "<keyword> class </keyword>\n", 5)
        else if (String.substring(content ,0 , 1) = "c") andalso (String.substring(content ,1 , 1) = "o") andalso (String.substring(content ,2 , 1) = "n") andalso  (String.substring(content ,3 , 1) ="s") andalso (String.substring(content ,4  , 1) = "t") andalso (String.substring(content ,5  , 1) = "r") andalso (String.substring(content ,6  , 1) = "u") andalso (String.substring(content ,7  , 1) = "c") andalso (String.substring(content ,8  , 1) = "t") andalso (String.substring(content ,9  , 1) = "o") andalso (String.substring(content ,10  , 1) = "r")
        then writeXML(fName, "<keyword> constructor </keyword>\n", 11)
        else if (String.substring(content ,0 , 1) = "d") andalso (String.substring(content ,1 , 1) = "o")
        then writeXML(fName, "<keyword> do </keyword>\n", 2)
        else if (String.substring(content ,0 , 1) = "e") andalso (String.substring(content ,1 , 1) = "l") andalso (String.substring(content ,2 , 1) = "s") andalso (String.substring(content ,3 , 1) = "e") 
        then writeXML(fName, "<keyword> else </keyword>\n", 4)
        else if (String.substring(content ,0 , 1) = "f") andalso (String.substring(content ,1 , 1) = "a") andalso (String.substring(content ,2 , 1) = "l") andalso (String.substring(content ,3 , 1) = "s") andalso (String.substring(content ,4 , 1) = "e")
        then writeXML(fName, "<keyword> false </keyword>\n", 5)
        else if (String.substring(content ,0 , 1) = "f") andalso (String.substring(content ,1 , 1) = "i") andalso (String.substring(content ,2 , 1) = "e") andalso (String.substring(content ,3 , 1) = "l") andalso (String.substring(content ,4 , 1) = "d")
        then writeXML(fName, "<keyword> field </keyword>\n", 5)
        else if (String.substring(content ,0 , 1) = "f") andalso (String.substring(content ,1 , 1) = "u") andalso (String.substring(content ,2 , 1) = "n") andalso  (String.substring(content ,3 , 1) ="c") andalso (String.substring(content ,4  , 1) = "t") andalso (String.substring(content ,5  , 1) = "i") andalso (String.substring(content ,6  , 1) = "o") andalso (String.substring(content ,7  , 1) = "n")
        then writeXML(fName, "<keyword> function </keyword>\n", 8)
        else if (String.substring(content ,0 , 1) = "i") andalso (String.substring(content ,1 , 1) = "f")
        then writeXML(fName, "<keyword> if </keyword>\n", 2)
        else if (String.substring(content ,0 , 1) = "i") andalso (String.substring(content ,1 , 1) = "n") andalso (String.substring(content ,2 , 1) = "t")
        then writeXML(fName, "<keyword> int </keyword>\n", 3)
        else if (String.substring(content ,0 , 1) = "l") andalso (String.substring(content ,1 , 1) = "e") andalso (String.substring(content ,2 , 1) = "t")
        then writeXML(fName, "<keyword> let </keyword>\n", 3)
        else if (String.substring(content ,0 , 1) = "m") andalso (String.substring(content ,1 , 1) = "e") andalso (String.substring(content ,2 , 1) = "t") andalso (String.substring(content ,3 , 1) = "h") andalso (String.substring(content ,4 , 1) = "o") andalso (String.substring(content ,5 , 1) = "d")
        then writeXML(fName, "<keyword> method </keyword>\n", 6)
        else if (String.substring(content ,0 , 1) = "n") andalso (String.substring(content ,1 , 1) = "u") andalso (String.substring(content ,2 , 1) = "l") andalso (String.substring(content ,3 , 1) = "l")
        then writeXML(fName, "<keyword> null </keyword>\n", 4)
        else if (String.substring(content ,0 , 1) = "r") andalso (String.substring(content ,1 , 1) = "e") andalso (String.substring(content ,2 , 1) = "t") andalso (String.substring(content ,3 , 1) = "u") andalso (String.substring(content ,4 , 1) = "r") andalso (String.substring(content ,5 , 1) = "n")
        then writeXML(fName, "<keyword> return </keyword>\n", 6)
        else if (String.substring(content ,0 , 1) = "s") andalso (String.substring(content ,1 , 1) = "t") andalso (String.substring(content ,2 , 1) = "a") andalso (String.substring(content ,3 , 1) = "t") andalso (String.substring(content ,4 , 1) = "i") andalso (String.substring(content ,5 , 1) = "c")
        then writeXML(fName, "<keyword> static </keyword>\n", 6)
        else if (String.substring(content ,0 , 1) = "t") andalso (String.substring(content ,1 , 1) = "h") andalso (String.substring(content ,2 , 1) = "i") andalso (String.substring(content ,3 , 1) = "s")
        then writeXML(fName, "<keyword> this </keyword>\n", 4)
        else if (String.substring(content ,0 , 1) = "t") andalso (String.substring(content ,1 , 1) = "r") andalso (String.substring(content ,2 , 1) = "u") andalso (String.substring(content ,3 , 1) = "e") 
        then writeXML(fName, "<keyword> true </keyword>\n", 4)
        else if (String.substring(content ,0 , 1) = "v") andalso (String.substring(content ,1 , 1) = "a") andalso (String.substring(content ,2 , 1) = "r") 
        then writeXML(fName, "<keyword> var </keyword>\n", 3)
        else if (String.substring(content ,0 , 1) = "v") andalso (String.substring(content ,1 , 1) = "o") andalso (String.substring(content ,2 , 1) = "i") andalso (String.substring(content ,3 , 1) = "d")
        then writeXML(fName, "<keyword> void </keyword>\n", 4)
        else if (String.substring(content ,0 , 1) = "w") andalso (String.substring(content ,1 , 1) = "h") andalso (String.substring(content ,2 , 1) = "i") andalso (String.substring(content ,3 , 1) = "l") andalso (String.substring(content ,4 , 1) = "e")
        then writeXML(fName, "<keyword> while </keyword>\n", 5)
            else if(String.substring(content ,0 , 1) = "{")
            then writeXML(fName, "<symbol> { </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "}")
            then writeXML(fName, "<symbol> } </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "(")
            then writeXML(fName, "<symbol> ( </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = ")")
            then writeXML(fName, "<symbol> ) </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "[")
            then writeXML(fName, "<symbol> [ </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "]")
            then writeXML(fName, "<symbol> ] </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = ".")
            then writeXML(fName, "<symbol> . </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = ",")
            then writeXML(fName, "<symbol> , </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = ";")
            then writeXML(fName, "<symbol> ; </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "+")
            then writeXML(fName, "<symbol> + </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "-")
            then writeXML(fName, "<symbol> - </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "*")
            then writeXML(fName, "<symbol> * </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "/")
            then writeXML(fName, "<symbol> / </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "&")
            then writeXML(fName, "<symbol> &amp; </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "|")
            then writeXML(fName, "<symbol> | </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "<")
            then writeXML(fName, "<symbol> &lt; </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = ">")
            then writeXML(fName, "<symbol> &gt; </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "=")
            then writeXML(fName, "<symbol> = </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "~")
            then writeXML(fName, "<symbol> ~ </symbol>\n", 1)
            else if(String.substring(content ,0 , 1) = "\"")
              then itsAString(fName, String.substring(content, 1, (contentSize - 1)), contentSize, "")
              else if(Char.isDigit(String.sub(content, 0)))
                then itsANumber(fName, String.substring(content, 0, contentSize), contentSize, "")
                else itsAnID(fName, String.substring(content, 0, contentSize), contentSize, "")
  end


(*using - P
ogSize = original size *)
fun itsALine(fName, content, s) = 
  let
    val str = s ^ String.substring(content, 0, 1)
    val contentSize = String.size(content)
  in
    if(String.substring(content, 1, 1) <> "\n") (*) orelse ((String.substring(content, 1, 1) <> "\n") andalso (String.substring(content, 2, 1) <> "\"")) *)
    then itsALine(fName, String.substring(content, 1, (contentSize - 1)), str) (*continue to read the next char*)
    else str^"\n" (*returns the whole line*)
  end


fun demi_write_P_XML(fName,nextLine,sizeNextLine) = 
  let
    val a = "a"
  in
    if((nextLine) = "<symbol> ) </symbol>\n")
    then writeXML(fName, "", 0)
    else write_P_XML(fName,nextLine,sizeNextLine)(*continue regulary*)
  end

(*using p*)
fun demi_var_write_P_XML(fName,nextLine,sizeNextLine) = 
  let
    val a = "a"
  in
    if((nextLine = "<keyword> var </keyword>\n") orelse(nextLine = "<keyword> let </keyword>\n") orelse (nextLine = "<keyword> if </keyword>\n") orelse (nextLine = "<keyword> while </keyword>\n") orelse (nextLine = "<keyword> do </keyword>\n") orelse (nextLine = "<keyword> return </keyword>\n"))
    then writeXML(fName, "", 0)
    else write_P_XML(fName,nextLine,sizeNextLine)(*continue regulary*)
  end

  fun ex_list_demi_write_P_XML(fName,nextLine,sizeNextLine) = 
  let
    val a = "a"
  in
    if((nextLine) = "<symbol> ) </symbol>\n")
    then writeXML(fName, "", 0)
    else write_P_XML(fName,nextLine,sizeNextLine)(*continue regulary*)
  end


(*if this is op - we want it!!! - for rinat*)
fun op_demi_write_P_XML(fName, nextLine, sizeNextLine) = 
  let
    val a = "a"
  in
    if((nextLine = "<symbol> + </symbol>\n") orelse (nextLine = "<symbol> - </symbol>\n") orelse (nextLine = "<symbol> * </symbol>\n") orelse (nextLine = "<symbol> / </symbol>\n") orelse (nextLine = "<symbol> &amp; </symbol>\n") orelse (nextLine = "<symbol> | </symbol>\n") orelse (nextLine = "<symbol> &lt; </symbol>\n") orelse (nextLine = "<symbol> &gt; </symbol>\n") orelse (nextLine = "<symbol> = </symbol>\n")) 
    then write_P_XML(fName,nextLine,sizeNextLine)(*continue regulary*)
    else writeXML(fName, "", 0)
  end

fun term_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    (*if((nextLine = "<symbol> [ </symbol>\n") orelse (nextLine = "<symbol> ( </symbol>\n"))*)
    (*then term_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine)), counter + sizeNextLine)*)
    if((nextLine <> "<symbol> ] </symbol>\n") andalso (nextLine <> "<symbol> ) </symbol>\n"))
    then term_Inside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine), counter + sizeNextLine)
    else counter + sizeNextLine
  end


fun term(fName, content) =
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<term>\n", 0)
    val counter = term_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</term>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end


(*using p *)
fun expression_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = op_demi_write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if ((nextLine = "<symbol> + </symbol>\n") orelse (nextLine = "<symbol> - </symbol>\n") orelse (nextLine = "<symbol> * </symbol>\n") orelse (nextLine = "<symbol> / </symbol>\n") orelse (nextLine = "<symbol> &amp; </symbol>\n") orelse (nextLine = "<symbol> | </symbol>\n") orelse (nextLine = "<symbol> &lt; </symbol>\n") orelse (nextLine = "<symbol> &gt; </symbol>\n") orelse (nextLine = "<symbol> = </symbol>\n")) 
    then expression_Inside(fName, term(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine)), counter + sizeNextLine)
    else if (nextLine = "<symbol> ] </symbol>\n") orelse (nextLine = "<symbol> ; </symbol>\n") orelse (nextLine = "<symbol> ) </symbol>\n")
    then counter
    else expression_Inside(fName, term(fName, content), counter)
    (*else expression_Inside(fName, content, counter)*)
    
  end


(*using p *)
fun expression(fName, content) = 
  let
   val conSize = String.size(content)

    val opening = write_P_XML(fName, "<expression>\n", 0)
    val counter = expression_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</expression>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end



(*subroutineDec + subroutineDecINSIDE
ParameteteList + ParameterLIST-Inside*)
(*using P*)
fun parameterList_Inside(fName, content ,counter ) =
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w =  demi_write_P_XML(fName,nextLine,sizeNextLine)(*writes the next line into the xml &retuns the size of the line*)
  in
    if((nextLine) <> "<symbol> ) </symbol>\n")(*remeember to write it*)
    then parameterList_Inside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine), counter + sizeNextLine) (*in the end returns the cut size of the word*)
    else counter+sizeNextLine
  end

(*subroutineDec + subroutineDecINSIDE
ParameteteList + ParameterLIST-Inside*)
(*using - p*)
fun parameterList(fName, content) = 
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<parameterList>\n", 0)
    val counter = parameterList_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</parameterList>\n<symbol> ) </symbol>\n<subroutineBody>\n", 0)
  in
    String.substring(content, counter, conSize - counter) (*counter for the 'num' in 'readyForParsing'*)
  end
  (*using p*)
fun varDec_Inside(fName, content ,counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*writes the next line into the xml &retuns th*)
  in
    if((nextLine) <> "<symbol> ; </symbol>\n")(*remeember to write it*)
    then varDec_Inside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine), counter + sizeNextLine) (*in the end returns the cut size of the word*)
    else counter+sizeNextLine
  end
  (*using p*)
fun varDec(fName, content) = 
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<varDec>\n", 0)
    val counter = varDec_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</varDec>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end






(*using p *)
fun let_Statment_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if((nextLine = "<symbol> [ </symbol>\n") orelse (nextLine = "<symbol> = </symbol>\n"))
    then let_Statment_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else if (nextLine <> "<symbol> ; </symbol>\n")
    then let_Statment_Inside(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine),  counter + sizeNextLine)
    else  counter + sizeNextLine (*at the end - at ';'*)
  end

(*using p*)  
fun let_Statment(fName, content) = 
  let
   val conSize = String.size(content)
    val opening = write_P_XML(fName, "<letStatement>\n", 0)
    val counter = let_Statment_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</letStatement>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

(*using p *)
fun if_Statment_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if(nextLine = "<symbol> ( </symbol>\n")
    then if_Statment_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else counter + sizeNextLine (*at the end - at ';'*)
  end

  (*using p*)  
fun if_Statment(fName, content) = 
  let
   val conSize = String.size(content)
    val opening = write_P_XML(fName, "<ifStatement>\n", 0)
    val counter = if_Statment_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</ifStatement>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

  (*using p *)
fun while_Statment_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if(nextLine = "<symbol> ( </symbol>\n")
    then while_Statment_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else counter + sizeNextLine (*at the end - at ';'*)
  end

  (*using p*)  
fun while_Statment(fName, content) = 
  let
   val conSize = String.size(content)
    val opening = write_P_XML(fName, "<whileStatement>\n", 0)
    val counter = while_Statment_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</whileStatement>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

fun expressionList_Inside(fName, content, counter) = 
let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = ex_list_demi_write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if(nextLine <> "<symbol> ) </symbol>\n")
    then expressionList_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else counter (*+ sizeNextLine*) (*at the end - at ';'*)
  end

fun expressionList(fName, content) = 
  let
   val conSize = String.size(content)

    val opening = write_P_XML(fName, "<expressionList>\n", 0)
    val counter = expressionList_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</expressionList>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

(*using p *)
fun do_Statment_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if(nextLine = "<symbol> ( </symbol>\n")
    then do_Statment_Inside(fName, expressionList(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else counter + sizeNextLine (*at the end - at ';'*)
  end

  (*using p*)  
fun do_Statment(fName, content) = 
  let
   val conSize = String.size(content)
    val opening = write_P_XML(fName, "<doStatement>\n", 0)
    val counter = do_Statment_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</doStatement>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

  (*using p *)
fun return_Statment_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if(nextLine <> "<symbol> ; </symbol>\n")
    then return_Statment_Inside(fName, expression(fName, String.substring(content, sizeNextLine, contentSize- sizeNextLine )), counter + sizeNextLine)
    else counter + sizeNextLine (*at the end - at ';'*)
  end

  (*using p*)  
fun return_Statment(fName, content) = 
  let
   val conSize = String.size(content)
    val opening = write_P_XML(fName, "<returnStatement>\n", 0)
    val counter = return_Statment_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</returnStatement>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

(*using p*)
fun statments_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w = demi_var_write_P_XML(fName,nextLine,sizeNextLine)(*demi var is for all the statements*)
  in
    if (nextLine = "<keyword> let </keyword>\n")
    then statments_Inside(fName, let_Statment(fName, content), counter)
    else if (nextLine = "<keyword> if </keyword>\n")
    then statments_Inside(fName, if_Statment(fName, content), counter)
    else if (nextLine = "<keyword> while </keyword>\n")
    then statments_Inside(fName, while_Statment(fName, content), counter)
    else if (nextLine = "<keyword> do </keyword>\n")
    then statments_Inside(fName, do_Statment(fName, content), counter)
    else if (nextLine = "<keyword> return </keyword>\n")
    then statments_Inside(fName, return_Statment(fName, content), counter)
    else counter
  end

  (*using p*)
fun statments(fName, content) = 
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<statements>\n", 0)
    val counter = statments_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</statements>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end

  
  (*using p*)
fun subroutineBody_Inside(fName, content, counter) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w =  demi_var_write_P_XML(fName,nextLine,sizeNextLine)(*writes the next line into the xml &retuns the size of the line*)
    val var = "<keyword> var </keyword>\n"
    val a = "<symbol> { </symbol>\n"
  in
    if((nextLine) = var)
    then subroutineBody_Inside(fName, varDec(fName, content), counter)
    else if ((nextLine = "<keyword> let </keyword>\n") orelse (nextLine = "<keyword> if </keyword>\n") orelse (nextLine = "<keyword> while </keyword>\n") orelse (nextLine = "<keyword> do </keyword>\n") orelse (nextLine = "<keyword> return </keyword>\n"))
    then subroutineBody_Inside(fName, statments(fName, content), counter)
    else if (nextLine <>  "<symbol> } </symbol>\n")
    then subroutineBody_Inside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine ), counter + sizeNextLine)
    else counter+sizeNextLine
    
  end

  (*using p*)
fun subroutineBody(fName, content) = 
  let
    val conSize = String.size(content)

    (*val opening = write_P_XML(fName, "<subroutineBody>\n", 0)*)
    val counter = subroutineBody_Inside(fName, content, 0)
    val closing  = write_P_XML(fName,  "</subroutineBody>\n", 0)
  in
    String.substring(content, counter, conSize - counter)
  end


(*subroutineDec + subroutineDecINSIDE*)
(*using - P*)
fun subRoutineDecInside(fName, content ,counter ) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w =  write_P_XML(fName,nextLine,sizeNextLine)(*writes the next line into the xml &retuns the size of the line*)
  in 
    if((nextLine) = "<symbol> ( </symbol>\n")(*remeember to write it*)
    then subRoutineDecInside(fName, parameterList(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine)), counter) (*in the end returns the cut size of the word*)
    else if((nextLine) ="<symbol> { </symbol>\n")
    then subRoutineDecInside(fName, subroutineBody(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine)), counter)
    else if((nextLine) <>  "<symbol> } </symbol>\n")(*remeember to write it*)
    then subRoutineDecInside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine), counter + sizeNextLine) (*in the end returns the cut size of the word*)
    else counter+ sizeNextLine
    
  end

(*using - p*)
fun subRoutineDec(fName, content) = 
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<subroutineDec>\n", 0)
    val counter = subRoutineDecInside(fName, content, 0)
    val closing  = write_P_XML(fName, "</subroutineDec>\n", 0)
  in
    counter (*counter for the 'num' in 'readyForParsing'*)
  end




(*CLASSVARDEC + CLASSVARDECINSIDE*)
(*using - P*)
fun classVarDecInside(fName, content ,counter ) = 
  let
    val contentSize = String.size(content)
    val nextLine = itsALine(fName, content, "") (*returns the next line *)
    val sizeNextLine = String.size(nextLine)
    val w =  write_P_XML(fName,nextLine,sizeNextLine)(*writes the next line into the xml &retuns the size of the line*)
  in 
    if((nextLine) <>  "<symbol> ; </symbol>\n")
    then classVarDecInside(fName, String.substring(content, sizeNextLine, contentSize - sizeNextLine), counter + sizeNextLine) (*in the end returns the cut size of the word*)
    else counter+ sizeNextLine
    
  end


(*using - P*)
fun classVarDec(fName, content)  =
  let
    val conSize = String.size(content)

    val opening = write_P_XML(fName, "<classVarDec>\n", 0)
    val counter = classVarDecInside(fName, content, 0)
    val closing  = write_P_XML(fName, "</classVarDec>\n", 0)
  in
    counter (*counter for the 'num' in 'readyForParsing'*)
  end

    (*if (true)
    then write_P_XML(fName, "<classVarDec>\n", 0)
    else 0;

    if (true)
    then classVarDecInside(fName, content )
    else 0;

    if (true)
    then write_P_XML(fName, "</classVarDec>\n", 0)
    else 0;*)
  

(*using - P*)
fun read_XML_file(fName, content : string) =
  let
    val fNameSize = String.size(fName)
    val FileName = String.substring(fName, 0, fNameSize)
    val contentSize = String.size(content)
    (*defines the 1st 3 lines at each file - 'class Main {'*)
    val firstThreeLines = "<keyword> class </keyword>\n<identifier> " ^ FileName ^ " </identifier>\n<symbol> { </symbol>\n"(*the lines inside t1.xml*)
    val firstThreeLinesSize = String.size(firstThreeLines)
    val newFirstThreeLines = "\t<keyword> class </keyword>\n\t<identifier> " ^ FileName ^ " </identifier>\n\t<symbol> { </symbol>\n"(*the lines that will be written inside 1.xml file*)
    (*classVarDec*)
    val classVarDec_static = "<keyword> static </keyword>\n"
    val classVarDec_field = "<keyword> field </keyword>\n" 
    (*SubRoutineDec*)
    val subRoutineDec_ctor = "<keyword> constructor </keyword>\n"
    val subRoutineDec_method = "<keyword> method </keyword>n"
    val subRoutineDec_func = "<keyword> function </keyword>\n"
  in
    if(String.substring(content ,0 , 9) = "<tokens>\n")
      then write_P_XML(fName, "", 9)(*delete "<tokens> from the content*)
      else if(String.substring(content ,0 , 10) = "</tokens>\n")
      then write_P_XML(fName, "", 10)
      else if(String.substring(content ,0 , firstThreeLinesSize) = firstThreeLines)
      then write_P_XML(fName, newFirstThreeLines, firstThreeLinesSize)
      else if ((String.substring(content ,0 , String.size(classVarDec_static)) = classVarDec_static) orelse (String.substring(content ,0 , String.size(classVarDec_field)) = classVarDec_field))
      then classVarDec(fName, content)
      else if((String.substring(content ,0 , String.size(subRoutineDec_ctor)) = subRoutineDec_ctor) orelse (String.substring(content ,0 , String.size(subRoutineDec_method)) = subRoutineDec_method) orelse (String.substring(content ,0 , String.size(subRoutineDec_func)) = subRoutineDec_func))
      then subRoutineDec(fName, content)
      else 0

  end


fun read_XML_file1(fName, content : string) = 
  let
    val fNameSize = String.size(fName)
    val FileName = String.substring(fName, 0, fNameSize)
    val contentSize = String.size(content)
    (*defines the 1st 3 lines at each file - 'class Main {'*)
    val firstThreeLines = "<keyword> class </keyword>\n<identifier> " ^ FileName ^ " </identifier>\n<symbol> { </symbol>\n"(*the lines inside t1.xml*)
    val firstThreeLinesSize = String.size(firstThreeLines)
    val newFirstThreeLines = "\t<keyword> class </keyword>\n\t<identifier> " ^ FileName ^ " </identifier>\n\t<symbol> { </symbol>\n"(*the lines that will be written inside 1.xml file*)

    val classVarDec = "<classVarDec>\n"(*just for writing*)
    val classVarDec_static = "<keyword> static </keyword>\n"(*read & write*)
    val classVarDec_field = "<keyword> field </keyword>\n" (*read & write*)

    val classVarDec_type_int = "<keyword> int </keyword>\n" (*read & write*)
    val classVarDec_type_char = "<keyword> char </keyword>\n" (*read & write*)
    val classVarDec_type_bool = "<keyword> boolean </keyword>\n" (*read & write*)
    val identifier = "<identifier> "
  in
    if(String.substring(content ,0 , 9) = "<tokens>\n")
    then write_P_XML(fName, "", 9)(*delete "<tokens> from the content*)
    else if(String.substring(content ,0 , 10) = "</tokens>\n")
      then write_P_XML(fName, "", 10)
      else if(String.substring(content ,0 , firstThreeLinesSize) = firstThreeLines)
        then write_P_XML(fName, newFirstThreeLines, firstThreeLinesSize)
        (*from here - to the rest of the file*)
        (*classVarDec*)
          else if (String.substring(content ,0 , String.size(classVarDec_static)) = classVarDec_static)
          then write_P_XML(fName,classVarDec_static , String.size(classVarDec_static))
          else if (String.substring(content ,0 , String.size(classVarDec_field)) = classVarDec_field)
          then write_P_XML(fName,classVarDec_field , String.size(classVarDec_field))
             (*type*)
             else if (String.substring(content ,0 , String.size(classVarDec_type_int)) = classVarDec_type_int)
             then write_P_XML(fName,classVarDec_type_int , String.size(classVarDec_type_int))
             else if (String.substring(content ,0 , String.size(classVarDec_type_char)) = classVarDec_type_char)
             then write_P_XML(fName,classVarDec_type_char , String.size(classVarDec_type_char))
             else if (String.substring(content ,0 , String.size(classVarDec_type_bool)) = classVarDec_type_bool)
             then write_P_XML(fName,classVarDec_type_bool , String.size(classVarDec_type_bool))
             (*else if ((String.substring(content ,0 , String.size(identifier)) = identifier)  (*className instance*)
             then write_P_XML(fName,classVarDec_type_bool , String.size(classVarDec_type_bool))*)
             else if (String.substring(content ,0 , String.size(identifier)) = identifier)  (*varName*)
             then itsAnID_P(fName, String.substring(content, 0, contentSize), contentSize, "")
          else if(String.substring(content ,0 , 1) = ",")
          then write_P_XML(fName, "<symbol> , </symbol>\n", 1)
          else if (String.substring(content ,0 , String.size(identifier)) = identifier)  (*varName*)
          then itsAnID_P(fName, String.substring(content, 0, contentSize), contentSize, "")
          else if(String.substring(content ,0 , 1) = ";")
          then write_P_XML(fName, "<symbol> ; </symbol>\n", 1)
          else write_P_XML(fName, "\t</classVarDec>\n\t\t", 1)(*here supposed to be </classVarDec>*)

  end


(*using - T*)
fun readyForTokenizing(fName, content : string) = 
  let
    val num = read_Jack_file(fName, content) 
    val contentSize = String.size(content)
  in
    if (contentSize - num) <> 0
    then readyForTokenizing(fName, String.substring(content, num, (contentSize - num)))
    else print("")
  end

(*using - P*)
fun readyForParsing(fName, content : string) = 
  let
    val num = read_XML_file(fName, content) 
    val contentSize = String.size(content)
  in
    if (contentSize - num) <> 0
    then readyForParsing(fName, String.substring(content, num, (contentSize - num)))
    else print("")
  end


(*gets all lines of the vm file and opens a matching asm file*)
(*using - T*)
fun turnJackIntoString(fName) = 
  let
    val fileName = fName ^ ".jack"
    val file = TextIO.openIn fileName
    val contents = TextIO.inputAll file (*file as string*)
    val _ = TextIO.closeIn file
  in
    readyForTokenizing(fName, contents)
  end


(*using - P*)
fun turnXMLIntoString(fName) = 
  let
    val fileName = fName ^ "T1.xml"
    val file = TextIO.openIn fileName
    val contents = TextIO.inputAll file (*file as string*)
    val _ = TextIO.closeIn file
  in
    readyForParsing(fName, contents)
  end


(*get the [("inputA","vm"),...] list *)
(*only opens the vm files, skipes the others in the folder*)
(*using*)
fun mainHelper (dirName, splitFileNameList) =
  let
    val headFile = hd(splitFileNameList)(*the first file  as tuple ("inputA","VM")*)
    val tailFiles = tl(splitFileNameList)(*the rest of the  tuple list*)

    fun first (a, _) = a
    fun second (_,b) = b

    val fName = first(headFile) (*instead of ("inputA","vm") we will get "inputA"*)
    val sName = second(headFile)(*instead of ("inputA","vm") we will get "VM"*)
 
  in
    if sName = "jack"
    then writeXML(fName, "<tokens>\n", 0)
    else 0;

    if sName = "jack"
    then turnJackIntoString(fName)
    else print("");

    if sName = "jack"
    then writeXML(fName, "</tokens>\n", 0)
    else 0;

    
    (*from here - read the fnameT1.xml file and parsering it*)
   (*) if sName = "jack"
    then write_P_XML(fName, "<class>\n", 0) 
    else 0;

    if sName = "jack"
    then turnXMLIntoString(fName)
    else print("");

    if sName = "jack"
    then write_P_XML(fName, "</class>\n", 0) 
    else 0;*)

    if tailFiles <> []
    then mainHelper(dirName, tailFiles)
    else []
  end;


(*using*)
fun main(dirName) = 
  let
    val listFiles = read_dir(dirName) (*send the directory name and gets a list with all the files*)
    val splitFileName = List.map splitByDot listFiles (*send the list of files and return list of [("inputA","vm"),...]*)
    val dir = OS.FileSys.getDir() ^ String.substring("f\\g", 1, 1) ^ dirName  (*subString- the sting, starts at index, counts chars*)
    val temp = OS.FileSys.chDir(dir)
  in
    mainHelper(dirName, splitFileName)
  end;


(*using*)
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
    if sName = "" (*if it's another folder*)
    then main(fName)  (*fName = folder name*)
    else [];

    if tailFiles <> []
    then mHelper(path, tailFiles)
    else []

  end;


(*using*)
fun m(name) =
  let
    val listFiles = read_dir(name) (*send the directory name and gets a list with all the files*)
    val splitFileName = List.map splitByDot listFiles (*send the list of files and return list of [("inputA","vm"),...]*)
    val path = OS.FileSys.getDir() ^ String.substring("f\\g", 1, 1) ^ name  (*subString- the sting, starts at index, counts chars*)
  in 
    mHelper(path, splitFileName)
  end;
  