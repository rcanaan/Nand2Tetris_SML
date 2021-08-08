(*ex0 
Rinat Canaan 207744012
Oriya Segal 209338193*)


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


(* writes the sent text into the sent writing stream *)
fun TextHelper(writestream, content)=
     TextIO.output (writestream, content);


(* gets a 'buy' row, writes into the output file and calculates the total amount *)
fun HandleBuy(one, two, three) = 
  let
    val a = one
    val b = Real.fromString two
    val c = Real.fromString three
    val total = Option.getOpt(b, 0.0) * Option.getOpt(c, 0.0)
    val writestream = TextIO.openAppend "output.asm"
    val writestream2 = TextIO.openAppend "totalBuy.asm"
    val content = "### BUY " ^ a ^ " ### \n" ^ Real.toString total ^ "\n" 
    val content2 = Real.toString total ^ "\n"
    val textHelp = TextHelper(writestream, content)
    val textHelp2 = TextHelper(writestream2, content2)
  in
    total
  end;


(* gets a 'cell' row, writes into the output file and calculates the total amount *)
fun HandleCell(one , two , three) = 
 let
    val a = one
    val b = Real.fromString two
    val c = Real.fromString three
    val total = Option.getOpt(b, 0.0) * Option.getOpt(c, 0.0)
    val writestream = TextIO.openAppend "output.asm"
    val writestream2 = TextIO.openAppend "totalCell.asm"
    val content = "$$$ SELL " ^ a ^ " $$$ \n" ^ Real.toString total ^ "\n" 
    val content2 = Real.toString total ^ "\n"
    val textHelp = TextHelper(writestream, content)
    val textHelp2 = TextHelper(writestream2, content2)
  in
    total
  end;


(* gets the end of a line from the file as a string, splits it into 3 parts and sends to the matching 'Handle' function *)
fun helper(st, prior) =
  let
    val str1 = splitFirstSpace(st)     (* ("bread", "3 4.5") *)
    fun first (a, _) = a
    val one = first(str1)              (* "bread" *)
    fun second (_,b) = b 
    val sec = second(str1)             (* "3 4.5" *)
    val str2 = splitFirstSpace(sec)    (* ("3", "4.5") *)
    val two = first(str2)              (* "3" *)
    val three = second(str2)           (* "4.5" *)
  in
    if prior = "buy"
    then HandleBuy(one, two ,three)
    else HandleCell(one, two ,three)
  end;


(* gets all the lines of the file as a list of pairs, sends the info to the 'helper' function and calculates the total of 'buy' amounts and 'cell' amounts *)
fun buyOrSell (lines) = 
  let
    val head = hd(lines)
    val tail = tl(lines)
    fun first (a, _) = a
    val fHead = first(head);
    fun second (_,b) = b
    val sec = second(head)
    val writestream = TextIO.openAppend "output.asm"
  in
    if fHead = "buy"
    then helper(sec, "buy")
    else if fHead = "cell"
          then helper(sec, "cell")
          else 0.0;
    
    if tail <> []
    then buyOrSell(tail)
    else 0.0
  end;


(* turns a file into a list of reals *)
fun readlist (infile : string) = 
let
  val ins = TextIO.openIn infile 
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => Option.valOf(Real.fromString line) :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins 
end;


(* recursively sums the values of a list (real list list) *)
fun listAdd [] = 0.0
  | listAdd ([]::L) = listAdd L
  | listAdd ((x::xs)::L) = x + listAdd (xs::L)

(*in addition to write the tootalbuy and totalsell to the file - print it to the screen *)
fun printToScreen (totalBuy, totalCell)=
  let
    val con = "TOTAL BUY: " ^ Real.toString totalBuy ^ "\nTOTAL CELL: " ^ Real.toString totalCell ^ "\n"
  in 
    con
  end;

(* gets two files (that have only reals in them) and sums each one by its rows *)
fun totals(file1, file2) = 
  let
    val listBuy = readlist(file1)
    val listCell = readlist(file2)
    val totalBuy = listAdd([listBuy])
    val totalCell = listAdd([listCell])
    val content = "TOTAL BUY: " ^ Real.toString totalBuy ^ "\n TOTAL CELL: " ^ Real.toString totalCell ^ "\n"
    val writestream = TextIO.openAppend "output.asm"
    val printto = print(printToScreen(totalBuy, totalCell))
  in
    TextHelper(writestream, content)
  end;

(* opens a new fie named 'output.asm', saves the content of the 'InputA.vm' file and breaks it by new lines, maps the lines with the splitFirstSpace function and calls the 'buyOrCell' function *)
fun InputA() = 
  let
    val readstream = TextIO.openAppend "output.asm"
    val file = TextIO.openIn "InputA.vm"
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val lines = String.tokens (fn c => c = #"\n") contents
    val linesss = List.map splitFirstSpace lines
  in
    buyOrSell(linesss)
  end;


(* opens a new fie named 'output.asm', saves the content of the 'InputB.vm' file and breaks it by new lines, maps the lines with the splitFirstSpace function and calls the 'buyOrCell' function *)
fun InputB() = 
  let
    val readstream = TextIO.openAppend "output.asm"
    val file = TextIO.openIn "InputB.vm"
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val lines = String.tokens (fn c => c = #"\n") contents
    val linesss = List.map splitFirstSpace lines
  in
    buyOrSell(linesss)
  end;


val writestream1 = TextIO.openAppend "output.asm";
val content1 = "InputA:" ^ "\n";
TextHelper(writestream1, content1);
InputA();

val writestream2 = TextIO.openAppend "output.asm";
val content2 = "\n" ^ "InputB:" ^ "\n";
TextHelper(writestream2, content2);
InputB();

val writestream3 = TextIO.openAppend "output.asm";
val content3 = "\n";
TextHelper(writestream3, content3);

totals("totalBuy.asm", "totalCell.asm");
