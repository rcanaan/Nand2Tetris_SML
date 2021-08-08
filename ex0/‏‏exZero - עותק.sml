
(*  C:\cygwin64\ex0*)
(*סעיף א ב*)
(*val path = "C:\cygwin64\ex0" : string*)
(*val ab = String.toChar (#"\"")
val ac = "InputA.vm" : string
val res = ab ^ ac : string*)
val hell = "hello \\ bob \n ribat";
print hell;


(*fun curPath (path)= 
  let
     val temp = String.sub(path ,15)
     
  in
     print temp
  end;*)


(*val writestream = TextIO.openAppend "output.asm"; (*creating file - סעיף ד'*)
val content = "### BUY " ^ "productName" ^ " ###"; (*opening it for writing*)
TextIO.output (writestream, content);    *)

(*val readstream = TextIO.openIn "output.asm";*) 

(*fun readlist (infile : string) = 
let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end
val lines = readlist("InputA.vm");
val head = hd(lines);*)

fun splitCharsFirstSpace cs =
  case cs of
    [] => ([], [])
  | c :: cs' =>
      if Char.isSpace c then ([], cs')
      else let val (l, r) = splitCharsFirstSpace cs'
           in (c :: l, r)
           end

fun splitFirstSpace s =
  let
    val (l, r) = splitCharsFirstSpace (String.explode s)
  in
    (String.implode l, String.implode r)
  end

(*val file = TextIO.openIn "InputA.vm";
val contents = TextIO.inputAll file;
val _ = TextIO.closeIn file;
val lines = String.tokens (fn c => c = #"\n") contents;
val linesss = List.map splitFirstSpace lines;*)
fun TextBuyCellHelper(writestream, content)=
     TextIO.output (writestream, content);

(*treats only bread 3 4.5*)
fun HandleBuy(one ,two , three) = 
  let
    val a = one
    val b = Real.fromString two
    val c = Real.fromString three
    val total = Option.getOpt(b, 0.0) * Option.getOpt(c, 0.0)
    val writestream = TextIO.openAppend "output.asm"(*creating file - סעיף ד'*)
    val content = "### BUY " ^ a ^ " ### \n" ^ Real.toString total ^ "\n" 
     val textHelp = TextBuyCellHelper(writestream, content)
  in
    total
  end;

fun HandleCell(one , two , three) = 
 let
    val a = one
    val b = Real.fromString two
    val c = Real.fromString three
    val total = Option.getOpt(b, 0.0) * Option.getOpt(c, 0.0)
    val writestream = TextIO.openAppend "output.asm"
    val content = "$$$ SELL " ^ a ^ " $$$ \n" ^ Real.toString total ^ "\n" 
    val textHelp = TextBuyCellHelper(writestream, content)
  in
    total
  end;

(*"bread 3 4.5"*)
fun helper(st, prior) =
  let
    val str1 = splitFirstSpace(st) (*("bread", "3 4.5")*)
    fun first (a, _) = a
    val one = first(str1)(*bread*)

    fun second (_,b) = b 
    val sec = second(str1) (*"3 4.5"*)

    val str2 = splitFirstSpace(sec) (*("3", "4.5")*)
    val two = first(str2)(*"3"*)
    val three = second(str2)(*"4.5"*)

  in
    if prior = "buy"
    then HandleBuy(one, two ,three)
    else HandleCell(one, two ,three)
  end;



(*head and tail are the items in list first and second are the items in tuple*)
(*this func checks if buy or sell*)
fun buyOrSell (lines) = 
    let
        val head =  hd(lines)
        val tail = tl(lines)
        (*val first = #1 head*)
        fun first (a, _) = a
        val fHead = first(head);

        fun second (_,b) = b
        val sec = second(head)
        val sumTotalBuy = 1.3
        val sumTotalCell = 0.0

        val writestream = TextIO.openAppend "output.asm"
    in
        if fHead = "buy"
        then sumTotalBuy + helper(sec, "buy") (*HandleBuy  - returns the total of each line*)
        else if fHead = "cell"
             then sumTotalCell + helper(sec, "cell") (*HandleSell *)
             else 1.2;
        
        if tail <>[]
        then buyOrSell(tail)
        else TextBuyCellHelper(writestream, Real.toString sumTotalBuy)
    end;

(*val list = buyOrSell(linesss);*)


fun main() = 
  let
    val readstream = TextIO.openIn "output.asm" (*25*)
    val file = TextIO.openIn "InputA.vm" (*56-60*)
    val contents = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val lines = String.tokens (fn c => c = #"\n") contents
    val linesss = List.map splitFirstSpace lines
    val list =[]
  in
    buyOrSell(linesss) (*132*)
  end;

main();





(*let
    val head = hd(linesss);
    val first = #1 head;
in
    while !linesss<>[] do 
    (
        head := hd(linesss);
        linesss = tl(linesss);
        first := #1 head;
        if first = "buy"
        then false (*HandleBuy *)
        else if first = "cell"
        then true (*HandleSell *)
        else false
    );
end;*) 













(*fun add(x, y) = x + y; *)

(* func for writing
 fun foo(abc : string) = 
   let
    val writestream = TextIO.openAppend "output.asm"
   in 
        TextIO.open writestream 
    end*)
(* val writestream = TextIO.openOut "output.txt"*)

(*fun handleBuy (productName , mount , price ) = 
    val writestream = TextIO.openAppend "output.asm"
    val content = ["### BUY ", productName, " ###"]
    TextIO.output (writestream, content)*)

    

    (*== or 

    TextIO.output (writestream, "### BUY ");
    TextIO.output (writestream, ProductName); 
    TextIO.output (writestream, " ###");

    val total = Amount * Price;

    == or == *)

  (*  val total = real(Amount) * Price;

    TextIO.output (writestream, total);
    TextIO.closeOut writestream; *)

