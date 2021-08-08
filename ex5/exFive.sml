(*ex5*)
use "exFour.sml"; (*first, we will run ex4 and create the xml files*)

type tableRow = { Name:string, Type:string, Kind:string, Index:int};

fun getName(x : tableRow ) = #Name(x);
fun getType(x : tableRow ) = #Type(x);
fun getKind(x : tableRow ) = #Kind(x);
fun getIndex(x : tableRow ) = #Index(x);


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

fun findByName(str, lst) = 
(
	let
		fun func(x) = ((getName(x)) = str)
	in List.find func lst end
);

fun exists(str, lst) = 
(
	let
		val tmp = findByName(str, lst);
	in
		if (tmp=NONE) then false
		else true
	end
);

fun readLine(in_stream, wordsList) = 
(
	let
		val tmpLine = TextIO.inputLine(in_stream);
		val line = if(tmpLine = NONE) then "NIL NIL NIL" else valOf(tmpLine);
	in (wordsList := String.tokens (Char.isSpace) line) end
);

fun classSymbolTable(in_stream, wordsList, classList) = 
(
	let
		val name = ref "";
		val typ = ref "";
		val kind = ref "";
		val staticIndex = ref 0;
		val fieldIndex = ref 0;
		fun classVarDec() = 
		(
			let
				fun addToClassList() = 
				(
					if ((!kind) = "static") then
					(
						classList := ((!classList) @ [{ Name = (!name), Type = (!typ), Kind = (!kind), Index = (!staticIndex) }]);
						staticIndex := (!staticIndex + 1)
					)
					else 
					(
						classList := ((!classList) @ [{ Name = (!name), Type = (!typ), Kind = (!kind), Index = (!fieldIndex) }]);
						fieldIndex := (!fieldIndex + 1)
					)
				);
				fun classVarDecList() = (*הצהרות משתנה המחלקה*)
				(
					if (hd(tl(!wordsList)) = ",") then
					(
						readLine(in_stream, wordsList); (* <identifier> "???" </identifier> *)
						name := hd(tl(!wordsList));
						addToClassList();
						readLine(in_stream, wordsList); (* <symbol> "," / ";" </symbol> *)
						classVarDecList()
					) 
					else
					(
						readLine(in_stream, wordsList); (* </classVarDec> *)
						readLine(in_stream, wordsList); (* <classVarDec> / <subroutineDec> *)
						classVarDec()
					)
				)
			in
				if (hd(!wordsList) = "<classVarDec>") then
				(
					readLine(in_stream, wordsList); (* <keyword> "field" / "static" </keyword> *)
					kind := hd(tl(!wordsList));
					readLine(in_stream, wordsList); (* <keyword> "int" / "char" / ... </keyword> *)
					typ := hd(tl(!wordsList));
					readLine(in_stream, wordsList); (* <identifier> "???" </identifier> *)
					name := hd(tl(!wordsList));
					addToClassList();
					readLine(in_stream, wordsList); (* <symbol> "," / ";" </symbol> *)
					classVarDecList()
				)
				else ()
			end
		)
	in (readLine(in_stream, wordsList); classVarDec()) end
);

fun methodSymbolTable(in_stream, wordsList, methodList) = 
(
	let
		val name = ref "";
		val typ = ref "";
		val kind = ref "";
		val argumentIndex = ref 0;
		val localIndex = ref 0;
		fun parameterList() = 
		(
			let
				fun addToParameterList() = 
				(
					kind := "argument";
					methodList := ((!methodList) @ [{ Name = (!name), Type = (!typ), Kind = (!kind), Index = (!argumentIndex) }]);
					argumentIndex := (!argumentIndex + 1)
				);
			in
				if (hd(!wordsList) = "</parameterList>") then ()
				else
				(
					readLine(in_stream, wordsList); (* <keyword> "int" / "char" / ... </keyword> *)
					typ := hd(tl(!wordsList));
					readLine(in_stream, wordsList); (* <identifier> "???" </identifier> *)
					name := hd(tl(!wordsList));
					addToParameterList();
					readLine(in_stream, wordsList); (* <symbol> "," </symbol> / </parameterList> *)
					parameterList()
				)
			end
		);
		fun varDec() = 
		(
			let
				fun addToParameterList() = 
				(
					kind := "local";
					methodList := ((!methodList) @ [{ Name = (!name), Type = (!typ), Kind = (!kind), Index = (!localIndex) }]);
					localIndex := (!localIndex + 1)
				);
				fun varDecList() = 
				(
					if (hd(tl(!wordsList)) = ",") then
					(
						readLine(in_stream, wordsList); (* <identifier> "???" </identifier> *)
						name := hd(tl(!wordsList));
						addToParameterList();
						readLine(in_stream, wordsList); (* <symbol> "," / ";" </symbol> *)
						varDecList()
					) 
					else
					(
						readLine(in_stream, wordsList); (* <symbol> ";" </symbol> *)
						readLine(in_stream, wordsList); (* </varDec> *)
						varDec()
					)
				)
			in
				if (hd(!wordsList) = "<varDec>") then
				(
					readLine(in_stream, wordsList); (* <keyword> var </keyword> *)
					readLine(in_stream, wordsList); (* <keyword> "int" / "char" / ... </keyword> *)
					typ := hd(tl(!wordsList));
					readLine(in_stream, wordsList); (* <identifier> "???" </identifier> *)
					name := hd(tl(!wordsList));
					addToParameterList();
					readLine(in_stream, wordsList); (* <symbol> "," / ";" </symbol> *)
					varDecList()
				)
				else ()
			end
		)
	in 
	 	if (hd(!wordsList) = "<parameterList>") then
		(
			parameterList();
			methodSymbolTable(in_stream, wordsList, methodList)
		)
		else if (hd(!wordsList) = "<varDec>") then
		(
			varDec(); 
			methodSymbolTable(in_stream, wordsList, methodList)
		)
		else if (hd(!wordsList) = "<statements>") then ()
		else 
		(
			readLine(in_stream, wordsList);
			methodSymbolTable(in_stream, wordsList, methodList)
		)
	end
);



fun symbolTable (fName, content) =
let
    val wordsList = ref nil : string list ref;
    val classList = ref nil : tableRow list ref;	
    val methodList = ref nil : tableRow list ref;
in
    readLine(fName, wordsList); (* <class> *)
	readLine(fName, wordsList); (* <keyword> class </keyword> *)	
    readLine(fName, wordsList); (* <identifier> ??? </identifier> *)
	readLine(fName, wordsList); (* <symbol> { </symbol> *)

    classSymbolTable(fName, wordsList, classList);
	methodSymbolTable(fName, wordsList, methodList);
	(!classList, !methodList)
end

fun turnJackIntoString(fName) = 
  let
    val fileName = fName ^ ".jack"
    val file = TextIO.openIn fileName
    val contents = TextIO.inputAll file (*file as string*)
    val _ = TextIO.closeIn file
  in
    symbolTable(fName, contents)
  end;
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
    if sName = "xml"
    then turnJackIntoString(fName)
    else print("");

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
