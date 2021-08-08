(******************Exercise #4**************************************************
	Ruth Malka 318327822
	Hevi Elyovich 318787777
*******************************************************************************)

exception Err of string; (* global exception of type string *)

fun convert(NONE) = "" | convert(SOME str) = str; (* convert the file stream to string *)

(**************************TOKENIZING*************************************)

fun isKeyword(token) = 
(
	if(token = "class" 
		orelse token = "constructor"
		orelse token = "function"
		orelse token = "method"
		orelse token = "field"
		orelse token = "static"
		orelse token = "var"
		orelse token = "int"
		orelse token = "char"
		orelse token = "boolean"
		orelse token = "void"
		orelse token = "true"
		orelse token = "false"
		orelse token = "null"
		orelse token = "this"
		orelse token = "let"
		orelse token = "do"
		orelse token = "if"
		orelse token = "else"
		orelse token = "while"
		orelse token = "return")
		then true
	else false
);

fun isSymbol(token) = 
(
	if(token = "{"
		orelse token = "}"
		orelse token = "("
		orelse token = ")"
		orelse token = "["
		orelse token = "]"
		orelse token = "."
		orelse token = ","
		orelse token = ";"
		orelse token = "+"
		orelse token = "-"
		orelse token = "*"
		orelse token = "/"
		orelse token = "&"
		orelse token = "|"
		orelse token = "<"
		orelse token = ">"
		orelse token = "="
		orelse token = "~")
		then true
	else false
);

fun isInteger(token) =
(
	List.all Char.isDigit (String.explode(token)) (*String.explode converts string to char list*)
);

fun isDoubleQuote(char) = 
(
	if(char = #"\"") 
		then true 
	else false
);

fun isString(token) = 
(
	isDoubleQuote(String.sub(token,0))
);

fun isOp(token) = 
(
	if(token = "+" 
		orelse token = "-" 
		orelse token = "*" 
		orelse token = "/" 
		orelse token = "&" 
		orelse token = "|"
		orelse token = "<" 
		orelse token = ">" 
		orelse token = "=")
		then true
	else false
);

fun tokenWriter(token, out_stream) = 
(
	let
		val output = 
		(
			if(token = ">") 
				then ("<symbol> &gt; </symbol>\n")
			else if(token = "<")
				then ("<symbol> &lt; </symbol>\n")
			else if(token = "&")
				then ("<symbol> &amp; </symbol>\n")
			else if(isKeyword(token))
				then ("<keyword> " ^ token ^ " </keyword>\n")
			else if (isSymbol(token))
				then ("<symbol> " ^ token ^ " </symbol>\n")
			else if (isInteger(token))
				then ("<integerConstant> " ^ token ^ " </integerConstant>\n")
			else if (isString(token))
				then ("<stringConstant> " ^ String.substring(token,1,size(token) - 2) ^ " </stringConstant>\n")
			else ("<identifier> " ^ token ^ " </identifier>\n")
		)
	in TextIO.output(out_stream, output) end
);					

fun remove_comments(str) =
(
	let
		val i = ref 0;
		val j = ref 0;
		fun just_a_comment() = 
		(
			let
				val i = ref 0;
			in
				while (!i < size(str) andalso Char.isSpace(String.sub(str,!i))) do i := !i + 1;
				if (!i >= size(str) orelse String.substring(str,!i,2) = "//" orelse String.substring(str,!i,2) = "/*" orelse String.sub(str,!i) = #"*") then true
				else false
			end
		);
	in
		if (just_a_comment ()) then ""
		else if (String.isSubstring "//" str) then
		(
			while not(String.sub(str,!i) = #"/" andalso String.sub(str,!i + 1) = #"/") do i := !i + 1;
			j := !i - 1;
			while (Char.isSpace(String.sub(str,!j))) do j := !j - 1;
			substring(str,0, !j+1) ^ "\n"
		)			
		else str
	end
);

fun tokenizer(in_stream, out_stream) =   
(
	let
		fun helper () =
			let
				val str = convert(TextIO.inputLine(in_stream));
				fun lineRedear() =
				(
					let
						val i = ref 0;
						val j = ref 0;					
						val word = ref "";
						val line = ref (remove_comments(str));
						fun isSpace(line) = 
						(	let
								val i = ref 0;
							in
			 	 				while (!i < size(line) andalso Char.isSpace(String.sub(line,!i))) do i := !i + 1;
								if (!i >= size(line)) then true
								else false
							end
						); 	 
					in
						if(not (!line = "")) then
						(
							while not(isSpace(!line)) do 
							(
								i := 0;
								while (!i < size(!line) andalso Char.isSpace(String.sub(!line,!i))) do
									i := !i + 1;
								line := substring(!line, !i, size(!line) - !i);
								i := 0;
								while (!i < size(!line) andalso (not(Char.isSpace(String.sub(!line,!i))))
														andalso not(isSymbol(String.substring(!line,!i,1)))
														andalso not(isDoubleQuote(String.sub(!line,!i)))) do
										i := !i + 1;
								if(isSymbol(String.substring(!line,0,1))) then
								(
									word := substring(!line, 0, 1);
									line := remove_comments(substring(!line, 1, size(!line) - size(!word)))
								)
								else if(!i = 0 andalso isDoubleQuote(String.sub(!line,!i))) then
								(
									j := !i+1;
									while (!j < size(!line) andalso (not(isDoubleQuote(String.sub(!line,!j))))) do
										j := !j + 1;
									word := substring(!line, 0, !j+1);
									line := remove_comments(substring(!line, !j+1, size(!line) - size(!word)))
								)
								else							
								(
									word := substring(!line, 0, !i);
									line := remove_comments(substring(!line, !i, size(!line) - size(!word)))
								);
								tokenWriter(!word, out_stream)
							)
						)
						else ()
					end
				);	
			in
				if not(str = "") then
				(
					lineRedear();			
					helper()  
				)
				else ()
			end;
	in
		TextIO.output(out_stream, "<tokens>\n");
		helper();
		TextIO.output(out_stream, "</tokens>\n")
	end
);

(**************************PARSING*************************************)

fun writer(lst, out_stream) = 
(
	let 
		fun wHelper(token) = tokenWriter(token, out_stream);
	in List.app wHelper lst end
);


fun lineReader(str) =
(
	let
		val i = ref 0;
		val j = ref 0;					
		val list = ref [];
		val word = ref "";
		val line = ref (remove_comments(str));
		fun isSpace(line) = 
		(
			let
				val i = ref 0;
			in
 				while (!i < size(line) andalso Char.isSpace(String.sub(line,!i))) do i := !i + 1;
				if (!i >= size(line)) then true
				else false
			end
		); 	 
	in
		if(not (!line = "")) then
		(
			while not(isSpace(!line)) do 
			(
				i := 0;
				while (!i < size(!line) andalso Char.isSpace(String.sub(!line,!i))) do i := !i + 1;
				line := substring(!line, !i, size(!line) - !i);
				i := 0;
				while (!i < size(!line) andalso (not(Char.isSpace(String.sub(!line,!i))))
															  andalso not(isSymbol(String.substring(!line,!i,1)))
															  andalso not(isDoubleQuote(String.sub(!line,!i)))
							)	do i := !i + 1;
				if(isSymbol(String.substring(!line,0,1))) then
				(
					word := substring(!line, 0, 1);
					line := remove_comments(substring(!line, 1, size(!line) - size(!word)))
				)
				else if(!i = 0 andalso isDoubleQuote(String.sub(!line,!i))) then
				(
					j := !i+1;
					while (!j < size(!line) andalso (not(isDoubleQuote(String.sub(!line,!j))))) do j := !j + 1;
					word := substring(!line, 0, !j+1);
					line := remove_comments(substring(!line, !j+1, size(!line) - size(!word)))
				)
				else							
				(
					word := substring(!line, 0, !i);
					line := remove_comments(substring(!line, !i, size(!line) - size(!word)))
				);
				list := !word :: !list
			);
			rev(!list)
		)
		else ([])
	end
);	

fun parser(in_stream, out_stream) =   
(
	let
		val wordList = ref [];
		
		fun varDec() = 
		(
			let
				val str = convert(TextIO.inputLine(in_stream));
			in
				wordList := lineReader(str);
				if (!wordList = nil) then varDec()
				else if (not(hd(!wordList) = "var")) then ()
				else
				(
					TextIO.output(out_stream, "<varDec>\n");
					writer(!wordList,out_stream);
					TextIO.output(out_stream, "</varDec>\n");
					varDec()
				)
			end 
		); (* end of varDec *)
		  
		fun findLastClosingParenthesis(list, token) = 
		(
			let
				val i = ref 0;
				val counter = ref 0;
				val result = ref 0;
			in
				if(token = ")") then
				(
					while(!i < length(list)) do 
					(	
						if(List.nth(list, !i) = "(") then (counter := !counter + 1; i := !i + 1)
						else if (List.nth(list, !i) = ")" andalso !counter > 0) then (counter := !counter - 1; i := !i + 1)
						else if (List.nth(list, !i) = ")") then (result := !i; i := length(list))
						else i := !i + 1
					)
				)
				else
				(
					while(!i < length(list)) do 
					(	
						if(List.nth(list, !i) = "[") then (counter := !counter + 1; i := !i + 1)
						else if (List.nth(list, !i) = "]" andalso !counter > 0) then (counter := !counter - 1; i := !i + 1)
						else if (List.nth(list, !i) = "]") then (result := !i; i := length(list))
						else i := !i + 1
					)
				);
				!result
			end
		); (* end of findLastClosingParenthesis *)
		
		fun expression(list, needTags) = 
		(
			let
				val i = ref 0;
				val temp = ref 0;
				
				fun expressionList(list) =
				(
					let
						fun helper(list) = 
						(
							let
								val i = ref 0;
							in
								if(not(list = nil)) then
								(
									while(!i < length(list) andalso not(List.nth(list,!i) = ",")) do i := !i + 1;
									if(!i < length(list)) then  (*  there is ','   *)
									(
										expression(List.take(list, !i), true);
										writer([","], out_stream);
										helper(List.drop(list, !i+1))
									)
									else
										expression(List.take(list, !i), true)
								)
								else()
							end
						); (* end of helper *)
					in
						TextIO.output(out_stream, "<expressionList>\n");
						if(not(hd(list) = ")")) then
							helper(list)
						else ();
						TextIO.output(out_stream, "</expressionList>\n")
					end 
				); (* end of expressionList *)
				
				fun subroutineCall(list) = 
				(
					if(hd(tl(list)) = "(") then	(*  subroutineName '(' expressionList ')'  *)
					(
						writer([hd(list), "("], out_stream);		
						expressionList(List.drop(list,2))
					)
					else                        (* (className|varName)'.'subroutineName '(' expressionList ')'  *)
					(
						writer([hd(list), ".", List.nth(list,2), "("], out_stream);		
						expressionList(List.drop(list,4))
					);
					writer([")"], out_stream)
				); (* end of subroutineCall *)
		
				fun helper(list) = 
				(
					if(hd(list) = "(") then    (* '(' expression ')' *)
					(
						TextIO.output(out_stream, "<term>\n");
						writer(["("], out_stream);
						expression(tl(list), true);
						writer([")"], out_stream);	
						TextIO.output(out_stream, "</term>\n");
						temp := findLastClosingParenthesis(tl(list), ")");
						if(isOp((List.nth(tl(list), !temp+1)))) then 
						(
							writer([List.nth(tl(list), !temp+1)], out_stream);
							helper(List.drop(tl(list),!temp+2))
						)
						else()
					)
					else if(length(list) > 1 andalso List.nth(list,1) = "[") then    (* varName'['(expression)']' *)
					(
						TextIO.output(out_stream, "<term>\n");
						writer([hd(list),"["], out_stream);
						expression(List.drop(list,2),true);
						writer(["]"], out_stream);	
						TextIO.output(out_stream, "</term>\n");
						temp := findLastClosingParenthesis(List.drop(list,2), "]");
						if(isOp((List.nth(List.drop(list,2), !temp+1)))) then 
						(
							writer(["]"], out_stream);	
							TextIO.output(out_stream, "</term>\n");
							writer([List.nth(List.drop(list,2), !temp+1)], out_stream);
							helper(List.drop(List.drop(list,2),!temp+2))
						)
						else()
					)
					else if(length(list) > 1 andalso List.nth(list,1) = ".") then     (* subroutineCall  =  (className|varName)'.'subroutineName '(' expressionList ')'  *)
					( 
						if(needTags = true) then
						(
							TextIO.output(out_stream, "<term>\n");
							subroutineCall(list);
							TextIO.output(out_stream, "</term>\n");
							temp := findLastClosingParenthesis(List.drop(list,4), ")");
							if(isOp((List.nth(List.drop(list,4), !temp+1)))) then 
							(
								writer([List.nth(List.drop(list,4), !temp+1)], out_stream);
								helper(List.drop(List.drop(list,4), !temp+2))
							)
							else()
						)
						else subroutineCall(list)
					)
					else if(hd(list) = "~" orelse hd(list) = "-") then     (* unaryOp term  *)
					( 
						TextIO.output(out_stream, "<term>\n");
						writer([hd(list)], out_stream);
						helper(tl(list));
						TextIO.output(out_stream, "</term>\n")
					)
					else if(length(list) > 1 andalso List.nth(list, 1) = "(") then     (* subroutineCall  =  subroutineName '(' expressionList ')'  *)
					(
						if(needTags = true) then
						( 
							TextIO.output(out_stream, "<term>\n");
							subroutineCall(list);
							TextIO.output(out_stream, "</term>\n");
							temp := findLastClosingParenthesis(List.drop(list,2), ")");
							if(isOp((List.nth(List.drop(list,2), !temp+1)))) then 
							(
								writer([List.nth(List.drop(list,2), !temp+1)], out_stream);
								helper(List.drop(List.drop(list,2), !temp+2))
							)
							else()
						)
						else subroutineCall(list)
					)
					else
					(
						TextIO.output(out_stream, "<term>\n");
						writer([hd(list)], out_stream);
						TextIO.output(out_stream, "</term>\n");
						if(length(list) > 1 andalso isOp(hd(tl(list)))) then 
						(
							writer([hd(tl(list))], out_stream);
							helper(List.drop(list,2))
						)
						else()
					)
				); (* end of helper *)
			in
				if(needTags = true) then
				(
					TextIO.output(out_stream, "<expression>\n");
					helper(list);
					TextIO.output(out_stream, "</expression>\n")
				)
				else helper(list)
			end
		); (* end of expression *)

		fun letStatement() = 
		(	
			let
				val i = ref 1;
			in
				TextIO.output(out_stream, "<letStatement>\n");
				writer(List.take(!wordList, 2), out_stream);
				if(List.nth(!wordList, 2) = "[") then 
				(
					while(not(List.nth(!wordList, !i) = "]" andalso List.nth(!wordList, !i+1) = "=")) do i := !i + 1;
					writer(["["],out_stream);
					expression(List.drop(!wordList,3),true); 
					writer(["]"],out_stream)
				)
				else ();
				writer(["="],out_stream);
				expression(List.drop(!wordList,!i+2),true); 
				writer([";"],out_stream);
				TextIO.output(out_stream, "</letStatement>\n")			
			end
		); (* end of letStatement *)
			
		fun doStatement() = 
		(
			TextIO.output(out_stream, "<doStatement>\n");
			writer(["do"], out_stream);
			expression(tl(!wordList), false);            
			writer([";"], out_stream);
			TextIO.output(out_stream, "</doStatement>\n")
		); (* end of doStatement *)
				
		fun returnStatement() = 
		(
			TextIO.output(out_stream, "<returnStatement>\n");
			writer(["return"], out_stream);
			if(not(List.nth(!wordList, 1) = ";")) 
				then expression(tl(!wordList), true)    
			else();           
			writer([";"],out_stream);
			TextIO.output(out_stream, "</returnStatement>\n")
		); (* end of returnStatement *)
		
		fun statements() =
		(
			let
				fun statement() = 
				(
					let
						fun ifStatement() = 
						(
							let
								val str = ref "";
							in
								TextIO.output(out_stream, "<ifStatement>\n");
								writer(["if","("], out_stream);
								expression(List.drop(!wordList, 2),true); 
								writer([")","{"], out_stream);
								str := remove_comments(convert(TextIO.inputLine(in_stream)));
								while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
								wordList := lineReader(!str);
								statements();
								writer(["}"], out_stream);
								if(hd(!wordList) = "else") then
								(
									writer(["else","{"], out_stream);
									str := remove_comments(convert(TextIO.inputLine(in_stream)));
									while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
									wordList := lineReader(!str);
									statements();
									writer(["}"], out_stream)
								)
								else ();
								TextIO.output(out_stream, "</ifStatement>\n")
							end
						); (* end of ifStatement *)
							
						fun whileStatement() = 
						(
							let
								val str = ref "";
							in
								TextIO.output(out_stream, "<whileStatement>\n");
								writer(["while","("], out_stream);
								expression(List.drop(!wordList, 2),true); 
								writer([")","{"], out_stream);
								str := remove_comments(convert(TextIO.inputLine(in_stream)));
								while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
								wordList := lineReader(!str);
								statements();
								writer(["}"], out_stream);
								TextIO.output(out_stream, "</whileStatement>\n")
							end
						);	(* end of whileStatement *)	
						
						val str = ref "";
						val next = ref true;
					in
						if(!wordList = nil) then ()
						else if (hd(!wordList) = "let") then letStatement()
						else if (hd(!wordList) = "if") then ifStatement()
						else if (hd(!wordList) = "while") then whileStatement()
						else if (hd(!wordList) = "do") then doStatement()
						else if (hd(!wordList) = "return") then returnStatement()
						else (next := false);

						if(!next = true) then
						(
							str := remove_comments(convert(TextIO.inputLine(in_stream)));
							while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
							wordList := lineReader(!str);
							statement()
						)
						else()							
					end
				); (* end of statement *)	
			in
				if (hd(!wordList) = "let" orelse hd(!wordList) = "if" orelse hd(!wordList) = "while" orelse hd(!wordList) = "do" orelse hd(!wordList) = "return") then
				(
					TextIO.output(out_stream, "<statements>\n");
					statement();
					TextIO.output(out_stream, "</statements>\n")
				)
				else()
			end
		); (* end of statements *)
			
		fun helper () =
		(
			let
				val str = convert(TextIO.inputLine(in_stream));
			in
				if not(str = "") then
				(
					wordList := lineReader(str);
					if(not (!wordList = nil)) then 
					(
						if (hd(!wordList) = "}") then ()
						else if (hd(!wordList) = "class") then
						(
							TextIO.output(out_stream, "<class>\n");
							writer(!wordList,out_stream);
							helper();
							writer(["}"], out_stream);				
							TextIO.output(out_stream, "</class>\n")
						)
						else if (hd(!wordList) = "static" orelse hd(!wordList) = "field") then
						(
							TextIO.output(out_stream, "<classVarDec>\n");
							writer(!wordList,out_stream);
							TextIO.output(out_stream, "</classVarDec>\n");
							helper()
						)
						else if (hd(!wordList) = "constructor" orelse hd(!wordList) = "function" orelse hd(!wordList) = "method") then
						(
							TextIO.output(out_stream, "<subroutineDec>\n");
							writer(List.take(!wordList, 4), out_stream);
							TextIO.output(out_stream, "<parameterList>\n");
							writer(List.take(List.drop(!wordList, 4), length(!wordList)-6),  out_stream);
							TextIO.output(out_stream, "</parameterList>\n");
							writer([")"], out_stream);					
							TextIO.output(out_stream, "<subroutineBody>\n");
							writer(["{"], out_stream);					
							varDec();
							statements();
							writer(["}"], out_stream);					
							TextIO.output(out_stream, "</subroutineBody>\n");
							TextIO.output(out_stream, "</subroutineDec>\n");
							helper()
						)
						else
						(
							writer(!wordList,out_stream);
							helper()
						)
					)
					else (helper())
				)
				else ()
			end
		); 
	in
		helper()
	end
); (* end of parser *)

(**************************MAIN*************************************)	

fun makeDirFilesList(dirPath) =  
(
		let 
			val dirstream = OS.FileSys.openDir(dirPath); (* open dirstream *)
			fun fEzer(lst) = 
			(
				let 
					val fileOptionName = OS.FileSys.readDir (dirstream); (* the file name in string option type *)
				in
					if not (fileOptionName = NONE) then fEzer ((valOf fileOptionName) :: lst)
					else (OS.FileSys.closeDir dirstream; List.rev(lst)) (* close dirstream, and return *)
				end
			);
		in fEzer [] end
);

fun analyzer(path, action) = 
(
	let 
		fun fileAnalyzer(file) = 
		(
			let
				val in_stream = TextIO.openIn(path ^ "/" ^ file);
				val new_file = String.substring(file, 0, size(file) - size(".jack"));
				val out_stream = 
				(
					case action of "T" => TextIO.openOut(path ^ "/_" ^ new_file ^ "T.xml")
								 | "P" => TextIO.openOut(path ^ "/_" ^ new_file ^ ".xml")
								 | err =>  raise Err("Invalid action")
				)
			in
				(case action of "T" => tokenizer(in_stream, out_stream)
							  | "P" => parser(in_stream, out_stream)
							  | err =>  raise Err("Invalid action"));
				TextIO.closeIn(in_stream);
				TextIO.flushOut(out_stream);
				TextIO.closeOut(out_stream)
			end
		);
		fun isJackFile(file) = (String.isSuffix ".jack" file)
		val JackFilesList =  (List.filter isJackFile (makeDirFilesList(path)))		
	in List.map fileAnalyzer JackFilesList end 		
);


fun main() = 
(
	let
		val path = "C:/cygwin64/compiler/ex3";
		val _ = print("\n=====================================\n");
		fun test() = 
		(
			let
				val _ = print("Please enter test number: \n"
							^ "1 for arrayTest \n"
							^ "2 for expressionlessSquare \n"
							^ "3 for square \n");
				val user = TextIO.inputLine(TextIO.stdIn);
			in
				case valOf(user) of "1\n" => (path ^ "/ArrayTest")
								  | "2\n" => (path ^ "/ExpressionlessSquare")
								  | "3\n" => (path ^ "/Square")
								  |  err  => raise Err("Invalid test number!")
			end
		);
		fun action() = 
		(
			let
				val _ = print("Please enter action: \n"
							^ "1 for tokenizer \n"
							^ "2 for parser \n");
				val user = TextIO.inputLine(TextIO.stdIn);
			in
				case valOf(user) of "1\n" => "T"
								  | "2\n" => "P"
								  |  err  => raise Err("Invalid action!")
			end
		);
		val result = length(analyzer(test(), action())) handle Err msg => (print("ERROR: " ^ msg ^ "\n"); 0)
	in
		if (result > 0) then print(Int.toString(result) ^ " files has been successfully created.\n")
		else ();
		print("=====================================\n")
	end
);

while true do main();
