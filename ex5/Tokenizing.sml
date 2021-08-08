fun convert(NONE) = "" | convert(SOME str) = str; (* convert the file stream to string *)
fun isKeyword(token) =  (*verify if token is a keyword*)
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

fun isSymbol(token) =   (*verify if token is a symbol*)
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

fun isInteger(token) = (*verify if token is a number*)
(                                                 (*la fct list.all recoit 2 parametre et applique le 1er parametre (Char.isDigit) sur chaque membre de la liste du 2eme parametre(String.explode(token))*)
	List.all Char.isDigit (String.explode(token)) (*la fct string.explode transforme mon token en list de char*)
	                                              (*char.isDigit verifie que chaque char est un digit et renvoie true si c'est le cas*)
);

fun isDoubleQuote(char) = 
(
	if(char = #"\"") (*verify if char=" *)
		then true 
	else false
);

fun isString(token) =  (*verify if token is a string*)
(
	isDoubleQuote(String.sub(token,0)) (*sub return the letter in the  makom 0. if it's a doubleQuote, the fct return true.*)
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

fun tokenWriter(token, out_stream) =  (*out_stream c le nouveau fichier ou il va ecrire *)
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
				then ("<stringConstant> " ^ String.substring(token,1,size(token) - 2) ^ " </stringConstant>\n") (*substring return le mot sans les guillemets.*)
			else ("<identifier> " ^ token ^ " </identifier>\n")
		)
	in TextIO.output(out_stream, output) end   (*write "output" to the text stream "out_stream"*)
);					

fun remove_comments(str) =
( let   val i = ref 0;
		val j = ref 0;
		fun just_a_comment() = 
		(
			let
				val i = ref 0;
			in
				while (!i < size(str) andalso Char.isSpace(String.sub(str,!i))) do i := !i + 1; (* ! signifie une reference, un pointer. *)
				                                                                                (*verify if the char is a space. if it's the case, it continue with the next char*)
				if (!i >= size(str) orelse String.substring(str,!i,2) = "//" orelse String.substring(str,!i,2) = "/*" orelse String.sub(str,!i) = #"*") then true
				else false
			end
		);
	in
		if (just_a_comment ()) then "" (*si ton str c'est des espaces ou si il commence par // ou /*, alors tu efface tout ce str *)
		else if (String.isSubstring "//" str) then (* sinon si au milieu de ton str tu as des éarote, *)
		(
			while not(String.sub(str,!i) = #"/" andalso String.sub(str,!i + 1) = #"/") do i := !i + 1; (* trouve le makom ou commence la éara *)
			j := !i - 1;
			while (Char.isSpace(String.sub(str,!j))) do j := !j - 1; (* le pointer j se place juste avant la éara, et enleve tous les espaces  *)
			substring(str,0, !j+1) ^ "\n" (*retourne le mot du debut jusqu a la fin de la ligne sans la éara et sans les espaces en plus (avant la éara)  *)
		)
		else str
		
	end
);

fun tokenizer(in_stream, out_stream) =  (  (*pour identifier chaque lettre de la ligne, au cas ou il n'y a pas d'espace entre eux *)
	let
		fun helper () =
			let
				val str = convert(TextIO.inputLine(in_stream)); (* "convert" converti en string . "inputLine" prend une ligne du kovets inStream *)
				fun lineReader() =
				(
					let
						val i = ref 0;
						val j = ref 0;					
						val word = ref "";
						val line = ref (remove_comments(str)); (* str=la ligne qu'il a pris du file "in_stream", line= cette ligne sans éarote, ni espace *)
						fun isSpace(line) =   (* retourne true si on a une ligne vide = avec que des espaces *)
						(	let
								val i = ref 0;
							in
			 	 				while (!i < size(line) andalso Char.isSpace(String.sub(line,!i))) do i := !i + 1; (* sub retourne la lettre du makom i de la ligne "line"*)
								if (!i >= size(line)) then true
								else false
							end
						); 	 
					in
						if(not (!line = "")) then
						(
							while not(isSpace(!line)) do (    (*tant que ma ligne n'est pas une ligne avec que des espaces *)
								i := 0;
								while (!i < size(!line) andalso Char.isSpace(String.sub(!line,!i))) do (* dans le cas ou j'ai des espaces au debut de la ligne *)
									i := !i + 1;
								line := substring(!line, !i, size(!line) - !i); (* j'enleve ces espaces *)
								i := 0;
								while (!i < size(!line) andalso (not(Char.isSpace(String.sub(!line,!i)))) (*si ma premiere lettre=makom i, n'est pas un espace *)
														andalso not(isSymbol(String.substring(!line,!i,1)))   (*si ma premiere lettre=makom i, n'est pas un symbol  *)
														andalso not(isDoubleQuote(String.sub(!line,!i)))) do  (*si ma premiere lettre=makom i, n'est pas un guillement *)
										i := !i + 1; (* alors j'avance *)
								if(isSymbol(String.substring(!line,0,1))) then
								(
									word := substring(!line, 0, 1);
									line := remove_comments(substring(!line, 1, size(!line) - size(!word)))  (* line= toute la ligne sauf le symbol *)
								)
								else if(!i = 0 andalso isDoubleQuote(String.sub(!line,!i))) then
								(
									j := !i+1;
									while (!j < size(!line) andalso (not(isDoubleQuote(String.sub(!line,!j))))) do  (* tant qu'il n'apas trouvé le deuxieme guillement, il avance *)
										j := !j + 1;
									word := substring(!line, 0, !j+1); (* return le mot avec les guillemet *)
									line := remove_comments(substring(!line, !j+1, size(!line) - size(!word)))  (* line= toute la ligne sauf les guillemets et le mot entre guillemet *)
								)
								else (
									word := substring(!line, 0, !i);  (*le mot correspond normalement est un identifier *)
									line := remove_comments(substring(!line, !i, size(!line) - size(!word))) (* line= toute la ligne sauf l'identifier *)
								);
								tokenWriter(!word, out_stream)
							)
						)
						else ()
					end  );	
			in
				if not(str = "") then
				(
					lineReader();			
					helper()  
				)
				else ()
			end;
	in
		TextIO.output(out_stream, "<tokens>\n"); (* affiche <tokens> dans le fichier out_stream *)
		helper();  (*fait la fct helper *)
		TextIO.output(out_stream, "</tokens>\n") (* affiche </tokens> dans le fichier out_stream *)
	end  );
