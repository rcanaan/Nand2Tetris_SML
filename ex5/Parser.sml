(*use "Tokenizing.sml";*)


fun writer(lst, out_stream) = (* envoie chaque mot de ma liste a la fct tokenWriter *)
( let   fun wHelper(token) = tokenWriter(token, out_stream);
	    in List.app wHelper lst end   );  (* applies wHelper to the elements of lst, from left to right. *)


fun lineReader(str) = (  (* retourne une liste avec tous les elements de mon str *)
	let
		val i = ref 0;
		val j = ref 0;					
		val list = ref [];
		val word = ref "";
		val line = ref (remove_comments(str));
		fun isSpace(line) = 
		(	let val i = ref 0;
			in
 				while (!i < size(line) andalso Char.isSpace(String.sub(line,!i))) do i := !i + 1; (* sub retourne la lettre du makom i de la ligne "line"*)
				if (!i >= size(line)) then true
				else false
			end
		); 	 
	in
		if(not (!line = "")) then
		(
			while not(isSpace(!line)) do (*tant que ma ligne n'est pas une ligne avec que des espaces *)
			(
				i := 0;
				while (!i < size(!line) andalso Char.isSpace(String.sub(!line,!i))) do i := !i + 1; (* dans le cas ou j'ai des espaces au debut de la ligne *)
				line := substring(!line, !i, size(!line) - !i);  (* j'enleve ces espaces *)
				i := 0;
				while (!i < size(!line) andalso (not(Char.isSpace(String.sub(!line,!i))))  (*si ma premiere lettre=makom i, n'est pas un espace *)
															  andalso not(isSymbol(String.substring(!line,!i,1)))  (*si ma premiere lettre=makom i, n'est pas un symbol *)
															  andalso not(isDoubleQuote(String.sub(!line,!i)))  (*si ma premiere lettre=makom i, n'est pas un guillemet *)
							)	do i := !i + 1;    (* alors j'avance *)
				if(isSymbol(String.substring(!line,0,1))) then
				(
					word := substring(!line, 0, 1);
					line := remove_comments(substring(!line, 1, size(!line) - size(!word)))  (* line= toute la ligne sauf le symbol *)
				)
				else if(!i = 0 andalso isDoubleQuote(String.sub(!line,!i))) then
				(
					j := !i+1;
					while (!j < size(!line) andalso (not(isDoubleQuote(String.sub(!line,!j))))) do j := !j + 1;  (* tant qu'il n'apas trouvé le deuxieme guillement, il avance *)
					word := substring(!line, 0, !j+1);  (* return le mot avec les guillemet *)
					line := remove_comments(substring(!line, !j+1, size(!line) - size(!word)))  (* line= toute la ligne sauf les guillemets et le mot entre guillemet *)
				)
				else							
				(
					word := substring(!line, 0, !i);  (*le mot correspond normalement a un identifier *)
					line := remove_comments(substring(!line, !i, size(!line) - size(!word)))  (* line= toute la ligne sauf l'identifier *)
				);
				list := !word :: !list (* :: -> rajoute le word au debut de la liste "list" *)
			);
			rev(!list)  (* rev=reverse car les mots ne sont pas dans l'ordre *)
		)
		else ([])
	end
);	

fun parser(in_stream, out_stream) =   
 (  let val wordList = ref [];
		fun convert(NONE) = "" | convert(SOME str) = str; (* convert the file stream to string *)
		fun varDec() =   (*ecrit une nouvelle tag pour les lignes commencant par var *)
		(  let val str = convert(TextIO.inputLine(in_stream));  (* "convert" converti en string . "inputLine" prend une ligne du kovets inStream *)
			in
				wordList := lineReader(str);  (* wordList est une liste avec tous les elements de mon str *)
				if (!wordList = nil) then varDec() (* si la ligne est vide, il refait la fct: prend une autre ligne de str...*)
				else if (not(hd(!wordList) = "var")) then () (*si le 1er mot de la ligne n'est pas var , alors il fait rien *)
				else  (   (* si le 1er mot est var*)
					TextIO.output(out_stream, "<varDec>\n"); (* affiche dans mon fichier <varDec>*)
					writer(!wordList,out_stream); (* la fct writer fait appel a la fct tokenwritersur chaque mot *)
					TextIO.output(out_stream, "</varDec>\n"); (* affiche dans mon fichier <varDec>*)
					varDec()  (* prend une autre ligne de str, et refait la meme chose *)
				)
			end    ); (* end of varDec *)
		  
		fun findLastClosingParenthesis(list, token) =  (*elle returne 0 si le nb de parenthese ouverte=nb de parenthese fermée; sinon elle retourne le makom de la ')' *)
		( let   val i = ref 0;
				val counter = ref 0;
				val result = ref 0;
			in
				if(token = ")") then
				(
					while(!i < length(list)) do 
					(	
						if(List.nth(list, !i) = "(") then (counter := !counter + 1; i := !i + 1)  (* nth= return le i-ème element de la liste *)
						else if (List.nth(list, !i) = ")" andalso !counter > 0) then (counter := !counter - 1; i := !i + 1) (*des qu'on '(' on fait counter +1; des qu'on a ')', on fait counter-1, pour au final verifierqu'a chaque fois qu'on ouvre une, on la referme *)
						else if (List.nth(list, !i) = ")") then (result := !i; i := length(list))
						else i := !i + 1
					)	)
				else (
					while(!i < length(list)) do 
					(	
						if(List.nth(list, !i) = "[") then (counter := !counter + 1; i := !i + 1) (* nth= return le i-ème element de la liste *)
						else if (List.nth(list, !i) = "]" andalso !counter > 0) then (counter := !counter - 1; i := !i + 1)(*des qu'on '(' on fait counter +1; des qu'on a ')', on fait counter-1, pour au final verifierqu'a chaque fois qu'on ouvre une, on la referme *)
						else if (List.nth(list, !i) = "]") then (result := !i; i := length(list))
						else i := !i + 1
					)
				);
				!result
			end  	); (* end of findLastClosingParenthesis *)
		
		fun expression(list, needTags) = (* verifie c'est quoi comme expression: subroutineCall, ExpressionList... *)
		( let   val i = ref 0;
				val temp = ref 0;
				
				fun expressionList(list) = (*c'est qd on a une liste d'expression séparée par des virgules*)
				( let   fun helper(list) =  (  (*cherche si il y a une virgule dans la ligne, et separe la ligne en fct de ca , pour faire les tokenwriter*) 
							let val i = ref 0;
							in
								if(not(list = nil)) then   (*si ya qqc ds la liste *)
								(   while(!i < length(list) andalso not(List.nth(list,!i) = ",")) do i := !i + 1; (*tant que t'a pas de virguke ds ta liste, t'avance *)
									if(!i < length(list)) then  (*on a trouvé la virgule *)
									(   expression(List.take(list, !i), true); (*list.take= retourne les i 1er elements de la liste, donc tout ce qu'il y a ds la ligne avant la virgule *)
										writer([","], out_stream); (* fait la fct tokenwriter pour la virgule *)
										helper(List.drop(list, !i+1)) (* drop= renvoie la list apres avoir supprimé les (i+1) 1er element: prend le reste de la ligne apres la virgule *)
									)
									else (*si ya pas de virgule *)
										expression(List.take(list, !i), true)  (*list.take= retourne les i 1er elements de la liste*)
								)
								else()
							end
						); (* end of helper *)
					in
						TextIO.output(out_stream, "<expressionList>\n"); (* affiche <expressionList> dans out_stream *)
						if(not(hd(list) = ")")) then  (*si le 1er element de la liste est different de ')' , alors on fait la fct helper, pour trouver encore la virgule *)
							helper(list)  (*car tant que on n'a pas trouvé la ')' , c'est qu'il y a encore des expressions et des virgules *)
						else ();
						TextIO.output(out_stream, "</expressionList>\n")  (* affiche </expressionList> dans out_stream *)
					end 
				); (* end of expressionList *)
				
				fun subroutineCall(list) = 
				(   if(hd(tl(list)) = "(") then	(*  subroutineName '(' expressionList ')'  *)
					(   writer([hd(list), "("], out_stream);		
						expressionList(List.drop(list,2))  (* drop= renvoie la list apres avoir supprimé les 2 1er element*)
					)
					else                        (*on aura une liste de la forme:  (className|varName)'.'subroutineName '(' expressionList ')'  *)
					(
						writer([hd(list), ".", List.nth(list,2), "("], out_stream);		(* nth= return le 2ème element de la liste *)
						expressionList(List.drop(list,4))   (* drop= renvoie la list apres avoir supprimé les 4 1er element*)
					);
					writer([")"], out_stream)   ); (* end of subroutineCall *)
		
				fun helper(list) = (* identifie chaque element de la liste (si c'est un operand, un term,... *)
				(   if(hd(list) = "(") then    (* '(' expression ')' *)
					(   TextIO.output(out_stream, "<term>\n");
						writer(["("], out_stream);
						expression(tl(list), true);  (*tl(list)= expression ')' *)
						writer([")"], out_stream);	
						TextIO.output(out_stream, "</term>\n");
						temp := findLastClosingParenthesis(tl(list), ")"); (* la fct retourne la makom de ')'  *) 
						if(isOp((List.nth(tl(list), !temp+1)))) then (*  si le premier element est un operand *)
						(	writer([List.nth(tl(list), !temp+1)], out_stream); (* fait token writer du 1er element *)
							helper(List.drop(tl(list),!temp+2)) (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
						)
						else()
					)
					else if(length(list) > 1 andalso List.nth(list,1) = "[") then    (*si on a:  varName'['(expression)']' *)
					(
						TextIO.output(out_stream, "<term>\n");
						writer([hd(list),"["], out_stream); (* fait token writer de '[' *)
						expression(List.drop(list,2),true);  (*renvoie le reste de la liste a la fct expression *)
						writer(["]"], out_stream);	(* fait token writer de ']' *)
						TextIO.output(out_stream, "</term>\n");
						temp := findLastClosingParenthesis(List.drop(list,2), "]"); (*a priori la fct devrait retourner -1 pour la dernier crochet qui est seul *)
						if(isOp((List.nth(List.drop(list,2), !temp+1)))) then 
						(
							writer([List.nth(List.drop(list,2), !temp+1)], out_stream); (* fait token writer du 1er element *)
							helper(List.drop(List.drop(list,2),!temp+2))  (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
						)
						else()
					)
					else if(length(list) > 1 andalso List.nth(list,1) = ".") then     (* subroutineCall  =  (className|varName)'.'subroutineName '(' expressionList ')'  *)
					( if(needTags = true) then
						(
							TextIO.output(out_stream, "<term>\n");
							subroutineCall(list);
							TextIO.output(out_stream, "</term>\n");
							temp := findLastClosingParenthesis(List.drop(list,4), ")");
							if(isOp((List.nth(List.drop(list,4), !temp+1)))) then 
							(   writer([List.nth(List.drop(list,4), !temp+1)], out_stream); (* fait token writer du 1er element *)
								helper(List.drop(List.drop(list,4), !temp+2))  (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
							)
							else()
						)
						else subroutineCall(list)
					)
					else if(hd(list) = "~" orelse hd(list) = "-") then     (* si  term= unaryOp term  *)
					(   TextIO.output(out_stream, "<term>\n");
						writer([hd(list)], out_stream); (* fait token writer du 1er element *)
						helper(tl(list));  (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
						TextIO.output(out_stream, "</term>\n")
					)
					else if(length(list) > 1 andalso List.nth(list, 1) = "(") then     (* subroutineCall  =  subroutineName '(' expressionList ')'  *)
					(	if(needTags = true) then
						( 
							TextIO.output(out_stream, "<term>\n");  (* on ecrit <term> car subroutineCall est un derivé de term *)
							subroutineCall(list);
							TextIO.output(out_stream, "</term>\n");
							temp := findLastClosingParenthesis(List.drop(list,2), ")");  (*a priori la fct devrait retourner -1 pour la dernier crochet qui est seul *)
							if(isOp((List.nth(List.drop(list,2), !temp+1)))) then  (* si le 1er element de " expressionList ')' " est un operand *)
							(	writer([List.nth(List.drop(list,2), !temp+1)], out_stream);  (* fait token writer du 1er element *)
								helper(List.drop(List.drop(list,2), !temp+2))  (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
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
						(	writer([hd(tl(list))], out_stream);  (* fait token writer du 1er element *)
							helper(List.drop(list,2))  (* fait helper sur le reste de la liste (toute la liste sauf le 1er element *)
						)
						else()
					)
				); (* end of helper *)
			in
				if(needTags = true) then
				(	TextIO.output(out_stream, "<expression>\n");
					helper(list);
					TextIO.output(out_stream, "</expression>\n")
				)
				else helper(list)
			end
		); (* end of expression *)

		fun letStatement() =  (* 'let' varName ('[' expression ']')? '=' expression ';' *)
		(	let val i = ref 1;
			in
				TextIO.output(out_stream, "<letStatement>\n");
				writer(List.take(!wordList, 2), out_stream);  (* take=retourne les 2 1er element de la wordList= let et varName *)
				if(List.nth(!wordList, 2) = "[") then 
				(	while(not(List.nth(!wordList, !i) = "]" andalso List.nth(!wordList, !i+1) = "=")) do i := !i + 1; (* tant que le ieme element n'est pas ] et que celui d'apres n'est pas = alors on avance*)
					writer(["["],out_stream);
					expression(List.drop(!wordList,3),true); 
					writer(["]"],out_stream)
				)
				else (); (* si on n a pas eu '[' expression ']'*)
				writer(["="],out_stream);(*on continue a partir du egal*)
				expression(List.drop(!wordList,!i+2),true); (* on fait la fonction expression sur la liste a partir du = *)
				writer([";"],out_stream);
				TextIO.output(out_stream, "</letStatement>\n")			
			end    ); (* end of letStatement *)
			
		fun doStatement() = (*'do' subroutineCall ';' *)
		(
			TextIO.output(out_stream, "<doStatement>\n");
			writer(["do"], out_stream);
			expression(tl(!wordList), false);   (*--> expression(subroutineCall ';') *)         
			writer([";"], out_stream);
			TextIO.output(out_stream, "</doStatement>\n")
		); (* end of doStatement *)
				
		fun returnStatement() = (* 'return' expression? ';' *)
		(
			TextIO.output(out_stream, "<returnStatement>\n");
			writer(["return"], out_stream);
			if(not(List.nth(!wordList, 1) = ";")) (*si j'ai une expression*)
				then expression(tl(!wordList), true)  (* --> expression(expression ';') *)    
			else();     (* sinon on fait rien *)      
			writer([";"],out_stream);
			TextIO.output(out_stream, "</returnStatement>\n")
		); (* end of returnStatement *)
		
		fun statements() =  (*    statement*     *)
		(	let	fun statement() =  (* letStatement|ifStatement|whileStatement|doStatement|returnStatement  *)
				(	let	fun ifStatement() =   (* --> 'if' '('expression')' '{'statements'}' ('else''{'statements'}')?  *)
						(	let	val str = ref "";
							in
								TextIO.output(out_stream, "<ifStatement>\n");
								writer(["if","("], out_stream);  (* envoie au tokenWriter *)
								expression(List.drop(!wordList, 2),true); (* on fait expression sur ts sans le if et ( *)
								writer([")","{"], out_stream);  (* envoie au tokenWriter *)
								str := remove_comments(convert(TextIO.inputLine(in_stream)));
								while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
								wordList := lineReader(!str);(* lineReader fait une liste de ts les mots *)
								statements();
								writer(["}"], out_stream);  (* envoie au tokenWriter *)
								if(hd(!wordList) = "else") then
								(
									writer(["else","{"], out_stream);  (* envoie au tokenWriter *)
									str := remove_comments(convert(TextIO.inputLine(in_stream)));
									while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
									wordList := lineReader(!str);
									statements();
									writer(["}"], out_stream)  (* envoie au tokenWriter *)
								)
								else ();
								TextIO.output(out_stream, "</ifStatement>\n")
							end   ); (* end of ifStatement *)
							
						fun whileStatement() =  (* 'while' '('expression')''{'statements'}' *)
						(	let	val str = ref "";
							in
								TextIO.output(out_stream, "<whileStatement>\n");
								writer(["while","("], out_stream);  (* envoie au tokenWriter *)
								expression(List.drop(!wordList, 2),true);  (* on fait expression sur tous sans le while et (   *)
								writer([")","{"], out_stream);  (* envoie au tokenWriter *)
								str := remove_comments(convert(TextIO.inputLine(in_stream)));
								while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
								wordList := lineReader(!str);  (* lineReader fait une liste de ts les mots *)
								statements();
								writer(["}"], out_stream);  (* envoie au tokenWriter *)
								TextIO.output(out_stream, "</whileStatement>\n")
							end		);	(* end of whileStatement *)	
						
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

						if(!next = true) then (* comme statements= statement*, alors on fait ca pour qu'il recommence les fct tant qu'on a encore des statement  *)
						(	str := remove_comments(convert(TextIO.inputLine(in_stream)));
							while(!str = "") do	str := remove_comments(convert(TextIO.inputLine(in_stream)));
							wordList := lineReader(!str);  (* lineReader fait une liste de ts les mots *)
							statement()
						)
						else()							
					end  ); (* end of statement *)	
			in
				if (hd(!wordList) = "let" orelse hd(!wordList) = "if" orelse hd(!wordList) = "while" orelse hd(!wordList) = "do" orelse hd(!wordList) = "return") then
				(   (* si mon head est = a un de ces 5 statement   *)
					TextIO.output(out_stream, "<statements>\n");  (* alors j'affiche "<statements> au debut *)
					statement();
					TextIO.output(out_stream, "</statements>\n")  (* et j'affiche "</statements> a la fin *)
				)
				else()
			end     ); (* end of statements *)
			
		fun helper () =
		( let	val str = convert(TextIO.inputLine(in_stream));
			in
				if not(str = "") then
				(	wordList := lineReader(str);  (* lineReader fait une liste de ts les mots *)
					if(not (!wordList = nil)) then (* si ya qqc dans mon wordList *)
					(	if (hd(!wordList) = "}") then ()
						else if (hd(!wordList) = "class") then  (*  si le 1er mot c'est classe*)
						(	TextIO.output(out_stream, "<class>\n");
							writer(!wordList,out_stream);   (* 'class' className '{' classVarDec* subroutineName* '}'  *)
							helper();
							writer(["}"], out_stream);				
							TextIO.output(out_stream, "</class>\n")
						)
						else if (hd(!wordList) = "static" orelse hd(!wordList) = "field") then   (*  si le 1er mot c'est static ou field*)
						(	TextIO.output(out_stream, "<classVarDec>\n");
							writer(!wordList,out_stream);   (* ('static'|'field') type varName (',' varName)* ';'    *)
							TextIO.output(out_stream, "</classVarDec>\n");
							helper()
						)
						else if (hd(!wordList) = "constructor" orelse hd(!wordList) = "function" orelse hd(!wordList) = "method") then  (*  si le 1er mot c'est ctr ou function ou method *)
						(   TextIO.output(out_stream, "<subroutineDec>\n");
							writer(List.take(!wordList, 4), out_stream); (* ('constructor'|'function'|'method')('void'|type)subroutineName'('     *)
							TextIO.output(out_stream, "<parameterList>\n");
							writer(List.take(List.drop(!wordList, 4), length(!wordList)-6),  out_stream);  (* fait le tokenWriter du parameterList  *)
							TextIO.output(out_stream, "</parameterList>\n");
							writer([")"], out_stream);		(*  ')'   *)			
							TextIO.output(out_stream, "<subroutineBody>\n");
							writer(["{"], out_stream);		(*   subroutineBody= '{' varDec*statements'}'   *)			
							varDec();
							statements();
							writer(["}"], out_stream);					
							TextIO.output(out_stream, "</subroutineBody>\n");
							TextIO.output(out_stream, "</subroutineDec>\n");
							helper()
						)
						else  (
							writer(!wordList,out_stream);
							helper()     )
					)
					else (helper()) )
				else ()
			end  ); 
	in
		helper()
	end
	);
