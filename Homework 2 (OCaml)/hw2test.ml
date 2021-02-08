let accept_all string = Some (string)

type simpleGramNTs =
  | Statement | NPhrase | A | V | Nn

let simpleGram =
	(Statement, function
	| Statement -> 
		[
			[N NPhrase; N V];
			[N NPhrase; N V; N NPhrase]
		]
	| NPhrase ->
		[
			[N A; N Nn];
			[N Nn]
		]
	| A ->
		[
			[T "young"];
			[T "old"];
			[T "excited"]
		]
	| V ->
		[
			[T "builds"];
			[T "studies"];
			[T "runs"]
		] 
	| Nn ->
		[
			[T "man"];
			[T "simulations"];
			[T "ucla"; T "student"]
		]
	)

let make_matcher_test = 
	((make_matcher simpleGram accept_all ["young"; "ucla"; "student"; "runs"; "simulations"]) = Some ["simulations"])

let make_parser_test = 
	match make_parser simpleGram ["old"; "ucla"; "student"; "builds"; "excited"; "simulations"] with
	| Some tree -> parse_tree_leaves tree = ["old"; "ucla"; "student"; "builds"; "excited"; "simulations"]
	| _ -> false
