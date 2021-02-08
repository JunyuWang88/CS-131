(* small helper functions *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* get left hand side of tuple *)
let getLeftTuple (l,_) = l

(* gets right hand side of tuple *)
let getRightTuple (_, r) = r

(* 1 *)
let rec getConvertedList grammarType hw1Grammar = 
	match hw1Grammar with
	| [] -> []
	| hd::tl -> if (getLeftTuple hd = grammarType)
	then ((getRightTuple hd)::(getConvertedList grammarType tl))
	else (getConvertedList grammarType tl)
	
(* function to deconstruct given grammar *)
let convertHelper givenGrammar = 
	let s = getLeftTuple givenGrammar in
	let grammarRules = getRightTuple givenGrammar in 
	let convertedList = getConvertedList grammarRules in
	(s, convertedList)

let convert_grammar gram1 = 
	convertHelper gram1

(* 2 *)
let rec getParsed givenTree =
	let rec parseTree givenTree = 
		match givenTree with
		| Leaf leaf -> [leaf]
		| Node (_, head::tail) -> (getParsed head)@(parseKiddos tail)
		| _ -> []
	and parseKiddos nodeKids = 
		match nodeKids with
		| [] -> []
		| hd::tl -> ((parseTree hd)@(parseKiddos tl)) in parseTree givenTree

let parse_tree_leaves tree = 
	getParsed tree

(* 3 *) 
let rec matcherParser givenGrammar acceptor givenRules givenFragment = 
	match givenRules with
		| [] -> (None)
		| hd::tl -> 
			match (partialParser givenGrammar acceptor hd givenFragment) with
			| None -> (matcherParser givenGrammar acceptor tl givenFragment)
			| rule -> (rule)
		and partialParser givenGrammar acceptor givenRules givenFragment =
		match givenRules with
		| [] -> acceptor givenFragment
		| hd::tl -> match givenFragment with
			| [] -> (None)
			| hd::tl ->
				match givenRules with
				| [] -> (None)
				| ((N nonterminal)::parseList) -> 
					(matcherParser givenGrammar (partialParser givenGrammar acceptor 						parseList) (givenGrammar nonterminal) givenFragment)
				| ((T terminal)::parseList) -> if (hd = terminal) 
				then (partialParser givenGrammar acceptor parseList tl)
				else (None) 
				
let rec matcherHelper givenGrammar = 
	match givenGrammar with
	| (s, rules) -> 
		fun acceptor fragment -> 
			(matcherParser rules acceptor (rules s) fragment)

let rec make_matcher gram = 
	matcherHelper gram

(* 4 *)

(* acceptor checks if the fragment is empty or not *)
let checkEmpty givenFragment givenTree =
  match givenFragment with
  | [] -> (Some givenTree)
  | hd::tl -> (None)

let matchEmpty givenFragment givenAcceptor x = givenFragment givenAcceptor x

(* heavy lifting function *)
(* I tried to use my matcher helper function, but couldn’t get it to work with parser so I *)
(* built a whole new function. It isn’t efficient, but it works… *)
let rec treeParser s nodes pFunction covertedList givenFragment treeAcceptor =
	match covertedList with
	| [] -> (None)
	| hd::tl ->
		match (ruleParser s nodes pFunction hd givenFragment treeAcceptor) with
		| Some tempVar -> (Some tempVar)
		| None -> (treeParser s nodes pFunction tl givenFragment treeAcceptor) 
			and ruleParser s nodes pFunction toParseL givenFragment treeAcceptor =
			match toParseL with
			| [] -> (treeAcceptor givenFragment (Node(s, nodes)))
			| hd_2::tl_2 ->
				match hd_2 with
				| (N nonterminal) ->
					let treeAcceptor_2 updatedFrag updatedTree = 
						(ruleParser s (nodes@[updatedTree]) pFunction 									tl_2 updatedFrag treeAcceptor) in (treeParser nonterminal [] 							pFunction (pFunction nonterminal) givenFragment treeAcceptor_2 )
 	   			| (T terminal) ->
					match givenFragment with
      					| [] -> (None)
      					| hd_3::tl_3 -> if (hd_3 = terminal) then (ruleParser s (nodes@[Leaf 						terminal]) pFunction tl_2 tl_3 treeAcceptor) 
					else (None)

let parserHelper givenFragment givenGrammar =
	let pFunction = (getRightTuple givenGrammar) and
	s = (getLeftTuple givenGrammar) in 
	(let covertedList = (pFunction s) in
	treeParser s [] pFunction covertedList givenFragment checkEmpty)

let make_parser gram frag = parserHelper frag gram
