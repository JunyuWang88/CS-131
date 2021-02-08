(* WARM-UPS *)

(* 0 *)
(* helper function to check whether the given element is present within the given list *)
let elementInList givenList givenElement = 
	List.exists (fun x -> x = givenElement) givenList

(* 1 *) (* returns true if set a is a subset of set b *) 
let rec subset a b = 
	match a with 
	| [] -> true
	| hd::tl -> if (elementInList b hd) 
	then (subset tl b)
	else (false)

(* 2 *) (* returns true iff set a and set b are equal *)
let equal_sets a b = 
	if ((subset a b) && (subset b a)) 
	then (true)
	else (false)

(* 3 *) (* returns a list that represents the union of set a and set b *)
let rec set_union a b = 
	match a with
        | [] -> b
        | hd::tl -> if (elementInList b hd) 
	then (set_union tl b)
	else (set_union tl (hd::b))

(* 4 *) (* returns a list that represents the intersection of set a and b *)
let rec set_intersection a b =
	List.filter (elementInList b) a	

(* 5 *) (* returns a list that represents the elements of set a that are not in set b *) 
let rec set_diff a b = 
	match a with
	| [] -> []
	| hd::tl -> if ((elementInList b hd) != true) 
	then (hd::(set_diff tl b))
	else (set_diff tl b)

(* 6 *) (* returns fixed point of f with respect to x *)
let rec computed_fixed_point eq f x =
	if (eq (f x) x) 
	then (x)
  	else (computed_fixed_point eq f (f x))

(* 7 *)
type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

(* get left hand side of tuple *)
let getLeftTuple (l,_) = l

(* gets right hand side of tuple *)
let getRightTuple (_, r) = r

(* checks to see if a given symbol from a rule is terminal or nonterminal *)
let isNonterminal givenSymbol = 
	match givenSymbol with
	| T terminal -> false
	| N nonterminal -> true  

(* function that removes the terminal symbols in a given set of rules *)
let rec filterTerminals givenRules = 
	match givenRules with
	| [] -> []
	| N hd::tl -> hd::(filterTerminals tl)
	| T hd::tl -> (filterTerminals tl)

(* function that adds rules to the list of reachable rules *)
let rec addToReachable givenRules givenSymbol = 
	match givenRules with
	| [] -> []
	| hd::tl -> if ((getLeftTuple hd) = givenSymbol) 
	then (hd::(addToReachable tl givenSymbol))
	else (addToReachable tl givenSymbol)

(* function that searches given list of rules to find matches based on the s value *)
let rec matchingRules givenRules givenSymbol = 
	match givenRules with
	| [] -> []
	| hd::tl -> if (getLeftTuple hd = givenSymbol) 
	then ((getRightTuple hd)::(matchingRules tl givenSymbol))
	else (matchingRules tl givenSymbol)

(* function to concatenate rules to the list *)
let rec collectAndConcatenate givenRules givenSymbols = 
	match givenSymbols with
	| [] -> []
	| hd::tl -> ((collectAndConcatenate givenRules tl)@(addToReachable givenRules hd))

let nextRuleList givenRules givenSymbol = 
	filterTerminals (List.concat (matchingRules givenRules givenSymbol))

(* function to find reachable rules when provided with the rules in the input grammar *)
let rec markNextReachableRule givenRules rulesInReach = 
	match givenRules with
	| [] -> rulesInReach
	| hd::tl -> if (elementInList rulesInReach (getLeftTuple hd)) 
	then (let nextRule = nextRuleList givenRules (getLeftTuple hd) 
	in let updatedReachables = set_union nextRule rulesInReach 
	in markNextReachableRule tl updatedReachables)
	else (markNextReachableRule tl rulesInReach)

(* function that preserves the original order of the rule tuples, sorts reachable rules *)
let rec revertToOriginalOrder givenRules parsedRules = 
	match givenRules with
	| [] -> []
	| hd::tl -> if (elementInList parsedRules hd) 
	then (hd::(revertToOriginalOrder tl parsedRules))
	else (revertToOriginalOrder tl parsedRules)

(* function that retrieves rules given the input grammar, and calls necessary functions for *)
(* (cont.) reachable rule filtering *)
let setUp givenGrammar =
	let ruleTuples = ((getLeftTuple givenGrammar):: markNextReachableRule 
	(getRightTuple givenGrammar) 
	(nextRuleList (getRightTuple givenGrammar) (getLeftTuple givenGrammar))) 
	in (getLeftTuple givenGrammar), 
	revertToOriginalOrder (getRightTuple givenGrammar) (collectAndConcatenate 
	(getRightTuple givenGrammar) (ruleTuples))

let filter_reachable g = setUp g