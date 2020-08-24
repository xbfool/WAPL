(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer;

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern;

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu;

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;

(**** you can put all your code here ****)
fun only_capitals xs = 
	List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs;

fun longest_string1 xs =
	let val (s, _) =
		foldl (fn (x,(x1, len)) => 
				if String.size(x) > String.size(x1)
				then (x, String.size(x))
				else (x1, len))
			("", 0)
		  	xs
	in
		s
	end;

fun longest_string2 xs =
	let val (s, _) =
		foldl (fn (x,(x1, len)) => 
				if String.size(x) >= String.size(x1)
				then (x, String.size(x))
				else (x1, len))
			("", 0)
			xs
	in
		s
	end;

fun longest_string_helper f xs =
	let val (s, _) =
		foldl (fn (x,(x1, len)) => 
				if f (String.size(x),String.size(x1))
				then (x, String.size(x))
				else (x1, len))
			("", 0)
			xs
	in
		s
	end;

val longest_string3 = longest_string_helper (fn (x, y) => x > y);

val longest_string4 = longest_string_helper (fn (x, y) => x >= y);

val longest_capitalized = longest_string3 o only_capitals;

val rev_string = String.implode o List.rev o String.explode;

fun first_answer f xs =
	let
		val x = foldl (fn (x, a) =>
					case a of
					NONE => x
					|_ => a) NONE (map f xs)
	in
		case x of
		NONE => raise NoAnswer
		|SOME a => a
	end;

fun all_answers f xs =
	foldl (fn (x, a) => 
		case x of
		NONE => a
		|SOME lst => 
			case a of
			NONE => SOME [lst]
			|SOME lsts => SOME (lst::lsts))
		NONE (map f xs)