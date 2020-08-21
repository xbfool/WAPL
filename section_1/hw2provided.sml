(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)
fun all_except_option (s : string, l: string list) = 
   let 
      fun f (s, l1, l2) = 
         case l2 of
         [] => NONE
         |s1::xs => 
            if same_string(s1, s) 
            then SOME (l1 @ xs) 
            else f(s, l1 @ [s1], xs)
   in 
      f(s, [], l)
   end;
fun get_substitutions1(l : string list list, s : string) =
   case l of
   [] => []
   |s1::xs => 
      let 
         val x = all_except_option(s, s1)
      in
         case x of
         NONE => get_substitutions1(xs, s)
         |SOME(tmpl) => tmpl @ get_substitutions1(xs, s)
      end;

fun get_substitutions2(l : string list list, s : string) =
   let
     fun f (l1, l2, s) = 
       case l2 of
       [] => l1
       |s1::xs =>
         let 
            val x = all_except_option(s, s1)
         in
            case x of
            NONE => f(l1, xs, s)
            | SOME(tmpl) => f(l1 @ tmpl, xs, s)
         end 
   in
     f([], l, s)
   end;

fun similar_names(nl:string list list, name: {first:string, last:string, middle:string}) =
   let 
      val {first=f, middle=m, last=l} = name
      val name_list = f::get_substitutions2(nl, f)
      fun ff(name_list, name) =
         let 
            val {first=f, middle=m, last=l} = name
         in
            case name_list of
            [] => []
            |x::xs => {first=x, middle=m, last=l}::ff(xs, name)
         end     
   in
      ff(name_list, name)
   end;
  


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int ;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw ;

exception IllegalMove;

(* put your solutions for problem 2 here *)
