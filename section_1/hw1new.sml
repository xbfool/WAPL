fun is_older (a: int * int * int, b: int * int * int) = 
    let 
        val (a1, a2, a3) = a
        val (b1, b2, b3) = b 
    in
        if a1 < b1 then true
        else if a1 > b1 then false
        else if b2 < b2 then true
        else if a2 > b2 then false
        else if a3 < b3 then true
        else false
    end



fun number_in_month (a: (int * int * int) list, b: int) = 
    let 
        fun number_in_month_a ([], b, count) = count
        |number_in_month_a (x::xs: (int * int * int) list, b, count) =
            if #2(x) = b then number_in_month_a(xs, b, count + 1)
        else number_in_month_a(xs, b, count) 
    in
        number_in_month_a(a, b, 0)
    end;


    
fun number_in_months (a: (int * int * int) list, b: int list) = 
    let
        fun number_in_month_b (a: (int * int * int) list, [], count) = count
        |number_in_month_b (a, x::xs: int list, count) = number_in_month_b(a, xs, count + number_in_month(a, x))
    in
        number_in_month_b(a, b, 0)
    end;

fun dates_in_month ([], b) = []
|   dates_in_month (x::xs: (int * int * int) list, b: int) =
    if #2(x) = b then x :: dates_in_month(xs, b)
    else dates_in_month(xs, b);

fun dates_in_months (a: (int * int * int) list, []) = []
|   dates_in_months (a, x::xs: int list) = dates_in_month(a, x) @ dates_in_months(a, xs);

fun get_nth([], n) = ""
| get_nth(x::xs, n) = if n = 1 then x else get_nth(xs, n - 1);

fun get_nth_month(n) =
    let val l = ["January", "February", "March",  "April", "May", "June", "July", 
    "August", "September", "October", "November", "December"] in
        get_nth(l, n)
    end;

fun date_to_string(a: int * int * int) =
    get_nth_month(#2(a)) ^ " " ^ Int.toString(#3(a)) ^ ", " ^ Int.toString(#1(a));

fun number_before_reaching_sum(a: int, []) = 0
| number_before_reaching_sum(a: int, x::xs:int list) =
    if a <= x then 0
    else number_before_reaching_sum(a-x, xs) + 1;

fun what_month(a) = 
    let val l = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] in
        number_before_reaching_sum(a, l) + 1
    end;

fun month_range(s, e) =
    if s > e then []
    else what_month(s) :: month_range(s+1, e);
    
fun oldest([]) = NONE
| oldest(x::xs) = 
    case oldest(xs) of
    SOME(x1) => if is_older(x, x1) then SOME(x) else SOME(x1)
    | NONE => SOME(x);