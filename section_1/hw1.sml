val is_older = fn (a: int * int * int, b: int * int * int) => 
    if #1(a) < #1(b) then true
    else if #1(a) > #1(b) then false
    else if #2(a) < #2(b) then true
    else if #2(a) > #2(b) then false
    else if #3(a) < #3(b) then true
    else false;

fun number_in_month_a ([], b, count) = 0
| number_in_month_a (x::xs: date list, b, count) =
    if #2(x) = b then number_in_month_a(xs, b, count + 1)
    else number_in_month_a(xs, b, count);

fun number_in_month (a: (int * int * int) list, b: int) = number_in_month_a(a, b, 0);

fun number_in_month_b (a: (int * int * int) list, [], count) = 0
|   number_in_month_b (a, x::xs: int list, count) =
        number_in_month_b(a, xs, count + number_in_month(a, x));
    
fun number_in_month (a: (int * int * int) list, b: int list) = number_in_month_b(a, b, 0);

fun date_in_month ([], b) = []
|   date_in_month (x::xs: (int * int * int) list, b: int) =
    if #2(x) = b then x :: date_in_month(xs, b)
    else date_in_month(xs, b);

fun date_in_months (a: (int * int * int) list, []) = []
|   date_in_months (a, x::xs: int list) = date_in_month(a, x) @ date_in_months(a, xs);

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
        number_before_reaching_sum(a, l)
    end;

fun month_range(s, e) =
    if s > e then []
    else what_month(s) :: month_range(s+1, e);
    
fun oldest([]) = NONE
| oldest(x::xs) = 
    case oldest(xs) of
    SOME(x1) => if is_older(x, x1) then SOME(x) else SOME(x1)
    | NONE => SOME(x);