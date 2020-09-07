let inc x = x + 1

let _ = print_endline "Hello world!"

let double x = x * 2

let rec pow x y =
    if y = 0 then 1.
    else x *. pow x (y - 1)

let sign x =
    if x > 0 then 1
    else if x = 0 then 0
    else -1

let ar r = 3.14 *. r *. r

let rms x y = sqrt (x *. x +. y *. y)

let date (d:int) (m:string) : bool =
    if m = "Jun" || m = "Nov" || m = "Apr" || m = "Sep" then
        d <= 30 && d > 0
    else if m = "Feb" then
        d <= 28 && d > 0
    else
        d <= 31 && d > 0

let rec fib n =
    if n = 1 then 1
    else if n = 2 then 1
    else fib (n - 1) + fib (n - 2)

let rec h n pp p =
    if n = 1 then p
    else h (n - 1) p (pp + p)

let fib_fast n =
        h n 0 1

let divide x y =
    if y = 0. then raise Division_by_zero
    else x /. y

let (+/.) x y = (x +. y) /. 2.
