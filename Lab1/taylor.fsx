// function to compute
let f x : float = float (System.Math.E ** (2. * x))

let a = 0.1
let b = 0.6
let n = 10

let rec iter f acc a b =
    if a <= b then
        iter f (f a acc) (a+1) b
    else
        acc

let fact n = iter (*) 1 1 n

let pown a = iter (fun x acc -> acc*a) 1. 1

let rec taylor_naive' (x: float) n (acc: float) =
    let eps = 0.000001

    if abs (f x - acc) < eps then
        acc, n
    else
        let new_Acc = acc + (pown (2. * x) n) / float (fact n)
        taylor_naive' x (n + 1) new_Acc


// Define a function to compute f using naive taylor series method
let taylor_naive (x: float) = taylor_naive' x 0 0.

let rec taylor' (x: float) n (prev: float) (acc: float) =
    let eps = 0.000001

    if abs (f x - acc) < eps then
        acc, n
    else
        let new_Acc = acc + prev * 2. * x / float n
        let new_Prev = prev * 2. * x / float n
        taylor' x (n + 1) new_Prev new_Acc

// Define a function to do the same in a more efficient way
let taylor (x: float) = taylor' x 1 1. 1.

let main1 =
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let x1, n1 = taylor_naive x
        let x2, n2 = taylor x
        printfn "%5.2f    %10.6f  %10.6f %d  %10.6f %d" x (f x) x1 n1 x2 n2
// make sure to improve this table to include the required number of iterations
// for each of the methods

main1