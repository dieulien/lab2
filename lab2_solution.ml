(* 
                              CS51 Lab 2
                    Polymorphism and record types
                             Spring 2018
 *)

(*
                               SOLUTION
 *)

(*
Objective:

In this lab, you'll exercise your understanding of polymorphism and
record types. Some of the problems extend those from Lab 1, but we'll
provide the necessary background code from that lab.

During the lab session, we recommend that you work on the following
exercises in this order:

1-5; 7; 9-11; 16-17

and complete the remaining ones at your leisure.
 *)

(*======================================================================
Part 1: Currying and uncurrying

........................................................................
Exercise 1: In this exercise, you'll define higher-order functions
curry and uncurry for currying and uncurrying binary functions
(functions of two arguments). The functions are named after
mathematician Haskell Curry '1920. (By way of reminder, a curried
function takes its arguments one at a time. An uncurried function
takes them all at once in a tuple.)

To think about before you start coding:

  * What should the types of curry and uncurry be?

  * What is an example of a function that curry could apply to?
    Uncurry?

  * What are some tests that you could try to verify that your
    implementations of curry and uncurry work properly?

Now implement the two functions curry and uncurry.
......................................................................*)

(* In order to think through this problem, it helps to start with the
types of the functions. The curry function is a *function*; it has a
function type, of the form _ -> _. It is intended to take an uncurried
binary function as its argument, and return the corresponding curried
function. An uncurried binary function is a function that takes its
two arguments both "at the same time", that is, as a
pair. Generically, the type of such a function is thus 'a * 'b ->
'c. A curried binary function takes its two arguments "one at a
time". Its type is 'a -> ('b -> 'c). Putting these together, the type
of curry should be

    (('a * 'b) -> 'c) -> ('a -> ('b -> 'c))      .

(Dropping extraneous parentheses since the -> type operator is right
associative, we can also right this as

    (('a * 'b) -> 'c) -> 'a -> 'b -> 'c)         .

This type information already gives us a big hint as to how to write
the curry function. We start with the first line giving the argument
structure:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c = ...

We call the argument function "uncurried" to emphasize that it is an
uncurried function, and indeed, its type is consistent with that.

The return type is a function type, so we'll want to build a function
value to return. We use the "fun _ -> _" anonymous function
construction to do so, carefully labeling the type of the function's
argument as a reminder of what's going on:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c = 
      fun (x : 'a) -> ...

The type of the argument of this anonymous function is 'a because its
type as a whole -- the return type of curry itself -- is 'a -> ('b ->
'c). This function should return a function of type 'b -> 'c. We'll
construct that as an anonymous function as well:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c = 
      fun (x : 'a) -> 
        fun (y : 'b) -> ...

Now, how should we construct the value (of type 'c) that this inner
function should return? Remember that curry should return a curried
function whose value is the same as the uncurried function would have
delivered on arguments x and y. So we can simply apply uncurried to x
and y (in an uncurried fashion, of course), to obtain the value of
type 'c:

    let curry (uncurried : ('a * 'b) -> 'c) : 'a -> 'b -> 'c = 
      fun (x : 'a) -> 
        fun (y : 'b) -> uncurried (x, y) ;;

You'll note that all of these anonymous functions are a bit
cumbersome, and we have a nicer notation for defining functions in let
expressions incorporating the arguments in the definition part
itself. We've already done so for the argument uncurried. Let's use
that notation for the x and y arguments as well.

    let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c =
      uncurried (x, y) ;;

To make clearer what's going on, we can even drop the explicit types
to show the structure of the computation:

    let curry uncurried x y = uncurried (x, y) ;;

Here, we see what's really going on: "curry uncurried" when applied to
x and y in curried fashion gives the same value that uncurried gives
when applied to x and y in uncurried fashion.

By a similar argument (which it might be useful to carry out
yourself), uncurry is implemented as

    let uncurry curried (x, y) = curried x y ;;

Below, we use the version with explicit types, as we generally want to
do to make our typing intentions known to the compiler/interpreter.
 *)
  
let curry (uncurried : ('a * 'b) -> 'c) (x : 'a) (y : 'b) : 'c =
  uncurried (x, y) ;;
     
let uncurry (curried : 'a -> 'b -> 'c) ((x, y) : ('a * 'b)) : 'c =
  curried x y ;;

(*......................................................................
Exercise 2: OCaml's built in binary operators, like ( + ) and ( * ) are
curried:

# ( + ) ;;
- : int -> int -> int = <fun>
# ( * ) ;;
- : int -> int -> int = <fun>

Using your uncurry function, define uncurried plus and times
functions.
......................................................................*)

let plus = uncurry ( + ) ;;
     
  let times = uncurry ( * ) ;;
  
(*......................................................................
Exercise 3: Recall the prods function from Lab 1:

let rec prods (lst : (int * int) list) : int list =
  match lst with
  | [] -> []
  | (x, y) :: tail -> (x * y) :: (prods tail) ;;

Now reimplement prods using map and your uncurried times function. Why
do you need the uncurried times function?
......................................................................*)

let prods =
  List.map times ;;

(*======================================================================
Part 2: Option types

In Lab 1, you implemented a function max_list that returns the maximum
element in a non-empty integer list. Here's a possible implementation
for max_list:

let rec max_list (lst : int list) : int =
  match lst with
  | [elt] -> elt
  | head :: tail -> max head (max_list tail) ;;

(This implementation makes use of the polymorphic max function from
the Pervasives module.)

As written, this function generates a warning that the match is not
exhaustive. Why? What's an example of the missing case? Try entering
the function in ocaml and see what information you can glean from the
warning message.

The problem is that there is no reasonable value for the maximum
element in an empty list. This is an ideal application for option
types.

........................................................................
Exercise 4: 

Reimplement max_list, but this time, it should return an int option
instead of an int.
......................................................................*)

let rec max_list (lst : int list) : int option =
  match lst with
  | [] -> None
  | head :: tail ->
     match (max_list tail) with
     | None -> Some head
     | Some max_tail -> Some (max head max_tail) ;;
  
(*......................................................................
Exercise 5: Write a function to return the smaller of two int options,
or None if both are None. If exactly one argument is None, return the
other.  The built-in function min from the Pervasives module may be
useful.
......................................................................*)

let min_option (x : int option) (y : int option) : int option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (min left right) ;;
     
(*......................................................................
Exercise 6: Write a function to return the larger of two int options, or
None if both are None. If exactly one argument is None, return the
other.
......................................................................*)

let max_option (x : int option) (y : int option) : int option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (max left right) ;;

(*======================================================================
Part 3: Polymorphism practice

........................................................................
Exercise 7: Do you see a pattern in your implementations of min_option
and max_option? How can we factor out similar code?  

Write a higher-order function for binary operations on options taking
three arguments in order: the binary operation (a curried function)
and its first and second argument. If both arguments are None, return
None.  If one argument is (Some x) and the other argument is None,
function should return (Some x). If neither argument is none, the
binary operation should be applied to the argument values and the
result appropriately returned. 

What is calc_option's function signature? Implement calc_option.
......................................................................*)

let calc_option (f : 'a -> 'a -> 'a) (x : 'a option) (y : 'a option)
              : 'a option =
  match x, y with
  | None,      None       -> None
  | None,      Some right -> Some right
  | Some left, None       -> Some left
  | Some left, Some right -> Some (f left right) ;;
     
(*......................................................................
Exercise 8: Now rewrite min_option and max_option using the higher-order
function calc_option. Call them min_option_2 and max_option_2.
......................................................................*)
  
let min_option_2 : int option -> int option -> int option =
  calc_option min ;;

(* You may have not added in the specific type information in your
definition of min_option_2, and received an inscrutable warning
involving "weak type variables", and type problems when submitting
your code. Here's an example of that behavior:

  # let min_option_2 =
      calc_option min ;;
  val min_option_2 : '_weak1 option -> '_weak1 option -> '_weak1 option = <fun>
  # min_option_2 (Some 3) (Some 4) ;;
  - : int option = Some 3
  # min_option_2 (Some 4.2) (Some 4.1) ;;
  Error: This expression [namely, the 4.2] has type float but an expression
         was expected of type int

The type variables like '_weak1 (with the underscore) are "weak type
variables", not true type variables. They arise because in certain
situations OCaml's type inference can't figure out how to express the
most general types and must resort to this weak type variable
approach.

When a function with these weak type variables is applied to arguments
with a specific type, the polymorphism of the function
disappears. Notice that the first time we apply min_option_2 above to
int options, things work fine. But the second time, applied to float
options, there's a type clash because the first use of min_option_2
fixed the weak type variables to be ints. Since our unit tests try
using min_option_2 in certain ways inconsistent with weak type
variables, you'll get an error message saying that "The type of this
expression, '_weak1 option -> '_weak1 option -> '_weak1 option,
contains type variables that cannot be generalized."

To correct the problem, you can add in specific typing information (as
we've in the solution below) or make explicit the full application of
calc_option

  let min_option_2 x y =
    calc_option min x y ;;

rather than the partial application we use below. Either of these
approaches gives OCaml sufficient hints to infer types more
accurately.

For the curious, if you want to see what's going on in detail, you can
check out the discussion in the section "A function obtained through
partial application is not polymorphic enough" at
https://ocaml.org/learn/faq.html#Typing.
 *)
     
let max_option_2 : int option -> int option -> int option =
  calc_option max ;;

(*......................................................................
Exercise 9: Now that we have calc_option, we can use it in other
ways. Because calc_option is polymorphic, it can work on things other
than int options. Define a function and_option to return the boolean
AND of two bool options, or None if both are None. If exactly one is
None, return the other.
......................................................................*)
  
let and_option : bool option -> bool option -> bool option =
  calc_option (&&) ;;
  
(*......................................................................
Exercise 10: In Lab 1, you implemented a function zip that takes two
lists and "zips" them together into a list of pairs. Here's a possible
implementation of zip (here renamed zip_exn to distinguish it
from the zip you'll implement below, which has a different signature):

let rec zip_exn (x : int list) (y : int list) : (int * int) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip_exn xtl ytl) ;;

As implemented, this function works only on integer lists. Revise your
solution to operate polymorphically on lists of any type. What is the
type of the result? Did you provide full typing information in the
first line of the definition?
......................................................................*)

let rec zip_exn (x : 'a list) (y : 'b list) : ('a * 'b) list =
  match x, y with
  | [], [] -> []
  | xhd :: xtl, yhd :: ytl -> (xhd, yhd) :: (zip_exn xtl ytl) ;;

(* Notice how a polymorphic typing was provided in the first line, to
capture the intention of the polymorphic function. *)

(*......................................................................
Exercise 11: Another problem with the implementation of zip_exn is that,
once again, its match is not exhaustive and it raises an exception
when given lists of unequal length. How can you use option types to
generate an alternate solution without this property? 

Do so below in a new definition of zip.
......................................................................*)

let rec zip (x : 'a list) (y : 'b list) : (('a * 'b) list) option =
  match (x, y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
     (match zip xtl ytl with
      | None -> None
      | Some ztl -> Some ((xhd, yhd) :: ztl))
  | _, _ -> None ;;

(*====================================================================
Part 4: Factoring out None-handling

Recall the definition of dot_prod from Lab 1. Here it is adjusted to
an option type:

let dotprod (a : int list) (b : int list) : int option =
  let pairsopt = zip a b in
  match pairsopt with
  | None -> None
  | Some pairs -> Some (sum (prods pairs)) ;;

Also recall zip from Exercise 8 above.

Notice how in these functions we annoyingly have to test if a value of
option type is None, requiring a separate match, and passing on the
None value in the "bad" branch or introducing the Some in the "good"
branch. This is something we're likely to be doing a lot of. Let's
factor that out to simplify the implementation.

........................................................................
Exercise 12: Define a function called maybe that takes a function of 
type 'a -> 'b and an argument of type 'a option, and "maybe" (depending
on whether its argument is a None or a Some) applies the function to
the argument. The maybe function either passes on the None if its
first argument is None, or if its first argument is Some v, it applies
its second argument to that v and returns the result, appropriately
adjusted for the result type. Implement the maybe function.
......................................................................*)
  
let maybe (f : 'a -> 'b) (x : 'a option) : 'b option =
  match x with 
  | None -> None
  | Some v -> Some (f v) ;;

(*......................................................................
Exercise 13: Now reimplement dotprod to use the maybe function. (The
previous implementation makes use of functions sum and prods. You've
already (re)implemented prods above. We've provided sum for you
below.)  Your new solution for dotprod should be much simpler than in
Lab 1.
......................................................................*)

let sum : int list -> int =
  List.fold_left (+) 0 ;;

let dotprod (a : int list) (b : int list) : int option =
  maybe (fun pairs -> sum (prods pairs))
        (zip a b) ;;

(*......................................................................
Exercise 14: Reimplement zip along the same lines, in zip_2 below. 
......................................................................*)

let rec zip_2 (x : int list) (y : int list) : ((int * int) list) option =
  match (x, y) with
  | [], [] -> Some []
  | xhd :: xtl, yhd :: ytl ->
     maybe (fun ztl -> ((xhd, yhd) :: ztl))
           (zip_2 xtl ytl)
  | _, _ -> None ;;

(*......................................................................
Exercise 15: For the energetic, reimplement max_list along the same
lines. There's likely to be a subtle issue here, since the maybe
function always passes along the None.
......................................................................*)

let rec max_list_2 (lst : int list) : int option =
  match lst with
  | [] -> None
  | [single] -> Some single
  | head :: tail ->
     maybe (fun max_tail -> max head max_tail)
           (max_list_2 tail) ;;

(*======================================================================
Part 5: Record types

A college wants to store student records in a simple database,
implemented as a list of individual course enrollments. The
enrollments themselves are implemented as a record type, called
"enrollment", with string fields labeled "name" and "course" and an
integer student id number labeled "id". An appropriate type might be:
*)

type enrollment = { name : string;
                    id : int;
                    course : string } ;;

(* Here's an example of a list of enrollments. *)

let college = 
  [ { name = "Pat";   id = 1; course = "cs51" };
    { name = "Pat";   id = 1; course = "emr11" };
    { name = "Kim";   id = 2; course = "emr11" };
    { name = "Kim";   id = 2; course = "cs20" };
    { name = "Sandy"; id = 5; course = "ls1b" };
    { name = "Pat";   id = 1; course = "ec10b" };
    { name = "Sandy"; id = 5; course = "cs51" };
    { name = "Sandy"; id = 2; course = "ec10b" }
  ] ;;

(* In the following exercises, you'll want to avail yourself of the
List module functions, writing the requested functions in higher-order
style rather than handling the recursion yourself.

........................................................................
Exercise 16: Define a function called transcript that takes an
enrollment list and returns a list of all the enrollments for a given
student as specified with his or her id.

For example: 
# transcript college 5 ;;
- : enrollment list =
[{name = "Sandy"; id = 5; course = "ls1b"};
 {name = "Sandy"; id = 5; course = "cs51"}]
......................................................................*)

let transcript (enrollments : enrollment list)
               (student : int)
             : enrollment list =
  List.filter (fun { id; _ } -> id = student) enrollments ;;

(* Note the use of field punning, using the id variable to refer to
the value of the id field.

An alternative approach is to use the dot notation to pick out the
record field. 

    let transcript (enrollments : enrollment list)
                   (student : int)
                 : enrollment list =
      List.filter (fun studentrec -> studentrec.id = student)
                  enrollments ;;
 *) 
  
(*......................................................................
Exercise 17: Define a function called ids that takes an enrollment
list and returns a list of all the id numbers in that enrollment list,
eliminating any duplicates. The sort_uniq function from the List
module may be useful here.

For example:
# ids college ;;
- : int list = [1; 2; 5]
......................................................................*)

let ids (enrollments: enrollment list) : int list =
  List.sort_uniq (compare)
                 (List.map (fun student -> student.id) enrollments) ;;

(* Here we used the alternative strategy of picking out the id using
dot notation. *)
  
(*......................................................................
Exercise 18: Define a function called verify that determines whether all
the entries in an enrollment list for each of the ids appearing in the
list have the same name associated.

For example: 
# verify college ;;
- : bool = false
......................................................................*)

let names (enrollments : enrollment list) : string list =
  List.sort_uniq (compare)
                 (List.map (fun { name; _ } -> name) enrollments) ;;
  
let verify (enrollments : enrollment list) : bool =
  List.for_all (fun l -> (List.length l) = 1)
               (List.map
                  (fun student -> names (transcript enrollments student))
                  (ids enrollments)) ;;
