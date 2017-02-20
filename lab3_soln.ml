# 1 "lab3.mlpp"
(*
                              CS51 Lab 3
               Variants, algebraic types, and matching
                             Spring 2017
 *)

# 8
(*
                               SOLUTION
 *)
     
# 27
(*======================================================================
Part 1: Variants and Invariants (two separate concepts)

In this lab you'll use algebraic data types to create several data
structures.

Ultimately, you'll define several types and structures that allow you
to create a family tree. To do this, you need to create a type to
store a set of biographical information about a person, like their
name, birthdate, and favorite color. Note that this set of data is
different from the enrollment data from Lab 2, so you'll need to
create a new type.

You might be tempted to do something overly simplistic like the
below:

type person = { name : string; favorite : string; birthday : string; } ;;

Let's consider why this may not be appropriate by evaluating the type
for each record field individually.

First, it seems reasonable for a name to be a string, so let's declare
that complete and move on.

The "favorite" field is more problematic. Although we named it such
for simplicity, it doesn't convey very well that we intended for this
field to represent a person's favorite *color*. This could be resolved
with some documentation, but is not enforced at any level other than
hope. Next, it's very likely that many persons would select one of a
subset of colors. Let's fix this issue first.

........................................................................
Exercise 0: Define a new type, called "color_label", whose value can
be any of the following options: red, crimson, orange, yellow, green,
blue, indigo, or violet.
......................................................................*)

# 67
type color_label =
  | Red | Crimson | Orange | Yellow | Green | Blue | Indigo | Violet ;;

# 71
(* You've just defined a new variant type! But this is an overly
simplistic representation of colors. Let's make it more usable.

One of the most commonly used methods of representing color in digital
devices is RGB: a triplet of values to represent a red, green, and
blue component that, through additive mixing, produce the wide array of
colors our devices render.

Commonly, each of the red, green, and blue values are made up of a
single 8-bit (1-byte) integer. Since one byte represents 256 discrete
values, there are over 16.7 million (256 * 256 * 256) possible colors
that can be represented with this method.

The three components that make up an RGB color are referred to as
"channels". In this 8-bit-per-channel model, a value of 0 represents
no color and a value of 255 represents the full intensity of that
color. Some examples:

 R  |  G  |  B  | Color
----|-----|-----|------------
255 |   0 |   0 | Red
  0 |  64 |   0 | Dark green
  0 | 255 | 255 | Cyan
164 |  16 |  52 | Crimson

........................................................................
Exercise 1: Define a color type that supports either "Simple" colors
(from the color_label type you defined previously) or "RGB" colors,
which would incorporate a tuple of values for the three color
channels. You'll want to use Simple and RGB as the value constructors
in this new variant type.
......................................................................*)

# 107
type color =
  | Simple of color_label
  | RGB of int * int * int ;;

# 112
(* Note that there is an important assumption about the RGB values
that determine whether a color is valid or not. The RGB type contains
an *invariant*, that is, a condition that we assume to be true in
order for the type to be valid.

- The red, green, and blue channels must be an unsigned (positive-only)
  8-bit int. Therefore, each channel must be in the range [0, 255].

Since OCaml, unlike some other languages, does not have native support
for unsigned 8-bit integers, you should ensure the invariant remains
true in your code. (You might think to use the OCaml "char" type --
which is an 8-bit character -- but this would be an abuse of the
type. In any case, thinking about invariants will be useful practice
for upcoming problem sets.)

........................................................................
Exercise 2: Write a function, valid_rgb, that accepts a color and
raises an Invalid_Color exception with a useful message if the
invariant is violated, or returns the color back if it's valid.
......................................................................*)

exception Invalid_Color of string ;;

# 139
let valid_rgb (c : color) : color =
  let bad (x : int) : bool = (x < 0 || x > 255) in
  match c with
  | Simple x -> Simple x
  | RGB (r, g, b) ->
     if bad r then raise (Invalid_Color "bad red channel")
     else if bad g then raise (Invalid_Color "bad green channel")
     else if bad b then raise (Invalid_Color "bad blue channel")
     else RGB(r, g, b) ;;

# 150
(*......................................................................
Exercise 3: Write a function, make_color, that accepts three integers
for the channel values and returns a value of the color type. Be sure
to verify the invariant.
......................................................................*)

# 160
let make_color (r : int) (g : int) (b : int) : color =
  let c = RGB(r, g, b) in
  valid_rgb c ;;

# 165
(*......................................................................
Exercise 4: Write a function, convert_to_rgb, that accepts a color and
returns a 3-tuple of ints representing that color. This is trivial for
RGB colors, but not quite so easy for the hard-coded Simple colors.
We've already provided some RGB values for simple colors above, and
below are some other values you might find helpful.

 R  |  G  |  B  | Color
----|-----|-----|--------
255 | 165 |   0 | Orange
255 | 255 |   0 | Yellow
 75 |   0 | 130 | Indigo
240 | 130 | 240 | Violet
......................................................................*)

# 184
let convert_to_rgb (c : color) : int * int * int =
  match c with
  | RGB (r,g,b) -> (r,g,b)
  | Simple x ->
     match x with
     | Red ->     (255,   0,   0)
     | Crimson -> (164,  16,  52)
     | Orange ->  (255, 165,   0)
     | Yellow ->  (255, 255,   0)
     | Green ->   (  0, 255,   0)
     | Blue ->    (  0,   0, 255)
     | Indigo ->  ( 75,   0, 130)
     | Violet ->  (240, 130, 240) ;;

# 199
(* If we want to blend two colors, we might be tempted to average each
of the individual color channels. This might be fine, but a quirk in
the way colors are displayed means that the intensity of a screen's
pixels doesn't map linearly with a value. The blended value of two
channel values channel1 and channel2 is

   sqrt( (channel1^2 + channel2^2) / 2 )

(If you're interested in color and this non-linear phenomenon, look up
gamma correction (but not while in lab!). If you decide to test your
implementation against online tools, beware that many blend naively
by averaging without correcting for gamma.)

To be clear, the calculation above would have to be run three times to
blend two colors: once for each color channel of the colors you are
blending.

........................................................................
Exercise 5: Define a function, blend_channel, that takes two integers
and returns an integer whose result matches the calculation above. Be
sure to round your result when converting back to an integer.
......................................................................*)

# 226
let blend_channel (x : int) (y : int) : int =
  int_of_float ((sqrt (float_of_int (x*x + y*y) /. 2.0)) +. 0.5) ;;

# 230
(*......................................................................
Exercise 6: Now write a function, blend, that returns the result of
blending two colors. Do you need to do anything special to preserve
the invariant in this function after blending?
......................................................................*)

# 240
let blend (x : color) (y : color) : color =
  let (r1, g1, b1) = convert_to_rgb x in
  let (r2, g2, b2) = convert_to_rgb y in
  RGB (blend_channel r1 r2, blend_channel g1 g2, blend_channel b1 b2) ;;


# 247
(*======================================================================
Part 2: Records

Now let's move on to the last data type that will be used in the
biographical data type: the date field.

Above, we naively proposed a string for the date field. Does this make
sense for this field? Arguably not, since it will make comparison and
calculation very difficult.

Dates are frequently needed data in programming, and OCaml (like many
languages) supports it through a library module, named "Date".
Normally, we would reduce duplication of code by relying on that
module, but for the sake of practice you'll develop your own simple
version.

........................................................................
Exercise 7: Create a type, called "date", that supports values for
years, months, and days. First, consider what types of data each value
should be. Then, consider the implications of representing the overall
data type as a tuple or a record.
......................................................................*)

# 273
type date = { year : int; month : int; day : int; } ;;

# 276
(* After you've thought it through, look up the Date module in the
OCaml documentation to see how this was implemented there. If you
picked differently, why did you choose that way? Why might the Date
module have implemented this data type as it did?

........................................................................
Exercise 8: Change your data type, above, to implement it in a manner
identical to the Date module, but only with fields for year, month, and
day. If no changes are required...well, that was easy.
........................................................................

Like the color type, above, the date object has invariants. In fact,
the invariants for this type are more complex: we must ensure that
"days" fall within an allowable range depending on the month, and even
on the year.

The invariants are as follows:

- For our purposes, we'll only support positive years.

- January, March, May, July, August, October, and December have 31
  days.

- April, June, September, and November have 30 days.

- February has 28 days in common years, 29 days in leap years.

- Leap years are years that can be divided by 4, but not by 100,
  unless by 400.

You may find Wikipedia's leap year algorithm pseudocode useful:
https://en.wikipedia.org/wiki/Leap_year#Algorithm

........................................................................
Exercise 9: Create a valid_date function that raises Invalid_Date if
the invariant is violated, and returns the date if valid.
......................................................................*)

exception Invalid_Date of string ;;

# 320
let valid_date ({year; month; day} as d) : date =
  if year < 0 then raise (Invalid_Date "negative year")
  else
    let leap = (year mod 4 = 0 && year mod 100 <> 0)
	       || year mod 400 = 0 in
    let max_days = 
      match month with
      | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
      | 4 | 6 | 9 | 11 -> 30
      | 2 -> if leap then 29 else 28
      | _ -> raise (Invalid_Date "bad month") in
    if day > max_days then raise (Invalid_Date "day too large")
    else if day < 1 then raise (Invalid_Date "day too small")
    else d ;;


# 337
(*======================================================================
Part 3: Algebraic data types

Now, combine all of these different types to define a person record,
with a "name", a "favorite" color, and a "birthdate".

........................................................................
Exercise 10: Define a person record type. Use the field names "name",
"favorite", and "birthdate".
......................................................................*)

# 351
type person = { name : string; favorite : color; birthdate : date } ;;

# 354
(* Let's now do something with these person valuess. We'll create a
data structure that allows us to model simple familial relationships.

This family tree will be a data structure that shows the familial
status of persons. A person can be in one of three states for this
simple implementation:

1. An unmarried person with no children.
2. A married person.
3. A family made up of two married parents and some number of children.

(For simplicity, we postpone consideration of other familial structures.)

An easy mistake is to directly translate this to the following structure:

type family =
  | Single of person
  | Married of person * person
  | Family of person * person * family list ;;

But do we need to make the distinction between Married and a Family with
an empty list of children? Arguably, the latter corresponds to the former,
so we can remove that from the structure for these problems: *)

type family =
  | Single of person
  | Family of person * person * family list ;;

(* Let's now write a series of functions to build these family trees.

........................................................................
Exercise 11: Write a function that accepts a name, a color, and a date,
and returns a Single. If you completed the validity functions that
ensure the invariants are preserved for color and date, use them here
as well.
......................................................................*)

# 395
let new_child (n : string) (col : color) (birth : date) : family =
  let c = valid_rgb col in
  let d = valid_date birth in
  Single { name = n; favorite = c; birthdate = d } ;;

# 401
(*......................................................................
Exercise 12: Write a function that allows a person to marry in to a
family, by accepting a family and a person, and returning a new and
enlarged family. How should this behave in the event that the family
is already made up of a married couple?
......................................................................*)

exception Family_Trouble of string ;;

# 414
let marry (fam : family) (p : person) : family =
  match fam with
  | Single c -> Family (c, p, [])
  | _ -> raise (Family_Trouble ("Cannot add "
                                ^ p.name ^ " to a couple!")) ;;

# 421
(*......................................................................
Exercise 13: Write a function that accepts two families, and returns
an enlarged family with the second family added as a child of the
first. Note that this allows the addition of a single child to a
family, but also allows the general case. Consider the implicit
assumptions provided in the type definition of family to determine how
to behave in corner cases.
......................................................................*)

# 434
let add_to_family (fam : family) (c : family) : family =
  match fam with
  | Single _ -> raise (Family_Trouble "Singles can't have children")
  | Family (p1, p2, children) -> Family (p1, p2, c :: children) ;;

# 440
(*......................................................................
Exercise 14: Complete the function below that counts the number of
people in a given family. Be sure you count all spouses and children.
......................................................................*)

# 449
let rec count_people (fam: family) : int =
  match fam with
  | Single _ -> 1
  | Family (_, _, c) -> 2 + List.fold_left (+) 0
                                           (List.map count_people c) ;;

# 456
(*......................................................................
Exercise 15: Write a function find_parents of type

    find_parents : family -> string -> (person * person) option

which, given a family and a name, returns a tuple of persons
representing the parents of that person. You may assume that everyone
in the family has a unique name. Because not all people will have
parents in the family representation, the return value should be an
option type; None indicates no parents were found.

Hint: Use the "=" operator for string equality, and "<>" for
inequality. You may be accustomed to other operators, like, "==" and
"!=", but these will give unexpected results. (The documentation for
the Pervasives module can help explain why.)
......................................................................*)

# 477
(* a helper function to pick the first value in a pair of optionals *)
let pick (x : 'a option) (y : 'a option) : 'a option =
  match x, y with
  | None, None     -> None
  | None, Some _   -> y
  | Some _, None
  | Some _, Some _ -> x ;;

let find_parents (fam : family) (n : string)
    : (person * person) option =
  (* find' parents f -- returns the parents of n as per family f,
     where parents is taken to be the parents of f *)
  let rec find' (parents : (person * person) option) (f : family)  =
    match f with
    | Single {name; _} -> if n = name then parents else None
    | Family (p1, p2, c) ->
       if p1.name = n || p2.name = n then parents
       else
         (* starting with the children of the family *)
         c
         (* get parents of n according to each child of family f *)
         |> List.map (find' (Some (p1, p2)))
         (* and select out the non-None value from among them *)
         |> List.fold_left pick None
  in
  find' None fam ;;


# 506
(*======================================================================
Part 4: A different data structure

Perhaps, after wrestling with the find_parents function, above, you've
come to realize a disadvantage of the family tree data structure we
came up with in Part 3. A recursive tree-like structure like the one
we defined is great for hierarchical data, but by default it doesn't
make it easy to traverse back *up* the tree once we've traversed down.

One possibility to fix this is to acknowledge that the family tree is
more of a graph structure, which is just a more generalized tree.
Specifically, the graph could allow another edge that allows children
to refer to their parents.

One way to store this is to record all of the edges (relationships)
between nodes (people). First, we define various types of
relationships. *)

type relationship = SpouseOf | ParentOf ;;

(* Now we can define the graph in edge-clause form by storing a pair
of persons along with their relationship. *)

type graph = (person * relationship * person) list ;;

(* The direction that we define the edge for the parent relationship
matters; it is not commutative. However, the spouse relationship is
commutative. In other words, given two people A and B: (A, ParentOf,
B) does not imply (B, ParentOf, A). However, (C, SpouseOf, D) does
imply (D, SpouseOf, C). To keep the data structure consistent, when we
add spousal relationships, we should add edges in both directions.

........................................................................
Exercise 16: Re-write the marry function to build the graph in edge-
clause form. Keep in mind that you will need to add *two* graph edges.
The parameters you need to accept are, in order, the graph, a person
and another person.
......................................................................*)

# 549
let marry_graph (g: graph) (p1: person) (p2: person) : graph =
  (p1, SpouseOf, p2) :: (p2, SpouseOf, p1) :: g ;;

# 553
(*There are far fewer restrictions compared to our rigidly-defined
tree structure with variants. For instance, using the revised
structure, a person could (theoretically) be married to an arbitrary
number of people.

Before you herald a new age of progressive thought, keep in mind
that the freedom this structure provides doesn't necessarily mean
that this particular graph representation is preferred over tree
structures. It may work better in this case, however, due to the
complexity of modeling relationships.

........................................................................
Exercise 17: Write a function that accepts a graph and 3 persons
(representing a child and his or her two parents).  Return a new graph
that includes the relationship whereby the third person is a child of
the first two.
......................................................................*)

# 575
let add_child_to_graph (g : graph) (c : person)
                       (p1 : person) (p2 : person)
                     : graph =
  (p1, ParentOf, c) :: (p2, ParentOf, c) :: g ;;

# 581
(*......................................................................
Exercise 18: Now, rewrite find_parents using this new graph form. Note
that, since the structure no longer guarantees two or no parents, a
different return type is needed. You will need to accept a graph and
a string representing the name of the person whose parents you want
to find, returning a person list for the parents.
......................................................................*)

# 593
let find_parents_graph (g : graph) (n : string) : person list =
  let is_parent (_, r, {name; _}) : bool =
    match r with
    | ParentOf -> n = name
    | _ -> false in
  g
  |> List.filter is_parent
  |> List.map (fun (p, _, _) -> p) ;;
