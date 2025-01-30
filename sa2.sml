(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Kareem Khalidi                     *)
(* Time spent on HW6: In Progress           *)

(* Collaborators and references:            *)
(* - chat.openai.com                        *)
(* - stackoverflow.com                      *)
(* - geeksforgeeks.org                      *)


(* indicate planning to use the Unit testing module *)
use "Unit.sml";


(**** Problem A ****)
fun mynull [] = true
  | mynull (_::_) = false

(* Unit Tests for Problem A *)
val () =
    Unit.checkExpectWith Bool.toString
    "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "mynull [1] should be false"
    (fn () => mynull [1])
    false


(**** Problem B ****)
fun firstVowel [] = false
  | firstVowel (#"a" :: _) = true
  | firstVowel (#"e" :: _) = true
  | firstVowel (#"i" :: _) = true
  | firstVowel (#"o" :: _) = true
  | firstVowel (#"u" :: _) = true
  | firstVowel _ = false

(* Unit Tests for Problem B *)
val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel [] should be false"
    (fn () => firstVowel [])
    false

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'elm' should be true"
    (fn () => firstVowel [#"e",#"l",#"m"])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'isle' should be true"
    (fn () => firstVowel [#"i",#"s",#"l",#"e"])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'opt' should be true"
    (fn () => firstVowel [#"o",#"p",#"t"])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'urge' should be true"
    (fn () => firstVowel [#"u",#"r",#"g",#"e"])
    true

val () =
    Unit.checkExpectWith Bool.toString
    "firstVowel 'bot' should be false"
    (fn () => firstVowel [#"b",#"o",#"t"])
    false


(**** Problem C ****)
fun reverse x = foldl (fn (y, z) => y :: z) [] x

(* Unit Tests for Problem C *)
val () =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "reverse [] should be []"
    (fn () => reverse [])
    []

val () =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "reverse [1] should be [1]"
    (fn () => reverse [1])
    [1]

val () =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "reverse [1, 2, 3] should be [3, 2, 1]"
    (fn () => reverse [1, 2, 3])
    [3, 2, 1]


(**** Problem D ****)
exception EmptyList

fun minlist[] = raise EmptyList
  | minlist (x::xs) = foldl Int.min x xs

(* Unit Tests for Problem D *)
val () =
    Unit.checkExnWith Int.toString
    "minlist [] should raise an exception"
    (fn () => minlist [])

val() =
    Unit.checkExpectWith Int.toString
    "minlist [1] should be 1"
    (fn () => minlist [1])
    1

val() =
    Unit.checkExpectWith Int.toString
    "minlist [1, 2, 3] should be 1"
    (fn () => minlist [1, 2, 3])
    1


(**** Problem E ****)
exception Mismatch

fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip _ = raise Mismatch

(* Unit Tests for Problem E *)
val() =
    Unit.checkExnWith (Unit.listString (Unit.pairString Int.toString Int.toString))
    "zip ([1], [2, 3]) should raise an exception"
    (fn () => zip ([1], [2, 3]))

val() =
    Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
    "zip ([], []) should be []"
    (fn () => zip ([], []))
    []

val() =
    Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
    "zip ([1], [2]) should be [(1, 2)]"
    (fn () => zip ([1], [2]))
    [(1, 2)]

val() =
    Unit.checkExpectWith (Unit.listString (Unit.pairString Int.toString Int.toString))
    "zip ([1, 2], [3, 4]) should be []"
    (fn () => zip ([1, 2], [3, 4]))
    [(1, 3), (2, 4)]


(**** Problem F ****)
fun concat [] = []
  | concat (x::xs) = foldr (fn (y, ys) => y :: ys) (concat xs) x

(* Unit Tests for Problem F*)
val() =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "concat [[], [], []] should be []"
    (fn () => concat [[], [], []])
    []

val() =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "concat [[1], [2], [3]] should be [1, 2, 3]"
    (fn () => concat [[1], [2], [3]])
    [1, 2, 3]

val() =
    Unit.checkExpectWith (Unit.listString Int.toString)
    "concat [[1], [], [2, 3]] should be [1, 2, 3]"
    (fn () => concat [[1], [], [2, 3]])
    [1, 2, 3]


(**** Problem G ****)
fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false

(* Unit Tests for Problem G *)
val() =
    Unit.checkExpectWith Bool.toString
    "isDigit #\"0\" should be true"
    (fn () => isDigit #"0")
    true

val() =
    Unit.checkExpectWith Bool.toString
    "isDigit #\"3\" should be true"
    (fn () => isDigit #"3")
    true

val() =
    Unit.checkExpectWith Bool.toString
    "isDigit #\"7\" should be true"
    (fn () => isDigit #"7")
    true

val() =
    Unit.checkExpectWith Bool.toString
    "isDigit #\"a\" should be false"
    (fn () => isDigit #"a")
    false

val() =
    Unit.checkExpectWith Bool.toString
    "isDigit #\"@\" should be false"
    (fn () => isDigit #"@")
    false


(**** Problem H ****)
fun isAlpha c =
    (Char.ord c >= 65 andalso Char.ord c <= 90) orelse
    (Char.ord c >= 97 andalso Char.ord c <= 122)

(* Unit Tests for Problem H *)
val() =
    Unit.checkExpectWith Bool.toString
    "isAlpha #\"a\" should be true"
    (fn () => isAlpha #"a")
    true

val() =
    Unit.checkExpectWith Bool.toString
    "isAlpha #\"A\" should be true"
    (fn () => isAlpha #"A")
    true

val() =
    Unit.checkExpectWith Bool.toString
    "isAlpha #\"7\" should be false"
    (fn () => isAlpha #"7")
    false

val() =
    Unit.checkExpectWith Bool.toString
    "isAlpha #\"@\" should be false"
    (fn () => isAlpha #"@")
    false


(**** Problem I ****)
fun svgCircle (cx, cy, r, fill) =
    "<circle cx=\"" ^ Int.toString cx ^
    "\" cy=\"" ^ Int.toString cy ^
    "\" r=\"" ^ Int.toString r ^
    "\" fill=\"" ^ fill ^ "\" />"

(* Unit Tests for Problem I *)
val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"


(**** Problem J ****)


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
