type columnref = string
type tableref = string
(* Object containing (key, value) pairs. *)
type row = JSON.value
(* List of values that gets displayed to user.  *)
type rowvals = JSON.value list

datatype value = String of string | Int of int | Column of columnref | 
                 CurColumn of columnref | Time of int | Sub of value * value
datatype binop = Eq of value * value | Gt of value * value 
datatype pred = Expr of binop | And of pred * pred | Or of pred * pred |
                Not of pred | Next of pred | Until of pred * pred | True 
datatype targets = Columns of columnref list | All
datatype query = Select of (targets * tableref * pred option * int option)

exception Parse
exception BadQuery
exception Type
exception MalformedData
exception NoSuchFile

(* Constructed predicate types - no need to do additional work in the engine. *)
fun Neq (x, y) = Not(Expr(Eq (x, y)))
fun Lt (x, y) = And(Not(Expr(Gt (x, y))), Not(Expr(Eq (x, y))))

fun Eventually x = Until(True, x)
fun Henceforth x = Not(Eventually(Not x))
fun WeakUntil (x, y) = Or(Until(x, y), Henceforth x)
fun Release (x, y) = Not(Until(Not x, Not y))
fun StrongRelease (x, y) = Not(WeakUntil(Not x, Not y))
fun Within (x, 0) = x
  | Within (x, i) = Or(Next(x), Next(Within(x, i-1)))


(*--------------------------- Parsing logic. -----------------------------*)

structure P = ParserComb

fun skipWS parser = P.skipBefore Char.isSpace parser
                                 
(* Sequentially combines two parsers. *)
fun *> (a, b) = P.seq (a, b)
infixr 3 *>
(* Combines two parsers, ignoring whitespace. *)
fun +> (a, b) = P.seq (a, skipWS(b))
infixr 3 +>
(* Only keeps the right hand result. *)
fun %> (a, b) = P.wrap(a +> b, #2)
infixr 3 %>
(* Only keeps the left hand result. *)
fun @> (a, b) = P.wrap(a +> b, #1)
infixr 3 @>

(* Parses "ref" values: e.g. name of column or table *)
fun parseRef getc = (P.token (fn x => Char.isAlphaNum x orelse x = #"_")) getc

(* Parses a list of one or more target columns (including wildcard) *)
fun parseCols getc = P.or(
        P.wrap(P.string "*", (fn _ => All)),
        P.wrap(parseRef +> P.zeroOrMore((P.char #",") %> parseRef), (Columns o op::))
    ) getc

fun middle (_, (x, _)) = x
fun isQuote x = (x = #"'" orelse x = #"\"") 

fun parseTime getc = P.or'([
        P.wrap((Int.scan StringCvt.DEC) @> P.string "seconds", Time),
        P.wrap((Int.scan StringCvt.DEC) @> P.string "minutes", fn x => Time(x*60)),
        P.wrap((Int.scan StringCvt.DEC) @> P.string "hours", fn x => Time(x*60*60)),
        P.wrap((Int.scan StringCvt.DEC) @> P.string "days", fn x => Time(x*60*60*24)),
        P.wrap((Int.scan StringCvt.DEC) @> P.string "weeks", fn x => Time(x*60*60*24*7))
    ]) getc

fun parseSimpleValue getc = P.or'([
        P.wrap((P.eatChar isQuote) *> P.token (fn x => not (isQuote x)) 
               *> (P.eatChar isQuote), (String o middle)),        
        parseTime,
        P.wrap(Int.scan StringCvt.DEC, Int),
        P.wrap(P.string "cur." *> parseRef, CurColumn o #2),
        P.wrap(parseRef, Column)        
    ]) getc
and parseValue getc = P.or(
        P.wrap(parseSimpleValue +> P.char #"-" %> parseSimpleValue, Sub),
        parseSimpleValue
    ) getc

fun parseExpr getc = P.or'([
        P.wrap(parseValue +> P.char #"=" %> parseValue, Expr o Eq),        
        P.wrap(parseValue +> P.char #">" %> parseValue, Expr o Gt),
        P.wrap(parseValue +> P.char #"<" %> parseValue, Lt),
        P.wrap(parseValue +> P.string "!=" %> parseValue, Neq)
    ]) getc

fun parseSubexpr getc = P.or'([
        parseExpr,
        parseUnary,
        parseNested,
        parseLiteral
    ]) getc
and parseLiteral getc = P.or(
        P.wrap(P.string("TRUE"), fn _ => True),
        P.wrap(P.string("FALSE"), fn _ => Not(True))
    ) getc
and parseUnary getc = P.or'([
        P.wrap(P.string "NOT" %> parseSubexpr, Not),
        P.wrap(P.string "NEXT" %> parseSubexpr, Next),
        P.wrap(P.string "EVENTUALLY" %> parseSubexpr, Eventually),
        P.wrap(P.string "HENCEFORTH" %> parseSubexpr, Henceforth)
    ]) getc
and parseBinary getc = P.or'([
        P.wrap(parseSubexpr +> P.string "AND" %> parseSubexpr, And),
        P.wrap(parseSubexpr +> P.string "OR" %> parseSubexpr, Or),
        P.wrap(parseSubexpr +> P.string "UNTIL" %> parseSubexpr, Until),
        P.wrap(parseSubexpr +> P.string "WEAK UNTIL" %> parseSubexpr, WeakUntil),
        P.wrap(parseSubexpr +> P.string "RELEASE" %> parseSubexpr, Release),
        P.wrap(parseSubexpr +> P.string "STRONG RELEASE" %> parseSubexpr, StrongRelease),
        P.wrap(parseSubexpr +> P.string "WITHIN" %> (Int.scan StringCvt.DEC), Within)
    ]) getc
and parseNested getc = (P.char #"(" %> parseBinary @> P.char #")") getc

fun parsePredicate getc = (P.or'([parseUnary, parseBinary, parseExpr, parseLiteral])) getc

(* Optionally applies a parser. On failure, parses to NONE instead of passing
 * an actual failure up and causing the overall parse to fail. *)
fun parseOptional parser getc = P.or (P.wrap (parser, SOME), P.result NONE) getc

fun parseSelect getc =
    P.wrap((P.string "SELECT ") %> parseCols +> (P.string "FROM ") %> parseRef
           *> parseOptional(skipWS(P.string "WHERE ") %> parsePredicate)
           *> parseOptional(skipWS(P.string "LIMIT ") %> (Int.scan StringCvt.DEC))
           @> P.char(#";"), (fn (cs, (t, (p, l))) => Select (cs, t, p, l))) getc

fun parse s = Option.valOf (StringCvt.scanString parseSelect s)
              handle Option => raise Parse

(*--------------------------- Execution engine. -----------------------------*)

fun getCol (row : row) (col : columnref) =
    Option.valOf (JSONUtil.findField row col)

fun jsonEqual (JSON.INT x) (JSON.INT y)  = x = y
  | jsonEqual (JSON.STRING x) (JSON.STRING y) = x = y
  | jsonEqual _ _ = raise Type

fun colInt row c = JSONUtil.asInt (getCol row c)
fun colStr row c = JSONUtil.asString (getCol row c)

fun equal row x y cur =
    if x = y then true else
    (case (x, y) of
         ((String _), (Int _)) => raise Type
       | ((Int x), (Column c)) => x = colInt row c
       | ((Time x), (Column c)) => x = colInt row c
       | ((String x), (Column c)) => x = colStr row c
       | ((Column c1), (Column c2)) => jsonEqual (getCol row c1) (getCol row c2)
       | ((CurColumn c1), (Column c2)) => jsonEqual (getCol cur c1) (getCol row c2)
       | ((Time x), (Time y)) => x = y (* sml doesn't recognize Time as an eq type? *)
       | ((CurColumn c1), _) => raise Type
       | ((Time _), _) => raise Type
       | (x,y) => equal row y x cur)
    handle Option => raise BadQuery

(* TODO could be cleaned *)
fun greater row x y cur =
    (case (x, y) of
         ((Int x), (Int y)) => x > y
       | ((Time x), (Time y)) => x > y
       | ((Int x), (Column c)) => x > (colInt row c)
       | ((Column c), (Int x)) => (colInt row c) > x
       | ((Time x), (Column c)) => x > (colInt row c)
       | ((Column c), (Time x)) => (colInt row c) > x
       | ((Column c1), (Column c2)) =>  (colInt row c1) > (colInt row c2)
       | ((CurColumn c1), (Column c2)) => (colInt cur c1) > (colInt row c2)
       | ((Column c1), (CurColumn c2)) => (colInt row c1) > (colInt cur c2)
       | _ => raise Type)
    handle Option => raise BadQuery
                        
fun eval (Sub(x, y)) row cur = 
    (case (x,y) of 
         (Column(c1), Column(c2)) => Time((colInt row c1) - (colInt row c2))
       | (CurColumn(c1), CurColumn(c2)) => Time((colInt cur c1) - (colInt cur c2))
       | (Column(c1), CurColumn(c2)) => Time((colInt row c1) - (colInt cur c2))
       | (CurColumn(c1), Column(c2)) => Time((colInt cur c1) - (colInt row c2))
       | _ => raise Type)
  | eval x _ _ = x

(* Given a predicate and a row, check if the row satisfies the predicate. *)
fun check p row (start, future) = let
    val st = Option.getOpt (start, row)
    val S = (start, future)
in
    (case (p, future) of         
         (Expr(Eq(x, y)), _) => equal row (eval x row st) (eval y row st) st
       | (Expr(Gt(x, y)), _) => greater row (eval x row st) (eval y row st) st
       | (And(p1, p2), ft) => check p1 row S andalso check p2 row S
       | (Or(p1, p2), ft) => check p1 row S orelse check p2 row S
       | (Not(p), f) => not (check p row S)
       | (Next(_), []) => false
       | (Next(p), (next::ft)) => check p next (SOME st, ft)
       | (Until(p1, p2), []) => check p2 row (SOME st, [])
       | (Until(p1, p2), (next::rest)) =>
         check p2 row (SOME st, next::rest) orelse
         ((check p1 row (SOME st, next::rest)) andalso
          (check (Until(p1, p2)) next (SOME st, rest)))
       | (True, _) => true)
end

fun subset A B = List.all (fn x => List.exists (fn y => x = y) B) A

(* Returns list of all fields of JSON object, which given this challenge's
 * assumptions should be the list of all columns in the table. *)
fun getCols ((JSON.OBJECT r) : row) : columnref list = List.map (fn (name, _) => name) r
  | getCols _ = raise MalformedData

fun filterWith f [] = []
  | filterWith f (x::xs) = if f (x,xs) then x::(filterWith f xs) else filterWith f xs
                      
(* Optionally filters rows for those matching given predicate (if it exists). *)
fun predFilter (SOME p) L = filterWith (fn (r,rs) => check p r (NONE, rs)) L
  | predFilter NONE L = L

(* Optionally filters for the first n elements of a list. *)
fun optTake (SOME n) L = if n >= List.length L then L else List.take (L, n)
  | optTake NONE L = L

fun execute (Select(req, table, pred, lim)) : rowvals list * columnref list  =
     let
        val fname = table ^ ".json"
        val (r::rs) = (case JSONParser.parseFile fname of JSON.ARRAY x => x)
        val cols = getCols r
        val targets = (case req of All => cols | Columns(c) => c)
        val () = if not (subset targets (getCols r)) then raise BadQuery else ()
    in
        (List.map (fn row => List.map (fn c => getCol row c) targets)
                  (optTake lim (predFilter pred (r::rs))),
         targets)
end handle Match => raise MalformedData
         | Io => raise NoSuchFile

(*--------------------------- Display utilities. -----------------------------*)

(* Coerce JSON values in list of row values to string types *)
fun stringifyRow (row : rowvals) =
    List.map (fn (JSON.STRING x) => x
               | (JSON.INT x) => IntInf.toString x) row

(* Returns a list of the max widths of each columns for padding. *)
fun getMaxSizes L =  let
    val rows = (map (fn x => map String.size x) L)
in
    List.foldr (fn (x,y) => map Int.max (ListPair.zip (x,y))) (hd rows) rows
end

(* Pad rows to align nicely when printed out. *)
fun padRows rows = let
    val maxs = getMaxSizes rows
    val rec pad = (fn 0 => "" | n =>" " ^ pad (n-1))
in
    List.map (fn x => map (fn (x,y) => x ^ (pad (y - (String.size x))))
                          (ListPair.zip (x,maxs))) rows
end

(* Delimits a list of strings by a delimiter. *)
fun delimit delim strings = List.foldr (fn (x, y) => x ^ delim ^ y) "" strings

(* Given a set of rows and columns, pretty prints them to stdout. *)
fun display ((rows, cols) : rowvals list * columnref list) =
    (print o (delimit "\n") o (List.map (fn row => delimit " | "  row)) o padRows)
        (cols::(map stringifyRow rows))

(* Main execution loop of program. *)
fun loop () : unit =
    let
        val input = Option.valOf (TextIO.inputLine TextIO.stdIn)
                    handle Option => raise Parse
    in
        loop ((display o execute o parse) input
              handle BadQuery => print "bad query\n"
                   | MalformedData => print "malformed data\n"
                   | Type => print "type error\n"
                   | Parse => print "parse error\n"
                   | NoSuchFile => print "no such file\n")
    end
              
structure Main =
struct
fun main (_, _) = let
    val () = loop ()
in
    OS.Process.success
end
end
