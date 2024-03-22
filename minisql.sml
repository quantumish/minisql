type columnref = string
type tableref = string
(* Object containing (key, value) pairs. *)
type row = JSON.value
(* List of values that gets displayed to user.  *)
type rowvals = JSON.value list

datatype value = String of string | Int of int | Column of columnref
datatype binop = Eq of (value * value) | Neq of (value * value) |
                 Gt of (value * value) | Lt of (value * value)
datatype pred = Expr of binop | And of pred * pred | Or of pred * pred |
                Not of pred | Next of pred | Until of pred * pred | True 
datatype targets = Columns of columnref list | All
datatype query = Select of (targets * tableref * pred option * int option)

exception Parse
exception BadQuery
exception Type
exception MalformedData
exception NoSuchFile

fun Eventually x = Until(True, x)
fun Henceforth x = Not(Eventually(Not x))
fun WeakUntil (x, y) = Or(Until(x, y), Henceforth x)
fun Release (x, y) = Not(Until(Not x, Not y))
fun StrongRelease (x, y) = Not(WeakUntil(Not x, Not y))

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
fun parseValue getc = P.or'([
        P.wrap((P.eatChar isQuote) *> P.token (fn x => not (isQuote x)) 
               *> (P.eatChar isQuote), (String o middle)),
        P.wrap(Int.scan StringCvt.DEC, Int),
        P.wrap(parseRef, Column)
    ]) getc

fun parseBinop getc = P.or'([
        P.wrap(parseValue +> P.char #"=" %> parseValue, Eq),
        P.wrap(parseValue +> P.char #">" %> parseValue, Gt),
        P.wrap(parseValue +> P.char #"<" %> parseValue, Lt),
        P.wrap(parseValue +> P.string "!=" %> parseValue, Neq)
    ]) getc

fun parseSubexpr getc = P.or'([
        P.wrap(parseBinop, Expr),
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
        P.wrap(parseSubexpr +> P.string "STRONG RELEASE" %> parseSubexpr, StrongRelease)
    ]) getc
and parseNested getc = (P.char #"(" %> parseBinary @> P.char #")") getc

fun parsePredicate getc = (P.or'([parseUnary, parseBinary, P.wrap(parseBinop, Expr), parseLiteral])) getc

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

fun equal row x y =
    if x = y then true else
    (case (x, y) of
         ((String _), (Int _)) => raise Type
       | ((Column c1), (Column c2)) => jsonEqual (getCol row c1) (getCol row c2)
       | ((String x), (Column c)) => x = JSONUtil.asString (getCol row c)
       | ((Int x), (Column c)) => x = JSONUtil.asInt (getCol row c)
       | (x,y) => equal row y x)
    handle Option => raise BadQuery

fun greater row x y =
    (case (x, y) of
         ((Int x), (Int y)) => x > y
       | ((Int x), (Column c)) => x > JSONUtil.asInt (getCol row c)
       | ((Column c), (Int x)) => JSONUtil.asInt (getCol row c) > x
       | ((Column c1), (Column c2)) => JSONUtil.asInt (getCol row c1) > JSONUtil.asInt (getCol row c2)
       | _ => raise Type)
    handle Option => raise BadQuery
                           
(* Given a predicate and a row, check if the row satisfies the predicate. *)
fun check (Expr(bop)) (row : row) (rest : row list) =
    (case bop of
         Eq(x, y) => equal row x y
       | Neq(x, y) => not (equal row x y)
       | Gt(x, y) => greater row x y
       | Lt(x, y) => not (greater row x y) andalso not (equal row x y))
  | check (And(p1, p2)) row rest = check p1 row rest andalso check p2 row rest
  | check (Or(p1, p2)) row rest = check p1 row rest orelse check p2 row rest
  | check (Not(p)) row rest = not (check p row rest)
  | check (Next(p)) row (next::rest) = check p next rest
  | check (Next(_)) row [] = false
  | check (Until(p1, p2)) row (next::rest) =
    check p2 row (next::rest) orelse 
    ((check p1 row (next::rest)) andalso (check (Until(p1, p2)) next rest))
  | check (Until(p1, p2)) row [] = check p2 row []
  | check True _ _ = true

fun subset A B = List.all (fn x => List.exists (fn y => x = y) B) A

(* Returns list of all fields of JSON object, which given this challenge's
 * assumptions should be the list of all columns in the table. *)
fun getCols ((JSON.OBJECT r) : row) : columnref list = List.map (fn (name, _) => name) r
  | getCols _ = raise MalformedData

fun filterWith f [] = []
  | filterWith f (x::xs) = if f (x,xs) then x::(filterWith f xs) else filterWith f xs
                      
(* Optionally filters rows for those matching given predicate (if it exists). *)
fun predFilter (SOME p) L = filterWith (fn (r,rs) => check p r rs) L
  | predFilter NONE L = L

(* Optionally filters for the first n elements of a list. *)
fun optTake (SOME n) L = List.take (L, n)
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

val q = Select(All, "cities", SOME (Until(Expr(Eq(Column "region", String "South")),
                                          Expr(Eq(Column "region", String "North")))), SOME 5)
              
structure Main =
struct
fun main (_, _) = let
    val () = loop ()
in
    OS.Process.success
end
end
