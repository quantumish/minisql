# Linear Temporal Logic

## What is LTL? 
> [!NOTE]
> Github's inline $\LaTeX$ is somewhat limited in what it allows you to write, so some of this notation is really just abusing similar-looking symbols.

Linear temporal logic is an extension of propositional logic that allows for reasoning over arbitrary time streams. The two core operators it provides are the _next_ operator $\circ \varphi$ that is true iff $\varphi$ is true at the next state and the _until_ operator $\varphi \cup \psi$ that is true iff $\psi$ is true at some future or current state and $\varphi$ is true at all states preceding it. 

The two main operators then allow you to build up more complex operators:
- The _eventually_ operator $\diamondsuit$ which is defined as $\diamondsuit \varphi = true \cup \varphi$ 
- The _henceforth_ operator $\square$ defined as $\lnot \diamondsuit \lnot \varphi$
- The _weak until_ operator $\varphi \text{ W } \psi$ defined as $(\varphi \cup \psi) \lor \square \varphi$ which relaxes the constraint of the until operator that the second condition must be satisfied at some point.
- The _release_ operator $\varphi \text{ R } \psi$ is defined as $\lnot(\lnot\varphi \cup \lnot \psi)$ which is true iff $\psi$ always holds until "released" by $\varphi$ being true.
(the names for these operators can vary - what I've written here is the verbiage `minisql` uses) 

LTL is used primarily for formal verification of systems (which you could imagine in many cases would need to encode time-dependent constraints) and also is partially what the TLA language is built on top of! Probably the most interesting application I saw for it was encoding invariants about parallelism in [Michigan State's slides](https://www.cse.msu.edu/~cse814/Lectures/14_introLTL.pdf) on the topic. Say for example you have a critical section of code guarded by a mutex. Let $\text{inCS}_X$ denote a process $X$ being in the critical section: 
- Mutual exclusion is expressed by: $\square(\lnot \text{inCS}_A \lor \lnot\text{inCS}_B)$ (in English: it is always true that either A is or B is not in the critical section)
- You can also express that $A$ doesn't monopolize the lock: $\square(\text{inCS}_A \implies \diamondsuit \lnot\text{inCS}_A)$ (in English: it is always true that $A$ holding the lock implies it will eventually not hold the lock).

Those slides also include some cool formalizations of fairness guarantees for the dining philosophers problem!

## Adding Support (this can be skipped)
Adding support for the core of LTL is actually not that hard: `minisql` does predicate filtering via the higher-order function `filter`, which normally looks something like this in SML:
```sml
fun filter f [] = []
  | filter f (x::xs) = if f x then x::(filter f xs) else (filter f xs)
```
Since SML has singly-linked lists ("cons cells") as the first class list construct, it becomes very easy to pass in the rest of the list to our predicate function `f`: 
```sml
fun filterWith f [] = []
  | filterWith f (x::xs) = if f(x, xs) then x::(filter f xs) else (filter f xs)
```

The parser is also implemented in a fashion where adding more unary and binary constructs isn't a big ask. The execution engine now has to track where it "started" (the row that is actually being checked against the predicate), but otherwise these future-oriented predicates can be computed pretty easily by recursing on the rest of the list.

Once we implement support for next and until, we can just have the parser automatically translate the more complex queries to be in terms of the two basic primitives via "artificial" constructors like: 
```sml
fun Eventually x = Until(True, x)
fun Henceforth x = Not(Eventually(Not x))
fun WeakUntil (x, y) = Or(Until(x, y), Henceforth x)
fun Release (x, y) = Not(Until(Not x, Not y))
fun StrongRelease (x, y) = Not(WeakUntil(Not x, Not y))
```

## Queries
Now we can mess with some queries! Note that all of this data can be generated via `randgen.py`: calling `python randgen.py cities` will generate the cities dataset, `python randgen.py sequsers` the sequential user dataset, etc.

### Simple Queries
Let's start with a simple dataset: say we have a simple business with one product that can meet with up to one client businesses a day. These clients have a limited number of actions as described by the following FSM:

<p align="center">
  <img src="./fsm.png">
</p>

"New" is short for a client coming in contact with our business, "buy" is them buying our product, "com" is them complaining about it, "ret" is them returning it, and "leave" is them vowing to never work with us again.

`sequsers.json` is a table where each row represents what happened in a day and the rows are laid out chronologically (e.g. first day is the first row).

Let's try the simplest possible LTL query: maybe we're interested in predicting what events could indicate that a client is going to buy our product. To do that we'd want all events that _precede_ a buy order:
```
$ ./minisql
SELECT * FROM sequsers WHERE NEXT action = 'buy' LIMIT 10;
action | name               | headcount | 
new    | Sanctologist Corp. | 15221     | 
buy    | Sanctologist Corp. | 43864     | 
new    | Unskilful Corp.    | 56921     | 
new    | Wiredancing Corp.  | 68559     | 
new    | Dasylirion Corp.   | 78640     | 
new    | Unoxidated Corp.   | 17631     | 
buy    | Overtures Corp.    | 59852     | 
buy    | Clamative Corp.    | 75337     | 
new    | Macrospore Corp.   | 86109     | 
buy    | Attendant Corp.    | 18739     | 
```
Is this correct? Let's peek at the first 15 rows of the actual data using `viewer.py`:
```python
$ python viewer.py sequsers.json 15
{'action': 'new', 'name': 'Irrevocablaginable Corp.', 'headcount': 65498}
{'action': 'new', 'name': 'Psychopathologist Corp.', 'headcount': 43556}
{'action': 'new', 'name': 'Ararao Corp.', 'headcount': 39872}
{'action': 'new', 'name': 'Avant Corp.', 'headcount': 10441}
{'action': 'new', 'name': 'Obverts Corp.', 'headcount': 21201}
{'action': 'new', 'name': 'Unstaggering Corp.', 'headcount': 37987}
{'action': 'new', 'name': 'Overnobly Corp.', 'headcount': 12356}
{'action': 'new', 'name': 'Souterrain Corp.', 'headcount': 29849}
{'action': 'new', 'name': 'Upclimbed Corp.', 'headcount': 59171}
{'action': 'new', 'name': 'Ungibbet Corp.', 'headcount': 60191}
{'action': 'new', 'name': 'Sanctologist Corp.', 'headcount': 15221}
{'action': 'buy', 'name': 'Sanctologist Corp.', 'headcount': 43864}
{'action': 'buy', 'name': 'Ungibbet Corp.', 'headcount': 91063}
{'action': 'new', 'name': 'Dampnesses Corp.', 'headcount': 50746}
{'action': 'new', 'name': 'Protopectinase Corp.', 'headcount': 55577}
```
The first two buy actions line up with our output!

Let's try some more sample queries: 
- Get all returns that are followed by a buy order.
```sql
SELECT name FROM sequsers WHERE action = 'return' AND EVENTUALLY action = 'buy';
```
- Get all chains of new users followed by a buy order:
```sql
SELECT name, headcount FROM sequsers WHERE action = 'new' UNTIL action = 'buy';
```
- Get all events preceding large clients leaving us:
```sql
SELECT * FROM sequsers WHERE NEXT (action = 'leave' AND headcount > 10000);
```

Okay, let's try actually doing the query mentioned in the challenge ("get all customers who returned product within 2 weeks"). The best we can do with plain old linear temporal logic is something like:
```sql
SELECT * FROM sequsers WHERE action = 'buy' AND (action = 'leave' WITHIN 14);
```
Here the `WITHIN` operator is syntactic sugar over nested `Or` and `Next` clauses:
```sml
fun Within (x, 0) = x
  | Within (x, i) = Or(Next(x), Next(Within(x, i-1)))
```

Yet this isn't really what we want: all this results in is all events that are within 14 events of some client returning our product. We just want the event where said client bought it (if it is within 14 events!). Unfortunately vanilla LTL doesn't support this as it is very much future-oriented and applies predicates independent of time to several states across a time stream.

### Temporally Dependent Queries
Some variants of LTL solve the problem we had earlier by introducing operators that let you refer to _past_ values. This is a bit overkill for our concerns and it also would make the code a bit less pleasant. Instead, `minisql` introduces the notion of the "current" row for use in predicates.

For example, we can query all users who returned our product with:
```sql
SELECT * FROM sequsers WHERE action = 'buy' AND EVENTUALLY (name = cur.name AND action = 'return');
```
While `name` and `action` columns will refer to the columns of the state currently being checked at any point in the timestream, `cur.name` refers to the column of the current state.

We can also write the earlier query now: 
```sql
SELECT * FROM sequsers WHERE action = 'buy' AND ((name = cur.name AND action = 'return') WITHIN 14);
```

In general this expands the number of actually meaningful predicates we can make. Say we're interested in all the complaints that come from companies who have shrunk their headcounts since they bought our product (maybe they're just haggling to cut expenses...) 
```sql
SELECT name FROM sequsers WHERE action = 'buy' AND EVENTUALLY (name = cur.name AND (action = 'return' AND headcount < cur.headcount));
```

### The Query You Asked For
Okay, but there's still a big constraint on our data here: only one event can happen per day (or alternatively our only notion of time is the event number). Let's now consider `users.json` which is exactly the same as `sequsers.json` except for the fact that there is now a `time` column with the time of the event as a Unix timestamp. Combining the very limited time operations provided by `minisql` with the temporally dependent queries lets us finally do this the _right_ way.

To get all clients who bought and returned their product within two weeks, write 
```sql
SELECT name, headcount FROM users WHERE action = 'buy' AND EVENTUALLY (name = cur.name AND (action = 'return' AND time - cur.time < 2 weeks));
```
On my randomly generated copy, this returns 
```
name               | headcount | 
Mathurin Corp.     | 75192     | 
Ferrelling Corp.   | 76831     | 
Preoperating Corp. | 20880     | 
```

Sure enough, `grep`ping for "Mathurin" yields they did buy and return within about a week:
```python
$ python viewer.py users.json 1000 | grep Mathurin
{'time': 1725139942, 'action': 'new', 'name': 'Mathurin Corp.', 'headcount': 96156}
{'time': 1755384672, 'action': 'buy', 'name': 'Mathurin Corp.', 'headcount': 75192}
{'time': 1755982565, 'action': 'return', 'name': 'Mathurin Corp.', 'headcount': 5771}
{'time': 1789821027, 'action': 'leave', 'name': 'Mathurin Corp.', 'headcount': 20720}
```

This is not hardcoded: the parser will recognize any of "seconds", "minutes", "hours", "days", or "weeks" and the query engine prevents you from mixing up integers and timestamps, but beyond that the time system is not fully realized.
