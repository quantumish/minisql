# minisql
> simple sql querying over json files

`minisql` is a small program that lets you query flat JSON files with relatively simple SELECT queries. It also supports a number of extended queries based off of linear temporal logic for interacting with temporal data: see [LTL.md](https://github.com/quantumish/minisql/blob/master/ltl.md) for a writeup on the inclusion of these features!

## install
Run `install.sh` which will automatically install SML locally and compile `minisql`.

## usage 
Run `./minisql` to run queries. No need for a command line argument: simply use the name of the JSON file you want to query as the name of the table in your query and `minisql` will parse it on the fly.

You can generate some sample data to play with by running `randgen.py`. See `ltl.md` for more info.

Example query (over the file `cities.json` in the current directory):
```
$ ./minisql
SELECT * FROM cities WHERE (pop > 1000000 AND pop_male > 2) AND state != 'California' LIMIT 5;
```
which results in:
```
state          | region    | name                  | pop       | pop_male | pop_female
South Dakota   | Midwest   | Titanium Difficulties | 889974703 | 458536   | 250643    
Wisconsin      | West      | Idaho Montgomery      | 428577562 | 764134   | 683030    
Georgia        | Southwest | Glasgow Temper        | 35972038  | 44695    | 652128    
South Carolina | South     | Handles Mineral       | 260061223 | 515245   | 944841    
Delaware       | North     | Click Epa             | 658863391 | 73868    | 451517    
```
