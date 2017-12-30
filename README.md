# text-replace

Perform simple replacements in a text file, using a list of search/replace pairs.

The search for strings to replace is performed left-to-right, preferring longer matches to shorter ones.

All streams are assumed to be UTF-8 encoded.

## Command-line options

| Option | Description |
| --- | --- |
| `-h,--help` | Show help text |
| `-i,--in-file FILEPATH` | Input file to read (optional, defaults to stdin) |
| `-o,--out-file FILEPATH` | Output file to write (optional, defaults to stdout)
| `-m,--mapping MAPPING` | A list of search/replace pairs, separated by any of the delimiters |
| `-f,--map-file FILEPATH` | A file containing a list of search/replace pairs, separated by any of the delimiters |
| `-d,--delimiter DELIMITER` | Add a delimiter that separates search/replace strings in `--mapping` and in the contents of `--map-file` |
| `-n,--newline-delimiter` | Add newline as a delimiter |

## Examples

`text-replace` is useful for replacing characters with escape sequences:

```
$ echo "The (<&&>) operator"        \
    | text-replace --delimiter " "  \
                   --mapping "& &amp; > &gt; < &lt;"
The (&lt;&amp;&amp;&gt;) operator
```

You can use it to swap strings. In the following example we replace `*` with `**` and vice versa:

```
$ echo "What *is* going on **here**?"  \
    | text-replace --delimiter " "     \
                   --mapping "* ** ** *"
What **is** going on *here*?
```

You also have the option to read the input string and replacement list from files, and to write the output to a file:

```
$ cat input
I am extremely apt to like Haskell once I develop sufficient
aptitude with it.

$ cat replacements
apt -> likely
aptitude -> ability
like -> appreciate

$ text-replace --map-file replacements  \
               --in-file input          \
               --out-file output        \
               --delimiter " -> "       \
               --newline-delimiter

$ cat output
I am extremely likely to appreciate Haskell once I develop
sufficient ability with it.
