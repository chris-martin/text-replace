# text-replace

[![](https://travis-ci.org/chris-martin/text-replace.svg)](https://travis-ci.org/chris-martin/text-replace)

Perform simple replacements in a text file, using a list of search/replace pairs.

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

```
$ echo "The (<&&>) operator" | text-replace --delimiter " " --mapping "& &amp; > &gt; < &lt;"
The (&lt;&amp;&amp;&gt;) operator
```
