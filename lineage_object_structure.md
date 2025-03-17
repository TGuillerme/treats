### Structure of the internal `lineage` list  (for developers)

lineage IDs here represent the element's number (tip or node). It get assigned by order of appearance starting with 0, the original root (leading to 1, the actual root of the tree).

```
lineage
    |
    \---$parents = an "integer" vector: the list of parent lineages;
    |
    \---$livings = n "integer" vector: the list of lineages still not extinct;
    |   
    \---$drawn = a single "integer": the ID of the selected lineage; this must be a number in 1:lineage$n;
    |
    \---$current = a single "integer": the selected lineage (is equal to lineage$livings[lineage$drawn]);
    |
    \---$n = a single "integer": the current number of non extinct lineage (is equal to length(lineage$livings));
    |
    \---$split = a "logical" vector: the list of splits for each lineage (TRUE), the number of total tips is equal to sum(!lineage$split).
```

You can internally visiualise a lineage object by loading the internal functions of the package using `devtools::load_all()` in the root of the package (i.e. `my_path_to_treats/treats/`) and then call the function `internal.plot.lineage(my_lineage)`.
