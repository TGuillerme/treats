# `"treats"` `"traits"` structure object.

The `"traits"` object structure works as follows:

 * First it contains a list of two elements: `$main` and `$background` for the main trait and any number of background traits (which have a nested structure of also `$main` and `$background`, etc.)
 * The `$main` the contains the following (dotted bits are optional).

```
$main$<trait_name>
        |
        \__$process: a list of at least one function
        |   |
        |   \__[[1]]
        |       |
        |       \__a "function"
        |
        \__$start: a "numeric" vector of starting values of length <n> 
        |   |
        |   \__one or more "numeric"
        |
        \__$trait_id: an "integer" vector of starting values of length <n>
        :   |
        :   \__one or more "integer"
        :
        \..$link: optional, whether the function is linked or not
        :   :
        :   \..either NULL or a character string for the type of link
        :
        \..$...
```

With `<trait_name>` being any character string and `<n>` being the number of dimensions.

## Linked traits

Linked traits follow the same structure but with lists and options (e.g. with one conditional and two conditioned)

```
$main$<trait_name>
        |
        \__$process
        |   |
        |   \__$conditional
        |   |   |
        |   |   \__a "function" for the conditional process
        |   \__[[2]]
        |   |   |
        |   |   \__a "function" for the conditioned process 1
        |   \__[[3]]
        |       |
        |       \__a "function" for the conditioned process 2
        |
        \__$start
        |   |
        |   \__three numeric values
        |
        \__$trait_id
        |   |
        |   \__three integer values (sequential)
        |
        \__$link
        |   |
        |   \__as character "conditional"
        |
        \__$condition.test
            |
            \__[[1]]
            |   |
            |   \__a "function" for triggering process [[2]]
            \__[[2]]
                |
                \__a "function" for triggering process [[3]]
```