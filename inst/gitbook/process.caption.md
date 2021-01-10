Let's assume we already have a tree with three lineages after a period of `t0` has elapsed (with trait values `x` and `y` at the two nodes).

Step 1: Selecting a lineage among the living lineages. For example `A` is selected. This can be controlled through the `modifiers` object (with `selection`).

Step 2: Generating some waiting time applied to all the lineages that are alive. For example adding the time `t1` to the lineages `A`, `B` and `C`. This can be controlled through the `modifiers` object (with `branch.length`).

Step 3: Generating a trait value (or more) for the selected lineage. Say the value `a`. This value can be generated as a function of the elapsed time to since `A`'s ancestor (`t0` + `t1`) and the trait value of `A`'s ancestor (`x`). This is controlled through the `traits` object.

Step 4: Triggering a speciation or extinction event for the selected lineage. For example here, the lineage `A` speciates into two new ones: `D` and `E`. This can be controlled through the `modifiers` object (with `speciation`).

Step 5: Re-running step 1. E.g. selecting lineage `B` among the living lineages (`B`, `C`, `D`, `E`).

Step 6: Re-running step 2. E.g. increasing all living lineages branch lengths by `t2`.

Step 7: Re-running step 3. E.g. generating trait value `b` as a function of `t1+t2` and `B`'s ancestor trait value `y`.

Step 8: Re-running step 4. E.g. making the lineage `B` go extinct.

Step 9 and further: Repeating steps 1, 2, 3 and 4 again. Until a `stop.rule` condition is triggered (see step 11 - stop). Here let say the stopping rule is reaching a certain time limit. The selected lineage is `D` (step 1) and the branch length is increased by `t3` (step 3). This makes the tree reach the stop rule. Otherwise it would have continued to step 3 and 4 as previously.

Final step: when the stopping rule is reached (say reaching time `t0+t1+t2+t3`) then generate the missing trait values for all the lineages alive (`C`, `D` and `E`) as a function of their last ancestor's trait and the branch lengths leading to it.


Note that if no trait is simulated, steps 3 and 7 and skipped and the final step just stops the process (i.e. does not estimate traits for the tips).

Note also that the final step is applied as soon as one of the stop rule is reached (i.e. it does not necessary completes the cycle of steps 1 to 4).

Note finally that `events` object are triggered any time after steps 4, 8, etc... if the condition for the event is met.