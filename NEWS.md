
# priceTools 0.0.0.9000

* panel grid lines no longer removed by default within leap.zig; this can be accomplished however by a user outside of this command. For example,

```R
leap.zig(dat1,type='price',standardize=F,raw.points=F,legend=F)+
  theme_bw()+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank())
```

* feature 1
* feature 2
* etc


# Old notes

* graphing functions edited by KBM on 08-04-2016 to:
* move legend to bottom of plots for better stacking (leap.zig)
* change standardization=FALSE method (process.data)
* plot absolute EF on x-axis instead of raw change (process.data and leap.zig)
* remove gridlines from plots (leap.zig)

* 3/4/16 - added additional output columns tracking aggregations of price components, total function, and richness values

*	11/17/15 - turned off 'break' command for occurrence of non-overlapping communities. And added an error message/verbosity flag.

*	1/19/15 - The below change may be problematic if function values of 0 are meaningful.... although it keeps the math tidy.

*	changed missing species convention in pairwise.price. They should be represented by 0 abundance entries, rather than NA codes.
