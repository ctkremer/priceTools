
## To-do list

# - export jaccard.column and price.part.column - can be used to avoid running all possible pairwise comparisons
# - consider better method for running only a subset of pairwise comparisons, possibly by passing a filter option to pairwise.price, that would filter one or both internal data frames.
# - expand documentation and examples for the comp.sel() function, in preparation for modeling paper code.


# Wishlist:

# - update leap.zig vignette to reflect changes in functionality
# - Test out new plotting scheme for leap.zig.both() function
# - add 2-part partition (BEF style) option to get.dist.mat() function
# - come up with a better/faster/more efficient way of performing Jaccard computations
# - Shift current plotting code into true object-oriented approach, defining classes for bef, cafe, & price data sets, and associated plotting and testing methods?
# - try to reduce the length and redundancy of plotting functions
# - Generalize code to allow for facetting of ggplots over multiple levels of a treatment variable, rather than the current approach of pasting together complete graphs
# - Set up an option to draw individual vector paths for specific pairs of communities.
# - Add nonparametric tests to `test.partitions()`
# - Extend tools & existing code stubs to account for more complex experimental designs, including multiple treatments, interaction effects, etc.

# For test.partitions,
#  - significance tests of slope variables under 'cafe' tests is busted; for now the slope variables are simply excluded.
#  - leap.zip(type='both') appears to be busted; it runs but the visual results are incorrect

