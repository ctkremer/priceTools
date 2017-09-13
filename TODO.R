
## To-do list


# Wishlist:

# update leap.zig documentation and vignette to reflect changes in functionality
# Test out new plotting scheme for leap.zig.both() function
# write complete documentation
# add 2-part partition (BEF style) option to get.dist.mat() function
# come up with a better/faster/more efficient way of performing Jaccard computations
# add examples to pairwise.price() function's documentation
# work on examples in documentation and vignette.

#     * Shift current plotting code into true object-oriented approach, defining classes for bef, cafe, & price data sets, and associated plotting and testing methods?
#     * try to reduce the length and redundancy of plotting functions
#     * Generalize code to allow for facetting of ggplots over multiple levels of a treatment variable, rather than the current approach of pasting together complete graphs
#     * Set up an option to draw individual vector paths for specific pairs of communities.
#     * Add nonparametric tests to `test.partitions()`
#     * Extend tools & existing code stubs to account for more complex experimental designs, including multiple treatments, interaction effects, etc.

# For test.partitions,
#  - significance tests of slope variables under 'cafe' tests is busted; for now the slope variables are simply excluded.
#
#  - Change syntax to SIE rather than SCE
#  - leap.zip(type='both') appears to be busted, "Error in leap.zig.both(tmp, loc.standardize = standardize, group.vars = group.vars,  : unused argument (group.vars = group.vars)"
#  - linetype specification in leap.zig busted when using add=T
#  - update Cedar Creek data object so that it doesn't contain plot type '9'

