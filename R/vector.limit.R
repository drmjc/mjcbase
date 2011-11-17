#
# take a vector of values, and ensure they are all below some
# upper limit -- useful for adjusting Pvalue, but making sure
# that they are all below 1.0.
# eg Bonferroni correction:
#    vector.upper.limit(pvals * length(pvals), 1.0)
#
# Mark Cowley, 20/9/07
#
vector.upper.limit <- function(vec, limit) {
    return( sapply(vec, function(x) min(x,limit)) )
}

#
# Take a numeric vector OR matrix-like object, and replace
# any values that are above some upper limit by the upper
# limit.
#
# Mark Cowley, 20/9/07
upper.limit <- function(x, limit) {
    x[!is.na(x) & (x > limit)] <- limit
    return( x )
}

#
# take a vector of values, and ensure they are all above some
# lower limit, eg 0.0
#
# eg a vector of permuted P-values could have some test with p-values
# of 0.000; these can't be transformed by -log10, so replace all of the
# zeros with some low number, eg 0.0001
#   vector.lower.limit(pvals, 0.0001)
#
# eg: limit a normal distribution of values that could be negative
# vector.lower.limit(rnorm(20, 1, 1), 0.0)
#
# Mark Cowley, 20/9/07
#
vector.lower.limit <- function(vec, limit) {
    return( sapply(vec, function(x) max(x,limit)) )
}

#
# Take a numeric vector OR matrix-like object, and replace
# any values that are below some lower limit by the lower
# limit.
#
# Mark Cowley, 20/9/07
lower.limit <- function(x, limit) {
    x[!is.na(x) & (x < limit)] <- limit
    return( x )
}
