# ## Function to subset a list of matrix-like objects, either by rows or cols
# ##
# ## Eg:
# ##     s(expression.data, expressed.genes.ids, 1)
# ##
# ## Mark Cowley, 5 April 2006
# ##
# s <- function(x, entities, margin) {
# 
#     if( margin == 1 ) {
#         return( lapply(x, function(y) y[entities,]) )
#     }
#     else if( margin == 2 ) {
#         return( lapply(x, function(y) y[, entities]) )
#     }
#     else {
#         stop("unsupported margin")
#     }
# }
