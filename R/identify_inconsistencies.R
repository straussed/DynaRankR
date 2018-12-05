#' Identify inconsistencies in an interaction matrix 
#' 
#' This function identifies inconsistencies in an interaction matrix. Inconsistencies
#'      are cases where an individual lower in rank beats an individual higher in rank
#'      more often than vice versa. They are identifiable as the cells below the
#'      diagonal in a rank-ordered interaciton matrix with values greater than their
#'      corresponding cell above the diagonal. In a binarized matrix, dyads in an
#'      inconsistency will have a -1 in their cell above the diagonal and a 1 in 
#'      their cell below the diagonal.
#' 
#' @param mat An interaction matrix with winners as rows and losers as columns. 
#'            The order of the names individuals in the rows and columns 
#'            must be identical and is treated as a rank ordering.
#' 
#' @return A list of two elements: \describe{
#'         \item{is}{a vector of the unique indices of 
#'         individuals involved in an inconsistency}
#'         \item{i_count}{is the 
#'         number of inconsistencies in the matrix}
#' }
#' 
#' @export
#' 
identify_inconsistencies <- function(mat){
  index_is <- c()
  for(r in 2:length(mat[1,])){
    for(c in 1:(r-1)){
      if(mat[r,c] > mat[c,r]){
        index_is <- c(index_is, r, c)
      }
    }  
  }
  index_is <- dimnames(mat)[[1]][index_is]
  return(list(is = unique(index_is), i_count = length(index_is)))
}
