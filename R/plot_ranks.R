#' Plot ranks of individuals in a single social group over multiple study periods
#' 
#' @param ranks A dataframe of ranks. There should be one row per contestant per
#' study period. There should be three columns:
#'    \describe{
#'      \item{period}{Study periods. They should appear in chronological order.}
#'      \item{id}{The identity of each contestant. Each contestant should appear
#'      once per study period}
#'      \item{rank}{The rank of each contestant in each study period. This can be
#'      absolute rank, standardized rank, or elo scores.}
#'    }
#'
#' @param ... Arguments to be passed to \code{\link{plot}}
#' 
#' @export

plot_ranks <- function(ranks, ...){
  ##Check for standardized ranks vs absolute rank vs elo scores
  if(any(ranks$rank > 1 & ranks$rank < 0)){
    ylimit <- c(-1,1)
    ylabel <- 'Standardize Rank'
  }else if(sum(ranks$rank == 1) == length(unique(ranks$period))){
    ylimit <- c(max(ranks$rank), 0)
    ylabel <- 'Rank'
  }else{
    ylimit <- c(min(ranks$rank), max(ranks$rank))
    ylabel <- 'Score'
  }
  
  if(is.factor(ranks$period)){
    ranks$period <- as.character(ranks$period)
    ranks$x <- as.numeric(factor(ranks$period, levels = unique(ranks$period)))
    plot(x = ranks$x, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel, ... = ..., xaxt = 'n')
    axis(1, at = ranks$x, labels = ranks$period)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'x'], y = ranks[ranks$id == id,'rank'])
    }
  }else if(is.character(ranks$period)){
    ranks$x <- as.numeric(factor(ranks$period, levels = unique(ranks$period)))
    plot(x = ranks$x, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel, ... = ..., xaxt = 'n')
    axis(1, at = ranks$x, labels = ranks$period)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'x'], y = ranks[ranks$id == id,'rank'])
    }
  }else{
    plot(x = ranks$period, y = ranks$rank, type = 'n', ylim = ylimit,
         xlab = 'Study period', ylab = ylabel, ... = ...)
    for(id in unique(ranks$id)){
      lines(x = ranks[ranks$id == id,'period'], y = ranks[ranks$id == id,'rank'])
    }
  }
}
