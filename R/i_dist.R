#' Update rank order based on interaction data
#' 
#' This function updates the initial rank order based on the interaction data from the
#'      current study period. The order is altered to meet two criteria: first, 
#'      rank orders are rearranged to minimize the number of inconsistencies (i.e.,
#'      dyads for which observations from this year suggest the opposite dominance
#'      relationship). Second, the optimal order is selected as the order with
#'      minimal inconsistencies that is most similar to the starting order. 
#' 
#' @param mat A raw (i.e., not binarized) interaction matrix with winners as rows and losers as columns. 
#'            The orders of the individuals in the rows and columns 
#'            must be identical and are treated as the initial rank ordering.
#' @param n The number of reordering attempts to be undertaken
#' @param shuffles The number of shuffling steps taken per reordering attempt
#' @param future_intx A raw (i.e., not binarized) interaction matrix with 
#'        interaction outcomes from future study periods involving the individuals
#'        in mat. Used to resolve ties for most optimal ordering.
#'        
#' @param current.period The current period under consideration.
#' 
#' @return An interaction matrix reorderd to minimize inconsistencies with minimal changes
#' 
#' 

i_dist <- function(mat, n, shuffles, future_intx, current.period){
  ##Full matrix of interactions
  orig.full.mat <- mat
  ##Outcome matrix
  orig <- binarize_mat(mat)
  ##List of best orders, initialized to contain original order
  final_orders <- list(orig)
  #If no inconsistencies, return original order
  if(!identify_inconsistencies(orig)$i_count){return(list(orig))}
  
  for(iter in 1:n){
    #Initialize best order as the original order
    best <- orig
    #Initialize list of minimal inconsistency orders (MIOs)
    pick_next_mat <- list(best)
    
    for(shuff in 1:shuffles){
      #List of inconsistencies for best order
      best_stats <- identify_inconsistencies(best)
      
      #If there are no inconsistencies, the current order is the best order
      if(is.null(best_stats$is)){
        pick_next_mat <- unique(most_similar)
        break
      }
    
      
      #Otherwise, pick a random individual in an inconsistency to move
      #print(str(best_stats$is))
      i <- sample(best_stats$is, 1)
      
      
      #Check each potential destination for individual
      for(d in dimnames(mat)[[1]]){
        working <- moverowcol(best,i,d)
        working_stats <- identify_inconsistencies(working)
        #If the new destination is as good as the old location, save the new order
        #in the list of MIOs
        if(working_stats$i_count == best_stats$i_count){
          pick_next_mat[[length(pick_next_mat)+1]] <- working
          ##If new location is better than the current best, delete other MIOs and
          ##save the new order as the only MIO
        }else if(working_stats$i_count < best_stats$i_count){
          pick_next_mat <- list(working)
        }
      }
      ##After trying all destinations for the individual, select a new best order
      ##to work with from the list of MIOs based on similarity to starting order
      similarity_to_starting <- lapply(pick_next_mat, dyadic_similarity, orig)
      most_similar <- pick_next_mat[which(similarity_to_starting == similarity_to_starting[[which.max(similarity_to_starting)]])]
      best <- sample(unique(most_similar), 1)[[1]]
      
    }
    #After repeating this swapping procedure many times, add all MIOs to a final
    #list of best orders 
    final_orders[(length(final_orders)+1):(length(final_orders)+length(unique(pick_next_mat)))] <- unique(pick_next_mat)
  }
  #After doing all iterations, select the best order from the final orders list
  #using select_best_mats function
  return(select_best_mats(output = unique(final_orders), initial_matrix = orig.full.mat, future_intx = future_intx, current.period))
}