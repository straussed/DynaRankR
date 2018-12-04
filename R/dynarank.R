#' Infer latent hierarchy from longitudinal data incorporating prior information
#' 
#' Implements the method described in Strauss & Holekamp (in revision). For each
#' study period, ranks are inferred as modifications of theranks from the previous
#' study period. First, new contestants are added according to the convention specified
#' by the user, and emigrated/dead contestants are removed. Then, matrix reordering is used
#' to change the position of contestants for whom data from the current study period
#' are inconsistent with this ordering. The optimal order is selected as the order 
#' that is most consistent with the data from the current period and is minimally 
#' changed from the previous study period.
#' 
#' @param contestants A dataframe with the identities of the contestants for 
#'                    each study period along with relevant data for 
#'                    adding them to the hierarchy. There should be one row per
#'                    contestant per study period.
#'                    Periods should appear in chronological order.
#'                    The dataframe should contain the following columns: 
#'                    \describe{
#'                      \item{period}{Study period.}
#'                      \item{id}{Identity of contestant.}
#'                      \item{convention1}{The primary convention by which new
#'                      individuals are added to the hierarchy. Interpretation
#'                      of this column varies depending on the value of the 
#'                      \strong{convention} argument.}
#'                      \item{convention2}{Optional. The secondary data for 
#'                      resolving ties in convention1. Interpretation
#'                      of this column varies depending on the value of the 
#'                      \strong{convention} argument.}
#' }
#' @param convention A flag determining how new individuals are added to the
#'                   hierarchy. The value of this flag influences how the convention1
#'                   and convention2 columns of the contestants argument are interpreted.
#'                   Currently this function supports four options:
#'                   \describe{
#'                    \item{mri}{New contestants are added to the hierarchy
#'                    according to maternal rank inheritance with youngest
#'                    ascendancy. \strong{convention1} should be a vector of 
#'                    mother identities for each contestant. \strong{convention2}
#'                    should be an optional vector of intra-litter ranks (lower 
#'                    numbers = higher rank) for resolving the order of 
#'                    contestants from the same mother
#'                    joining the hierarchy in the same study period.}
#'                    \item{tenure}{New contestants are added to the hierarchy
#'                    according their tenure in the group. \strong{convention1} should be a vector of 
#'                    dates that each contestant joined the group.}
#'                    \item{age}{New contestants are added to the hierarchy
#'                    according their age (older = higher rank).
#'                    \strong{convention1} should be a vector of birthdates or 
#'                    numerical age classes. \strong{convention2} should be an
#'                    optional vector of numerical data for resolving ties
#'                    in convention1 (e.g., body size). Higher values are 
#'                    considered higher rank.}
#'                    \item{phys_attr}{New contestants are added to the hierarchy
#'                    according some physical attribute (larger value = higher rank). 
#'                    \strong{convention1} should be a vector of numerical attribute
#'                    measurements. \strong{convention2} should be an
#'                    optional vector of numerical data for resolving ties
#'                    in convention1. Higher values are 
#'                    considered higher rank.}
#'                   }
#' @param n Number of separate reordering attempts per study period
#' 
#' @param shuffles Number of reshuffling steps per reordering attempt 
#' 
#' @param require.corroboration A logical indicating whether to require corroborating
#'        evidence from multiple study periods before changing an contestant's position
#'        in the order. Useful for reducing the sensitivity of the method to abarrent
#'        observations that don't reflect a lasting change in the true latent order.  
#' 
#' @param initial.ranks The initial ordering of individuals for the first study
#'        period.
#' 
#' @param interactions A dataframe of interaction data with the following columns:
#'         \describe{
#'          \item{winner}{Identities of winners}
#'          \item{loser}{Identities of losers}
#'          \item{period}{Study period in which interactions occurred}}
#' 
#' @return Produces a dataframe with columnes: 
#'          \describe{
#'          \item{period}{Study period}
#'          \item{id}{Identitity of contestant}
#'          \item{rank}{Ordinal rank of contestant in study period. Lower numbers
#'          equal higher rank.}
#'          \item{stan.rank}{Rank of contestant standardized for group size.
#'          Values range from 1 (highest rank) to -1 (lowest rank).}
#'          \item{old.order}{Identity of contestants arranged in the order they
#'          were in before updating the order based on observations from current
#'          study period.}}
#' 
#' @references Strauss & Holekamp (in revision). Journal of Animal Ecology.
#' @import dplyr
#' 
#' @examples female.ranks <- dynarank(contestants = C.crocuta.female$contestants, convention = 'mri',
#' n = 10, shuffles = 50, require.corroboration = TRUE, 
#' initial.ranks = C.crocuta.female$initial.ranks,
#' interactions = C.crocuta.female$interactions)

female.ranks %>%  
  select(period, id, rank) %>%
  plot_ranks()
#' 
#' @export
#'
#' 
dynarank <- function(contestants, convention, n, shuffles, require.corroboration = FALSE, 
                     initial.ranks, interactions){
  periods <- unique(contestants$period)
  
  ##Error checking
  if(convention == 'mri'){
    missing.moms <- which(!contestants$convention %in% contestants$id)
    if(length(missing.moms)){
      stop('some moms not included in contestants. Missing moms: ', paste(missing.moms, collapse = ', '))
    }
  }
  if(!convention %in% c('mri','tenure','age','phys_attr'))
    stop('convention not recognized. Must be one of: \'mri\', \'tenure\', \'age\', \'phys_attr\'')
  
  if(any(!c('period', 'id', 'convention1') %in% names(contestants)))
    stop('contestants dataframe missing \'period\', \'id\', or \'convention1\' column')
  
  if(any(!c('winner', 'loser', 'period') %in% names(interactions)))
    stop('interactions dataframe missing \'winner\', \'loser\', or \'period\' column')
  
  ##initialize ranks 
  ranks <- contestants
  ranks$id <- NA
  ranks$rank <- NA
  ranks$old.order <- NA
  ranks <- select(ranks, period, id, rank, old.order)
  
  ##First period
  cat(paste0('\nWorking on period ', periods[1],' (1 of ', length(periods), ' periods)'))
  working.ranks <- initial.ranks
  
  ## filter interactions to only those in this period and with these contestants
  intx.matrix <- interactions %>%
    filter(period == periods[1]) %>%
    .[,c(1,2)] %>%
    edgelist_to_matrix(identities = working.ranks)

  if(require.corroboration == TRUE){
    intx.matrix <- corroborate_inconsistencies(intx.matrix, period = periods[1],
                                               interactions  = interactions, 
                                               periods = periods)
  }
  
  ## filter interactions from future periods. save as future.intx.matrix
  future.intx.matrix <- interactions %>%
    filter(period %in% periods[2:length(periods)],
           winner %in% working.ranks,
           loser %in% working.ranks) %>%
    .[,c(1,2)] %>%
    edgelist_to_matrix(identities = working.ranks)
    
  working.ranks <- colnames(i_dist(intx.matrix, n, shuffles, future.intx.matrix)[[1]])
  
  ## save to ranks object
  ranks[ranks$period == periods[1],]$id <- working.ranks
  ranks[ranks$period == periods[1],]$rank <- 1:length(working.ranks)
  ranks[ranks$period == periods[1],]$old.order <- initial.ranks
  
  for(current.period in periods[-1]){
    cat(paste0('\nWorking on period ', current.period,' (', which(periods == current.period), ' of ', length(periods), ' periods)'))
    ## Identify new individuals
    new.ids <- filter(contestants, period == current.period, 
                      !id %in% working.ranks)$id
    
    ## Add new ids according to convention
    if(length(new.ids)){
      working.ranks <- switch(convention,
                              mri = add_new_ids_mri(new.ids, working.ranks, contestants, current.period, periods, ranks),
                              tenure = add_new_ids_tenure(new.ids, working.ranks, contestants, current.period),
                              age = add_new_ids_age(new.ids, working.ranks, contestants, current.period),
                              phys_attr = add_new_ids_phys_attr(new.ids, working.ranks, contestants, current.period))
      new.ids <- NULL
    }
    
    ## Remove dead or emigrated individuals
    dead <- which(!working.ranks %in% filter(contestants, period == current.period)$id)
    if(length(dead)){working.ranks <- working.ranks[-dead]}
    
    initial.ranks <- working.ranks
    
    ## filter interactions to only those in this period and with these contestants
    intx.matrix <- interactions %>%
      filter(period %in% current.period,
             winner %in% working.ranks,
             loser %in% working.ranks) %>%
      .[,c(1,2)] %>%
      edgelist_to_matrix(identities = working.ranks)
    
    if(require.corroboration == TRUE){
      intx.matrix <- corroborate_inconsistencies(intx.matrix, period = current.period,
                                                 interactions  = interactions, 
                                                 periods = periods)
    }
    
    ## filter interactions from future periods. save as future.intx.matrix
    if(current.period != length(periods)){
      future.intx.matrix <- interactions %>%
        filter(period %in% periods[(which(periods == current.period)+1):length(periods)],
               winner %in% working.ranks,
               loser %in% working.ranks) %>%
        .[,c(1,2)] %>%
        edgelist_to_matrix(identities = working.ranks)
    }else{
      future.intx.matrix <- matrix(data = 0, dimnames = list(working.ranks, working.ranks),
                                   ncol = length(working.ranks), nrow = length(working.ranks))
    }
    
    working.ranks <- colnames(i_dist(intx.matrix, n, shuffles, future.intx.matrix)[[1]])
    
    ## save to ranks object
    ranks[ranks$period == current.period,]$id <- working.ranks
    ranks[ranks$period == current.period,]$rank <- 1:length(working.ranks)
    ranks[ranks$period == current.period,]$old.order <- initial.ranks
  }
  
  ranks <- ranks %>% 
    group_by(period) %>% 
    mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
    select(period, id, rank, stan.rank, old.order) %>% 
    as.data.frame()
  
  return(ranks)
}
