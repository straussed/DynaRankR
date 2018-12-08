#' Elo-rating method informed by prior information
#'
#' Use Elo-rating method to infer dominance hierarchy over multiple study periods.
#' New contestants are added according to the convention specified by the user. 
#' Full description of the addition of new individuals is describe
#' in Strauss & Holekamp (in revision). 
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
#' @param K Parameter that influences the magnitude of score changes after each outcome
#' 
#' @param lambda Parameter influencing the shape of the logistic function
#'               linking the difference in score between winner and loser 
#'               to the expected probability of each contestant winning.
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
#'          \item{score}{Elo score of contestant}
#'          \item{rank}{Ordinal rank of contestant in study period. Lower numbers
#'          equal higher rank.}
#'          \item{stan.rank}{Rank of contestant standardized for group size.
#'          Values range from 1 (highest rank) to -1 (lowest rank).}
#'          \item{old.order}{Identity of contestants arranged in the order they
#'          were in before updating the order based on observations from current
#'          study period.}}
#' 
#' @import dplyr
#' 
#' @references Strauss ED & Holekamp KE (in revision). Journal of Animal Ecology.
#'   
#'             Albers PCH & De Vries H (2000). Animal Behavior, 61, 489-495. 
#'
#'@export
#'
informed_elo <- function(contestants, convention, K = 200, lambda = 100, initial.ranks = NULL, interactions){
  
  periods <- unique(contestants$period)
  
  ##Error checking
  if(convention == 'mri'){
    missing.moms <- which(!contestants$convention %in% contestants$id)
    if(length(missing.moms)){
      stop('some moms not included in contestants. Missing moms: ', paste(missing.moms, collapse = ', '))
    }
  }
  if(!convention %in% c('mri','tenure','age','phys_attr','none'))
    stop('convention not recognized. Must be one of: \'mri\', \'tenure\', \'age\', \'phys_attr\', \'none\'')
  
  if(any(!c('period', 'id', 'convention1') %in% names(contestants)))
    stop('contestants dataframe missing \'period\', \'id\', or \'convention1\' column')
  
  if(any(!c('winner', 'loser', 'period') %in% names(interactions)))
    stop('interactions dataframe missing \'winner\', \'loser\', or \'period\' column')
  
  ##initialize ranks 
  ranks <- contestants
  ranks$id <- NA
  ranks$rank <- NA
  ranks$old.order <- NA
  ranks$score <- NA
  ranks <- select(ranks, period, id, score, rank, old.order)
  
  current.scores <- data.frame(id = initial.ranks, score = seq(from = K*(length(initial.ranks)-1), 
                                                                to = 0,
                                                                by = -K))
  
  for(current.period in periods){
    intx <- interactions %>%
      filter(period == current.period) %>%
      .[,c(1,2)]
    
    new.ids <- filter(contestants, period == current.period, 
                      !id %in% current.scores$id)$id
    
    if(length(new.ids)){
      current.scores <-rbind(current.scores, 
                             switch(convention,
                                    mri = add_new_ids_mri_elo(new.ids, current.scores, contestants, current.period, periods, ranks),
                                    tenure = add_new_ids_tenure_elo(new.ids, current.scores, contestants, current.period),
                                    age = add_new_ids_age_elo(new.ids, current.scores, contestants, current.period),
                                    phys_attr = add_new_ids_phys_attr_elo(new.ids, current.scores, contestants, current.period),
                                    none = add_new_ids_noconv_elo(new.ids, current.scores)))
      new.ids <- NULL
    }
    ## Remove dead or emigrated individuals
    dead <- which(!current.scores$id %in% filter(contestants, period == current.period)$id)
    if(length(dead)){current.scores <- current.scores[-dead,]}
      
      for(i in 1:nrow(intx)){
        initial.ranks <- current.scores$id
        winner <- intx[i,]$winner
        loser <- intx[i,]$loser
        E_winner = 1/(1 + exp((current.scores[current.scores$id == winner,'score'] - 
                                current.scores[current.scores$id == loser,'score'])/lambda))
        E_loser = 1/(1 + exp((current.scores[current.scores$id == loser,'score'] - 
                               current.scores[current.scores$id == winner,'score'])/lambda))
        
        current.scores[current.scores$id == winner,'score'] <- 
          current.scores[current.scores$id == winner,'score'] + K*(1-E_winner)
        
        current.scores[current.scores$id == loser,'score'] <- 
          current.scores[current.scores$id == loser,'score'] + K*(0-E_loser)
      }
    current.scores <- arrange(current.scores, desc(score))
    
    ## save to ranks object
    ranks[ranks$period == current.period,'old.order'] <- initial.ranks
    ranks[ranks$period == current.period,'id'] <- current.scores$id
    ranks[ranks$period == current.period,'rank'] <- 1:length(current.scores$id)
    ranks[ranks$period == current.period,'score'] <- current.scores$score
  }
  
  ranks <- ranks %>% 
    group_by(period) %>% 
    mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
    select(period, id, score, rank, stan.rank, old.order) %>% 
    as.data.frame()
  
  return(ranks)
}
