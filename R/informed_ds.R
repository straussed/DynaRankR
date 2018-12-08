#' David's Score
#'
#'
#' @import dplyr
#'
#' @export

informed_ds <- function(contestants, convention,
                        initial.ranks = NULL, interactions){
  periods <- unique(contestants$period)
  
  ##Error checking
  if(convention == 'none'){
    warning('No convention used. Ranks are calculated independently for each period and \'initial.ranks\' are ignored.')
  }
  
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
  
  ##Prep for first period
  current.Dij <- matrix(data = 0, nrow = length(initial.ranks), ncol = length(initial.ranks))
  current.Dij[upper.tri(current.Dij)] <- 1
  current.Dij <- ds_single(current.Dij)
  
  working.ranks <- initial.ranks
  if(convention == 'none'){
    working.ranks <- filter(contestants, period == periods[1])
  }
  
  for(current.period in periods){
    
    new.ids <- filter(contestants, period == current.period, 
                      !id %in% current.scores$id)$id
    
    ## Add new ids according to convention
    if(length(new.ids)){
      working.ranks <- switch(convention,
                              mri = add_new_ids_mri(new.ids, working.ranks, contestants, current.period, periods, ranks),
                              tenure = add_new_ids_tenure(new.ids, working.ranks, contestants, current.period),
                              age = add_new_ids_age(new.ids, working.ranks, contestants, current.period),
                              phys_attr = DynaRank:::add_new_ids_phys_attr(new.ids, working.ranks, contestants, current.period),
                              none = c(working.ranks, new.ids))
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
    
    ids.for.current.Dij <- working.ranks[which(!working.ranks %in% colnames(current.Dij))]
    
    if(length(ids.for.current.Dij)){
      
      mat.temp <- matrix(data = 0, nrow = length(working.ranks), ncol = length(working.ranks),
                         dimnames = list(working.ranks, working.ranks))
      mat.temp[rownames(current.Dij), colnames(current.Dij)] <- current.Dij
      ###New individuals get 0.75 for wins down hierarchy, 0.25 for wins up hierarchy
      mat.temp[lower.tri(mat.temp) & 
                 colnames(mat.temp) %in% ids.for.current.Dij] <- 0.25
      mat.temp[upper.tri(mat.temp) & colnames(mat.temp) %in% ids.for.current.Dij] <- 0.75
      ###New individuals get 0.25 for losses down hierarchy, 0.75 for losses up hierarchy
      mat.temp[t(lower.tri(mat.temp) & 
                 colnames(mat.temp) %in% ids.for.current.Dij)] <- 0.75
      mat.temp[t(upper.tri(mat.temp) & 
                   colnames(mat.temp) %in% ids.for.current.Dij)] <- 0.25
      
      current.Dij <- mat.temp
    }
    if(convention == 'none'){
      current.Dij <- ds_single(obs = intx.matrix)
    }else{
      current.Dij <- informed_ds_single(obs = intx.matrix,
                                        prior = current.Dij)
    }
    
    current.scores <- data.frame(id = working.ranks,
                             normDS = calc_ds(current.Dij))
    current.scores <- arrange(current.scores, desc(normDS))
    
    ## save to ranks object
    ranks[ranks$period == current.period,'old.order'] <- initial.ranks
    ranks[ranks$period == current.period,'id'] <- current.scores$id
    ranks[ranks$period == current.period,'rank'] <- 1:length(current.scores$id)
    ranks[ranks$period == current.period,'score'] <- current.scores$normDS
  }
  
  ranks <- ranks %>% 
    group_by(period) %>% 
    mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
    select(period, id, score, rank, stan.rank, old.order) %>% 
    as.data.frame()
  
  return(ranks)
}




informed_ds_single <- function(obs, prior){
  size <- obs + t(obs)
  Pij <- ifelse(size == 0, 0, obs/size)
  return(
    ifelse(size == 0,
           prior,
           Pij - ((Pij - prior)*(1/(size+1))))
  )
}

ds_single <- function(obs){
  size <- obs + t(obs)
  Pij <- ifelse(size == 0, 0, obs/size)
  return(
    ifelse(size == 0,
           0,
           Pij - ((Pij - 0.5)*(1/(size+1))))
  )
}

calc_ds <- function(Dij.mat){
  nids <- nrow(Dij.mat)
  w <- apply(X = Dij.mat, MARGIN = 1, FUN = sum)
  l <- apply(X = Dij.mat, MARGIN = 2, FUN = sum)
  w2 <- apply(X = Dij.mat * matrix(data = w, nrow = nids, ncol = nids, byrow = TRUE), MARGIN = 1, FUN = sum)
  l2 <- apply(X = Dij.mat * matrix(data = l, nrow = nids, ncol = nids, byrow = FALSE), MARGIN = 2, FUN = sum)
  
  return((w + w2 - l - l2 + nids*(nids-1)/2)/20)
}

