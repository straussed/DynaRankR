#' @importFrom dplyr filter arrange
add_new_ids_mri_elo <- function(new.ids, current.scores, contestants, period, periods, ranks){
  new.scores <- data.frame(id = rep(NA, length(new.ids)), score = rep(NA, length(new.ids)))
  new.ids <- contestants[contestants$id %in% new.ids &
                           contestants$period == period,]
  if('convention2' %in% names(new.ids)){
    if(!is.numeric(new.ids$convention2))
      stop('convention2 not numeric')
    new.ids <- new.ids[order(new.ids$convention2, decreasing = TRUE),]
  }
  for(nid in new.ids$id){
    mom <- new.ids[new.ids$id == nid,'convention1']
    if(!mom %in% contestants$id){
      stop(paste0('can\'t add \'', nid, '\' because mom never appears in hierarchy'))
    }
    
    if(mom %in%  current.scores$id){
      mom.index <- which(current.scores$id == mom)
      new.scores[which(nid == new.ids$id),'id'] <- nid
      new.scores[which(nid == new.ids$id),'score'] <- current.scores$score[mom.index]-0.1
    }else{
      period.index <- which(periods == period)-1
      mom.index <- NULL
      mom.score <- NULL
      while(!length(mom.score) & period.index > 0){
        prev.period.rank <- filter(ranks, period == periods[period.index])
        mom.score <- prev.period.rank$score[prev.period.rank$id == mom]
        period.index <- period.index - 1
      }
      if(length(mom.score)){
        new.scores[which(nid == new.ids$id),'id'] <- nid
        new.scores[which(nid == new.ids$id),'score'] <- mom.score-0.1
      }else{
        stop(paste0('Could not place ', nid, '. Mom not in ranks'))
      }
    }
  }
  new.scores$score <- as.numeric(new.scores$score)
  return(new.scores)
}

add_new_ids_tenure_elo <- function(new.ids, current.scores, contestants, period){
  
  new.scores <- data.frame(id = rep(NA, length(new.ids)), score = rep(NA, length(new.ids)))
  new.ids <- contestants[contestants$id %in% new.ids &
                           contestants$period == period,]
  
  if(any(!sapply(new.ids[1,startsWith(names(new.ids), 'convention')], class) %in% c('Date', 'numeric'))){
    stop('Conventions must be dates or numeric')
  }
  
  conts.this.period <- contestants[contestants$period == period,]
  
  if('convention2' %in% names(conts.this.period)){
    conts.ordered <- arrange(conts.this.period, convention1, desc(convention2))
  }else{
    conts.ordered <- arrange(conts.this.period, convention1)
  }
  for(nid in new.ids$id){
    prob = 1 - (which(nid == conts.ordered$id)-1)/nrow(conts.ordered)
    new.scores[which(nid == new.ids$id),]<- c(nid, 
                                              quantile(current.scores$score, probs = prob))
  }
  new.scores$score <- as.numeric(new.scores$score)
  return(new.scores)
}

add_new_ids_phys_attr_elo <- function(new.ids, current.scores, contestants, period){
  
  new.scores <- data.frame(id = rep(NA, length(new.ids)), score = rep(NA, length(new.ids)))
  new.ids <- contestants[contestants$id %in% new.ids &
                           contestants$period == period,]
  
  if(any(!sapply(new.ids[1,startsWith(names(new.ids), 'convention')], class) %in% c('Date', 'numeric'))){
    stop('Conventions must be numeric')
  }
  
  conts.this.period <- contestants[contestants$period == period,]
  
  if('convention2' %in% names(conts.this.period)){
    conts.ordered <- arrange(conts.this.period, desc(convention1), desc(convention2))
  }else{
    conts.ordered <- arrange(conts.this.period, desc(convention1))
  }
  for(nid in new.ids$id){
    prob = 1 - (which(nid == conts.ordered$id)-1)/nrow(conts.ordered)
    new.scores[which(nid == new.ids$id),]<- c(nid, 
                                              quantile(current.scores$score, probs = prob))
  }
  new.scores$score <- as.numeric(new.scores$score)
  return(new.scores)
}
add_new_ids_age_elo <- add_new_ids_tenure_elo

add_new_ids_noconv_elo <- function(new.ids, current.scores){
  return(
    data.frame(id = new.ids, score = mean(current.scores$score))
  )
}
