#' Skaters who Participated in Both Events
#'
#' @param dat1 data frame of Event 1
#' @param dat2 data frame of Event 2
#'
#' @return data frame of skaters who participated in both chosen events
#' @export
#'
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' Comp2 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2019/27958/SEGM040.html')
#' bothcomp(Comp1,Comp2)
bothcomp <- function(dat1, dat2){
  dat <- merge(dat1, dat2, by=c('Name','Club'))
  names(dat)[3] <- "Rank1"
  names(dat)[4] <- "TSS1"
  names(dat)[5] <- "TES1"
  names(dat)[6] <- "bv1"
  names(dat)[7] <- "TCS1"
  names(dat)[8] <- "Ded1"
  names(dat)[9] <- "GOE1"
  names(dat)[10] <- "Rank2"
  names(dat)[11] <- "TSS2"
  names(dat)[12] <- "TES2"
  names(dat)[13] <- "bv2"
  names(dat)[14] <- "TCS2"
  names(dat)[15] <- "Ded2"
  names(dat)[16] <- "GOE2"
  data <- dat[, c(1,2,3,10,4,11,5,12,6,13,7,14,8,15,9,16)]
  return(data)
}
