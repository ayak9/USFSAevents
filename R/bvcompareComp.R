#' Comparing Base Value Scores for two Events
#'
#' @param dat1 data frame of first event
#' @param dat2 data frame of second event
#'
#' @return boxplots for two competitions
#' @export
#'
#' @importFrom graphics boxplot
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' Comp2 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2019/27958/SEGM040.html')
#' bvcompareComp(Comp1, Comp2)
bvcompareComp = function(dat1, dat2){
  plot <- boxplot(dat1$bv, dat2$bv, main="Base Value Scores by Event", names=c("1", "2"), xlab="Event", ylab="Scores")
  return(plot)
}
