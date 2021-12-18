#' Boxplots for Quantitive Variables for one Event
#'
#' @param data data frame of event data
#'
#' @return boxplot of quantitative variables for a single event
#' @export
#'
#' @importFrom graphics boxplot
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' varvariationQuan(Comp1)
varvariationQuan <- function(data){
  plot <- boxplot(data$TSS, data$TES, data$bv, data$TCS, data$GOE, main="Variable Variation", names=c("TSS", "TES", "bv", "TCS", "GOE"), xlab="Score Type", ylab="Scores")
  return(plot)
}
