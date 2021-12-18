#' Histogram of Base Value Scores by Skater
#'
#' @param data data frame of chosen event data
#'
#' @return histogram of Base Value Scores for a single competition
#' @export
#'
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' bvcompareSkater(Comp1)
bvcompareSkater = function(data){
  Name <- bv <- NULL
  plot <- ggplot(data, aes(x=reorder(Name, rank), y=bv)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('Base Value Scores by Skater in Rank Order') + ylab('Base Value Scores') + xlab("Skater")
  return(plot)
}
