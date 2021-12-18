#' Histogram of GOE Scores by Skater
#'
#' @param data data frame of chosen event data
#'
#' @return histogram of GOE Scores for a single competition
#' @export
#'
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
#' goecompare(Comp1)
goecompare <- function(data){
  Name <- GOE <- NULL
  plot <- ggplot(data, aes(x=Name, y=GOE)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, hjust=0)) + ggtitle('GOE Scores by Skater in Rank Order') + ylab('GOE Scores') + xlab("Skater")
  return(plot)
}
