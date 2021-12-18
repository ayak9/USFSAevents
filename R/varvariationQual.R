#' Bar graph for Qualitive Variable: Deductions in one Event
#'
#' @param data data frame of event data
#'
#' @return bar graph for the qualitative variable: Deductions for a single event
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr n
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' varvariationQual(Comp1)
varvariationQual <- function(data){
  Ded <- counts <- NULL
  df <- data %>% group_by(Ded) %>% summarise(counts=n())
  ggplot(df, aes(x=Ded, y=counts)) + geom_bar(fill="#0073C2FF", stat="identity") + geom_text(aes(label=counts), vjust=-0.3) + ggtitle('Deduction Variation') + xlab("Deductions") + ylab("Count")
}
