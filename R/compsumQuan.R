#' Calculates Mean & Standard Deviation for Quantitative Variables for One Event
#'
#' @param dat data frame of event data
#'
#' @return data frame of summary statistics of quantitative variables for a single event
#' @export
#'
#' @importFrom modelsummary datasummary
#' @importFrom kableExtra kable_styling
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' compsumQuan(Comp1)
compsumQuan = function(dat){
  tbl <- datasummary(data=dat, (TSS+TES+bv+TCS+GOE)~(mean+sd)) %>% kableExtra::kable_styling(latex_options = "hold_position")
  return(tbl)
}
