#' Calculates Frequency & Percentage for the Qualitative variable for one Event
#'
#' @param dat data frame of summary statistics of the qualitative variable (deductions) for a single event
#'
#' @return data frame of summary statistics of deductions for a single event
#' @export
#'
#' @importFrom gtsummary tbl_summary
#' @importFrom dplyr select
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
#' compsumQual(Comp1)
compsumQual = function(dat){
  Ded <- NULL
  dat <- dat %>% select(Ded)
  tbl <- dat %>% tbl_summary()
  return(tbl)
}
