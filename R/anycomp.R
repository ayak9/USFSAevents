#' Scrapes Competition Event Data from USFSA website
#'
#' @param data html web link, passed to \code{\link{read_html}}
#'
#' @return data frame of usable Event data
#' @export
#'
#' @importFrom rvest read_html html_nodes html_text
#' @importFrom magrittr %>%
#' @importFrom stringr str_remove_all
#' @importFrom tidyr separate
#' @examples
#' Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
anycomp =  function(data){
  comp <- read_html(data)
  rank <- comp %>% html_nodes(xpath = '//tr/td[@class = "rank"]') %>% html_text %>% as.numeric
  name <- comp %>% html_nodes(xpath = '//tr/td[@class = "name"]') %>% html_text %>% str_remove_all("Falls:") %>% str_remove_all("Time violation:")
  name <- name[name != ""]
  TSS <- comp %>% html_nodes(xpath = '//tr/td[@class = "totSeg"]') %>% html_text %>% as.numeric
  TES <- comp %>% html_nodes(xpath = '//tr/td[@class = "totElm"]') %>% html_text %>% as.numeric
  bv <- comp %>% html_nodes(xpath = '//tr/td[@class = "tbvv"]') %>% html_text %>% as.numeric
  TCS <- comp %>% html_nodes(xpath = '//tr/td[@class = "totComp"]') %>% html_text %>% as.numeric
  Ded <- comp %>% html_nodes(xpath = '//tr/td[@class = "totDed"]') %>% html_text
  Dat <- data.frame(rank, name, TSS, TES, bv, TCS, Ded)
  Dat$GOE <- Dat$TES - Dat$bv
  Dat <- Dat %>% separate(name, c("Name", "Club"), sep=",")
  return(Dat)
}
