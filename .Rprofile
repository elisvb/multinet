.First <- function(){
  ### install and load packages (avoid package installation)
  list.of.packages <- c("data.table")
  new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  lapply(list.of.packages, function(x) suppressMessages(require(x, character.only = TRUE)))
  
  ### source R directory (avoid package installation)
  invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source, encoding = 'UTF-8'))
}
