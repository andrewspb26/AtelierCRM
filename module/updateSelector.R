updateSelector <- function(table, field, all=FALSE){
  if (!all) {
    choice <- c(global_pool %>% tbl(table) %>% select(field) %>% collect(), 
                recursive = TRUE, use.names=FALSE)
  } else {
    choice <- global_pool %>% tbl(table) %>% select(everything()) %>% collect()
  }
  
  choice
  
}