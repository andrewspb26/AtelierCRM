updateSelector <- function(table, field){
  
  choice <- c(global_pool %>% tbl(table) %>% select(field) %>% collect(), 
    recursive = TRUE, use.names=FALSE)
  choice
  
}