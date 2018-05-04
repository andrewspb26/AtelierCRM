insertRow <- function(con, table_name, input){
  
  cols <- con %>% tbl(table_name) %>% head %>% collect() %>%  lapply(type_sum) %>% unlist
  new_row <- data.frame(stringsAsFactors = FALSE, lapply(cols, type.convert))
  for (name in names(cols)){
    new_row[name] <- input[[name]]
  }
  print(new_row)
  dbWriteTable(con, table_name, new_row, append=TRUE)
}
