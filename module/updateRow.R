updateRow <- function(input, pool, table_name, to_update, id_field, delete=FALSE){
  
  cols <- pool %>% tbl(table_name) %>% head %>% collect() %>%  lapply(type_sum) %>% unlist
  new_row <- data.frame(stringsAsFactors = FALSE, lapply(cols, type.convert))
  
  for (name in names(cols)){
    new_row[name] <- input[[name]]
  }
  
  new_row <- new_row[,to_update, drop = FALSE]
  
  sql <- paste0("UPDATE ?table SET ", 
                paste0(names(new_row), " = ?", names(new_row), collapse = ", "), 
                " WHERE ", id_field, " = ?idVal;")
  
  sql <- sqlInterpolate(pool, sql, .dots = c(list(table = table_name),
  rlang::as_list(new_row), list(idVal =  as.integer(input[[id_field]]))))
  dbExecute(pool, sql)
  
}