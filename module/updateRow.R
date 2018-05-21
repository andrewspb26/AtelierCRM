updateRow <- function(input, pool, table_name, id_field=id_field, to_update=to_update, delete=FALSE){
  
  if (!delete) {
    
    cols <- pool %>% tbl(table_name) %>% head %>% collect() %>%  lapply(type_sum) %>% unlist
    new_row <- data.frame(stringsAsFactors = FALSE, lapply(cols, type.convert))
    
    for (name in names(cols)){
      
      new_row[name] <- input[[name]]
      
    }
    
    new_row <- new_row[,to_update, drop = FALSE]
    
    sql <- paste0("UPDATE ?table SET ", 
                  paste0(names(new_row), " = ?", names(new_row), collapse = ", "), 
                  " WHERE ", id_field, " = ?idVal;")
    
    sql <- sqlInterpolate(pool, sql, 
                          .dots = c(list(table = table_name),
                                  rlang::as_list(new_row), 
                                  list(idVal= input[[id_field]])))
    
  } else {
    
    sql <- paste0("DELETE FROM ?table WHERE ", id_field, " = ?idVal;")
    print(c(list(table = table_name), list(idVal=input[[id_field]])))
    sql <- sqlInterpolate(pool, sql, .dots = c(list(table = table_name), list(idVal=input[[id_field]])))
    print(sql)
  }
  
  
  dbExecute(pool, sql)
  
}