validateUpdatingFields <- function(con, table_name, input) {
  
  cols <- con %>% tbl(table_name) %>% head %>% collect() %>%  lapply(type_sum) %>% unlist
  cols <- names(cols)
  cols <- cols[cols %in% names(reactiveValuesToList(input))]
  print(cols)
  notEmptyFields <- c()
  for (name in cols) {
    if (input[[name]] != '') {
      notEmptyFields <- c(notEmptyFields, name)
    }
  }
  notEmptyFields
}
