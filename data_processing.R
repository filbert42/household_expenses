library(tidyverse)

load_cal_file <- function(path) {
    readr::read_tsv(path, locale=locale(date_names = 'he', encoding = 'UTF-8'),
                    quote = "\"", trim_ws = TRUE, col_types = 'ccccc')
}

load_files <- function(path, open_func) {
    files <- list.files(path, full.names = T)
    transactions <- map(files, open_func) %>% bind_rows()
    preprocess_transactions(transactions)
}

load_max_file <- function(path) {
    (readxl::read_xlsx(path, skip = 3)
     %>% mutate(`סכום החיוב` = `סכום חיוב`,
                `תאריך העסקה` = `תאריך עסקה`)
    )
}

load_transactions <- function() {
    cal_transactions <- load_files("rawdata/Cal", open_func=load_cal_file)
    max_transactions <- load_files("rawdata/MAX", load_max_file)
    bind_rows(cal_transactions, max_transactions)
}


 preprocess_transactions <- function(transactions) {
     (transactions
      %>% mutate(transaction_date = as.Date(`תאריך העסקה`, tryFormats=c("%d/%m/%y", "%d-%m-%y")),
                 transaction_sum =  `סכום החיוב`,
                 transaction_destination =  `שם בית העסק`)
      %>% filter(!is.na(transaction_date))
      %>% mutate(transaction_sum = as.numeric(stringr::str_remove(transaction_sum, '₪ ')))
      %>% select(starts_with('transaction'))
      )
 }

