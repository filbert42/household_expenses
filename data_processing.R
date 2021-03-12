library(tidyverse)
library(lubridate)

load_files <- function(path, open_func) {
    files <- list.files(path, full.names = T)
    transactions <- map(files, open_func) %>% bind_rows()
    preprocess_transactions(transactions)
}

load_cal_file <- function(path) {
    meta <- str_remove(path, ".csv") %>% str_split("_") %>% pluck(1)
    year <-  pluck(meta, 2)
    month <- pluck(meta, 3)
    (readr::read_tsv(path, locale=locale(date_names = 'he', encoding = 'UTF-8'),
                    quote = "\"", trim_ws = TRUE, col_types = 'ccccc')
     %>% mutate(transaction_year = as.numeric(year),
                transaction_month = as.numeric(month))
    )
}

load_max_file <- function(path) {
    (readxl::read_xlsx(path, skip = 3)
     %>% mutate(`סכום החיוב` = `סכום חיוב`,
                `תאריך העסקה` = `תאריך עסקה`)
     %>% mutate(transaction_year = NA_real_,
                transaction_month = NA_real_)
    )
}

load_transactions <- function() {
    Sys.setlocale("LC_ALL","en_IL.UTF-8")
    cal_transactions <- load_files("rawdata/Cal", open_func=load_cal_file)
    max_transactions <- load_files("rawdata/MAX", load_max_file)
    (bind_rows(cal_transactions, max_transactions)
        %>% add_translations_and_categories()
    )
}

replace_hebrew_cols <- function(transactions) {
    (transactions
     %>% mutate(transaction_date = as.Date(`תאריך העסקה`, tryFormats=c("%d/%m/%y", "%d-%m-%y")),
                transaction_sum =  `סכום החיוב`,
                transaction_destination =  `שם בית העסק`)
     %>% select(starts_with('transaction'))
    )
}

preprocess_transactions <- function(transactions) {
    (transactions
         %>% replace_hebrew_cols()
         %>% filter(!is.na(transaction_date))
         %>% mutate(transaction_sum = as.numeric(stringr::str_remove_all(transaction_sum, '[₪ ,]')),
                    transaction_year = if_else(is.na(transaction_year), year(transaction_date), transaction_year),
                    transaction_month = if_else(is.na(transaction_month), month(transaction_date), transaction_month))
     )
 }

add_translations_and_categories <- function(transactions) {
    dictionary <- readr::read_csv('rawdata/dictionary.csv', col_types = 'cccc')
    (transactions
        %>% left_join(dictionary, by="transaction_destination")
        %>% select(transaction_year, transaction_month, transaction_date, transaction_sum,
                   transaction_destination_eng, destination_category, destination_subcategory)
        %>% rename(transaction_destination = transaction_destination_eng)
    )
}

summarise_by_group <- function(transactions, groupby) {
    (transactions
     %>% group_by(group = {{ groupby }}, .add = TRUE)
     %>% summarise(sum = sum(transaction_sum))
     )
}

filter_year_month <- function(.data, year, month) {
    .data %>% filter(transaction_month == month, transaction_year == year)
}
