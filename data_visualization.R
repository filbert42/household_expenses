library(tidyverse)

draw_col_plot <- function(.data, category) {
    (.data
     %>% ggplot(aes(x = group, y = sum, fill = group))
         + geom_col()
         + geom_text(aes(label = sum), vjust = -0.5)
         + theme(legend.position = "none")
    )
}

plot_expenses <- function(transactions, category) {
    (transactions
     %>% summarise_by_group( {{ category }})
     %>% mutate(group = forcats::fct_reorder(group, sum))
     %>% draw_col_plot()
     )
}

plot_monthly_expenses <- function(transactions, year, month, category) {
    (transactions
     %>% filter_year_month(year, month)
     %>% plot_expenses({{ category }})
     %>% magrittr::add(coord_flip())
    )
}

plot_expenses_timeline <- function(transactions) {
    (transactions
        %>% summarise_by_group(factor(zoo::as.yearmon(transaction_date)))
        %>% draw_col_plot()
    )
}


