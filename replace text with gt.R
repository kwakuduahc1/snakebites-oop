bites %>% 
  select(id, cons:svs) %>% 
  sjlabelled::drop_labels() %>% 
  pivot_longer(
    c(cons:svs), 
    names_to = "item", 
    values_to = "cost"
    ) %>%
  mutate(
    item = fct_recode(item, 
                      "Consultation" = "cons", 
                      "Drugs" = "drugs", 
                      "Services" = 
                        "svs")
    ) %>% 
  select(-id) %>% 
  tbl_continuous(
    variable = cost,
    digits = everything() ~ 2,
    statistic = everything() ~ "{sum}, {median} ({p25}, {p75}, {p})",
    label = list(item ~ "Item")
    ) %>% 
  modify_footnote(stat_0 = "cost: Sum, Median (IQR)")


bites %>% 
  select(year) %>% 
  tbl_summary(statistic = everything() ~ "{p}%")
my("p")
str(ob$table_styling$footnote)  


library(carData)

tot <- Salaries$salary %>% sum()
Salaries %>% 
  select(rank, sex, salary) %>% 
  tbl_continuous(variable = salary, statistic = everything() ~ ("{percent}"))


# Raw gt ------------------------------------------------------------------
pay.tbl %>% 
  arrange(desc(Cost)) %>% 
  gt() %>% 
  fmt_number(
    columns = Cost
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Cost
    )
  ) %>% 
  cols_merge(columns = c("Median", "IQR"), pattern = "{1} ({2})") %>% 
  tab_style(
    style = list(
      "font-weight : bold"
    ),
    locations = cells_body(
      columns = Cost
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(style = "italic")
    ),
    locations = cells_body(
      columns = Median
    )
  ) %>% 
  cols_label(
    Median = "Median (IQR)",
    item = "Item"
  ) %>% 
  cols_merge(
    columns = c("Cost", "Percent"),
    pattern = "{1} ({2})"
  ) %>% 
  cols_label(
    Cost = "Cost (%)"
  )

(\(x) x) (6)



# gtsummary ---------------------------------------------------------------

df <- bites %>% 
  select(id, cons:svs) %>% 
  sjlabelled::drop_labels() %>% 
  pivot_longer(
    c(cons:svs), 
    names_to = "item", 
    values_to = "cost"
  ) %>%
  mutate(
    item = fct_recode(item, 
                      "Consultation" = "cons", 
                      "Drugs" = "drugs", 
                      "Services" = "svs")
  ) %>% 
  select(-id)

tb1 <- df %>% 
  tbl_continuous(
    variable = cost,
    digits = everything() ~ 0,
    statistic = everything() ~ "{sum}",
    label = list(item ~ "Item"),
    sort = everything() ~ "sum"
  ) %>% 
  modify_table_styling(columns = "stat_0", text_format = "bold")

tb2 <- df %>% 
  tbl_continuous(
    variable = cost,
    digits = everything() ~ 2,
    label = list(item ~ "Item")
  ) 

tbl_summary()

mrg <- tbl_merge(tbls = list(tb1, tb2), tab_spanner = c("Total cost", "Median (IQR)"))

write_rds(mrg, "merged summary stats 14June2022.rds")
inline_text(mrg, variable = item, level = "Consultation", pattern = "{stat_0_1}, {stat_0_2}")
