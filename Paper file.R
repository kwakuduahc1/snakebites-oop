library(carData)
my_stats <- function(data, full_data, ...) {
  sum_salary <- sum(data$cost, na.rm = TRUE)
  total_sum <- sum(full_data$cost, na.rm = TRUE)
  dplyr::tibble(
    sum = sum_salary,
    total_sum = total_sum,
    p = round(sum_salary / total_sum, 3)
  )
}

bites %>% 
  select(id, cons:svs) %>% 
  sjlabelled::drop_labels() %>% 
  pivot_longer(c(cons:svs), names_to = "item", values_to = "cost") %>%
  select(-id) %>% 
  mutate(
    item = fct_recode(item, "Consultation" = "cons", "Drugs" = "drugs", "Services" = "svs")
  ) %>%
  # group_by(item) %>%
  # summarise(
  #   Cost = round(sum(cost, na.rm = T), 2),
  #   Median = round(median(cost, na.rm = T), 1),
  #   IQR = str_glue("{round(quantile(cost)[2],2)} - {round(quantile(cost)[4],2)}")
  # ) %>%
  # mutate(
  #   Percent = percent(round(Cost/sum(Cost), 3)) 
  # ) %>%
  # arrange(desc(Cost)) %>% 
  # set_names(c("Item", "Cost")) %>% 
tbl_custom_summary(
  include = item,
  stat_fns = ~ my_stats,
  statistic = ~ "{sum_salary} ({prop}%)",
  digits = ~ list(style_number, style_percent)
)


ob <- bites %>% 
  select(mode, cons:year, -exems) %>% 
  # mutate(year = as.numeric(year)) %>% 
  pivot_longer(-c(mode, year)) %>% 
  group_by(mode, year, name) %>% 
  summarise(value= median(value)) %>% 
  mutate(name = fct_recode(name, "Drugs" = "drugs", "Consultation" = "cons", "Services" = "svs"),
         name = fct_relevel(name,  "Drugs", "Services", "Consultation")) %>% 
  ggline(
    x= "year",
    y = "value",xlab = "Year",
    ylab = "Median OOP (USD)",
    color = "mode",
    facet.by = "name",
    free.y = T,
    legend.title = "Payment type",
    scales = "free_y",
    palette = "npg"
  ) 
ob
