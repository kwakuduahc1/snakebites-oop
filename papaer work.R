df.m <-  rnorm(50, 11.5)
df.f <- rnorm(50, 10.5)
df <- tibble(male = df.m, female = df.f) %>% 
  pivot_longer(everything())

df %>% 
  ggboxplot(x = "name", y = "value", xlab = "Gender", ylab = "HB (g/dL)", title = "HB between males and females")
  
library(tidyverse)

library(gt)
dset <- tibble(temperature = c(1, 2, NA),
               pulse = c(NA, 1, 1.2))

dset %>%
  gt() %>%
  tab_style_body(
    fn = \(x) is.na(x),
    style = cell_fill(color = "lightblue")
  )


bites %>%
  select(id, mode, cons:svs) %>% 
  pivot_longer(cols = -c(id, mode), names_to = "item", values_to = "amnt")  %$% 
  aov(
    amnt ~ item*mode
  ) %>% TukeyHSD() %>%
#   broom::tidy() %>% 
#   mutate(id = row_number()) %>% 
#   arrange(contrast) %>% 
#   filter(
#     id %in% c(7, 12, 16)
#   ) %>% 
#   select(contrast, estimate, conf.low, conf.high, adj.p.value) %>% 
#   mutate(
#     contrast = fct_recode( contrast,
#       "Drugs" = "drugs:NHIS-drugs:Cash & Carry",
#       "Services" = "svs:NHIS-svs:Cash & Carry",
#       "Consulting" = "cons:NHIS-cons:Cash & Carry"
#     )
#   ) %>% 
#   set_names(c("Item", "Estimate", "LCI", "UCI", "pvalue")) %>% 
#   gt() %>% 
#   fmt_number(columns = 2:4) %>% 
#   fmt(columns = 5, fns = label_pvalue(prefix = c("😀", "🙅️", "**"))) %>% 
#   write_rds(file = "nhis item cost-diff.rds")
