with(bites, sum(invs + cons + drugs + wds + procs +  svs))
sum(bites$paid) - with(bites, sum(cds + exems))
sum(bites$paid) - sum(bites$np)


bites %>% 
  mutate(
    across(invs:paid, ~round(., 4)),
    calc = (invs + cons + drugs + wds + procs +  svs)
  ) %>% 
  filter(calc != paid) %>% 
  select(id, calc, paid, np)

bites %>% 
  select(id, mode, cons:svs) %>% 
  sjlabelled::drop_labels() %>% 
  pivot_longer(c(cons:svs), names_to = "item", values_to = "cost") %>%
  mutate(
    item = fct_recode(item, "Consultation" = "cons", "Drugs" = "drugs", "Services" = "svs")) %>% 
  select(-id) %>% 
  tbl_continuous(variable = cost, include = item, by = mode, statistic = list(everything() ~ "{sum}({max}) [{median}({p25}-{p75})]")) %>% 
  modify_header(update = all_stat_cols() ~ "**{level}**, Total = {myf({level})}") %>% 
  modify_footnote(all_stat_cols() ~ "sum(Max) [Median(IQR)]")


myf <- function(b){
  bites %>% 
    filter(mode == b) %>% 
    select(paid) %>% sum() %>% 
    scales::comma()
}

myf("Cash & Carry")


bites %>% 
  tbl_continuous(variable = paid, include = c(gender, year, seas, act, outcome),  by = mode) %>% add_p(test = everything() ~ "kruskal.test")


  with(bites, glm(paid ~ seas + mode)) %>% summary()

  