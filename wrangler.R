library(DBI)
library(ggpubr)
library(gtsummary)
library(flextable)
library(tidyverse)



#con <- dbConnect(odbc::odbc(), .connection_string = "Driver={ODBC Driver 17 for SQL Server};server=192.168.10.208;uid=sa;pwd=Ibrahim@66!88;database=Helix", timeout = 10)

# qry = "SELECT * FROM vw_snakebites"
# bites <- dbGetQuery(con, qry)
# 
# qry <- "SELECT [id],[year],[month],[name],[invs],[cons],[drugs],[exems],[wds],[cds],[procs],[svs],[cost],[paid] FROM [Helix].[dbo].[vw_payments]"
# payments <- dbGetQuery(con, qry)
# 
# 
# write_rds(payments, "payments.rds")

qry <- "spSnakesCosts"
bites <- dbGetQuery(con, qry) 
bites <- bites %>% 
  filter(!is.na(year) & year < 2020 ) %>% 
  mutate(
    across(c(cons: exems), ~case_when(.x <0 ~ 0, T ~ .x)),
    across(cons:exems, ~round(., 1)),
    across(c(cons: exems), ~replace_na(., 0)),
    paid = (cons + drugs + svs - exems),
    across(c(cons:exems), ~case_when(year == 2016 ~ .x/4.3, year == 2017 ~ .x/4.54, year == 2018 ~ .x/4.83, year == 2019 ~ .x/5.7)),
    occup = fct_lump_prop(occup, prop = .01),
    paid = (cons + drugs + svs - exems),
    nhis = factor(nhis, levels = 0:1, labels = c("No", "Yes")),
    act = case_when(month %in% c(11, 12) ~ 1, month %in%  c(1:5) ~ 2, month %in% c(6,7) ~ 3,  month %in% c(8:10) ~ 4),
    act = factor(act, levels = 1:4, labels = c("Harvesting", "Irrigation/hunting", "Land preparation", "Farming")),
    seas = case_when(month %in% c(1:7, 12) ~ 1, month %in% 8:11 ~ 2),
    seas = factor(seas, levels = 1:2, labels = c("Dry", "Rainy")),
    month = factor(month, levels = 1:12, labels = month.abb),
    los = abs(los),
    los = case_when(los > out ~ as.double(out), T ~ as.double(los)),
    age = abs(age),
    outcome = fct_collapse(outcome, "Successful treatment" = c("For Subsequent review", "Successful treatment"))
  ) %>% 
  select(everything(), mode = Name) 

bites <- sjlabelled::var_labels(bites, 
                                age = "Age", 
                                gender = "Gender", 
                                nhis = "NHIS", 
                                occup = "Occupation", 
                                outcome = "Outcome", 
                                los = "Length of stay", 
                                pid = "PatientID", 
                                id = "Encounter ID", 
                                year = "Year", 
                                month = "Month", 
                                act = "Activity related to farming", 
                                seas = "Season",
                                mode = "Payment mode", 
                                cons = "Consultation",
                                drugs = "Drugs",
                                exems = "Exemptions", 
                                svs = "Other Services",
                                paid = "Payment")

write_rds(bites, "bites.rds")

mn <- mean(bites[bites$los < 100, "los"], na.rm = T)
sd <- sd(bites[bites$los < 100, "los"], na.rm = T) 
out <- mn + (sd * 3)

sids <- sample(bites$id, 20)

smpl <- bites[bites$id %in% sids,]

write_rds(smpl, "sample bites.rds")
