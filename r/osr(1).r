

# code folding ----
# alt-o, shift-alt-o
# alt-l, shift-alt-l
# alt-r

# notes ----
# compare tax, osc to sgdp
# 


# libraries ----
source(here::here("include", "libraries.r"))
# remotes::install_github("tidyverse/dplyr") if needed
library(BEAData)

devtools::session_info()
(.packages()) %>% sort

data(package="BEAData")

#
glimpse(sgdp.a)

# osrdata ----
glimpse(slgfin)
count(slgfin, aggvar, ic) %>% as.data.frame()

# osr cit gst iit selsalestax sevtax tottax
# selsalestax     cit only from 2004+

vars <- c("osr", "tottax", "iit", "gst", "selsalestax", "cit")
df <- slgfin %>%
  filter(level==2, aggvar %in% vars)


df2 <- df %>%
  filter(year >= 1985) %>%
  select(stabbr, year, aggvar, value) %>%
  pivot_wider(names_from = aggvar) %>%
  mutate(nontax=osr - naz(tottax)) %>%
  select(stabbr, year, osr, nontax, vars[-c(1)]) %>%
  arrange(stabbr, year)

df2 %>%
  filter(stabbr=="MA") %>%
  select(stabbr, year, osr, nontax, tottax) %>%
  left_join(sgdp.a %>% select(stabbr, year, gdp)) %>%
  mutate(across(c(osr, nontax, tottax), ~ .x / gdp / 10)) %>%
  select(-gdp) %>%
  group_by(stabbr) %>%
  arrange(year) %>%
  mutate(across(c(osr, nontax, tottax), ~ .x - lag(.x))) %>%
  pivot_longer(-c(stabbr, year)) %>%
  filter(year >= 1997, name!="nontax") %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0)
    
  

iris %>%
  group_by(Species) %>%
  mutate(across(starts_with("Sepal"), mean)) #, .names = "mean_{col}"))

iris %>%
  group_by(Species) %>%
  summarise(across(starts_with("Sepal"), mean, .names = "mean_{col}"))


df %>%
  filter(stabbr=="NY", year >= 1985) %>%
  select(stabbr, year, aggvar, value) %>%
  pivot_wider(names_from = aggvar) %>%
  mutate(nontax=osr - naz(tottax)) %>%
  select(stabbr, year, osr, nontax, tottax, vars[-c(1:2)]) %>%
  arrange(year)
  

# taxdata ----
data(package="bdata")

glimpse(sgtax.a)
count(sgtax.a, itemsort, item, desc)

# establish correspondence between items and variable names 
items <- c("C105", "T09", "T40")
vnames <- c("tottax", "gst", "iit")

df <- sgtax.a %>%
  mutate(year=as.integer(year)) %>%
  filter(year >= 1990, item %in% items) %>%
  mutate(vname=factor(item, levels=items, labels=vnames)) %>%
  select(year, stabbr, vname, value) %>%
  pivot_wider(names_from = vname) %>%
  mutate(othertax=tottax - naz(gst) - naz(iit),
         source="Census")
summary(df)

df %>% filter(is.na(gst)) %>% count(stabbr)
df %>% filter(is.na(iit)) %>% count(stabbr)

saveRDS(df, here::here("data", "tax_history.rds"))

write_dta(df, here::here("data", "tax_history.dta"))

# now add forecasts
