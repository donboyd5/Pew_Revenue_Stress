library(fable)

usgdp %>%
  filter(src=="moodys_dec2020p50", year %in% 2010:2030)
  

cg <- readRDS(here::here("data", "uscapgains.rds"))
stgdp

st <- "NJ"
st <- "MA"

df <- stgdp %>%
  filter(stabbr==st, src=="moodys_dec2020p50") %>%
  select(stabbr, year, gdp=value) %>%
  inner_join(cg %>% filter(name=="capgains_lagged") %>% select(year, cglag=value)) %>%
  inner_join(osr %>% filter(stabbr==st, name=="iit") %>% select(stabbr, year, iit=value))

df2 <- df %>%
  pivot_longer(cols=-c(year, stabbr)) %>%
  group_by(name) %>%
  mutate(pch=value / value[match(year - 1, year)] * 100 - 100) %>%
  ungroup

df2 %>%
  ggplot(aes(year, pch, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0)

dfwp <- df2 %>%
  select(stabbr, year, name, pch) %>%
  pivot_wider(values_from = pch)
dfwp

dfwv <- df2 %>%
  select(stabbr, year, name, value) %>%
  pivot_wider(values_from = value)
dfwv


mod.lm <- lm(iit ~ 0 + gdp + cglag, data=dfwp)

mod.arima <- dfwp %>%
  as_tsibble(index=year) %>%
  model(ARIMA(iit ~ gdp + cglag))

mod.arima2 <- dfwp %>%
  as_tsibble(index=year) %>%
  model(ARIMA(iit ~ 0 + pdq(0, 0, 0) + gdp + cglag))

modv.arima <- dfwv %>%
  as_tsibble(index=year) %>%
  model(ARIMA(iit ~ gdp + cglag))

summary(mod.lm)
report(mod.arima)
report(mod.arima2)

report(modv.arima)



cgmod <- assumecg %>%
  left_join(usgdp %>% filter(src=="moodys_dec2020p50") %>% select(year, pch) %>% mutate(gdppch=pch / 100)) %>%
  filter(year %in% 1995:2020) %>%
  as_tsibble(index=year) %>%
  model(ARIMA(baseline ~ gdppch + sp500))
report(cgmod)

cglm <- lm(baseline ~ gdppch + sp500, data=assumecg)
summary(cglm)

combo %>%
  filter(stabbr=="US", name=="gdp", year %in% 2018:2023) %>%
  select(year, value, src) %>%
  pivot_wider(names_from = src)

combo %>%
  filter(stabbr=="NJ", name=="gdp", year %in% 2018:2023) %>%
  select(year, value, src) %>%
  pivot_wider(names_from = src)

count(combo %>% filter(stabbr=="NJ"), name, src)


tmp <- combo %>%
  filter(stabbr=="NJ", (name=="gdp" & src=="beaspliced") | name=="nontax") %>%
  select(year, name, value) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  mutate(pch=value / value[match(year - 1, year)] * 100 - 100) %>%
  ungroup

tmp %>%
  ggplot(aes(year, pch, colour=name)) +
  geom_line() +
  geom_point()


library(forecast)
library(tsibble)
library(tibble)
library(tidyverse)
library(fable)
library(feasts)
library(fabletools)


# us_change <- readr::read_csv("https://otexts.com/fpp3/extrafiles/us_change.csv") %>%
#   mutate(Time = yearquarter(Time)) %>%
#   as_tsibble(index = Time)

tmp2 <- tmp %>%
  filter(year %in% 1964:2019) %>%
  select(-pch) %>%
  pivot_wider() %>%
  as_tsibble(index = year)
tmp2

mod <- lm(nontax ~ gdp, data=tmp2 %>% filter(year >= 1990))
summary(mod)

tmp3 <- tmp %>%
  filter(year %in% 1964:2020) %>%
  select(-pch) %>%
  pivot_wider() %>%
  as_tsibble(index = year) 

tmp3 %>%
  model(lev=ARIMA(log(nontax))) %>%
  forecast(h="2 years") %>%
  autoplot(tmp3 %>% filter(year >= 2010), level=NULL) +
  geom_point(aes(x=year, y=nontax))

tmp4 <- tmp3 %>%
  mutate(pctgdp=nontax / gdp * 100)

tmp4 %>%
  model(lev=ARIMA(pctgdp)) %>%
  forecast(h="2 years") %>%
  autoplot(tmp4 %>% filter(year >= 2010), level=NULL) +
  geom_point(aes(x=year, y=pctgdp))


mod2 <- tmp2 %>%
  model(ARIMA(nontax ~ gdp))
str(mod2)
summary(mod2)

mod2 %>%
  glance() %>%
  arrange(AICc)

mod <- ARIMA(Consumption ~ Income + PDQ(0,0,0))

fit <- us_change %>% model(model_def)

report(fit)

us_change_future <- new_data(us_change, 8) %>% mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>% autoplot(us_change)

model_def = ARIMA(Consumption ~ Income + PDQ(0,0,0))

fit <- us_change %>% model(model_def)

report(fit)

us_change_future <- new_data(us_change, 8) %>% mutate(Income = mean(us_change$Income))

forecast(fit, new_data = us_change_future) %>% autoplot(us_change)

# https://otexts.com/fpp3/
# http://fable.tidyverts.org/

library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)
#> Warning: package 'lubridate' was built under R version 3.6.3
library(dplyr)

glimpse(aus_retail)

aus_retail %>%
  filter(
    State %in% c("New South Wales", "Victoria"),
    Industry == "Department stores"
  ) %>% 
  model(
    ets = ETS(box_cox(Turnover, 0.3)),
    arima = ARIMA(log(Turnover)),
    snaive = SNAIVE(Turnover)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(aus_retail, year(Month) > 2010), level = NULL)


osr

osr %>%
  filter(stabbr=="MA", year >= 2017) %>%
  select(-pch) %>%
  pivot_wider()

qtax %>%
  filter(stabbr=="MA", year(date) >= 2019, vname=="iit")


osrupdate %>%
  filter(stabbr=="MA", name=="osr") %>%
  filter(year >= 2017)

