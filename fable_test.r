

source(here::here("include", "libraries.r"))
library(fable)
library(tsibble)
library(tsibbledata)
library(lubridate)

combo <- readRDS(here::here("data", "combo.rds"))

combotab <- combo %>%
  group_by(fullname) %>%
  summarise(n=n(),
            n_notNA=sum(!is.na(value)),
            first_year=min(year),
            last_year=max(year),
            nyears=last_year - first_year + 1,
            nstates=length(unique(stabbr)),
            nus=sum(stabbr=="US", na.rm = TRUE),
            nnj=sum(stabbr=="NJ", na.rm = TRUE),
            minval=min(value, na.rm=TRUE),
            maxval=max(value, na.rm=TRUE),
            .groups="drop") %>%
  select(fullname, n, n_notNA, nyears, everything())
combotab


# we need our spliced national and state gdp forecasts
usgdp <- readRDS(here::here("data", "usgdp_spliced.rds"))
njgdp <- readRDS(here::here("data", "njgdp_spliced.rds"))


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
