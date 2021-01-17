
# C:\RPrograms PC\Pensions\Pew_Revenue_Stress\ignore\Kimberly

kbdir <- here::here("ignore", "Kimberly")

# files received from Kimberly Burnham Jan 6 and 7 2020

# Baseline and Downside Scenario 12_21_20.xlsx
#   CBO and fed alt severe growth rates
#   y0-y10 each; y11-20 avg
#   prob calendar year
#   prob massaged by Pew
#   need to find out what they are

# Moody State Population December 2020.xlsx
#   50-state pop
#   1900Q2 to 2050q4
#   thousands
#   Dec 2020 baseline

# Moody's GSP December 2020.xlsx
#  50-state GDP
#  quarterly
#  1977Q1 to 2050q4
#  $ billions SAAR
#  Dec 2020 baseline

# Moody's State Employment December 2020.xlsx
#  50-state eea
#  quarterly
#  1977Q1 to 2050q4
#  thousands
#  Dec 2020 baseline

# Moodys State Household Income December 2020.xlsx
#  50-state median HHI
#  quarterly
#  1977Q1 to 2050q4
#  dollars
#  Dec 2020 baseline

# Moodys State Pop Age December 2020.xlsx
#  50-state population by 5-year age groups
#  thousands
#  1970Q2 to 2050q4
#  dollars
#  Dec 2020 baseline

# Moodys GSP July 2020.xlsx
#  50-state GDP
#  quarterly
#  1977Q1 to 2050q4
#  $ billions SAAR
#  July 2020 baseline

# mutate(year=str_sub(yq, 1, 4) %>% as.numeric,
#        q=str_sub(yq, -1, -1) %>% as.numeric,
#        year=ifelse(q >= 3, year + 1, year))

# year, name, value, stabbr, src, fullname

kbfiles <- c("Baseline and Downside Scenario 12_21_20.xlsx",
             "Moody State Population December 2020.xlsx",
             "Moody's GSP December 2020.xlsx",
             "Moodys State Household Income December 2020.xlsx",
             "Moodys State Pop Age December 2020.xlsx",
             "Moodys GSP July 2020.xlsx")

# 50 states gdp
df <- read_excel(here::here(kbdir, kbfiles[3]))
stnames <- as_tibble(df[4, -1] %>% t, 
                     rownames="abbr") %>%
  rename(stname=V1)

df2 <- df %>%
  rename(date=1) %>%
  filter(row_number() > 4) %>%
  mutate(date=paste0(str_sub(date, 1, 4), "-", as.numeric(str_sub(date, -1)) * 3 - 2, "-01"),
         date=as.Date(date)) %>%
  pivot_longer(-date, names_to = "abbr") %>%
  mutate(value=as.numeric(value)) %>%
  left_join(stnames, by="abbr") %>%
  mutate(stabbr=str_sub(abbr, -2, -1))
df2
count(df2, stabbr, stname) # 54: 50 states, DC, GU, PR, VI
summary(df2)
  
# put the file in format to save
df3 <- df2 %>%
  select(stabbr, stname, date, value)
saveRDS(df3, here::here("data", "gdpq_moodys_dec2020.rds"))

# create a fiscal year file
# CAUTION: July 1 fiscal year for all states
df3 <- readRDS(here::here("data", "gdpq_moodys_dec2020.rds"))
df4 <- df3 %>%
  mutate(year=year(date),
         year=ifelse(month(date) >= 7, year + 1, year)) %>%
  group_by(stabbr, year) %>%
  summarise(value=mean(value), .groups = "drop")
summary(df4)
count(df4, date, year)
df4 %>% filter(is.na(value))

# get US totals excluding GU, PR, VI as they are not in for all years
df5 <- df4 %>%
  filter(!stabbr %in% c("GU", "PR", "VI"))

# get US totals
ustot <- df5 %>%
  group_by(year) %>%
  summarise(value=sum(value), n=n())
summary(ustot)

# now get the growth rates and diffs
df6 <- df5 %>%
  left_join(ustot %>% select(year, us=value), 
            by="year") %>%
  arrange(stabbr, year) %>%
  group_by(stabbr) %>%
  mutate(pch=value / value[match(year - 1, year)] * 100 - 100,
         pchus=us / us[match(year - 1, year)] * 100 - 100,
         pdelta=pch - pchus)

df6 %>%
  pivot_longer(cols=c(value, pch, pdelta))

dfl <- df6 %>%
  pivot_longer(cols=-c(stabbr, year))

dfl %>%
  filter(name=="pdelta", stabbr %in% c("MA", "NJ"), year>=2000, year <= 2030) %>%
  ggplot(aes(year, value, colour=stabbr)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(name="percent", breaks=seq(-10, 10, .5)) +
  scale_x_continuous(breaks=seq(2000, 2050, 5)) +
  ggtitle("% growth in state nominal GDP minus % growth in US nominal GDP",
          subtitle="State fiscal years")



# state gdp
df <- read_excel(here::here(kbdir, kbfiles[3]))
stnames <- as_tibble(df[4, -1] %>% t, 
                     rownames="abbr") %>%
  rename(stname=V1)

as.Date("2020-1-1")



df3 <- df2 %>%
  select(stabbr, stname, date, value)


mfn <- "Moody's 50 and 90 July.xlsx"
moody <- read_excel(here::here("ignore", "NJ", mfn), 
                    sheet="GDP", range="A6:C421", col_names = c("yq", "moodys50", "moodys90"))


