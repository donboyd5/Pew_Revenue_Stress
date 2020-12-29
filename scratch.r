
count(njgdp_fy, name)
count(njgdp_fy, src)
njgdp_fy %>%
  filter(year >= 2015) %>%
  ggplot(aes(year, value, colour=src)) +
  geom_line() +
  geom_point()
  