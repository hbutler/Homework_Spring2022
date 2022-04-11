library(tidyverse)
library(lubridate)

world_vaccines <- read_csv("STAT307/COVID/country_vaccinations.csv")


world_vaccines_summary <- world_vaccines %>%
  select(-c(total_vaccinations
            , people_fully_vaccinated
            , people_vaccinated
            , daily_vaccinations_raw
            , vaccines
            , source_name
            , source_website
            )
         ) %>%
  mutate(month = month(date)
         , year = year(date)
         ) %>%
  filter(year == 2021)  %>%
  group_by(country, month) %>%
  summarize(ppl_vax_per_hundred = max(people_vaccinated_per_hundred, na.rm = TRUE)) %>%
  filter(ppl_vax_per_hundred != -Inf) %>%
  arrange(country, month) %>%
  ungroup() %>%
  group_by(country)

country_list <- group_split(world_vaccines_summary)
monthly_vax <- lapply(country_list, function(x) {
  monthly_vax <- c(x$ppl_vax_per_hundred[1], diff(x$ppl_vax_per_hundred))
}) %>%
  do.call(c, .)

world_vaccines_summary$monthly_vax <- monthly_vax 

covid_vax <- world_vaccines_summary %>%
  select(-ppl_vax_per_hundred) %>%
  filter(country %in% c("United States", "New Zealand", "Brazil", "Nigeria", "South Korea", "India"))

covid_vax_summary <- covid_vax %>%
  group_by(country) %>%
  summarize(observations = n()
            , average = round(mean(monthly_vax), 2)
            , minimum = min(monthly_vax)
            , Q1 = quantile(monthly_vax, .25)
            , median = round(median(monthly_vax), 2)
            , Q3 = quantile(monthly_vax, .75)
            , maximum = max(monthly_vax)
            , st_dev = round(sd(monthly_vax), 2)
            )

write_csv(covid_vax, "STAT307/covid_vax_ANOVA.csv")

covid_vax %>%
  group_by(country) %>%
  mutate(residual = (monthly_vax - mean(monthly_vax))^2) %>%
  summarise(sse = sum(residual))
  
  
  summarize(sse = sum((monthly_vax - mean(monthly_vax))^2))
