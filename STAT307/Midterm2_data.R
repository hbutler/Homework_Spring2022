library(tidyverse)

pipelines <- read_csv("STAT307/oilpipelines.csv")
titanic <- read_csv("STAT307/train.csv")

titanic_reduced <- titanic %>%
  select(Age, Survived) %>%
  filter(complete.cases(.)) %>%
  group_by(Survived) %>%
  summarize(mean_age = mean(Age)
            , sd_age = sd(Age)
            , sample_size = n()
            )

pipeline_reduced <- pipelines %>%
  select(`Accident Year`, `Accident State`) %>%
  filter(`Accident Year` %in% c(2010, 2016)) %>%
  group_by(`Accident Year`, `Accident State`) %>%
  summarize(no_accidents = n()) %>%
  filter(!is.na(`Accident State`) ) %>%
  pivot_wider(names_from = `Accident Year`
              , values_from = no_accidents
              ) %>%
  filter(!is.na(`2010`) & !is.na(`2016`))

pipeline_reduced[sample(1:35, 10), ]
