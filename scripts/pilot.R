# load packages
library(tidyverse)
library(dplyr)
library(remotes)
library(devtools)
install_github("mdelacre/Routliers")

# load dataset
pilot <- read_csv("data/pilot.csv") %>%
  as_tibble() %>%
  mutate(affiliation = as.factor(affiliation), 
         affiliation = recode(affiliation, 
                              '1' = 'Left',
                              '2' = 'Centre',
                              '3' = 'Right',
                              '4' = 'Not affiliated')) %>%
  print()

# mean age of participants
mean(pilot$age)
sd(pilot$age)

# affiliation distribution (no. of people, mean age)
pilot %>%
  group_by(affiliation) %>%
  summarise(., mean_age = mean(age), sd_age = sd(age), n = n())

# reaction to conditions


# efficacy conditions
# scenario 1 (protesters shout down event)

# distribution
hist(pilot$attention_1_1)
hist(pilot$attention_1_2)

# outlier detection
attention_1_1 <- pilot$attention_1_1
attention_1_1_mad <- Routliers::outliers_mad(x = attention_1_1)
attention_1_1_mad # no outliers

attention_1_2 <- pilot$attention_1_2
attention_1_2_mad <- Routliers::outliers_mad(x= attention_1_2)
attention_1_2_mad
# mad of 0, and 97 outliers??

# descriptives
# overall
pilot %>%
  summarise(mean = mean(attention_1_1), sd = sd(attention_1_1))
# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_1_1), sd = sd(attention_1_1))



# scenario 2 (event successful)

# distribution
hist(pilot$attention_2_1)
hist(pilot$attention_2_2)

# outlier detection
attention_2_1 <- pilot$attention_2_1
attention_2_1_mad <- Routliers::outliers_mad(x=attention_2_1)
attention_2_1_mad
# mad of 0 and 98 outliers??

attention_2_2 <- pilot$attention_2_2
attention_2_2_mad <- Routliers::outliers_mad(x = attention_2_2)
attention_2_2_mad # 1 outlier, id 033



# descriptives



pilot %>%
  summarise(mean = mean(attention_2_2), sd = sd(attention_2_2))

# without outlier
pilot %>%
  filter(id != "033") %>%
  summarise(mean = mean(attention_2_2), sd = sd(attention_2_2), n = n())

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_2_2), sd = sd(attention_2_2))

# without outlier (Centre)
pilot %>%
  filter(id != "033") %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_2_2), sd = sd(attention_2_2))


# scenario 3 (interrupted music)
hist(pilot$attention_3_1)
hist(pilot$attention_3_2)

# outlier detection
attention_3_1 <- pilot$attention_3_1
attention_3_1_mad <- Routliers::outliers_mad(x=attention_3_1)
attention_3_1_mad # no outliers

attention_3_2 <- pilot$attention_3_2
attention_3_2_mad <- Routliers::outliers_mad(x=attention_3_2)
attention_3_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(attention_3_1), sd = sd(attention_3_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_3_1), sd = sd(attention_3_1))


pilot %>%
  summarise(mean = mean(attention_3_2), sd = sd(attention_3_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_3_2), sd = sd(attention_3_2))

# scenario 4 (interrupted signs)
hist(pilot$attention_4_1)
hist(pilot$attention_4_2)

# outlier detection
attention_4_1 <- pilot$attention_4_1
attention_4_1_mad <- Routliers::outliers_mad(x=attention_4_1)
attention_4_1_mad # no outliers

attention_4_2 <- pilot$attention_4_2
attention_4_2_mad <- Routliers::outliers_mad(x=attention_4_2)
attention_4_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(attention_4_1), sd = sd(attention_4_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_4_1), sd = sd(attention_4_1))


pilot %>%
  summarise(mean = mean(attention_4_2), sd = sd(attention_4_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_4_2), sd = sd(attention_4_2))

# scenario 5 (interrupted milkshake)
hist(pilot$attention_5_1)
hist(pilot$attention_5_2)

# outlier detection
attention_5_1 <- pilot$attention_5_1
attention_5_1_mad <- Routliers::outliers_mad(x=attention_5_1)
attention_5_1_mad # no outliers

attention_5_2 <- pilot$attention_5_2
attention_5_2_mad <- Routliers::outliers_mad(x=attention_5_2)
attention_5_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(attention_5_1), sd = sd(attention_5_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_5_1), sd = sd(attention_5_1))


pilot %>%
  summarise(mean = mean(attention_5_2), sd = sd(attention_5_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_5_2), sd = sd(attention_5_2))
