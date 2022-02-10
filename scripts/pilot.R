### load packages
library(tidyverse)
library(dplyr)
library(remotes)
library(devtools)
install_github("mdelacre/Routliers")

### load dataset
pilot <- read_csv("data/pilot.csv") %>%
  as_tibble() %>%
  mutate(affiliation = as.factor(affiliation), 
         affiliation = recode(affiliation, 
                              '1' = 'Left',
                              '2' = 'Centre',
                              '3' = 'Right',
                              '4' = 'Not affiliated')) %>%
  print()

### mean age of participants
mean(pilot$age)
sd(pilot$age)

### affiliation distribution (no. of people, mean age)
pilot %>%
  group_by(affiliation) %>%
  summarise(., mean_age = mean(age), sd_age = sd(age), n = n())

### reaction to conditions


## efficacy conditions
# scenario 1 (protesters shout down event)

# distribution
hist(pilot$attention_1_1)
hist(pilot$attention_1_2)

# outlier detection
attention_1_1 <- pilot$attention_1_1
attention_1_1_mad <- Routliers::outliers_mad(x = attention_1_1)
attention_1_1_mad # no outliers

attention_1_2 <- pilot$attention_1_2
attention_1_2_mad <- Routliers::outliers_mad(x=attention_1_2)
attention_1_2_mad
# mad of 0, 97 outliers??
outliers_attention_1_2 <- dplyr::filter(pilot, attention_1_2 != "2")

# descriptives
# overall
pilot %>%
  summarise(mean = mean(attention_1_1), sd = sd(attention_1_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_1_1), sd = sd(attention_1_1))

pilot %>%
  summarise(mean = mean(attention_1_2), sd = sd(attention_1_2))
# without outliers
pilot %>% 
  filter(attention_1_2 == "2") %>%
  summarise(mean = mean(attention_1_2), sd = sd(attention_1_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_1_2), sd = sd(attention_1_2))

# without outliers
pilot %>% 
  filter(attention_1_2 == "2") %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_1_2), sd = sd(attention_1_2))

# scenario 2 (event successful)

# distribution
hist(pilot$attention_2_1)
hist(pilot$attention_2_2)

# outlier detection
attention_2_1 <- pilot$attention_2_1
attention_2_1_mad <- Routliers::outliers_mad(x=attention_2_1)
attention_2_1_mad
# mad of 0 and 98 outliers
outliers_attention_2_1 <-dplyr::filter(pilot, attention_2_1 != "5")

attention_2_2 <- pilot$attention_2_2
attention_2_2_mad <- Routliers::outliers_mad(x = attention_2_2)
attention_2_2_mad # 1 outlier, id 033

# descriptives
pilot %>%
  summarise(mean = mean(attention_2_1), sd = sd(attention_2_1))

# without outlier
pilot %>%
  filter(attention_2_1 == "5") %>%
  summarise(mean = mean(attention_2_1), sd = sd(attention_2_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_2_1), sd = sd(attention_2_1))

# without outlier (Centre)
pilot %>%
  filter(attention_2_1 == "5") %>%
  group_by(affiliation) %>%
  summarise(mean = mean(attention_2_1), sd = sd(attention_2_1))


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


## humour conditions

# scenario 1 (music)

# distribution
hist(pilot$music_1)
hist(pilot$music_2)

# outlier detection
music_1 <- pilot$music_1
music_1_mad <- Routliers::outliers_mad(x=music_1)
music_1_mad # no outliers

music_2 <- pilot$music_2
music_2_mad <- Routliers::outliers_mad(x=music_2)
music_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(music_1), sd = sd(music_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(music_1), sd = sd(music_1))

# anova on intergroup differences
library(ggpubr)
library(ggplot2)
library(broom)
library(AICcmodavg)

anova_music_1 <- aov(music_1~affiliation, data = pilot)
summary(anova_music_1)

tukey_anova_music_1 <- TukeyHSD(anova_music_1)
tukey_anova_music_1


pilot %>%
  summarise(mean = mean(music_2), sd = sd(music_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(music_2), sd = sd(music_2))

# anova on intergroup differences
anova_music_2 <- aov(music_2~affiliation, data = pilot)
summary(anova_music_2)

tukey_anova_music_2 <- TukeyHSD(anova_music_2)
tukey_anova_music_2

# testing music_1 as significant from scale mid-point (3)

t.test(pilot$music_1, mu = 3, alternative = "two.sided")

music_1_left <- pilot %>%
  dplyr::filter(affiliation == "Left")

t.test(music_1_left$music_1, mu = 3, alternative = "two.sided")

music_1_centre <- pilot %>%
  dplyr::filter(affiliation == "Centre")

t.test(music_1_centre$music_1, mu = 3, alternative = "two.sided")

music_1_right <- pilot %>%
  dplyr::filter(affiliation == "Right")

t.test(music_1_right$music_1, mu = 3, alternative = "two.sided")

music_1_na <- pilot %>%
  dplyr::filter(affiliation == "Not affiliated")

t.test(music_1_na$music_1, mu = 3, alternative = "two.sided")

# scenario 2 (signs)

# distribution
hist(pilot$signs_1)
hist(pilot$signs_2)

# outlier detection
signs_1 <- pilot$signs_1
signs_1_mad <- Routliers::outliers_mad(x=signs_1)
signs_1_mad # no outliers

signs_2 <- pilot$signs_2
signs_2_mad <- Routliers::outliers_mad(x=signs_2)
signs_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(signs_1), sd = sd(signs_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(signs_1), sd = sd(signs_1))

# anova on intergroup differences
anova_signs_1 <- aov(signs_1~affiliation, data = pilot)
summary(anova_signs_1)

tukey_anova_signs_1 <- TukeyHSD(anova_signs_1)
tukey_anova_signs_1

pilot %>%
  summarise(mean = mean(signs_2), sd = sd(signs_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(signs_2), sd = sd(signs_2))

anova_signs_2 <- aov(signs_2~affiliation, data = pilot)
summary(anova_signs_2)

tukey_anova_signs_2 <- TukeyHSD(anova_signs_2)
tukey_anova_signs_2

# scenario 3 (milkshake)

# distribution
hist(pilot$milk_1)
hist(pilot$milk_2)

# outlier detection
milk_1 <- pilot$milk_1
milk_1_mad <- Routliers::outliers_mad(x=milk_1)
milk_1_mad # no outliers

milk_2 <- pilot$milk_2
milk_2_mad <- Routliers::outliers_mad(x=milk_2)
milk_2_mad # no outliers

# descriptives
pilot %>%
  summarise(mean = mean(milk_1), sd = sd(milk_1))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(milk_1), sd = sd(milk_1))

# anova on group differences
anova_milk_1 <- aov(milk_1~affiliation, data = pilot)
summary(anova_milk_1)

tukey_anova_milk_1<- TukeyHSD(anova_milk_1)
tukey_anova_milk_1

pilot %>%
  summarise(mean = mean(milk_2), sd = sd(milk_2))

# by group
pilot %>%
  group_by(affiliation) %>%
  summarise(mean = mean(milk_2), sd = sd(milk_2))

# anova on group differences
anova_milk_2 <- aov(milk_2~affiliation, data = pilot)
summary(anova_milk_2)

tukey_anova_milk_2<- TukeyHSD(anova_milk_2)
tukey_anova_milk_2

### reliability, normality, and validity 

## self-categorization (reliability)

# distribution items
hist(pilot$selfcat_1)
hist(pilot$selfcat_2)

# outlier detection
selfcat_1 <- pilot$selfcat_1
selfcat_1_mad <- Routliers::outliers_mad(x=selfcat_1)
selfcat_1_mad # no outliers

selfcat_2 <- pilot$selfcat_2
selfcat_2_mad <- Routliers::outliers_mad(x=selfcat_2)
selfcat_2_mad # no outliers

# cronbach's alpha
library(psych)

key <- list(
  selfcat = c("selfcat_1", "selfcat_2")
)

score.items(key, pilot) # reliability of 0.96

# inter-item correlation
library(performance)

interitem_selfcat <- pilot[, c("selfcat_1", "selfcat_2")]
item_intercor(interitem_selfcat) # inter-item correlation of 0.92

# create new variable "selfcat"

pilot <- pilot %>%
  mutate(
    selfcat = (selfcat_1 + selfcat_2)/2
  )

# normality
hist(pilot$selfcat)
describe(pilot$selfcat) # in the range of normality, but visually positively skewed

# group effect?
pilot_left <- pilot %>%
  filter(affiliation == "Left")

hist(pilot_left$selfcat) # strongly positively skewed
describe(pilot_left$selfcat) # skew = 1.2

pilot_centre <- pilot %>%
  filter(affiliation == "Centre")

hist(pilot_centre$selfcat) # still positively skewed but lesser than left
describe(pilot_centre$selfcat)

pilot_right <- pilot %>%
  filter(affiliation == "Right")

hist(pilot_right$selfcat) # rather normal looking
describe(pilot_right$selfcat)

pilot_na <- pilot %>%
  filter(affiliation == "Not affiliated")

hist(pilot_na$selfcat) # positively skewed
describe(pilot_na$selfcat)


## stereotype (competence)

# distribution
hist(pilot$stereo_1)
hist(pilot$stereo_2)

# outlier detection
stereo_1 <- pilot$stereo_1
stereo_1_mad <- Routliers::outliers_mad(x=stereo_1)
stereo_1_mad # no outliers

stereo_2 <- pilot$stereo_2
stereo_2_mad <- Routliers::outliers_mad(x=stereo_2)
stereo_2_mad # no outliers

# inter-item correlation

interitem_stereo <- pilot[, c("stereo_1", "stereo_2")]
item_intercor(interitem_stereo) # inter-item correlation of 0.76

# create new variable "selfcat"

pilot <- pilot %>%
  mutate(
    stereo = (stereo_1 + stereo_2)/2
  )

# normality
hist(pilot$stereo)
describe(pilot$stereo) # in the range of normality, and visually rather normal looking

## legitimacy 

# distribution
hist(pilot$legit_1)
hist(pilot$legit_2)
hist(pilot$legit_3)

# outlier detection
legit_1 <- pilot$legit_1
legit_1_mad <- Routliers::outliers_mad(x=legit_1)
legit_1_mad # no outliers

legit_2 <- pilot$legit_2
legit_2_mad <- Routliers::outliers_mad(x=legit_2)
legit_2_mad # no outliers

legit_3 <- pilot$legit_3
legit_3_mad <- Routliers::outliers_mad(x=legit_3)
legit_3_mad # no outliers

# cronbach's alpha
key_1 <- list(
  legit = c("legit_1", "legit_2", "legit_3")
)

score.items(key_1, pilot) # alpha = 0.83

# create new variable "legit"

pilot <- pilot %>%
  mutate(
    legit = (legit_1 + legit_2 + legit_3)/3
  )

# normality

hist(pilot$legit)
describe(pilot$legit) # rather normally distributed

# validity 
library(lavaan)
library(semPlot)
library(lm.beta)

legit.cfa <- 'legit.cfa =~ legit_1 + legit_2 + legit_3'
cfa_legit.sem <- sem(legit.cfa, data = pilot)
lavaan::summary(cfa_legit.sem, standardized = TRUE, fit.measures = TRUE) # model is saturated (sign chi-square, RMSEA = 0, CFI = 1)
# the adequacy of saturated models can be tested by experimentally targeting it, i.e., if its predictions match the observed 
# differences (or lack thereof) of the parameter estimates, then the model may be valid 
# (https://stats.stackexchange.com/questions/283/what-is-a-saturated-model#:~:text=If%20a%20model%20is%20saturated,that%20the%20model%20is%20valid.)
# --> can we observe differences in predictions based on affiliation (i.e., theoretically, individuals on the right should rate the
# the party as legitimate, whereas individuals on the left should not)

