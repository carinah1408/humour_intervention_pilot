### load packages ----
library(tidyverse)
library(dplyr)
library(remotes)
library(devtools)
install_github("mdelacre/Routliers")

### load dataset ----
pilot <- read_csv("data/pilot.csv") %>%
  as_tibble() %>%
  mutate(affiliation = as.factor(affiliation), 
         affiliation = recode(affiliation, 
                              '1' = 'Left',
                              '2' = 'Centre',
                              '3' = 'Right',
                              '4' = 'Not affiliated')) %>%
  print()



### mean age of participants ----
mean(pilot$age)
sd(pilot$age)

### affiliation distribution (no. of people, mean age) ----
pilot %>%
  group_by(affiliation) %>%
  summarise(., mean_age = mean(age), sd_age = sd(age), n = n())

### reaction to conditions ----
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

### reliability, normality, and validity ----

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

# construct validity further through correlation with related measures (convergent) and unrelated
# measures (discriminant)

## efficacy (factor analysis) ---- 

# EFA
library(corrplot)
library(car)

df_eff <- pilot %>%
  dplyr::select(poleff_1, poleff_2, poleff_3, orgaeff_1, orgaeff_2)

datamatrix <- cor(df_eff)
corrplot(datamatrix, method = "number")

KMO(r=cor(df_eff)) # Kaiser (1974) suggests a cutoff for determining the factorability of the data
# if KMO > .60, here, the total KMO is .72 and we can therefore conduct a factor analysis

cortest.bartlett(df_eff) # small values of the significant level indicate that a factor analysis 
# would be useful, here p < .001

det(cor(df_eff)) # positive determinant (0.052), indicating that factor analysis will probably run

# establishing factors

fafitfree <- fa(df_eff, nfactors = ncol(df_eff), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree <- data.frame(Factor_n = as.factor(1:n_factors),
                    Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) +
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs(title = "Scree Plot",
       subtitle = "Based on the unreduced correlation matrix") # 2 or 3 factors

parallel <- fa.parallel(df_eff) # 2 factors

# FA with fa function

fa.none <- fa(r= df_eff,
              nfactors = 2,
              fm= "pa",
              max.iter = 100,
              rotate = "Promax")
print(fa.none)

# FA with psych function (preferred)
library(GPArotation)

psych::fa.parallel(df_eff, fa= "fa") # suggested factors = 2
fa.none2 <- fa(df_eff,
              nfactors = 2,
              scores = "tenBerge")
print(fa.none2)

library(parameters)
parameters::model_parameters(fa.none2, sort= TRUE, threshold = "max")

# FA with factanal function

factanal.none <- factanal(df_eff, factors = 2, scores = c("regression"), rotation = "Promax")
print(factanal.none)

#  in orthogonal rotation, factor loadings = correlations (-1 to 1),if the factors are correlated 
# (oblique), the factor loading are regression coefficients (can be bigger than 1)

# Factor 1 (PolEff) explains 39% of the variables' variance; factor 2 explains 74% of the variables'
# variance

fa.diagram(fa.none)

# row scores

head(fa.none$scores)

regdata <- fa.none$scores
colnames(regdata) <- c("PolEff", "OrgEff")

# distribution

hist(pilot$poleff_1)
hist(pilot$poleff_2)
hist(pilot$poleff_3)

hist(pilot$orgaeff_1)
hist(pilot$orgaeff_2)

# outlier detection
poleff_1 <- pilot$poleff_1
poleff_1_mad <- Routliers::outliers_mad(x=poleff_1)
poleff_1_mad # no outliers

poleff_2 <- pilot$poleff_2
poleff_2_mad <- Routliers::outliers_mad(x=poleff_2)
poleff_2_mad # no outliers

poleff_3<- pilot$poleff_3
poleff_3_mad <- Routliers::outliers_mad(x=poleff_3)
poleff_3_mad # no outliers

orgaeff_1<- pilot$orgaeff_1
orgaeff_1_mad <- Routliers::outliers_mad(x=orgaeff_1)
orgaeff_1_mad # no outliers

orgaeff_2<- pilot$orgaeff_2
orgaeff_2_mad <- Routliers::outliers_mad(x=orgaeff_2)
orgaeff_2_mad # no outliers

# cronbach's alpha

key_2 <- list(
  poleff = c("poleff_1", "poleff_2", "poleff_3")
)

score.items(key_2, pilot) # alpha = 0.84

# create new variable "poleff"

pilot <- pilot %>%
  mutate(
    poleff = (poleff_1 + poleff_2 + poleff_3)/3
  )

key_3 <- list(
  poleff = c("orgaeff_1", "orgaeff_2")
)

score.items(key_3, pilot) # alpha = 0.93

# create new variable "orgaeff"

pilot <- pilot %>%
  mutate(
    orgaeff = (orgaeff_1 + orgaeff_2)/2
  )

# normality

hist(pilot$poleff)
describe(pilot$poleff) # rather normally distributed

hist(pilot$orgaeff)
describe(pilot$orgaeff) # rather normally distributed

### descriptive statistics ----

pilot %>%
  select(selfcat, endorse, poleff, orgaeff, stereo, legit, support) %>%
  psych::describe() %>%
  as_tibble(rownames="rowname")  %>%
  print()

# by group
summary <- pilot %>%
  dplyr::group_by(affiliation) %>%
  summarise(
    selfcat_min = min(selfcat),
    selfcat_max = max(selfcat),
    selfcat_mean = mean(selfcat),
    selfcat_sd = sd(selfcat),
    endorse_min = min(endorse),
    endorse_max = max(endorse),
    endorse_mean = mean(endorse),
    endorse_sd = sd(endorse),
    poleff_min = min(poleff),
    poleff_max = max(poleff),
    poleff_mean = mean(poleff),
    poleff_sd = sd(poleff),
    orgaeff_min = min(orgaeff),
    orgaeff_max = max(orgaeff),
    orgaeff_mean = mean(orgaeff),
    orgaeff_sd = sd(orgaeff),
    stereo_min = min(stereo),
    stereo_max = max(stereo),
    stereo_mean = mean(stereo),
    stereo_sd = sd(stereo),
    legit_min = min(legit),
    legit_max = max(legit),
    legit_mean = mean(legit),
    legit_sd = sd(legit),
    support_min = min(support),
    support_max = max(support),
    support_mean = mean(support),
    support_sd = sd(support)
  ) %>%
  mutate_at(vars(-affiliation), funs(round(., 2)))

summary <- summary %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value) 

names(summary) <- summary[1,]
summary <- summary[-1,]

### correlation ----

pilot_cor <-pilot %>%
  select(selfcat, endorse, stereo, poleff, orgaeff, legit, support) %>%
  round(., 2)

corrmatrix <- cor(pilot_cor)
corrplot(corrmatrix, method = "number")

library(Hmisc)
rcorr(as.matrix(pilot_cor)) %>%
  print()

library(apaTables)
apa.cor.table(pilot_cor,filename = "Correlation_pilot.doc",table.number = 1,show.conf.interval = F)

### bivariate relationships and mutlivariate outlier detection: stereo, poleff, ----
# orgaeff and legit and support; selfcat and legit and support

## stereo and legit
pilot %>%
  select(stereo, legit) %>%
  plot()

pilot %>%
  lm(legit ~ stereo, data = .) %>%
  abline()

legit_stereo <- lm(legit ~ stereo, data = pilot)
durbinWatsonTest(legit_stereo) # dw = 1.62 error independence met 

plot(legit_stereo) # normality met; homoscedasticity potentially violated; 
# potential outliers: ID26, 49, 112, but no influential cases

# checking homoscedasticity again
ncvTest(legit_stereo) # n.s. --> homoscedasticity seems to be ok

## stereo and support
pilot %>%
  select(stereo, support) %>%
  plot()

pilot %>%
  lm(legit ~ support, data = .) %>%
  abline()

support_stereo <- lm(support ~ stereo, data = pilot)
durbinWatsonTest(support_stereo) # dw = 1.77 error independence met

plot(support_stereo) # normality unclear, homoscedasticity potential violated; 
# potential outliers ID45, 74, 166, 134, 135, but no influential cases

# checking normality again
library(MASS)

sresid <- MASS::studres(support_stereo) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # normality violated --> robust regression

# checking homoscedasticity again
car::ncvTest(support_stereo) # sign., homoscedasiticy violated --> transform data 
# (but test on final model again)

# multivariate outliers stereo, legit and support (mcd)

legitimacy <- pilot$legit
stereotype <- pilot$stereo
legit_stereo_mcd <- Routliers::outliers_mcd(x = data.frame(legitimacy,stereotype))
legit_stereo_mcd # 13 outliers detected
Routliers::plot_outliers_mcd(legit_stereo_mcd, x = data.frame(legitimacy, stereotype))
# no influential cases (in line with diagnostics)

SupportIntention <- pilot$support
support_stereo_mcd <- Routliers::outliers_mcd(x = data.frame(supporting,stereotype))
support_stereo_mcd # 27 outliers found
Routliers::plot_outliers_mcd(support_stereo_mcd, x = data.frame(supporting, stereotype), pos_display = TRUE)
# slightly steeper slope without outliers 

# finding outliers' ID
outliers_support_stereo <- support_stereo_mcd$outliers_pos


## poleff and legit
pilot %>%
  select(poleff, legit) %>%
  plot()

pilot %>%
  lm(legit ~ poleff, data = .) %>%
  abline()

legit_poleff <- lm(legit ~ poleff, data = pilot)
durbinWatsonTest(legit_poleff) # dw = 1.62 independence of error is met

plot(legit_poleff) # homoscedasiticity potentially violated, normality seems ok
# ; outliers: ID13, 46, 197, but no influential cases

# checking homoscedasticity again
car::ncvTest(legit_poleff) # sign., homoscedasticity violated (transform data, but see in final model)

## poleff and support
pilot %>%
  select(poleff, support) %>%
  plot()

pilot %>%
  lm(support ~ poleff, data = .) %>%
  abline()

support_poleff <- lm(support ~ poleff, data = pilot)
durbinWatsonTest(support_poleff) # dw = 1.75, independence of errors met

plot(support_poleff) # homoscedasticity violated, normality?; outliers: ID45, 166, 170, no
# influential cases

# checking homoscedasticity again
car::ncvTest(support_poleff) # sign. homoscedasiticy violated

# checking normality again
sresid <- MASS::studres(support_poleff) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # normality slightly violated (robust regression)


# multivariate outliers poleff, legit and support (mcd)

Legitimacy <- pilot$legit
politicalefficacy <- pilot$poleff
legit_poleff_mcd <- Routliers::outliers_mcd(x = data.frame(politicalefficacy, legitimacy))
legit_poleff_mcd # 12 outliers detected
Routliers::plot_outliers_mcd(legit_poleff_mcd, x = data.frame(politicalefficacy, legitimacy))
# no influential cases

support_poleff_mcd <- Routliers::outliers_mcd(x = data.frame(politicalefficacy, supporting))
support_poleff_mcd # 61 outliers detected
Routliers::plot_outliers_mcd(support_poleff_mcd, x = data.frame(politicalefficacy, supporting))
# slope flater without outliers

# those in the centre, on the right and not affiliated push the slop up
outliers_support <- pilot %>% filter(support >= "4")

## orgaeff and legit
pilot %>%
  select(orgaeff, legit) %>%
  plot()

pilot %>%
  lm(legit ~ orgaeff, data = .) %>%
  abline()

legit_orgaeff <- lm(legit ~ orgaeff, data = pilot)
durbinWatsonTest(legit_orgaeff) # dw = 1.69, independence of errors met

plot(legit_orgaeff) # outliers: ID26, 45, 112 but no influential cases; normality met, 
# homoscedasticity unclear

# checking homoscedasticity again
car::ncvTest(legit_orgaeff) # n.s., homoscedasticity met

## orgaeff and support
pilot %>%
  select(orgaeff, support) %>%
  plot()

pilot %>%
  lm(support ~ orgaeff, data = .) %>%
  abline()

support_orgaeff <- lm(support ~ orgaeff, data = pilot)
durbinWatsonTest(support_orgaeff) # dw = 1.8 indepdendence errors met

plot(support_orgaeff) # homoscedasticity and normality potentially violated; outliers: ID45, 166, 174
# no influential cases

# checking homoscedasticity again
car::ncvTest(support_orgaeff) # sign. homoscedasticity violated

# checking normality again
sresid <- MASS::studres(support_orgaeff) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # normality violated (robust regression)

# multivariate outliers orgaeff, legit and support (mcd)

organizationalefficacy <- pilot$orgaeff
legit_orgaeff_mcd <- Routliers::outliers_mcd(x = data.frame(organizationalefficacy, legitimacy))
legit_orgaeff_mcd # 11 outliers detected
Routliers::plot_outliers_mcd(legit_poleff_mcd, x = data.frame(organizationalefficacy, legitimacy))
# no influential cases

support_orgaeff_mcd <- Routliers::outliers_mcd(x = data.frame(organizationalefficacy, supporting))
support_orgaeff_mcd # 14 outliers detected
Routliers::plot_outliers_mcd(support_poleff_mcd, x = data.frame(organizationalefficacy, supporting))
# slope flater without outliers (see above: those on the right, in the centre and not affiliated
# push the slope up)

## selfcat and legit and support

# selfcat and legit
pilot %>%
  select(selfcat, legit) %>%
  plot()

pilot %>%
  lm(legit ~ selfcat, data = .) %>%
  abline()

legit_selfcat <- lm(legit ~ selfcat, data = pilot)
durbinWatsonTest(legit_selfcat) # 1.8 independence of errors met

plot(legit_selfcat) # outliers: ID30, 96, 195; homoscedasticity potentially violated

# checking homoscedasticity again
car::ncvTest(legit_selfcat) # sign. homoscedasticity violated

# selfcat and support
pilot %>%
  select(selfcat, support) %>%
  plot()

pilot %>%
  lm(support ~ selfcat, data = .) %>%
  abline()

support_selfcat <- lm(support ~ selfcat, data = pilot)
durbinWatsonTest(support_selfcat) # 1.8 independence of errors met

plot(support_selfcat) # outliers: ID21, 99, 184; normality potentially violated, 
# homoscedasticity violated

# checking homoscedasticity again
car::ncvTest(support_selfcat) # homoscedasticity violated

# checking normality again
sresid <- MASS::studres(support_selfcat) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit) # normality seems slightly violated

# multivariate outliers selfcat, legit and support (mcd)

SocialIdentifification <- pilot$selfcat
legit_selfcat_mcd <- Routliers::outliers_mcd(x = data.frame(SocialIdentifification, Legitimacy))
legit_selfcat_mcd # 14 outliers detected
Routliers::plot_outliers_mcd(legit_poleff_mcd, x = data.frame(SocialIdentifification, Legitimacy))
# slope slightly steeper with outliers

support_selfcat_mcd <- Routliers::outliers_mcd(x = data.frame(selfcategorization, supporting))
support_selfcat_mcd # 24 outliers detected
Routliers::plot_outliers_mcd(support_poleff_mcd, x = data.frame(SocialIdentifification, SupportIntention))
# slope is strongly positively influenced by outliers (those on the right, centre and na)


legit_support_mcd <- Routliers::outliers_mcd(x = data.frame(Legitimacy, SupportIntention))
legit_support_mcd # 61 outliers
Routliers::plot_outliers_mcd(legit_support_mcd , x = data.frame(Legitimacy, SupportIntention))
legit_support_mcd$outliers_pos

# ID 2   8   9  10  18  22  27  29  32  33  35  37  42  44  45  46  52  55  56  68  71  76  81  91  96  99 104 123 124 127 133 
# 134 135 136 137 145 146 150 151 156 157 161 163 166 168 169 170 171 172 174 176 180 181 182 183 186 187 194 196 197 198 

outliers_legit_support <- filter(pilot, id == c(2, 8, 9, 10, 18, 22, 27, 29, 32, 33, 35, 37, 42, 44, 45, 46, 52, 55, 56, 68, 71, 76, 81, 91, 96, 99, 104, 123, 124, 127, 133,
                                                134, 135, 136, 137, 145, 146, 150, 151, 156, 157, 161, 163, 166, 168, 169, 170, 171, 172, 174, 176, 180, 181, 182, 183, 186, 187, 194, 196, 197, 198))

### dichotomize political affiliation ----
# (left vs right/centre/na) to run t-test on self-cat/ endorse, as well as legitimacy and support

pilot <- pilot %>%
  mutate(aff_split = dplyr::recode(affiliation,
    'Left' = 'Left',
    'Right' = 'Other',
    'Centre' = 'Other',
    'Not affiliated' = 'Other'))

t.test(selfcat ~ aff_split, data = pilot)

ggplot(pilot, aes(as.factor(aff_split), selfcat)) + 
  geom_point() + 
  labs(y = "Self-categorization", x = "Political affiliation")

t.test(endorse ~ aff_split, data = pilot)

t.test(legit ~ aff_split, data = pilot)

t.test(support ~ aff_split, data = pilot)

pilot %>% 
  group_by(aff_split) %>%
  summarise(., mean_selfcat = mean(selfcat), sd_selfcat = sd(selfcat),
            mean_endorse = mean(endorse), sd_endorse = sd(endorse),
            mean_legit = mean(legit), sd_legit = sd(legit),
            mean_support = mean(support), sd_support = sd(support))

### import process function----

source("scripts/process_function.R")

### mediational models ----

## mean-centre X and W (poleff, orgaeff, stereo, self-cat)

pilot <- pilot %>%
  mutate(
     cselfcat = scale(selfcat, scale = FALSE),
     cpoleff = scale(poleff, scale = FALSE), 
     corgaeff = scale(orgaeff, scale = FALSE),
     cstereo = scale(stereo, scale = FALSE),
     clegit = scale(legit, scale = FALSE)
  )

## simple mediation: eff (pol/orga) = X, legit = M, support = Y

# OLD process (data=pilot,y="support",x="cpoleff",m="legit",total=1,stand = 1, normal=1, model=4,seed=31216)
# NEW 
process(data=pilot, y = "support", x = "poleff", m = "legit", total = 1, model = 4, seed = 311022)


#OLD process (data=pilot,y="support",x="corgaeff",m="legit",total=1,stand = 1, normal=1, model=4,seed=31216)

#NEW
process(data = pilot, y = "support", x = "orgaeff", m = "legit", total = 1, model = 4, seed = 311022)

## multiple regression: testing effect of poleff vs orgaeff on legitimacy

polorgaeff_legit <- lm(legit~ poleff + orgaeff, data = pilot)
summary(polorgaeff_legit)

lm.beta::lm.beta(polorgaeff_legit)

car::durbinWatsonTest(polorgaeff_legit) # 1.65 independence of errors met
car::vif(polorgaeff_legit) 
plot(polorgaeff_legit) # homoscedasticity unclear
car::ncvTest(polorgaeff_legit) # sign. homoscedasticity violated

# bootstrapped CI

library(boot)

bootReg <-function(formula, data, indices)
{
  d <- data[indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

boot_polorgaeff_legit <- boot(statistic = bootReg, formula = legit~ poleff + orgaeff, data = pilot, R = 5000)
boot.ci(boot_polorgaeff_legit, type = "bca", index = 1)
boot.ci(boot_polorgaeff_legit, type = "bca", index = 2)
boot.ci(boot_polorgaeff_legit, type = "bca", index = 3)

## simple mediation: stereotype = X, legit = M, support = Y

#OLD process (data=pilot,y="support",x="cstereo",m="legit",total=1,stand = 1, normal=1, model=4,seed=31216)

#NEW
process(data = pilot, y = "support", x = "stereo", m = "legit", total = 1, model = 4, seed = 311022)

## multiple regression: testing effect of poleff vs orgaeff vs stereo on legitimacy

polorgaeffstereo_legit <- lm(legit~ poleff + orgaeff + stereo, data = pilot)
summary(polorgaeffstereo_legit)

lm.beta::lm.beta(polorgaeffstereo_legit)

car::durbinWatsonTest(polorgaeffstereo_legit) # 1.65 independence of errors met
car::vif(polorgaeffstereo_legit) 
plot(polorgaeffstereo_legit) # homoscedasticity unclear
car::ncvTest(polorgaeffstereo_legit) # n.s.

boot_polorgaeffstereo_legit <- boot(statistic = bootReg, formula = legit~ poleff + orgaeff + stereo, data = pilot, R = 5000)
boot.ci(boot_polorgaeffstereo_legit, type = "bca", index = 1)
boot.ci(boot_polorgaeffstereo_legit, type = "bca", index = 2)
boot.ci(boot_polorgaeffstereo_legit, type = "bca", index = 3)
boot.ci(boot_polorgaeffstereo_legit, type = "bca", index = 4)

## moderated mediation: poleff = X, legit = M, self-cat = W (on a-, b-, and c-path), support = Y

# interaction on a-path

# in process

#OLD process(data = pilot, y = "support", x = "cpoleff", m = "legit", w = "cselfcat", model = 7, plot = 1, seed = 42517)
# interaction n.s.

#NEW
process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat", model = 7, plot = 1, center = 2, moments = 1, seed = 311022)

# interaction on b-path

# in process

#OLD process (data=pilot,y="support",x="clegit",w="cselfcat",cov="poleff",model=1,plot=1, jn= 1) # at cselfcat of above 1.06, the interaction turns sign.
#OLDprocess (data=pilot,y="support",x="clegit",w="selfcat",cov="poleff",model=1,plot=1, jn= 1) 
# when poleff is covariate,  this equals to scores of =/> 2.20 on the scale; when orgaeff is covariate, this equals to 
# 1.95 on the scale; when competence is covariate, this equals to 2.20 on the scale
#NEW
process(data = pilot, y = "support", x = "legit", w = "selfcat", cov = "poleff", model = 1, moments = 1, plot = 1, jn = 1)
process(data = pilot, y = "support", x = "legit", w = "selfcat", cov = "orgaeff", model = 1, moments = 1, plot = 1, jn = 1)
process(data = pilot, y = "support", x = "legit", w = "selfcat", cov = "stereo", model = 1, moments = 1, plot = 1, jn = 1)

#OLD process(data = pilot, y = "support", x = "cpoleff", m = "clegit", w = "cselfcat", model = 14, plot = 1, seed = 42516)
#NEW
process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat", model = 14, center = 2, moments = 1, plot =1, seed = 311022)

# interaction on c-path

#OLD process (data=pilot,y="support",x="cpoleff",w="cselfcat",cov="legit",model=1,plot=1, jn= 1) # at a cselfcat of above -1.27 the interaction turns sign.
# OLD process (data=pilot,y="support",x="cpoleff",w="selfcat",cov="legit",model=1,plot=1, jn= 1) 
# with pol as predictor (and legit as cov), this equals to 1.90 on the scale; when orga = predictor, this 
# equals to 2.84 on scale; when stereo as predictor, this equals to 2.53 on scale
#NEW
process(data = pilot, y = "support", x= "poleff", w = "selfcat", cov = "legit", model = 1, moments = 1, plot = 1, jn = 1)
process(data = pilot, y = "support", x= "orgaeff", w = "selfcat", cov = "legit", model = 1, moments = 1, plot = 1, jn = 1)
process(data = pilot, y = "support", x= "stereo", w = "selfcat", cov = "legit", model = 1, moments = 1, plot = 1, jn = 1)

#OLD process(data = pilot, y = "support", x = "cpoleff", m = "legit", w = "cselfcat", model = 5, plot = 1, seed = 42515)
#NEW
process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat", model = 5, center = 2, moments = 1, plot =1, seed = 311022)

## moderated mediation: orgaeff = X, legit = M, self-cat = W (on a-, b-, and c-path), support = Y

# interaction between selfcat and orgaeff

# interaction on a-path

# in process

#OLD process(data = pilot, y = "support", x = "corgaeff", m = "legit", w = "cselfcat", model = 7, plot = 1, seed = 42514)
#NEW
process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", center = 2, moments = 1, model = 7, plot = 1, seed = 311022)
#NEW

# interaction on b-path

# in process

#OLD process (data=pilot,y="support",x="clegit",w="cselfcat",cov="orgaeff",model=1,plot=1, jn= 1) # at cselfcat of above -1.10, the interaction turns sign.

#OLDprocess(data = pilot, y = "support", x = "corgaeff", m = "clegit", w = "cselfcat", model = 14, plot = 1, seed = 42513)
#NEW
process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", center = 2, moments = 1, model = 14, plot = 1, seed = 311022)

# interaction on c-path

#OLDprocess (data=pilot,y="support",x="corgaeff",w="cselfcat",cov="legit",model=1,plot=1, jn= 1)
#OLDprocess(data = pilot, y = "support", x = "corgaeff", m = "legit", w = "cselfcat", model = 5, plot = 1, seed = 42512)

#NEW
process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", model = 5, center = 2, moment = 2, plot = 1, seed = 311022)


## moderated mediation: stereotype = X, legit = M, self-cat = W (on a, on b), support = Y

# interaction on a-path

#OLD process(data = pilot, y = "support", x = "cstereo", m = "legit", w = "cselfcat", model = 7, plot = 1, seed = 42511)
#NEW
process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", model = 7, center = 2, moment =1, plot =1, seed = 311022)

# interaction on b-path

#OLDprocess (data=pilot,y="support",x="clegit",w="cselfcat",cov="cstereo",model=1,plot=1, jn= 1) # at cselfcat of above -1.10, the interaction turns sign.

#OLDprocess(data = pilot, y = "support", x = "cstereo", m = "clegit", w = "cselfcat", model = 14, plot = 1, seed = 42510)
#NEW
process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", center = 2, moments =1, plot =1, model = 14, seed = 311022)

# interaction on c-path

#OLDprocess (data=pilot,y="support",x="cstereo",w="cselfcat",cov="legit",model=1,plot=1, jn= 1)
#OLDprocess(data = pilot, y = "support", x = "cstereo", m = "legit", w = "cselfcat", model = 5, plot = 1, seed = 42509)
#NEW
process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", center = 2, moments = 1, model = 5, plot =1, seed = 311022)

# for display: re-do models (5 and 14) with automatic mean-centring and robust CI

process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 5, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat",modelbt =1, center = 1, model = 7, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "poleff", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 14, plot = 1, seed = 04822)

process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 5, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 7, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "orgaeff", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 14, plot = 1, seed = 04822)

process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 5, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 7, plot = 1, seed = 04822)
process(data = pilot, y = "support", x = "stereo", m = "legit", w = "selfcat", modelbt =1, center = 1, model = 14, plot = 1, seed = 04822)
