---
title: "Data Analysis"
author: "Freya Murray"
date: "2023-01-13"
output:
  word_document: default
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Dependencies

```{r, warning=FALSE, message=FALSE}
library(tidyverse)
library(dplyr) 
library(ltm)
library(pwr)
```

#### Power Analysis

```{r}
pwr.f2.test(u=1, f2=(.05/(1-.05)), sig.level = .008, power = .8)
```

Power analysis reveals that this study requires n=234 participants to be appropriately powered (based on Bonferroni correction).

#### Reading in data

```{r message=FALSE}
rawdata <- read_csv("DATA_FINAL.csv")
```

### Data wrangling

As the raw data is in a very long, complex format, some wrangling is required to make it workable

#### Removing unnecessary columns

```{r warning=FALSE}
datatidy <- rawdata %>% dplyr::select(-project_id, -quest_id, -q_id, -order) %>% 
  filter(starttime > "2022-11-10 15:19:38") %>% dplyr::select(-starttime, -endtime, -user_sex, -user_age, -session_id, -user_status) %>%
  transform(dv = as.numeric(dv)) 
```

#### Recoding

Recoding the data to clearly identify the subscales of both questionnaires. The following letters denote each subscale:

MEQ:

-   R = reactive musical behaviour

-   S = social uplift

-   A = affective reactions

-   C = commitment to music

-   I = innovative musical aptitude

-   P = positive psychotropic effects

AQ:

-   AD = attention to detail

-   AS = attention switching

-   CO = communication

-   IM = imagination

-   SS = social skill

```{r}
recoded <- datatidy %>% mutate(q_name = recode(q_name, #MEQ 
                                                      "R10" = "R",
                                                      "R117" = "R",
                                                      "R122" = "R",
                                                      "R21" = "R",
                                                      "R30" = "R",
                                                      "R42" = "R",
                                                      "R67" = "R",
                                                      "R78" = "R",
                                                      "R78" = "R",
                                                      "R99" = "R",
                                                      
                                                      "S29" = "S",
                                                      "S52" = "S",
                                                      "S65" = "S",
                                                      "S69" = "S",
                                                      
                                                      "A107" = "A",
                                                      "A123" = "A",
                                                      "A137" = "A", 
                                                      "A49" = "A",
                                                      "A50" = "A",
                                                      "A60" = "A",
                                                      "A76" = "A", 
                                                      "A77" = "A", 
                                                      "A85" = "A", 
                                                      "A96" = "A",
                                                      
                                                      "C128" = "C",
                                                      "C2" = "C",
                                                      "C37" = "C", 
                                                      "C56" = "C", 
                                                      "C57" = "C", 
                                                      "C64" = "C", 
                                                      "C68" = "C", 
                                                      
                                                      "I111" = "I",
                                                      "I114" = "I",
                                                      "I130" = "I",
                                                      "I16" = "I",
                                                      "I24" = "I",
                                                      "I28" = "I",
                                                      "I39" = "I",
                                                      
                                                      "P104" = "P",
                                                      "P119" = "P",
                                                      "P134" = "P",
                                                      "P135" = "P",
                                                      "P136" = "P",
                                                      "P138" = "P",
                                                      "P140" = "P",
                                                      "P141" = "P",
                                                      "P27" = "P",
                                                      "P31" = "P",
                                                      "P34" = "P",
                                                      "P55" = "P",
                                                      "P82" = "P",
                                                      "P86" = "P",
                                                      "P91" = "P",
                                                      "P93" = "P",
                                                      
                                                      #AQ
                                                      "AD_12" = "AD",
                                                      "AD_19" = "AD",
                                                      "AD_23" = "AD",
                                                      "AD_28" = "AD",
                                                      "AD_29" = "AD",
                                                      "AD_30" = "AD",
                                                      "AD_49" = "AD",
                                                      "AD_5" = "AD",
                                                      "AD_6" = "AD",
                                                      "AD_9" = "AD",
                                                      
                                                      "AS_10" = "AS",
                                                      "AS_16" = "AS",
                                                      "AS_25" = "AS",
                                                      "AS_32" = "AS",
                                                      "AS_34" = "AS",
                                                      "AS_37" = "AS",
                                                      "AS_4" = "AS",
                                                      "AS_43" = "AS",
                                                      "AS_46" = "AS",
                                                      "AS_q2" = "AS",
                                                      
                                                      "C_17" = "CO",
                                                      "C_18" = "CO",
                                                      "C_26" = "CO",
                                                      "C_27" = "CO", 
                                                      "C_31" = "CO",
                                                      "C_33" = "CO",
                                                      "C_35" = "CO",
                                                      "C_38" = "CO",
                                                      "C_39" = "CO", 
                                                      "C_7" = "CO",
                                                      
                                                      "I_14" = "IM",
                                                      "I_20" = "IM",
                                                      "I_21" = "IM",
                                                      "I_24" = "IM",
                                                      "I_3" = "IM",
                                                      "I_40" = "IM",
                                                      "I_41" = "IM",
                                                      "I_42" = "IM",
                                                      "I_50" = "IM",
                                                      "I_8" = "IM",
                                                      
                                                      "SS_1" = "SS",
                                                      "SS_11" = "SS",
                                                      "SS_13" = "SS",
                                                      "SS_15" = "SS",
                                                      "SS_22" = "SS",
                                                      "SS_36" = "SS",
                                                      "SS_44" = "SS",
                                                      "SS_45" = "SS",
                                                      "SS_47" = "SS",
                                                      "SS_48" = "SS")) %>%
  group_by(q_name)
```

#### Calculating subscale totals

```{r}
subscale_totals <- recoded %>% group_by(user_id, q_name) %>%
  mutate(total_subs = sum(dv)) 
```

#### Pivoting to a more workable format

```{r warning=FALSE}
pivd <- subscale_totals %>%
  dplyr::select(-quest_name, -dv) %>%
  pivot_wider(names_from = q_name, values_from = total_subs, values_fn = mean) 
```

#### Standardising MEQ Scores

As the MEQ does not have equal numbers of questions in each subscale, equal values hold unequal weights. To standardise each subscale total, I divide the original subscale total by the number of questions in the subscale. I then multiply by ten to put MEQ scores in the same ballpark area range as the AQ scores, for better aesthetics in figures.

Additionally, the MEQ is scored as a mean, rather than a sum, so I will further divide each value by the number if questions in each subscale.

```{r message=FALSE}
std <- pivd %>% mutate(A = ((A/10)*10)/10, C = ((C/7)*10)/7, I = ((I/7)*10)/7, P = ((P/16)*10)/10, R = ((R/9)*10)/9, S = ((S/4)*10)/4) %>% mutate_if(is.numeric, round, 1)  %>%
  mutate("total_aq" = (IM + CO + AD + AS + SS), "total_meq" = (P+I+S+C+A+R)) %>%
  na.omit()
```

### Exclusions

Participants were excluded from they analysis if they:

-   *Did not complete both questionnaires*

-   Are under 18

-   Did not put English as their first language

-   Did not identify as female/male/non-binary

-   Did not consent to be included in the study

```{r}
excl <- std %>% na.omit() %>%
  filter(englang == "1",age >= "18", consent == "1", gender <="3") %>%
  transform(age = as.numeric(age), gender = as.character(gender), diagnosis = as.character(diagnosis)) %>% 
  dplyr::select(-consent, -englang)
```

198 participants were excluded for above reasons.

#### Cronbach's Alpha

##### AQ

```{r}
cronbach_aq <- excl %>% dplyr::select(user_id, IM, CO, AS, AD, SS) %>% 
  na.omit() %>%
  pivot_longer(2:6, names_to = "qtype", values_to = "score") %>%
  pivot_wider(names_from = qtype, values_from = score) %>%
  dplyr::select(-user_id)

cronbach.alpha(cronbach_aq, CI=TRUE)
```

##### MEQ

```{r}
cronbach_meq <- excl %>% dplyr::select(user_id, A, C, S, R, I, P) %>% 
  na.omit()%>%
  pivot_longer(2:7, names_to = "qtype", values_to = "score") %>%
  pivot_wider(names_from = qtype, values_from = score) %>%
  dplyr::select(-user_id)

cronbach.alpha(cronbach_meq, CI=TRUE)
```

### Demographics

```{r}
demographics <- excl %>% ungroup() %>%
  dplyr::select(age, gender, diagnosis) %>%
  mutate(gender = recode(gender, "1" = "Female", "2" = "Male", "3" = "Non-Binary"), diagnosis = recode(diagnosis, "1" = "Diagnosed", "2" = "No Diagnosis")) %>% na.omit() # one participant did not disclose their age

demog_stats <- demographics %>% 
  summarise("total_n" = n(), "total_f" = sum(gender == "Female"), "total_m" = sum(gender =="Male"), "total_nb" = sum(gender =="Non-Binary"), "mean_age" = mean(age), "median_age" = median(age), "sd_age" = sd(age), "min_age" = min(age), "max_age" = max(age), "total_non_diagnosed" = sum(diagnosis=="No Diagnosis"), "total_diagnosed" = sum(diagnosis =="Diagnosed"))

print(demog_stats) 
```

#### Demographic graphs

```{r}
ggplot(demographics, aes(age)) +
  geom_density(fill = "black", alpha = .2) +
  geom_vline(xintercept = demog_stats$mean_age, col = "red") +
  geom_vline(xintercept = demog_stats$median_age)+
  theme_bw() +
  labs(x = "Age", y = "Density", title = "Age Distribution", caption = "Red: mean
       Black: median
       
       This graph indicates a strong positive skew in the age data. This is because a large number of participants were recruited from 
       the pool of first year psychology undergraduates at the University of Glasgow.")
```

```{r}
ggplot(demographics, aes(gender, age, group = gender, col=gender)) +
geom_point(position = position_jitter(.1), alpha = .5) +
    stat_summary(geom = "crossbar",fun = mean, fun.min = mean, fun.max = mean,width = .5, size = .4) +
  theme_bw() +
  labs(x = "Gender", y = "Age", title = "Age Distributions by Gender")
```

### Graphs

#### Setting parameters of the overall data

```{r}
#AQ
mean_aq <- mean(excl$total_aq)
median_aq <- median(excl$total_aq)
#MEQ
mean_meq <- mean(excl$total_meq)
median_meq <- median(excl$total_meq)
```

#### Wrangling original data to investigate MEQ subscales

```{r}
mfx <- excl %>% dplyr::select(-age, -gender, - diagnosis, - IM, -CO, -AS, -AD, -SS) %>%
  pivot_longer(cols = 2:7, names_to = "qtype", values_to = "total") %>%
  group_by(user_id)
```

#### Wrangling original data to investigate AQ subscales

```{r}
mfx_aq <- excl %>% dplyr::select(-age, -gender, - diagnosis, -total_aq, -total_meq, -P, -I, -S, -C, -A, -R) %>%
  pivot_longer(cols = 2:6, names_to = "qtype", values_to = "total") %>%
  group_by(user_id)
```

#### Wrangling to investigate all subscales

```{r}
mfx_all <- excl %>% dplyr::select(-age, -gender, - diagnosis, -total_aq, -total_meq) %>%
  pivot_longer(cols = 7:12, names_to = "qtype", values_to = "total") %>%
  group_by(user_id)
```

#### Figures

```{r}
ggplot(excl, aes(total_aq)) +
  geom_density(fill = "chartreuse3", alpha=.2) +
  geom_vline(xintercept = mean_aq, col = "red") +
  geom_vline(xintercept = median_aq) +
  theme_bw() +
  labs(x = "Total AQ Score", y = "Density", title = "AQ Score Distribution", caption = "Red: mean
       Black: median")
```

```{r}
ggplot(mfx_aq, aes(total, fill=qtype)) +
  geom_density(alpha = .4) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(title= "Distribution of AQ Subscale Scores", x = "AQ Subscale Score", y="AQ Subscale Score Density", caption = "All approximately normally distributed, Attention Switching (AS) heavy negative skew")
```

```{r}
ggplot(mfx, aes(total, fill=qtype)) +
  geom_density(alpha = .4) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(title= "Distribution of MEQ Subscale Scores", x = "MEQ Subscale Score", y="MEQ Subscale Score Density", caption = "All approximately normally distributed")
```

```{r}
ggplot(mfx, aes(total_aq, total, col=qtype)) +
  geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(x="Total AQ Score", y="MEQ Subscale Score", title = "MEQ Subscales by total AQ Linear Relationships")
```

### Analysis

#### A priori power analysis

```{r}
pwr.f2.test(u=1, v=235, f2=(.05/(1-.05)), sig.level = .008)
```

A priori power analysis shows that this study is suitably powered (\beta = .197).

#### Hypothesis Testing

###### \*Positive psychotropic effects

```{r}
reg_p <- lm(total_aq ~ P, excl)
summary(reg_p) # significant
```

###### Innovative musical aptitude

```{r}
reg_i <- lm(total_aq ~ I, excl)
summary(reg_i)
```

###### \*Social uplift

```{r}
reg_s <- lm(total_aq ~ S, excl)
summary(reg_s) #significant
```

###### \*Commitment to music

```{r}
reg_c <- lm(total_aq ~ C, excl)
summary(reg_c) # significant
```

###### Affective reactions

```{r}
reg_a <- lm(total_aq ~ A, excl)
summary(reg_a)
```

###### Reactive musical behaviour

```{r}
reg_r <- lm(total_aq ~ R, excl)
summary(reg_r)
```

#### Exploratory Analysis

##### Communication \* bMEQ subscales

```{r}
ggplot(mfx_all, aes(CO, total, col = qtype)) +
geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(y="Brief-MEQ Subscale Score ", x="Communication Score", title = "Communication by b-MEQ Subscales Linear Relationships")
```

###### \*Positive psychotropic effects

```{r}
ereg_CO_P <- lm(CO ~ P, excl)
summary(ereg_CO_P)
```

###### Innovative Musical Aptitude

```{r}
ereg_CO_I <- lm(CO ~ I, excl)
summary(ereg_CO_I)
```

###### Social Uplift

```{r}
ereg_CO_S <- lm(CO ~ S, excl)
summary(ereg_CO_S)
```

###### Commitment to Music

```{r}
ereg_CO_C <- lm(CO ~ C, excl)
summary(ereg_CO_C)
```

###### Affective Reaction

```{r}
ereg_CO_A <- lm(CO ~ A, excl)
summary(ereg_CO_A)
```

###### Reactive Musical Behaviour

```{r}
ereg_CO_R <- lm(CO ~ R, excl)
summary(ereg_CO_R)
```

##### Imagination

```{r}
ggplot(mfx_all, aes(IM, total, col = qtype)) +
geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(x="Imagination Score", y="Brief-MEQ Subscale Score", title = "Imagination by b-MEQ Subscales Linear Relationships")
```

###### Positive psychotropic effects

```{r}
ereg_IM_P <- lm(IM ~ P, excl)
summary(ereg_IM_P)
```

###### Innovative Musical Aptitude

```{r}
ereg_IM_I <- lm(IM ~ I, excl)
summary(ereg_IM_I)
```

###### Social Uplift

```{r}
ereg_IM_S <- lm(IM ~ S, excl)
summary(ereg_IM_S)
```

###### Commitment to Music

```{r}
ereg_IM_C <- lm(IM ~ C, excl)
summary(ereg_IM_C)
```

###### \*Affective Reaction

```{r}
ereg_IM_A <- lm(IM ~ A, excl)
summary(ereg_IM_A)
```

###### Reactive Musical Behaviour

```{r}
ereg_IM_R <- lm(IM ~ R, excl)
summary(ereg_IM_R)
```

##### Attention to Detail

```{r}
ggplot(mfx_all, aes(AD, total, col = qtype)) +
geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(x="Brief-MEQ Subscale Score ", y="Attention to Detail Score", title = "Attention to Detail by b-MEQ Subscales Linear Relationships")
```

###### \*Positive Psychotropic Effects

```{r}
ereg_AD_P <- lm(AD ~ P, excl)
summary(ereg_AD_P)
```

###### \*Innovative Musical Aptitude

```{r}
ereg_AD_I <- lm(AD ~ I, excl)
summary(ereg_AD_I)
```

###### Social Uplift

```{r}
ereg_AD_S <- lm(AD ~ S, excl)
summary(ereg_AD_S)
```

###### \*Commitment to Music

```{r}
ereg_AD_C <- lm(AD ~ C, excl)
summary(ereg_AD_C)
```

###### \*Affective Reaction

```{r}
ereg_AD_A <- lm(AD ~ A, excl)
summary(ereg_AD_A)
```

###### \*Reactive Musical Behaviour

```{r}
ereg_AD_R <- lm(AD ~ R, excl)
summary(ereg_AD_R)
```

##### Attention Switching

```{r}
ggplot(mfx_all, aes(AS, total, col = qtype)) +
geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(x="Brief-MEQ Subscale Score ", y="Attention Switching Score", title = "Attention Switching by b-MEQ Subscales Linear Relationships")
```

###### \*Positive Psychotropic Effects

```{r}
ereg_AS_P <- lm(AS ~ P, excl)
summary(ereg_AS_P)
```

###### Innovative Musical Aptitude

```{r}
ereg_AS_I <- lm(AS ~ I, excl)
summary(ereg_AS_I)
```

###### \*Social Uplift

```{r}
ereg_AS_S <- lm(AS ~ S, excl)
summary(ereg_AS_S)
```

###### \*Commitment to Music

```{r}
ereg_AS_C <- lm(AS ~ C, excl)
summary(ereg_AS_C)
```

###### \*Affective Reaction

```{r}
ereg_AS_A <- lm(AS ~ A, excl)
summary(ereg_AS_A)
```

###### Reactive Musical Behaviour

```{r}
ereg_AS_R <- lm(AS ~ R, excl)
summary(ereg_AS_R)
```

##### Social Skills

```{r}
ggplot(mfx_all, aes(SS, total, col = qtype)) +
geom_point(position = position_jitter(.5), alpha = .5) +
  geom_smooth(formula = y ~ x, method = lm, se=FALSE, col = "black", size = .7) +
  theme_bw() +
  facet_wrap(~qtype) +
  labs(x="Brief-MEQ Subscale Score ", y="Social Skills Score", title = "Social Skills by b-MEQ Subscales Linear Relationships")
```

###### Positive Psychotropic Effects

```{r}
ereg_SS_P <- lm(SS ~ P, excl)
summary(ereg_SS_P)
```

###### \*Innovative Musical Aptitude

```{r}
ereg_SS_I <- lm(SS ~ I, excl)
summary(ereg_SS_I)
```

###### \*Social Uplift

```{r}
ereg_SS_S <- lm(SS ~ S, excl)
summary(ereg_SS_S)
```

###### Commitment to Music

```{r}
ereg_SS_C <- lm(SS ~ C, excl)
summary(ereg_SS_C)
```

###### Affective Reaction

```{r}
ereg_SS_A <- lm(SS ~ A, excl)
summary(ereg_SS_A)
```

###### Reactive Musical Behaviour

```{r}
ereg_SS_R <- lm(SS ~ R, excl)
summary(ereg_SS_R)
```
