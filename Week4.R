################################################################################
###
### Data analysis in R: Week 4
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Marx Library
### Yale University
###
################################################################################

## Load packages ##

library(tidyverse)
library(rstatix)

## Load the data ##

# Download and read in files for Study1a and Study 2: https://osf.io/5gtyv/

Study1a <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week4/Study1a_Data.csv")

Study2 <- read.csv("/Users/sgc/Documents/Workshops/Data analysis in R/Week4/Study2_Data.csv")

## Study 1a ##

# Data manipulation

# Subset to filter for participants that were not suspicious (Suspicion == 0), and 
# did pay attention (Exclude == 1).
# Recode the condition variable to make it more interpretable (.5 = Implicit, -.5 = Explicit).

Study1a_analysis <- Study1a %>%
  filter(Suspicion == 0 & Exclude == 1) %>%
  mutate(Condition = case_when(Condition == .5 ~ "Implicit",
                               Condition == -.5 ~ "Explicit"))

nrow(Study1a_analysis)
tapply(Study1a_analysis$Condition, Study1a_analysis$Condition, length)

# Analyses 

# Compute correlation matrix of accountability, punishment, concern, and reform.

cor_matrix <- cbind(Study1a_analysis$xAccountability, Study1a_analysis$xPunish,
                    Study1a_analysis$xConcern, Study1a_analysis$xReform)

cor_matrix2 <- Study1a_analysis %>%
  select(xAccountability, xPunish, xConcern, xReform)

cor(cor_matrix2)

# t-test: condition and accountability. 

t.test(Study1a_analysis$xAccountability ~ Study1a_analysis$Condition)

# t-test: condition and punishment

t.test(Study1a_analysis$xPunish ~ Study1a_analysis$Condition, var = TRUE)

# t-test: condition and concern

t.test(Study1a_analysis$xConcern ~ Study1a_analysis$Condition, var = TRUE)

# t-test: condition and reform

t.test(Study1a_analysis$xReform ~ Study1a_analysis$Condition, var = TRUE)

# Visualization: bar graph type of judgment (accountability, punishment, concern, reform) 
# on the x-axis,average judgment on the y-axis, and fill bars according to condition.
# Don't forget error bars (+/- 1 standard error).
# HINT: first transpose the data to long format, then create a summary table, then plot the data.

Study1a_long <- Study1a_analysis %>%
  select(Pnum, Condition, xAccountability, xPunish, xConcern, xReform) %>%
  gather(key = "Judgment_type", value = "Judgment", -Pnum, -Condition)

Study1a_summary <- Study1a_long %>%
  group_by(Condition, Judgment_type) %>%
  summarize(mean = mean(Judgment),
            sd = sd(Judgment),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study1a_summary, aes(x = Judgment_type, y = mean, fill = Condition)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, 
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(1, 7))

ggplot(data = Study1a_summary, mapping = aes(x = Judgment_type, y = mean, fill = Condition)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, 
                position = position_dodge(.9))

## Study 2 ##

# Data manipulation 

# Recode levels of BiasCondition and HarmCondition to read more intuitively
# (.5 = Implicit, -.5 = Explicit; .5 = High, -.5 = Low).
# Filter to include only participants that were not suspicios (Suspicion == 0).

Study2_analysis <- Study2 %>% 
  filter(Suspicion == 0) %>%
  mutate(BiasCondition = case_when(BiasCondition == .5 ~ "Implicit",
                                   BiasCondition == -.5 ~ "Explicit"),
         HarmCondition = case_when(HarmCondition == .5 ~ "High",
                                   HarmCondition == -.5 ~ "Low"))

# Analyses

# Two-way ANOVA: harm/bias on accountability.

anova_test(Study2_analysis,
           dv = xAccountability,
           between = c(BiasCondition, HarmCondition))

anova1 <- lm(xAccountability ~ BiasCondition*HarmCondition, data = Study2_analysis)
summary(anova1)
anova(anova1)

# Two-way ANOVA: harm/bias on punishment.

anova_test(Study2_analysis,
           dv = xPunish,
           between = c(BiasCondition, HarmCondition))

# Two-way ANOVA: harm/bias on concern.

anova_test(Study2_analysis,
           dv = xConcern,
           between = c(BiasCondition, HarmCondition))

# Two-way ANOVA: harm/bias on reform.

anova_test(Study2_analysis,
           dv = xReform,
           between = c(BiasCondition, HarmCondition))

# Plot the data just for accountability.
# Create a bar graph with Bias Condition on the x-axis, and average
# accountability judgment on the y-axis. Fill the bars by Harm Condition.

Study2_summary <- Study2_analysis %>%
  select(Pnum, BiasCondition, HarmCondition, xAccountability) %>%
  group_by(BiasCondition, HarmCondition) %>%
  summarize(mean = mean(xAccountability),
            sd = sd(xAccountability),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study2_summary, aes(x = BiasCondition, y = mean, fill = HarmCondition)) +
  geom_col(position = "dodge") +
  coord_cartesian(ylim = c(1, 7)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), 
                position = position_dodge(.9), width = .2)


