### Practice your R skills: Analyzng candy ratings ###
### Sara Gottlieb-Cohen, StatLab Manager           ###

# Load packages 

library(tidyverse)
library(rstatix)

# Load the data for Study 1a and Study 2

Study1a <- read_csv("https://raw.githubusercontent.com/sarago88/ImplicitBias/master/Study1a_Data.csv")

Study2 <- read_csv("https://raw.githubusercontent.com/sarago88/ImplicitBias/master/Study2_Data.csv")

## Analyze the data: Study 1a ##

# Subset the data to only include individuals who were not suspicious (Suspicion == 0), and 
# did pay attention (Exclude == 1).
# Also recode the condition variable to make it more interpretable (.5 = Implicit, -.5 = Explicit).

Study1a_analysis <- Study1a %>%
  filter(Suspicion == 0 & Exclude == 1) %>%
  mutate(Condition = case_when(Condition == .5 ~ "Implicit",
                                  Condition == -.5 ~ "Explicit"))

# Confirm that we now have 267 observations.

nrow(Study1a_analysis)

# Create a correlation matrix of accountability, punishment, concern, and reform.
# First create a matrix with only the variables of interest. 
# I have demonstrated two ways to perform this step - the first uses cbind(), and
# the second uses tidyverse notation.

cor_matrix <- cbind(Study1a_analysis$xAccountability, Study1a_analysis$xPunish,
                     Study1a_analysis$xConcern, Study1a_analysis$xReform)

cor_matrix2 <- Study1a_analysis %>%
  select(xAccountability, xPunish, xConcern, xReform)

cor(cor_matrix)
cor(cor_matrix2)

# Perform a series of t-tests to test for whether accountability, punishment,
# concern and reform differ between conditions.

t.test(Study1a_analysis$xAccountability ~ Study1a_analysis$Condition)

t.test(Study1a_analysis$xPunish ~ Study1a_analysis$Condition, var = TRUE)

t.test(Study1a_analysis$xConcern ~ Study1a_analysis$Condition, var = TRUE)

t.test(Study1a_analysis$xReform ~ Study1a_analysis$Condition, var = TRUE)

# Visualize the data: create a bar graph with type of judgment (accountability, punishment, 
# concern, reform) on the x-axis, average judgment on the y-axis, and fill bars according to condition.
# Don't forget error bars (+/- 1 standard error).

# The data must be in a summary table prior to graphing it. First reshape the data to long format, 
# then create a summary table, then plot the data.

Study1a_long <- Study1a_analysis %>%
  select(Pnum, Condition, xAccountability, xPunish, xConcern, xReform) %>%
  gather(key = "Judgment_type", value = "Judgment", -Pnum, -Condition)

Study1a_summary <- Study1a_long %>%
  group_by(Judgment_type, Condition) %>%
  summarize(mean = mean(Judgment),
            sd = sd(Judgment),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study1a_summary, aes(x = Judgment_type, y = mean, fill = Condition)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, 
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(1, 7))

## Analyze the data: Study 2 ##

# Recode levels of BiasCondition and HarmCondition to read more intuitively
# (.5 = Implicit, -.5 = Explicit; .5 = High, -.5 = Low).
# Subset the data to include only participants that were not suspicious (Suspicion == 0).

Study2_analysis <- Study2 %>% 
  filter(Suspicion == 0) %>%
  mutate(BiasCondition = case_when(BiasCondition == .5 ~ "Implicit",
                                      BiasCondition == -.5 ~ "Explicit"),
         HarmCondition = case_when(HarmCondition == .5 ~ "High",
                                      HarmCondition == -.5 ~ "Low"))

# Perform a series of two-way ANOVAs; harm condition and bias condition are the independent
# variables, and accountability/punishment/concern/reform are the dependent variables.

# There are several different ways to perform an ANOVA in R. For accountability,
# I have outlined the code in three different ways.

anova_test(Study2_analysis,
           dv = xAccountability,
           between = c(HarmCondition, BiasCondition))

anova1 <- lm(xAccountability ~ HarmCondition*BiasCondition, data = Study2_analysis)
anova(anova1)

anova2 <- aov(xAccountability ~ HarmCondition*BiasCondition, data = Study2_analysis)
summary(anova2)

anova_test(Study2_analysis,
           dv = xPunish,
           between = c(HarmCondition, BiasCondition))

anova_test(Study2_analysis,
           dv = xConcern,
           between = c(HarmCondition, BiasCondition))

anova_test(Study2_analysis,
           dv = xReform,
           between = c(HarmCondition, BiasCondition))


# Replicate Figure 1 to plot the data just for punishment
# Create a bar graph with harm condition on the x-axis, and average
# punishment judgment on the y-axis. Fill the bars by bias condition.

Study2_summary <- Study2_analysis %>%
  select(Pnum, BiasCondition, HarmCondition, xPunish) %>%
  group_by(HarmCondition, BiasCondition) %>%
  summarize(mean = mean(xPunish),
            sd = sd(xPunish),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study2_summary, aes(x = HarmCondition, y = mean, fill = BiasCondition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .2, 
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(3, 6))
