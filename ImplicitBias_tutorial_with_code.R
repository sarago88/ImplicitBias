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
  filter(___ == ___ & ___ == ___) %>%
  mutate(Condition = case_when(___ == .5 ~ "___",
                               ___ == -.5 ~ "___"))

# Confirm that we now have 267 observations.

nrow(Study1a_analysis)

# Create a correlation matrix of accountability, punishment, concern, and reform.
# First create a matrix with only the variables of interest. 

cor_matrix2 <- Study1a_analysis %>%
  select(___, ___, ___, ___)

cor(___)

# Perform a series of t-tests to test for whether accountability, punishment,
# concern and reform differ between conditions.

t.test(___ ~ Study1a_analysis$Condition)

t.test(___ ~ Study1a_analysis$Condition, var = TRUE)

t.test(___ ~ Study1a_analysis$Condition, var = TRUE)

t.test(___ ~ Study1a_analysis$Condition, var = TRUE)

# Visualize the data: create a bar graph with type of judgment (accountability, punishment, 
# concern, reform) on the x-axis, average judgment on the y-axis, and fill bars according to condition.
# Don't forget error bars (+/- 1 standard error).

# The data must be in a summary table prior to graphing it. First reshape the data to long format, 
# then create a summary table, then plot the data.

Study1a_long <- Study1a_analysis %>%
  select(Pnum, Condition, xAccountability, xPunish, xConcern, xReform) %>%
  gather(key = "___", value = "___", -___, -___)

Study1a_summary <- Study1a_long %>%
  group_by(___, ___) %>%
  summarize(mean = mean(___),
            sd = sd(___),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study1a_summary, aes(x = ___, y = ___, fill = ___)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ___, ymax = ___), width = .2, 
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(1, 7))

## Analyze the data: Study 2 ##

# Recode levels of BiasCondition and HarmCondition to read more intuitively
# (.5 = Implicit, -.5 = Explicit; .5 = High, -.5 = Low).
# Subset the data to include only participants that were not suspicious (Suspicion == 0).

Study2_analysis <- Study2 %>% 
  filter(___ == ___) %>%
  mutate(BiasCondition = case_when(___ == .5 ~ "___",
                                   ___ == -.5 ~ "___"),
         HarmCondition = case_when(___ == .5 ~ "___",
                                   ___ == -.5 ~ "___"))

# Perform a series of two-way ANOVAs; harm condition and bias condition are the independent
# variables, and accountability/punishment/concern/reform are the dependent variables.

# There are several different ways to perform an ANOVA in R. For accountability,
# I have outlined the code in three different ways.

anova_test(Study2_analysis,
           dv = ___,
           between = c(___, ___))

anova1 <- lm(___ ~ ___*___, data = Study2_analysis)
anova(anova1)

anova2 <- aov(___ ~ ___*___, data = Study2_analysis)
summary(anova2)

anova_test(Study2_analysis,
           dv = ___,
           between = c(___, ___))

anova_test(Study2_analysis,
           dv = ___,
           between = c(___, ___))

anova_test(Study2_analysis,
           dv = ___,
           between = c(___, ___))

# Replicate Figure 1 to plot the data just for punishment
# Create a bar graph with harm condition on the x-axis, and average
# punishment judgment on the y-axis. Fill the bars by bias condition.

Study2_summary <- Study2_analysis %>%
  select(Pnum, BiasCondition, HarmCondition, xPunish) %>%
  group_by(___, ___) %>%
  summarize(mean = mean(___),
            sd = sd(___),
            n = n(),
            se = sd/sqrt(n))

ggplot(Study2_summary, aes(x = ___, y = mean, fill = ___)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = ___, ymax = ___), width = .2, 
                position = position_dodge(.9)) +
  coord_cartesian(ylim = c(3, 6))
