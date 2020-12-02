library(dplyr)
library(tidyr)
library(ggplot2)

# Analysis

# 1 Implement the analyses of Chapter 8 of MABS using the RATS data
# Read the dataset 
RATSL_my <- read.csv("RATSL_data.csv")

# Factor variables ID and Group
RATSL_my$ID <- factor(RATSL_my$ID)
RATSL_my$Group <- factor(RATSL_my$Group)

str(RATSL_my)
dim(RATSL_my)

# Draw the plot of the dataset
ggplot(RATSL_my, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) + scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) + theme(legend.position = "top")

# Standardise the variable Weight
RATSL_my <- RATSL_my %>%
  group_by(Time) %>%
  mutate(stdWeight = (Weight-mean(Weight))/sd(Weight)) %>%
  ungroup()

glimpse(RATSL_my)

# Plot again with the standardized Weight
ggplot(RATSL_my, aes(x = Time, y = stdWeight, group = ID)) +
  geom_line(aes(linetype = Group)) + scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) + theme(legend.position = "top")

# Number of Time
n <- RATSL_my$Time %>% unique() %>% length()

# Summary data with mean and standard error of Weight by Group and Time 
RATS_myS <- RATSL_my %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(n) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATS_myS)

# Plot the mean profiles
ggplot(RATS_myS, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

# Create a summary data by Group and ID with mean as the summary variable.
RATSL_my8S <- RATSL_my %>%
  filter(Time > 0) %>%
  group_by(Group, ID) %>%
  summarise( mean=mean(Weight) ) %>%
  ungroup()

# Glimpse the data
glimpse(RATSL_my8S)

# Draw a boxplot of the mean versus Group
ggplot(RATSL_my8S, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight)")

# Create a new data by filtering the outliers. Glimpse and draw the plot again

RATSL_my8S1 <- RATSL_my8S %>%
  filter( ID!=2 & ID!= 12 & ID!= 13)
  
glimpse(RATSL_my8S1)

ggplot(RATSL_my8S1, aes(x = Group, y = mean)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape=23, size=4, fill = "white") +
  scale_y_continuous(name = "mean(Weight)")

# Perform a t-test on the groups of rats
a <- filter(RATSL_my8S1, Group != 1) # Only groups 2 and 3
t.test(mean ~ Group, data = a, var.equal = TRUE)

a <- filter(RATSL_my8S1, Group != 2) # Only groups 1 and 3
t.test(mean ~ Group, data = a, var.equal = TRUE)

a <- filter(RATSL_my8S1, Group != 3) # Only groups 1 and 2
t.test(mean ~ Group, data = a, var.equal = TRUE)

# Interpretation: .....

# Add the WD1 from the original data as a new variable to the summary data

RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')
str(RATS)
RATSL_my8S2 <- RATSL_my8S %>%
  mutate(baseline = RATS$WD1)

# Fit the linear model with the mean as the response 
fit <- lm(baseline ~ mean, data = RATSL_my8S2)

# Compute the analysis of variance table for the fitted model with anova()
anova(fit)

# Interpretation: ...


# 2 Implement the analyses of Chapter 9 of MABS using the BPRS data.


BPRSL_my <- read.csv("BPRSL_data.csv")

# Factor variables treatment and subject
BPRSL_my$treatment <- factor(BPRSL_my$treatment)
BPRSL_my$subject <- factor(BPRSL_my$subject)

str(BPRSL_my)
dim(BPRSL_my)

# Plot the BPRSL_my data
ggplot(BPRSL_my, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL_my$bprs), max(BPRSL_my$bprs)))


# create a regression model BPRS_reg
BPRS_reg <- lm(bprs ~ week + treatment, data = BPRSL_my)

# print out a summary of the model
summary(BPRS_reg)

# Create a random intercept model
library(lme4)
BPRS_ref <- lmer(bprs ~ week + treatment + (1 | subject), data = BPRSL_my, REML = FALSE)

# Print the summary of the model
summary(BPRS_ref)

# create a random intercept and random slope model
BPRS_ref1 <- lmer(bprs ~ week + treatment + (week | subject), data = BPRSL_my, REML = FALSE)

# print a summary of the model
summary(BPRS_ref1)

# perform an ANOVA test on the two models
anova(BPRS_ref1, BPRS_ref)
#Pay attention to the chi-squared statistics and p-value of the likelihood ratio test between RATS_ref1 and RATS_ref. The lower the value the better the fit against the comparison model.

# create a random intercept and random slope model with the interaction
BPRS_ref2 <- lmer(bprs ~ week + treatment + (week | subject) + week * treatment, data = BPRSL_my, REML = FALSE)

# print a summary of the model
summary(BPRS_ref2)

# perform an ANOVA test on the two models
anova(BPRS_ref2, BPRS_ref1)

# draw the plot of BPRSL_my with the observed bprs values
ggplot(BPRSL_my, aes(x = week, y = bprs, treatment = subject)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Rated BPRS") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- fitted(BPRS_ref2)

# Create a new column fitted to RATSL
BPRSL_my <- mutate(BPRSL_my, fitted = Fitted)

# draw the plot of BPRSL_my with the Fitted values of bprs
ggplot(BPRSL_my, aes(x = week, y = fitted, treatment = subject)) +
  geom_line(aes(linetype = treatment)) +
  scale_x_continuous(name = "Time (weeks)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted BPRS") +
  theme(legend.position = "top")








