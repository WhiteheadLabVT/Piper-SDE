##########################################################################
## Associated paper: Nelson, A.S., M. Gelambi, E. Morales-M., and S.R. Whitehead. 2023. Fruit secondary 
## metabolites mediate the quantity and quality of a seed dispersal mutualism. Ecology.
## File: Effect of amides on seed dispersal quality (Obj. 2)
## Corresponding author: Annika S. Nelson (University of California, Irvine; annikasn@uci.edu)
##########################################################################

## README ----
# This script is divided into the following sections:
# 1. Preliminaries
#    1.1 Load required packages
#    1.2 Set working directory
#    1.3 Import data
# 2. Test for the effects of amide extracts on seed dispersal distance
# 3. Test for the effects of amide extracts on seed removal from feeding dish
# 4. Test for the effects of amide extracts on seeds being cleaned from fruit pulp
# 5. Test for the effect of amide extracts on seeds being redispersed outside of the nest
# 6. Data visualization
#    6.1 Make Figure 4

##########################################################################
## 1. Preliminaries ----
##########################################################################
## 1.1 Load required packages ----
library(dplyr)
library(lmerTest)
library(ggplot2)

## 1.2 Set working directory ----
setwd() # Choose your working directory based on where the data files are saved

## 1.3 Import and clean data ----

# Import Dispersal distance data
distance_data <- read.csv("Obj2_Dispersal_Distance.csv", header = T, na.strings = "")

# Set station and treatment as factors
distance_data$station <- as.factor(as.character(distance_data$station))
distance_data$treatment <- as.factor(as.character(distance_data$treatment))

# Order the levels of treatment
distance_data$treatment <- ordered(distance_data$treatment, levels = c("Ethanol","2% amides"))

# Set dispersal distance as numeric variables
distance_data$dispersal.distance <- as.numeric(as.character(distance_data$dispersal.distance))


# Import lab colony data
lab_data <- read.csv("Obj2_Lab_Data.csv", header = T, na.strings = "")

# Set ant colony and treatment as factors
lab_data$colony <- as.factor(as.character(lab_data$colony))
lab_data$treatment <- as.factor(as.character(lab_data$treatment))

# Order the levels of treatment
lab_data$treatment <- ordered(lab_data$treatment, levels = c("Ethanol","2% amides"))

# Set the proportions of seeds removed, redispersed, and cleaned as numeric variables
lab_data$prop.seeds.removed <- as.numeric(as.character(lab_data$prop.seeds.removed))
lab_data$prop.seeds.redispersed <- as.numeric(as.character(lab_data$prop.seeds.redispersed))
lab_data$prop.seeds.cleaned <- as.numeric(as.character(lab_data$prop.seeds.cleaned))

##########################################################################
## 2. Test for the effects of amide extracts on seed dispersal distance ----
##########################################################################

# Linear mixed effects model testing for the effects of amides on seed dispersal distance by ants
model <- lmer(dispersal.distance ~ treatment + (1|station), data = distance_data)

# Test for normality of residuals
shapiro.test(resid(model))
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F test for statistical significance
anova(model, test.statistic = "F")

##########################################################################
## 3. Test for the effects of amide extracts on seed removal from feeding dish ----
##########################################################################

# Linear mixed effects model testing for the effects of amides on seed removal from the feeding dish
model <- lmer(prop.seeds.removed ~ treatment + (1|colony), data = lab_data)

# Test for normality of model residuals
shapiro.test(resid(model))
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F Test for statistical significance
anova(model, test.statistic = "F")

##########################################################################
## 4. Test for the effects of amide extracts on seeds being cleaned from fruit pulp ----
##########################################################################

# Linear mixed effects model testing for the effects of amides on seeds being cleaned from fruit pulp
model <- lmer(prop.seeds.cleaned ~ treatment + (1|colony), data = lab_data)

# Test for normality of model residuals
hist(resid(model))
shapiro.test(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F Test for statistical significance
anova(model)

##########################################################################
## 5. Test for the effect of amide extracts on seeds being redispersed outside of the nest ----
##########################################################################

# Linear mixed effects model testing for the effects of amides on seed redispersal back outside of the nest
model <- lmer(prop.seeds.redispersed ~ treatment + (1|colony), data = lab_data)

# Test for normality of model residuals
hist(resid(model))
shapiro.test(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F test for statistical significance
anova(model)

##########################################################################
## 6. Data visualization ----
##########################################################################
## Make Figure 4 ----

# Fig 4a
fig4a <- ggplot(distance_data, aes(x = treatment, y = dispersal.distance)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + theme_classic() + 
  ylab("Dispersal distance (m)") + xlab(NULL) +
  theme(axis.text.x = element_blank(), legend.position = "None") +
  annotate(geom = "text", x = 1.5, y = 4.2, label = "n.s.", color = "black") + 
  scale_color_manual(values = palette) + 
  theme(legend.position = "none")

# Fig 4b
fig4b <- ggplot(lab_data, aes(x = treatment, y = prop.seeds.removed)) + 
  geom_boxplot(outlier.shape = NA) + theme_classic() + xlab(NULL) + 
  theme(axis.text.x = element_blank(), legend.position = "None") +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  ylab("Prop. seeds dispersed") +
  annotate(geom = "text", x = 1.5, y = 1.1, label = "***", color = "black") + 
  scale_color_manual(values = palette) + 
  theme(legend.position = "none")

# Fig 4c
fig4c <- ggplot(lab_data, aes(x = treatment, y = prop.seeds.cleaned)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + theme_classic() + 
  ylab("Prop. seeds cleaned") + xlab(NULL) +
  theme(axis.text.x = element_blank(), legend.position = "None") +
  annotate(geom = "text", x = 1.5, y = 0.65, label = "***", color = "black") +
  scale_color_manual(values = palette) +
  theme(legend.position = "none")

# Fig 4d
fig4d <- ggplot(lab_data, aes(x = treatment, y = prop.seeds.redispersed)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + theme_classic() + 
  ylab("Prop. seeds redispersed") + theme(legend.position = "None") +
  annotate(geom = "text", x = 1.5, y = 1.1, label = "***", color = "black") + 
  scale_color_manual(values = palette) + 
  theme(legend.position = "none") + xlab(NULL)

# Combine all panels together into one figure
fig4 <- ggpubr::ggarrange(fig4a, fig4b, fig4c, fig4d, ncol = 1, nrow = 4, align = "hv", labels = c("(a)","(b)","(c)","(d)"))

# Visually inspect the figure
fig4
dev.off()

# Save the figure as a pdf
ggsave("fig4.pdf", fig4, width = 3.5, height = 10)
