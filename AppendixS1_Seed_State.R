##########################################################################
## Associated paper: Nelson, A.S., M. Gelambi, E. Morales-M., and S.R. Whitehead. 2023. Fruit secondary 
## metabolites mediate the quantity and quality of a seed dispersal mutualism. Ecology.
## File: Effects of seed state on seed dispersal quantity (Appendix S1)
## Corresponding author: Annika S. Nelson (University of California, Irvine; annikasn@uci.edu)
##########################################################################

## README ----
# This script is divided into the following sections:
# 1. Preliminaries
#    1.1 Load required packages
#    1.2 Set working directory
#    1.3 Import and clean data
# 2. Test for the effect of seed state on ant recruitment rate
# 3. Test for the effect of seed state on the community composition of recruited ants
# 4. Test for the effect of seed state on mass loss rate
#    4.1 Statistical analysis of mass loss in experimental baits
#    4.2 Statistical analysis of mass loss in evaporation control baits
# 5. Test for the effect of ant recruitment on mass loss rate
# 6. Data visualization
#    6.1 Make Appendix S1: Figure S2
#    6.2 Make Appendix S1: Figure S3
#    6.3 Make Appendix S1: Figure S4

##########################################################################
## 1. Preliminaries ----
##########################################################################
## 1.1 Load required packages ----
library(dplyr)
library(lmerTest)
library(multcomp)
library(vegan)
library(ggplot2)
library(wesanderson)

## 1.2 Set working directory ----
setwd() # Choose your working directory based on where the data files are saved

## 1.3 Import and clean data ----

# Import data
recruit_data <- read.csv("AppendixS1_Seed_State.csv", header = T, na.strings = "")

# Define station, seed state, and evaporation control as factors
recruit_data$station <- as.factor(as.character(recruit_data$station))
recruit_data$seed.state <- as.factor(as.character(recruit_data$seed.state))
recruit_data$evaporation.control <- as.factor(as.character(recruit_data$evaporation.control))

# Order the levels of seed state
recruit_data$seed.state <- ordered(recruit_data$seed.state, levels = c("Unripe","Ripe","Overripe","Cleaned"))

# Define ant recruitment rates as numeric variables
recruit_data$all.ants <- as.numeric(as.character(recruit_data$all.ants))
recruit_data$e.ruidum <- as.numeric(as.character(recruit_data$e.ruidum))
recruit_data$pheidole.sp.1 <- as.numeric(as.character(recruit_data$pheidole.sp.1))
recruit_data$pheidole.subarmata <- as.numeric(as.character(recruit_data$pheidole.subarmata))

# Create fruit or seed mass loss rate as a new variable, calculated as the starting mass minus the ending mass, divided by 4, which is the number of hours elapsed in the experiment.
recruit_data$mass.loss.rate <- (recruit_data$start.fruit.mass - recruit_data$end.fruit.mass)/4

##########################################################################
## 2. Test for the effect of seed state on ant recruitment rate ----
##########################################################################

# Linear mixed effects model testing for the effects of seed state on ant recruitment. The model excludes data from evaporation control baits, as ants were experimentally excluded from those treatments.
recruit_model <- lmer(log(all.ants+1) ~ seed.state + (1|station), data = subset(recruit_data, evaporation.control != "y"))

# Test for normality of model residuals
shapiro.test(resid(recruit_model))
hist(resid(recruit_model))
qqnorm(resid(recruit_model))
qqline(resid(recruit_model)) # note residuals still aren't normally distributed, but better than if not log transformed

# F test for statistical significance
anova(recruit_model)

# Tukey test for multiple comparisons
summary(glht(recruit_model, linfct = mcp(seed.state = "Tukey")))

##########################################################################
## 3. Test for the effect of seed state on the community composition of recruited ants ----
##########################################################################

# Subset the data frame, removing cleaned seeds, unripe fruits, evaporation controls, and baits where no ants were ever observed.
subset_data <- subset(recruit_data, seed.state != "Cleaned" & seed.state != "Unripe" & evaporation.control == "n" & all.ants > 0)

# PERMANOVA testing for the effect of seed state on ant community composition. Because few ants recruited to cleaned seeds and unripe fruits, we only compared the community composition of ants at ripe and overripe fruits in this analysis. Note that the response variables are the recruitment rates of the three most abundant ant species (E. ruidum, Pheidole sp. 1, and Pheidole subarmata), taken from columns 6, 7, and 8 of the data frame.
ants.permanova <- adonis2(subset_data[,c(6:8)] ~ seed.state + station, data = subset_data, permutations = 999, method ="bray")

# Test for statistical significance
ants.permanova

##########################################################################
## 4. Test for the effect of seed state on mass loss rate ----
##########################################################################
## 4.1 Statistical analysis of mass loss in experimental baits ----

# Linear mixed effects model testing for the effects of seed state on mass loss rate
mass_model <- lmer(sqrt(mass.loss.rate) ~ seed.state + (1|station), data = subset(recruit_data, evaporation.control == "n"))

# Test for normality of model residuals
shapiro.test(resid(mass_model))
hist(resid(mass_model))
qqnorm(resid(mass_model))
qqline(resid(mass_model))

# F test for statistical significance
anova(mass_model)

# Tukey test for multiple comparisons
summary(glht(mass_model, linfct = mcp(seed.state = "Tukey")))

## 4.2 Statistical analysis of mass loss in evaporation control baits -----

# Linear mixed effects model testing for the effects of seed state on mass loss rate in the absence of ants
mass_model <- lmer(sqrt(mass.loss.rate) ~ seed.state + (1|station), data = subset(recruit_data, evaporation.control == "y"))

# Test for normality of model residuals
shapiro.test(resid(mass_model))
hist(resid(mass_model))
qqnorm(resid(mass_model))
qqline(resid(mass_model))

# F test for statistical significance
anova(mass_model)

##########################################################################
## 5. Test for the effect of ant recruitment on mass loss rate ----
##########################################################################

# Linear mixed effects model testing for the effects of seed state and and recruitment rates on mass loss
loss_model <- lmer(sqrt(mass.loss.rate) ~ seed.state + e.ruidum + pheidole.sp.1 + pheidole.subarmata + (1|station), data = subset(recruit_data, evaporation.control == "n"))

# Test for normality of residuals
shapiro.test(resid(loss_model))
hist(resid(loss_model))
qqnorm(resid(loss_model))
qqline(resid(loss_model))

# F test for statistical significance
anova(loss_model)

##########################################################################
## 6. Data visualization ----
##########################################################################
## 6.1 Make Appendix S1: Figure S2 -----

# Figure S2a
figS2a <- ggplot(data = recruit_data, aes(x = seed.state, y = all.ants, color = seed.state)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  theme_classic() + theme(legend.position = "None", axis.text.x = element_blank()) + 
  ylab("No. ants per observation") + xlab(NULL) +
  annotate(geom = "text", x = 1, y = 15, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 15, label = "B", color = "black") +
  annotate(geom = "text", x = 3, y = 15, label = "B", color = "black") +
  annotate(geom = "text", x = 4, y = 15, label = "A", color = "black") +
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 4))

# Figure S2b
figS2b <- ggplot(data = subset(recruit_data, evaporation.control == "n"), aes(x = seed.state, y = mass.loss.rate, color = seed.state)) + 
  geom_boxplot(outlier.shape = NA, alpha = 0.5) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  theme_classic() + theme(legend.position = "None") +
  ylab("Mass loss rate (g/h)") + xlab("Seed state") +
  annotate(geom = "text", x = 1, y = 0.11, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 0.11, label = "AB", color = "black") +
  annotate(geom = "text", x = 3, y = 0.11, label = "B", color = "black") +
  annotate(geom = "text", x = 4, y = 0.11, label = "A", color = "black") + 
  scale_fill_manual(values = wes_palette("GrandBudapest1", n = 4)) + 
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 4))

# Combine the two panels into one figure
figS2 <- ggpubr::ggarrange(figS2a, figS2b, ncol = 1, nrow = 2, align = "hv", labels = c("(a)","(b)"))

# Visually inspect the figure
figS2
dev.off()

# Save the figure as a pdf
ggsave("figS2.pdf", figS2, width = 3, height = 5)

## 6.2 Make Appendix S1: Figure S3 --------

# Carry out an NMDS analysis
ants.MDS <- metaMDS(subset_data[,c(6:8)], distance = "bray", k = 2, autotransform = F, noshare = F)

# Summarize the NMDS
ants.MDS

# Make the figure and save it as a pdf
subset_data$seed.state <- as.factor(as.character(subset_data$seed.state))
pdf("FigS3.pdf", height = 5, width = 5)
ordiplot(ants.MDS, type = "n")
ordiellipse(ants.MDS, groups = subset_data$seed.state, kind = "se", conf = 0.95, pch = 1, col = c(wes_palette("GrandBudapest1", n = 4)[3], wes_palette("GrandBudapest1", n = 4)[2]), label = F)
orditorp(ants.MDS, display = "sites", air = 0.01, cex = 1.25, pch = 20, col = c(rep(wes_palette("GrandBudapest1", n = 4)[3],7), rep(wes_palette("GrandBudapest1", n = 4)[2],8)), label = F)
with(subset_data, legend("topright", legend = levels(seed.state), bty = "n", col = c(wes_palette("GrandBudapest1", n = 4)[3], wes_palette("GrandBudapest1", n = 4)[2]), pch = 20, cex = 1.3))
dev.off()

## 6.3 Make Appendix S1: Figure S4 -----

# Fig S4a
x_title <- expression(paste("No. ", italic("E. ruidum"), " per observation"))
FigS4a <- ggplot(data = recruit_data, aes(x = e.ruidum, y = mass.loss.rate)) + 
  stat_smooth(method = "lm", col = "black") + 
  labs(color = "Seed state") + 
  geom_point(alpha = 1, aes(color = seed.state)) + theme_classic() + 
  ylab("Mass loss rate (g/h)") + xlab(x_title)+ 
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 4))

legend <- cowplot::get_legend(FigS4a)
FigS4a <- FigS4a + theme(legend.position = "none")

# Fig S4b
x_title <- expression(paste("No. ", italic("Pheidole"), " sp. 1 per observation"))
FigS4b <- ggplot(data = recruit_data, aes(x = pheidole.sp.1, y = mass.loss.rate)) + 
  stat_smooth(method = "lm", col = "black") + 
  geom_point(alpha = 1, aes(color = seed.state)) + theme_classic() + 
  theme(legend.position = "none") +
  ylab("Mass loss rate (g/h)") + xlab(x_title)+ 
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 4))

# Fig S4c
x_title <- expression(paste("No. ", italic("P. subarmata"), " per observation"))
FigS4c <- ggplot(data = recruit_data, aes(x = pheidole.subarmata, y = mass.loss.rate)) + 
  stat_smooth(method = "lm", col = "black", linetype = "dashed") + 
  geom_point(alpha = 1, aes(color = seed.state)) + theme_classic() + 
  theme(legend.position = "none") +
  ylab("Mass loss rate (g/h)") + xlab(x_title)+ 
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 4))

# Combine the panels into one figure
FigS4 <- ggpubr::ggarrange(FigS4a, "", FigS4b, legend, FigS4c,"", ncol = 2, nrow = 3, 
                           align = "hv", labels = c("(a)","","(b)","","(c)"), 
                           heights = c(5,5), widths = c(2.2,0.8))

# Visually inspect the figure
FigS4
dev.off()

# Save the figure as a pdf
ggsave("figS4.pdf", FigS4, width = 3.5, height = 8)

