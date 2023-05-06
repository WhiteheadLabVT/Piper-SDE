##########################################################################
## Associated paper: Nelson, A.S., M. Gelambi, E. Morales-M., and S.R. Whitehead. 2023. Fruit secondary 
## metabolites mediate the quantity and quality of a seed dispersal mutualism. Ecology.
## File: Effects of amides on seed dispersal quantity (Obj. 1)
## Corresponding author: Annika S. Nelson (University of California, Irvine; annikasn@uci.edu)
##########################################################################

## README ----
# This script is divided into the following sections:
# 1. Preliminaries
#    1.1 Load required packages
#    1.2 Set working directory
# 2. Test for the effects of pure amides
#    2.1 Import and clean the pure amide data
#    2.2 Test for the effects of pure amides on ant recruitment rate
#    2.3 Test for the effects of pure amides on ant community composition
#    2.4 Test for the effects of pure amides on fruit mass loss rate
# 3. Test for the effects of amide extracts
#    3.1 Import and clean the amide extract data
#    3.2 Test for the effects of amide extracts on ant recruitment rate
#    3.3 Test for the effects of amide extracts on ant community composition
#    3.4 Test for the effects of amide extracts on fruit mass loss rate
#    3.5 Test for the effects of differences in ant recruitment on fruit mass loss rate
# 4. Data visualization
#    4.1 Make Figure 2
#    4.2 Make Figure 3
#    4.3 Make Appendix S1: Figure S6
#    4.4 Make Appendix S1: Figure S7
#    4.5 Make Appendix S1: Figure S8
#    4.6 Make Appendix S1: Figure S9

##########################################################################
## 1. Preliminaries ----
##########################################################################
## 1.1 Load required packages ----
library(dplyr)
library(lmerTest)
library(multcomp)
library(vegan)
library(ggplot2)
library(viridis)

## 1.2 Set working directory ----
setwd() # Choose your working directory based on where the data files are saved

##########################################################################
## 2. Test for the effects of pure amides ----
##########################################################################
## 2.1 Import and clean the pure amide data ----

# Import data
pure_data <- read.csv("Obj1_Pure_Amides.csv", header = T, na.strings = "")

# Define station and treatment as factors
pure_data$station <- as.factor(as.character(pure_data$station))
pure_data$treatment <- as.factor(as.character(pure_data$treatment))

# Order the levels of treatment
pure_data$treatment <- ordered(pure_data$treatment, levels = c("Ethanol","2% piperine","2% piplartine","1% piperine + 1% piplartine"))

# Define ant recruitment rate as a numeric variable
pure_data$all.ants <- as.numeric(as.character(pure_data$all.ants))

# Create fruit mass loss rate as a new variable, calculated as the starting mass minus the ending mass, divided by 4, which is the number of hours elapsed in the experiment.
pure_data$mass.loss.rate <- (as.numeric(as.character(pure_data$start.fruit.mass)) - as.numeric(as.character(pure_data$end.fruit.mass)))/4

## 2.2 Test for the effects of pure amides on ant recruitment rate ----

# Linear mixed effects model testing for the effects of pure amide treatment on ant recruitment rate
recruit_model <- lmer(log(all.ants+1) ~ treatment + (1|station), data = pure_data)

# Test for normality of residuals
hist(resid(recruit_model))
shapiro.test(resid(recruit_model))
qqnorm(resid(recruit_model))
qqline(resid(recruit_model))

# F test for statistical significance
anova(recruit_model, test.statistic = "F")

# Tukey post-hoc test for multiple comparisons
summary(glht(recruit_model,linfct = mcp(treatment  = "Tukey")))

## 2.3 Test for the effects of pure amides on ant community composition ----

# PERMANOVA testing for differences in ant community composition based on pure amide treatment. Note that the response variables are the recruitment rates of the three most abundant ant species (E. ruidum, Pheidole sp. 1, and Pheidole subarmata), taken from columns 6, 7, and 8 of the original data frame. Two samples had none of these three ant species present and were removed from this analysis (rows 5 and 59). 
ants.permanova <- adonis2(pure_data[-c(5,59),c(6:8)] ~ treatment + station, data = pure_data[-c(5,59),c(3,4)], permutations = 999, method ="bray")

# Test for statistical significance
ants.permanova

## 2.4 Test for the effects of pure amides on fruit mass loss rate ----

# Linear mixed effects model testing for the effects of pure amide treatment on fruit consumption rates
mass_model <- lmer(sqrt(mass.loss.rate) ~ treatment + (1|station), data = pure_data)

# Test for normality of model residuals
shapiro.test(resid(mass_model))
hist(resid(mass_model))
qqnorm(resid(mass_model))
qqline(resid(mass_model))

# F test for statistical significance
anova(mass_model)

##########################################################################
## 3. Test for the effects of amide extracts ----
##########################################################################
## 3.1 Import and clean the amide extract data ----
extract_data <- read.csv("Obj1_Amide_Extracts.csv", header = T, na.strings = "")

# Define station and treatment as factors
extract_data$station <- as.factor(as.character(extract_data$station))
extract_data$treatment <- as.factor(as.character(extract_data$treatment))

# Order the levels of treatment
extract_data$treatment <- ordered(extract_data$treatment, levels = c("Ethanol","2% amides","Evaporation control"))

# Define ant recruitment rates as numeric
extract_data$all.ants <- as.numeric(as.character(extract_data$all.ants))
extract_data$e.ruidum <- as.numeric(as.character(extract_data$e.ruidum))
extract_data$pheidole.sp.1 <- as.numeric(as.character(extract_data$pheidole.sp.1))
extract_data$pheidole.subarmata <- as.numeric(as.character(extract_data$pheidole.subarmata))
extract_data$pheidole.simonsi <- as.numeric(as.character(extract_data$pheidole.simonsi))

# Create fruit mass loss rate as a new variable, calculated as the starting mass minus the ending mass, divided by 4, which is the number of hours elapsed in the experiment.
extract_data$mass.loss.rate <- (as.numeric(as.character(extract_data$start.fruit.mass)) - as.numeric(as.character(extract_data$end.fruit.mass)))/extract_data$hours
extract_data$mass.loss.rate <- ifelse(extract_data$mass.loss.rate >0, extract_data$mass.loss.rate, 0) # set all negative mass loss rates to 0

## 3.2 Test for the effects of amide extracts on ant recruitment rate ----

# Linear mixed effects model testing for the effects of amide extracts on ant recruitment rate
recruit_model <- lmer(log(all.ants+1) ~ treatment + date + (1|station), data = subset(extract_data, treatment != "Evaporation control"))

# Test for normality of model residuals
shapiro.test(resid(recruit_model))
hist(resid(recruit_model))
qqnorm(resid(recruit_model))
qqline(resid(recruit_model))

# F test for statistical significance
anova(recruit_model)

## 3.3 Test for the effects of amide extracts on ant community composition ----

# PERMANOVA testing for differences in ant community composition based on amide extract treatment. Note that the response variables are the recruitment rates of the three most abundant ant species (E. ruidum, Pheidole sp. 1, and Pheidole subarmata), taken from columns 6, 7, and 8 of the original data frame. Ten samples had none of these three ant species present and were removed from this analysis (rows 3, 7, 10, 12, 15 through 18, 20, and 40). We also removed evaporation controls from this analysis, as ants were excluded from those treatments.
ants.permanova <- adonis2(extract_data[-c(3,7,10,12,15:18,20,40:60),c(6:8)] ~ treatment + station, data = extract_data[-c(3,7,10,12,15:18,20,40:60),], permutations = 999, method ="bray")

# Test for statistical significance
ants.permanova

# Conduct a follow-up analysis to test for effects of amides on E. ruidum recruitment
model <- lmer(log(e.ruidum + 1) ~ treatment + date + (1|station), data = subset(extract_data, e.ruidum != "NA" & treatment != "Evaporation control"))
anova(model)

# Conduct a follow-up analysis to test for effects of amides on Pheidole sp. 1 recruitment
model <- lmer(log(pheidole.sp.1 + 1) ~ treatment + date + (1|station), data = subset(extract_data, pheidole.sp.1 != "NA" & treatment != "Evaporation control"))
anova(model)

# Conduct a follow-up analysis to test for effects of amides on Pheidole subarmata recruitment
model <- lmer(log(pheidole.subarmata + 1) ~ treatment + date + (1|station), data = subset(extract_data, pheidole.subarmata != "NA" & treatment != "Evaporation control"))
anova(model)

## 3.4 Test for the effects of amide extracts on fruit mass loss rate ----

# Linear mixed effects model, with the mass lost per hour transformed (sqrt) to improve normality of residuals
mass_model <- lmer(sqrt(mass.loss.rate) ~ treatment + date + (1|station), data = subset(extract_data, mass.loss.rate != "NA"))

# Test for normality of model residuals
shapiro.test(resid(mass_model))
hist(resid(mass_model))
qqnorm(resid(mass_model))
qqline(resid(mass_model))

# F test for significance
anova(mass_model)

# Tukey test for multiple comparisons
summary(multcomp::glht(mass_model,linfct = multcomp::mcp(treatment  = "Tukey")))

## 3.5 Test for the effects of differences in ant recruitment on fruit mass loss rate ----

# Linear mixed effect model, with the mass lost per hour transformed (sqrt) to improve normality of residuals
model <- lmer(sqrt(mass.loss.rate) ~ e.ruidum + pheidole.sp.1 + pheidole.subarmata + date + treatment + (1|station), data = subset(extract_data, treatment != "Evaporation control"))

# Test for normality of model residuals
shapiro.test(resid(model))
hist(resid(model))
qqnorm(resid(model))
qqline(resid(model))

# F test for statistical significance
anova(model)

##########################################################################
## 4. Data visualization ----
##########################################################################
## 4.1 Make Figure 2 ----

# Make figure 2a
fig2a <- ggplot(data = pure_data, aes(x = treatment, y = all.ants)) + 
  geom_boxplot(outlier.shape = NA) + 
  theme_classic() + theme(legend.position = "None") + 
  theme(axis.text.x = element_blank(), axis.title.y = element_text(vjust = 3.5)) + 
  ylab("Mean no. ants/observation") + xlab(NULL) +
  annotate(geom = "text", x = 1, y = 17, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 17, label = "AB", color = "black") +
  annotate(geom = "text", x = 3, y = 17, label = "AB", color = "black") +
  annotate(geom = "text", x = 4, y = 17, label = "B", color = "black") +
  geom_point(position = position_jitter(height = 0, width = 0.1))

# Make figure 2b
fig2b <- ggplot(pure_data, aes(x = treatment, y = mass.loss.rate)) + 
  geom_boxplot(outlier.shape = NA) + theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  ylab("Mass loss rate (g/h)") + xlab(NULL) +  theme(legend.position = "None") + 
  annotate(geom = "text", x = 1, y = 0.13, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 0.13, label = "A", color = "black") +
  annotate(geom = "text", x = 3, y = 0.13, label = "A", color = "black") +
  annotate(geom = "text", x = 4, y = 0.13, label = "A", color = "black") +
  geom_point(position = position_jitter(height = 0, width = 0.1))

# Combine the panels into one figure
fig2 <- ggpubr::ggarrange(fig2a, fig2b, ncol = 1, nrow = 2, align = "v", 
                          labels = c("(a)","(b)"), heights = c(3.5,5))

# Visually inspect the figure
fig2
dev.off()

# Save the figure as a pdf
ggsave("fig2.pdf", fig2, width = 3.5, height = 6)

## 4.2 Make Figure 3 -----

# Make Figure 3a
fig3a <- ggplot(extract_data, aes(x = treatment, y = all.ants)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) + theme_classic() + 
  ylab("Mean no. ants/observation") + xlab(NULL) + theme(legend.position = "None") +
  scale_x_discrete(labels = c("Ethanol","2% amides","Evaporation")) + 
  annotate(geom = "text", x = 1, y = 11.5, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 11.5, label = "B", color = "black") + 
  scale_color_manual(values = palette)

# Make Figure 3b
fig3b <- ggplot(subset(extract_data, mass.loss.rate != "NA"), aes(x = treatment, y = mass.loss.rate)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + theme_classic() + 
  ylab("Mass loss rate (g/h)") + theme(legend.position = "None") + xlab(NULL) +
  scale_x_discrete(labels = c("Ethanol","2% amides","Evaporation")) +
  annotate(geom = "text", x = 1, y = 0.05, label = "A", color = "black") +
  annotate(geom = "text", x = 2, y = 0.05, label = "B", color = "black") +
  annotate(geom = "text", x = 3, y = 0.05, label = "B", color = "black") +
  scale_color_manual(values = palette)

# Combine panels into one figure
fig3 <- ggpubr::ggarrange(fig3a, fig3b, ncol = 1, nrow = 2, align = "hv", labels = c("(a)","(b)"), hjust = 0.08)

# Visually inspect the figure
fig3
dev.off()

# Save the figure as a pdf
ggsave("fig3.pdf", fig3, width = 3.1, height = 5)

## 4.3 Make Appendix S1: Figure S6 ----

# Carry out an NMDS analysis
ants.MDS <- metaMDS(pure_data[-c(5,59),c(6:8)], distance = "bray", k = 2, autotransform = F, noshare = F)

# Summarize the NMDS
ants.MDS

# Plot the NMDS and save it as a pdf
pdf("FigS6.pdf", height = 6.3, width = 6)
ordiplot(ants.MDS, type = "n")
ordiellipse(ants.MDS, groups = pure_data[-c(5,59),]$treatment, kind = "se", conf = 0.95, pch = 1, col = c("#440154FF","#3B528BFF","#21908CFF","#5DC863FF"), label = F)
orditorp(ants.MDS, display = "sites", air = 0.01, cex = 1.25, pch = 20, col = c(rep("#5DC863FF",15),rep("#3B528BFF",16),rep("#21908CFF",16),rep("#440154FF",15)), label = F)
with(pure_data[-c(5,59),], legend("topleft", legend = c("1% piperine + 1% piplartine","2% piperine","2% piplartine","Ethanol"), bty = "n", col = c("#5DC863FF","#3B528BFF","#21908CFF","#440154FF"), pch = 20, cex = 1.3))
dev.off()

## 4.4 Make Appendix S1: Figure S7 ----

# Carry out an NMDS analysis
ants.MDS <- metaMDS(extract_data[-c(3,7,10,12,15:18,20,40:60),c(6:8)], distance = "bray", k = 2, autotransform = F, noshare = F)

# Summarize the NMDS
ants.MDS

# Plot the NMDS and save it as a pdf
pdf("figS7.pdf", height = 6, width = 6)
ordiplot(ants.MDS, type = "n")
ordiellipse(ants.MDS, groups = extract_data[-c(3,7,10,12,15:18,20,40:60),]$treatment, kind = "se", conf = 0.95, pch = 1, col = c("#440154FF","#21908CFF"), label = F)
orditorp(ants.MDS, display = "sites", air = 0.01, cex = 1.25, pch = 20, col = c(rep("#21908CFF",11), rep("#440154FF", 19)), label = F)
with(extract_data[-c(3,7,10,12,15:18,20,40:60),], legend("topright", legend = c("2% amides","Ethanol"), bty = "n", col = c("#21908CFF", "#440154FF"), pch = 20, cex = 1.3))
dev.off()

## 4.5 Make Appendix S1: Figure S8 ----

# Define a color palette
palette <- viridis(5)[c(1,3)]

# Figure S8a
y_title1 <- expression(paste(italic("E. ruidum"), " per obs."))
figS8a <- ggplot(subset(extract_data, e.ruidum != "NA" & treatment != "Evaporation control"), 
               aes(x = treatment, y = e.ruidum, col = treatment)) +
  geom_boxplot() + theme_classic() +
  scale_x_discrete(labels = c("Ethanol","2% amides")) +
  xlab(NULL) +
  geom_point(position = position_jitter(width = 0.1, height = 0)) + 
  ylab(y_title1) +
  annotate(geom = "text", x = 1.5, y = 2, label = "***", color = "black") +  
  scale_color_manual(values = palette) + theme(legend.position = "None")

# Figure S8b
y_title2 <- expression(paste(italic("Pheidole"), " sp. 1 per obs."))
figS8b <- ggplot(subset(extract_data, pheidole.sp.1 != "NA" & treatment != "Evaporation control"), 
               aes(x = treatment, y = pheidole.sp.1, col = treatment)) +
  geom_boxplot() + theme_classic() +
  scale_x_discrete(labels = c("Ethanol","2% amides")) +
  geom_point(position = position_jitter(width = 0.1, height = 0)) + 
  ylab(y_title2) + xlab(NULL) +
  annotate(geom = "text", x = 1.5, y = 8, label = "**", color = "black") +
  scale_color_manual(values = palette) + theme(legend.position = "None")

# Figure S8c
y_title3 <- expression(paste(italic("P. subarmata"), " per obs."))
figS8c <- ggplot(subset(extract_data, pheidole.subarmata != "NA" & treatment != "Evaporation control"), 
              aes(x = treatment, y = pheidole.subarmata, col = treatment)) +
  geom_boxplot() + theme_classic() + xlab(NULL) +
  scale_x_discrete(labels = c("Ethanol","2% amides")) +
  geom_point(position = position_jitter(width = 0.1, height = 0)) + 
  ylab(y_title3) +
  annotate(geom = "text", x = 1.5, y = 2.3, label = "n.s.", color = "black") + 
  scale_color_manual(values = palette) + theme(legend.position = "None")

# Combine panels into one figure
figS8 <- ggpubr::ggarrange(figS8a, figS8b, figS8c, ncol = 1, nrow = 3, align = "hv", labels = c("(a)","(b)","(c)"), hjust = 0.05)

# Visually inspect the figure
figS8
dev.off()

# Save the figure as a pdf
ggsave("figS8.pdf", figS8, width = 3, height = 7)

## 4.6 Make Appendix S1: Figure S9 ----

# Fig S9a
x_title <- expression(paste("No. ", italic("E. ruidum"), " per observation"))
figS9a <- ggplot(subset(extract_data, treatment != "Evaporation control"), 
                   aes(x = e.ruidum, y = mass.loss.rate, color = treatment)) + 
  geom_point() + stat_smooth(method = "lm", color = "black") +
  scale_color_manual(values = palette, labels = c("Ethanol","2% amides"), name = "Treatment") + 
  theme_classic() + xlab(x_title) + ylab("Mass loss rate (g/h)")

# Fig S9b
x_title <- expression(paste("No. ", italic("Pheidole"), " sp. 1 per observation"))
figS9b <- ggplot(subset(extract_data, treatment != "Evaporation control"),
                   aes(x = pheidole.sp.1, y = mass.loss.rate, color = treatment)) + 
  geom_point() + stat_smooth(method = "lm", color = "black", linetype = "dashed") + theme_classic() + 
  scale_color_manual(values = palette) +
  theme(legend.position = "none") + 
  xlab(x_title) + ylab("Mass loss rate (g/h)")

# Fig S9c
x_title <- expression(paste("No. ", italic("P. subarmata"), " per observation"))
figS9c <- ggplot(subset(extract_data, treatment != "Evaporation control"),
                   aes(x = pheidole.subarmata, y = mass.loss.rate, color = treatment)) + 
  geom_point() + stat_smooth(method = "lm", color = "black", linetype = "dashed") + theme_classic() + 
  scale_color_manual(values = palette) +
  theme(legend.position = "none") + 
  xlab(x_title) + ylab("Mass loss rate (g/h)")

# Extract the legend
legend <- cowplot::get_legend(figS9a)
figS9a <- figS9a + theme(legend.position = "none")

# Combine panels into one figure
figS9 <- ggpubr::ggarrange(figS9a, "", figS9b, legend, figS9c, ncol = 2, nrow = 3, 
                             align = "hv", labels = c("(a)","","(b)","", "(c)"), 
                             heights = c(5,5), widths = c(2.2,0.8), hjust = 0.05)

# Visually inspect the figure
figS9
dev.off()

# Save the figure as a pdf
ggsave("figS9.pdf", figS9, width = 3.5, height = 8)

