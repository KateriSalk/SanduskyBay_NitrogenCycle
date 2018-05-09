#### Sandusky Bay Data: Statistics and Figures
# Manuscript: Nitrogen cycling in Sandusky Bay, Lake Erie: oscillations between strong and weak export and implications for harmful algal blooms
# Written by K. Salk (krsalkgu@uwaterloo.ca)

#### N budget loads and rates (kg/d) ----
Nbudget <- read.csv("Nbudget.csv")
head(Nbudget)

library(ggplot2)
library(tidyverse)

mytheme <- theme_bw(base_size=14, base_family="") + 
  theme(panel.border=element_rect(color="black", size=1), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position="top",  
        legend.background=element_rect(color="white"), 
        legend.key=element_rect(colour="black", size=0.001), 
        #legend.text.align=0, 
        legend.title = element_text(size=12),
        axis.text.x=element_text(angle = 45, hjust = 1))


Nbudget <- mutate(Nbudget, log.Load = log(Load))
# 2 instances of log.Load either negative or -inf. Changing these to 0
Nbudget$log.Load[8] <- 0
Nbudget$log.Load[49] <- 0

Nbudget <- mutate(Nbudget, log.Discharge = log(Discharge))

Nbudget$Process <- factor(Nbudget$Process, 
                          levels = c("NO3.load", "TKN.load", 
                                     "N.fixation", "NO3.uptake", "NH4.uptake", 
                                     "Denitrification", "Anammox"), 
                          labels = c("NO3 load", "TKN load", "N fixation", 
                                     "NO3 uptake", "NH4 uptake", 
                                     "Denitrification", "Anammox"))


ggplot(data = Nbudget, aes(x = Process, y = Load, color = log.Discharge)) +
  geom_line(color = "black") +
  geom_point(size = 3) +
  scale_color_gradient(low = "gray90", high = "black", breaks = c(11, 12, 13, 14, 15), name = "log(Discharge)") +
  xlab(expression("")) +
  ylab(expression("Rate (kg d"^"-1"*")")) +
  guides(fill = guide_colorbar(nbin = 3)) +
  scale_x_discrete(labels = c(expression(NO[3]~load), expression(TKN~load), 
                              expression(N~fixation), expression(NO[3]~uptake), 
                              expression(NH[4]~uptake), expression(Denitrification), 
                              expression(Anammox))) +
  scale_y_continuous(trans='log2', breaks = c(0, 1, 10, 100, 1000, 10000)) +
  theme(legend.title = element_text("log(Discharge)")) +
  mytheme

