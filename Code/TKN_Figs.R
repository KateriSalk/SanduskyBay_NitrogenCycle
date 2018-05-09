#### Sandusky Bay Data: Statistics and Figures
# Manuscript: Nitrogen cycling in Sandusky Bay, Lake Erie: oscillations between strong and weak export and implications for harmful algal blooms
# Written by K. Salk (krsalkgu@uwaterloo.ca)

#### TKN Figures ----
TKNdata <- read.csv("TKNData.csv")
head(TKNdata)

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(ggpubr)

mytheme <- theme_bw(base_size=14, base_family="") + 
  theme(panel.border=element_rect(color="black"), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        axis.title.y = element_text(size=12),
        legend.position="none", 
        legend.title = element_text(size=12, color = "white"),
        legend.key=element_rect(colour="white"))


TKNdata$Date <- as.Date(TKNdata$Date, "%m/%d/%y")
TKNdata <- mutate(TKNdata, Year = format(TKNdata$Date, "%Y"))

TKNriver2015 <- subset(TKNdata, Year == "2015" & Location == "River")
TKNriver2016 <- subset(TKNdata, Year == "2016" & Location == "River")
TKNbay2015 <- subset(TKNdata, Year == "2015" & Location != "River")
TKNbay2016 <- subset(TKNdata, Year == "2016" & Location != "River")
TKNbay2015$Location <- factor(TKNbay2015$Location, levels = c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"))
TKNbay2016$Location <- factor(TKNbay2016$Location, levels = c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "Bells"))

TKN2015plot <-
ggplot() + 
  geom_line(data = TKNriver2015, aes(x = Date, y = TKN), size = 0.35) +
  geom_point(data = TKNbay2015, aes(x = Date, y = TKN, shape = Location, color = Location), size = 1.5) +
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2)) +
  scale_color_manual(values=c("black", "black", "gray50", "gray50", "gray50", "gray50")) +
  xlab(expression("")) +
  ylab (expression("TKN ("*mu*"mol L"^"-1"*")")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  annotate("text", x = as.Date("2015-05-01"), y = 450, label = "a", hjust = 0, fontface = "bold") +
  annotate("text", x = as.Date("2015-11-01"), y = 450, label = "2015", hjust = 1, fontface = "bold") +
  mytheme

TKN2016plot <-
ggplot() + 
  geom_line(data = TKNriver2016, aes(x = Date, y = TKN), size = 0.35) +
  geom_point(data = TKNbay2016, aes(x = Date, y = TKN, shape = Location, color = Location), size = 1.5) +
  scale_shape_manual(values=c(15, 16, 17, 15, 2)) +
  scale_color_manual(values=c("black", "black", "gray50", "gray50", "gray50")) +
  mytheme +
  xlab(expression("")) +
  ylab (expression("TKN ("*mu*"mol L"^"-1"*")")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(breaks = c(50, 100, 150, 200, 250)) +
  annotate("text", x = as.Date("2016-05-01"), y = 260, label = "b", hjust = 0, fontface = "bold") +
  annotate("text", x = as.Date("2016-11-01"), y = 260, label = "2016", hjust = 1, fontface = "bold") +
  theme(legend.position = "none") 

ggarrange(TKN2015plot, TKN2016plot, ncol = 1, nrow = 2,  common.legend = TRUE, legend = "top")
