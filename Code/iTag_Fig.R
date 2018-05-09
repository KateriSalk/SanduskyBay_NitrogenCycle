#### Sandusky Bay Data: Statistics and Figures
# Manuscript: Nitrogen cycling in Sandusky Bay, Lake Erie: oscillations between strong and weak export and implications for harmful algal blooms
# Written by K. Salk (krsalkgu@uwaterloo.ca)


#### 16S iTag sequencing data for cyanobacteria ----
iTags <- read.csv("iTags_denormalized.csv")
attach(iTags)
head(iTags)

iTags$Taxa <- factor(iTags$Taxa, levels = c("Aphanizomenon", "Dolichospermum", "Planktothrix", "Other"))
iTags$Date <- factor (iTags$Date, levels = c("5/21/15", "5/28/15", "6/8/15", "6/22/15", "6/29/15", 
                                             "7/6/15", "7/13/15", "7/20/15", "7/27/15", "8/3/15",
                                             "8/18/15", "8/31/15", "9/11/15", "9/18/15", "9/25/15", 
                                             "10/12/15", "10/9/15", "6/15/16", "6/22/16", "7/6/16", 
                                             "7/11/16", "7/25/16", "7/27/16", "8/17/16", "8/31/16"))

library(ggplot2)

mytheme <- theme_bw(base_size=14, base_family="") + 
  theme(panel.border=element_rect(color="black", size=1), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position="top",  
        legend.background=element_rect(color="white"), 
        legend.key=element_rect(colour="black", size=0.001), 
        legend.text = element_text(size = 11),
        legend.text.align=0, 
        legend.title=element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))

ggplot(data = iTags, aes(x=Date, y=Proportion, fill=Taxa)) + 
  geom_bar(stat="identity", position = position_stack(), color="black") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = c("white", "black", "gray80", "gray30"), 
                    labels = c("Aphanizomenon sp.", "Dolichospermum spp.", "Planktothrix spp.", "Other")) +
  ylab(expression("Prop. of cyano. community")) + 
  xlab(expression("")) +
  mytheme


  