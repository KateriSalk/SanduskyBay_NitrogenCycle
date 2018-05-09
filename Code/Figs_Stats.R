#### Sandusky Bay Data: Statistics and Figures
# Manuscript: Nitrogen cycling in Sandusky Bay, Lake Erie: oscillations between strong and weak export and implications for harmful algal blooms
# Written by K. Salk (krsalkgu@uwaterloo.ca)


#### N uptake processes by date ----
Nuptakemeans=read.csv("N_Uptake_Means.csv")
attach(Nuptakemeans)
Nuptakemeans
Station1=factor(Station, c("ODNR4", "EC1163", "Bells"))
Date=as.Date(Date, format="%m/%d/%Y")
Nuptakemeans1=data.frame(Nuptakemeans[,1], Date, Nuptakemeans[,3:14])
Nuptakemeans1

library(ggplot2)

mytheme <- theme_bw(base_size=14, base_family="") + 
  theme(panel.border=element_rect(color="black", size=1), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position="none",  
        legend.background=element_rect(color="white"), 
        legend.key=element_rect(colour="black", size=0.001), 
        legend.text.align=0)

#NH4 uptake by date and station
ggplot(data=subset(Nuptakemeans, !is.na(NH4.mean)), aes(x=Date.code, y=NH4.mean, fill=Station)) + 
  geom_bar(stat="identity", position="dodge", color="black") + 
  guides(fill=guide_legend(override.aes=list(colour=NULL))) + 
  scale_fill_manual(values=c("black", "gray60", "white"), labels=c("ODNR4", "1163", "Bells")) +  
  geom_errorbar(aes(ymin=NH4.mean, ymax=NH4.mean+NH4.sd), width=0.2, position=position_dodge(0.9)) + 
  ylab(expression("NH"[4]*""^"+"*" uptake ("*mu*"mol N L"^"-1"*" h"^"-1"*")")) + 
  mytheme + 
  scale_x_discrete(labels=c("6/15/16", "7/6/16", "7/25/16", "8/17/16")) + 
  xlab(expression("")) + 
  scale_y_continuous(limits=c(0, 0.15))

#NO3 uptake by date and station
ggplot(data=subset(Nuptakemeans, !is.na(NO3.mean)), aes(x=Date.code, y=NO3.mean, fill=Station)) + 
  geom_bar(stat="identity", position="dodge", color="black") + 
  guides(fill=guide_legend(override.aes=list(colour=NULL))) + 
  scale_fill_manual(values=c("black", "gray60", "white"), labels=c("ODNR4", "1163", "Bells")) +  
  geom_errorbar(aes(ymin=NO3.mean, ymax=NO3.mean+NO3.sd), width=0.2, position=position_dodge(0.9)) + 
  ylab(expression("NO"[3]*""^"-"*" uptake ("*mu*"mol N L"^"-1"*" h"^"-1"*")")) + 
  mytheme + 
  scale_x_discrete(labels=c("6/15/16", "7/6/16", "7/25/16", "8/17/16")) + 
  xlab(expression("")) + 
  scale_y_continuous(limits=c(0, 2.5))

#N fixation by date and station
ggplot(data=subset(Nuptakemeans, !Date.code=="g"), aes(x=Date.code, y=Nfix.mean, fill=Station)) + 
  geom_bar(stat="identity", position="dodge", color="black") + 
  guides(fill=guide_legend(override.aes=list(colour=NULL))) + 
  scale_fill_manual(values=c("black", "gray60", "white"), labels=c("ODNR4", "1163", "Bells")) +  
  geom_errorbar(aes(ymin=Nfix.mean, ymax=Nfix.mean+Nfix.sd), width=0.2, position=position_dodge(0.9)) + 
  ylab(expression("N fixation ("*mu*"mol N L"^"-1"*" h"^"-1"*")")) + 
  mytheme + 
  scale_x_discrete(labels=c("6/22/15", "7/27/15", "8/31/15", "10/12/15", "6/15/16", "7/25/16", "8/17/16")) + 
  xlab(expression("")) + 
  scale_y_continuous(limits=c(0, 2.5), breaks=c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0))

#### N loss processes by date ----
Nlossmeans=read.csv("N_Loss_means.csv")
attach(Nlossmeans)
Nlossmeans

mytheme <- theme_bw(base_size=20, base_family="") + 
  theme(panel.border=element_rect(color="black", size=1), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position=c(1,1), 
        legend.justification=c(1,1), 
        legend.background=element_rect(color="white"), 
        legend.key=element_rect(colour="black", size=0.001), 
        legend.text.align=0, 
        legend.title=element_blank())

ggplot(data=Nlossmeans, aes(x=Date.code, y=Rate, fill=Process)) + 
  geom_bar(stat="identity", position="dodge", color="black") + 
  guides(fill=guide_legend(override.aes=list(colour=NULL))) + 
  scale_fill_manual(values=c("gray60", "black", "white"), labels=c("Denitrification", "Anammox", expression("N"[2]*"O"))) +  
  geom_errorbar(aes(ymin=Rate, ymax=Rate+sd), width=0.2, position=position_dodge(0.9)) + 
  ylab(expression("Rate ("*mu*"mol N m"^"-2"*" h"^"-1"*")")) + 
  mytheme + 
  scale_x_discrete(labels=c( "6/22/15", "7/27/15", "8/31/15", "10/12/15")) + 
  xlab(expression("")) 

#### N cycling processes vs. DIN concentration ----
NcyclingvsDIN=read.csv("N_Cycling_vs_DIN.csv")
attach(NcyclingvsDIN)
NcyclingvsDIN
NcyclingvsDIN.assim <- NcyclingvsDIN[1:26,]
NcyclingvsDIN.dissim <- NcyclingvsDIN[27:38,]

NH4regression=lm(Rate[Process=="NH4"]~DIN[Process=="NH4"])
summary(NH4regression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)           0.459677   0.208121   2.209   0.0693 .
# DIN[Process == "NH4"] 0.010483   0.007422   1.412   0.2075  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4645 on 6 degrees of freedom
# Multiple R-squared:  0.2495,	Adjusted R-squared:  0.1245 
# F-statistic: 1.995 on 1 and 6 DF,  p-value: 0.2075

NH4conc <- c(4.85, 0.62, 1.09, 0.80, 0.47, 1.12, 0.09, 1.08)
NH4regressionb <- lm(Rate[Process=="NH4"]~NH4conc)
summary(NH4regressionb)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.24482    0.08758   2.795 0.031352 *  
#   NH4conc      0.31260    0.04649   6.724 0.000527 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1835 on 6 degrees of freedom
# Multiple R-squared:  0.8828,	Adjusted R-squared:  0.8633 
# F-statistic: 45.21 on 1 and 6 DF,  p-value: 0.0005265

NO3regression=lm(Rate[Process=="NO3"]~DIN[Process=="NO3"])
summary(NO3regression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)           -0.93563    2.17724   -0.43   0.6824   
# DIN[Process == "NO3"]  0.36106    0.07764    4.65   0.0035 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.859 on 6 degrees of freedom
# Multiple R-squared:  0.7828,	Adjusted R-squared:  0.7466 
# F-statistic: 21.62 on 1 and 6 DF,  p-value: 0.003503

Nfixregression=lm(Rate[Process=="Nfix"]~DIN[Process=="Nfix"])
summary(Nfixregression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              7.4924     4.0207   1.863   0.0994 .
# DIN[Process == "Nfix"]   0.1224     0.1049   1.167   0.2769  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 9.98 on 8 degrees of freedom
# Multiple R-squared:  0.1454,	Adjusted R-squared:  0.03859 
# F-statistic: 1.361 on 1 and 8 DF,  p-value: 0.2769

Denitregression=lm(Rate[Process=="Denit"]~DIN[Process=="Denit"])
summary(Denitregression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)              11.7424    10.5496   1.113    0.382
# DIN[Process == "Denit"]   0.4642     0.1841   2.522    0.128
# 
# Residual standard error: 14.17 on 2 degrees of freedom
# Multiple R-squared:  0.7607,	Adjusted R-squared:  0.6411 
# F-statistic: 6.359 on 1 and 2 DF,  p-value: 0.1278

Anammoxregression=lm(Rate[Process=="Anammox"]~DIN[Process=="Anammox"])
summary(Anammoxregression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                 1.6119     2.8197   0.572    0.625
# DIN[Process == "Anammox"]   0.0334     0.0492   0.679    0.567
# 
# Residual standard error: 3.787 on 2 degrees of freedom
# Multiple R-squared:  0.1872,	Adjusted R-squared:  -0.2191 
# F-statistic: 0.4607 on 1 and 2 DF,  p-value: 0.5673

N2Oregression=lm(Rate[Process=="N2O"]~DIN[Process=="N2O"])
summary(N2Oregression)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)           -0.078708   0.496007  -0.159    0.888
# DIN[Process == "N2O"]  0.020342   0.008655   2.350    0.143
# 
# Residual standard error: 0.6662 on 2 degrees of freedom
# Multiple R-squared:  0.7342,	Adjusted R-squared:  0.6012 
# F-statistic: 5.523 on 1 and 2 DF,  p-value: 0.1432

library(ggplot2)
mytheme4 <- theme_bw(base_size=10, base_family="") + 
  theme(panel.border=element_rect(color="black", size=1), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.title=element_blank(), 
        legend.position=c("top"), 
        legend.justification=c(0,1), 
        legend.background=element_rect(color="white"), 
        legend.key=element_rect(colour="white"), 
        legend.text.align=0)

#assimilatory processes: rate vs. DIN conc.
ggplot(NcyclingvsDIN.assim, aes(x=DIN, y=Rate, shape=Process)) + 
  geom_pointrange(aes(ymin = Rate-Rate.SD, ymax = Rate+Rate.SD), position = position_dodge(width = 1)) +
  geom_point(size=3, fill="darkgray", position = position_dodge(width = 1)) + mytheme4 + 
  scale_shape_manual(values=c(19,22,23), labels=expression("N fixation", "NH"[4]*""^"+"*" uptake", "NO"[3]*""^"-"*" uptake")) + 
  ylab(expression("Rate ("*mu*"mol N L"^"-1"*" h"^"-1"*")")) + 
  xlab(expression("DIN ("*mu*"M)")) +
  geom_abline(intercept= 0.573723, slope= 0.007384, color="black") + #Nfix
  geom_abline(intercept=0.0314920, slope=  0.0006826, color="gray40") + #NH4 
  geom_abline(intercept= -0.056744, slope= 0.023982, color="gray40") +  #NO3
  annotate("text", x = 80, y = 2.2, label = paste("R^2 == 0.74"), parse=T, size=3) + #NO3
  annotate("text", x = 80, y = 1.3, label = paste("R^2 == 0.00"), parse=T, size=3) + #Nfix
  annotate("text", x = 80, y = 0.2, label = paste("R^2 == 0.11"), parse=T, size=3) #NH4  

#dissimilatory processes: rate vs. DIN conc.
ggplot(NcyclingvsDIN.dissim, aes(x=DIN, y=Rate, shape=Process)) + 
  geom_pointrange(aes(ymin = Rate-Rate.SD, ymax = Rate+Rate.SD),  position = position_dodge(width = 1)) +
  geom_point(size=3, fill="darkgray", position = position_dodge(width = 1)) + mytheme4 + 
  scale_shape_manual(values=c(21,22,23), labels=expression("Anammox", "Denitrification", "N"[2]*"O prod.")) + 
  ylab(expression("Rate ("*mu*"mol N m"^"-2"*" h"^"-1"*")")) + 
  xlab(expression("DIN ("*mu*"M)")) +
  geom_abline(intercept= 11.7424, slope= 0.4642, color="gray40") +  #Denitrification
  geom_abline(intercept= 1.6119, slope= 0.0334, color="gray40") +  #Anammox
  geom_abline(intercept= -0.078708, slope= 0.020342, color="gray40") + #N2O
  annotate("text", x = 80, y = 60, label = paste("D: R^2 == 0.64"), parse=T, size=2.75) + #Denitrification
  annotate("text", x = 82, y = 10, label = paste("A: R^2 == 0.00"), parse=T, size=2.75) + #Anammox
  annotate("text", x = 80, y = -5, label = paste("N[2]*O: R^2 == 0.60"), parse=T, size=2.75) #N2O
