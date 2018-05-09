#### Sandusky Bay Data: Statistics and Figures
# Manuscript: Nitrogen cycling in Sandusky Bay, Lake Erie: oscillations between strong and weak export and implications for harmful algal blooms
# Written by K. Salk (krsalkgu@uwaterloo.ca)

Nutrients=read.csv("Nutrients.csv")
Nutrients=read.csv("Nutrients2016.csv")
Nutrients=read.csv("Nutrients1516.csv")
attach(Nutrients)
head(Nutrients)
date=as.Date(Nutrients$Date, format="%m/%d/%Y")
nutrients=data.frame(date,Nutrients[,2:7])
nutrients <- na.omit(nutrients)

RiverNutrients15 <- read.csv("SanduskyNitrate15.csv")
attach(RiverNutrients15)
head(RiverNutrients15)
RiverNutrients15$Date <- as.Date(RiverNutrients15$Date, format="%m/%d/%Y")

RiverNutrients16 <- read.csv("SanduskyNitrate16.csv")
attach(RiverNutrients16)
head(RiverNutrients16)
RiverNutrients16$Date <- as.Date(RiverNutrients16$Date, format="%m/%d/%Y")

lm6=lm(Nitrate~Discharge)
summary(lm6)
# Call:
# lm(formula = Nitrate ~ Discharge)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -151.39  -31.64  -29.90   -1.58  591.96 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 29.48304    9.24833   3.188  0.00178 ** 
# Discharge    0.68753    0.09929   6.925 1.61e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 97.1 on 135 degrees of freedom
  # (10 observations deleted due to missingness)
# Multiple R-squared:  0.2621,	Adjusted R-squared:  0.2566 
# F-statistic: 47.95 on 1 and 135 DF,  p-value: 1.614e-10

lm7=lm(Ammonium~Discharge)
summary(lm7)
# Call:
# lm(formula = Ammonium ~ Discharge)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -2.4924 -1.0884 -0.6392 -0.2766 16.2449 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.205602   0.258218   4.669 7.22e-06 ***
# Discharge   0.004856   0.002772   1.752   0.0821 .  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.711 on 135 degrees of freedom
  # (10 observations deleted due to missingness)
# Multiple R-squared:  0.02223,	Adjusted R-squared:  0.01499 
# F-statistic: 3.069 on 1 and 135 DF,  p-value: 0.08206

lm8=lm(Phosphate~Discharge)
summary(lm8)
# Call:
# lm(formula = Phosphate ~ Discharge)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -0.8351 -0.2454 -0.1974 -0.0292  3.5414 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.2417751  0.0575329   4.202 4.77e-05 ***
# Discharge   0.0018390  0.0006176   2.977  0.00345 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.6041 on 135 degrees of freedom
  # (10 observations deleted due to missingness)
# Multiple R-squared:  0.06162,	Adjusted R-squared:  0.05467 
# F-statistic: 8.865 on 1 and 135 DF,  p-value: 0.003446


##figures for publication
library(ggplot2)
mytheme <- theme_bw(base_size=28, base_family="") + 
  theme(panel.border=element_rect(color="black"), 
        panel.grid.major=element_line(color="white"), 
        panel.grid.minor=element_line(color="white"), 
        legend.position=c(1,1), 
        legend.justification=c(1,1), 
        legend.background=element_rect(color="black"), 
        legend.key=element_rect(colour="white"))

#2015
ggplot(data=subset(nutrients, !is.na(Phosphate)), aes(x=date, y=Phosphate, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("PO"[4]*""^"3-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(limits=c(0,4.5))

#2016
ggplot(data=subset(nutrients, !is.na(Phosphate)), aes(x=date, y=Phosphate, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("PO"[4]*""^"3-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2016)")) + 
  scale_y_continuous(limits=c(0,4.5)) + 
  scale_x_date(date_breaks="1 month", date_labels="%b")

#2015
ggplot(data=nutrients,  aes(x=date, y=nutrients$Nitrate, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  ylab (expression("NO"[3]*""^"-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(limits=c(0,700)) 

ggplot(data=RiverNutrients15,  
       aes(x=RiverNutrients15$Date, y=RiverNutrients15$Nitrate, shape=Site, color=Site, linetype=Site)) + 
  geom_point(size=3) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + 
  scale_shape_manual(values=c(NA, 15, 16, 17, 15, 16, 2), 
                     labels=c("River", "ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  scale_color_manual(values=c("black", "black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("River", "ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  ylab (expression("NO"[3]*""^"-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(limits=c(0,1000)) + 
  scale_linetype_manual(values=c("dashed", "solid","solid","solid","solid","solid","solid"), guide="none")

#2016
ggplot(data=subset(nutrients, !is.na(Nitrate)), aes(x=date, y=Nitrate, shape=Site, color=Site)) + geom_point(size=3) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                               labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                               guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), guide=F) + 
  ylab (expression("NO"[3]*""^"-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2016)")) + 
  scale_y_continuous(limits=c(0,700)) + 
  scale_x_date(date_breaks="1 month", date_labels="%b")

ggplot(data=RiverNutrients16,  
       aes(x=RiverNutrients16$Date, y=RiverNutrients16$Nitrate, shape=Site, color=Site, linetype=Site)) + 
  geom_point(size=3) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + 
  scale_shape_manual(values=c(NA, 15, 16, 17, 15, 16, 2), 
                     labels=c("River", "ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  scale_color_manual(values=c("black", "black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("River", "ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells")) + 
  ylab (expression("NO"[3]*""^"-"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(limits=c(0,1000)) + 
  scale_linetype_manual(values=c("dashed", "solid","solid","solid","solid","solid","solid"), guide="none") + 
  theme(legend.position="none")

#2015
ggplot(data=subset(nutrients, !is.na(Ammonium)), aes(x=date, y=Ammonium, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() +
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("NH"[4]*""^"+"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2015)"))  + 
  scale_y_continuous(limits=c(0,18))

#2016
ggplot(data=subset(nutrients, !is.na(Ammonium)), aes(x=date, y=Ammonium, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("NH"[4]*""^"+"*" ("*mu*"mol L"^"-1"*")")) + 
  xlab(expression("Date (2016)"))  + 
  scale_y_continuous(limits=c(0,18)) + 
  scale_x_date(date_breaks="1 month", date_labels="%b")

#2015
ggplot(data=subset(nutrients, !is.na(NtoP)), aes(x=date, y=NtoP, shape=Site, color=Site)) + 
  geom_point(size=3, position=position_dodge(width=0.3)) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("N:P")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(trans='log2', breaks=c(0, 1, 5, 20, 100, 1000, 10000), limits=c(NA, 50000)) + 
  geom_hline(yintercept=16, lty=2)

#2016
ggplot(data=subset(nutrients, !is.na(NtoP)), aes(x=date, y=NtoP, shape=Site, color=Site)) + 
  geom_point(size=3, position=position_dodge(width=0.3)) + 
  geom_line(position=position_dodge(width=0.3)) + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("N:P")) + 
  xlab(expression("Date (2016)")) + 
  scale_y_continuous(trans='log2', breaks=c(0, 1, 5, 20, 100, 1000, 10000), limits=c(NA,50000)) + 
  geom_hline(yintercept=16, lty=2) + 
  scale_x_date(date_breaks="1 month", date_labels="%b")

#2015
ggplot(data=subset(nutrients, !is.na(Chla)), aes(x=date, y=Chla, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("Chl a ("*mu*"g L"^"-1"*")")) + 
  xlab(expression("Date (2015)")) + 
  scale_y_continuous(limits=c(0,150))

#2016
ggplot(data=subset(nutrients, !is.na(Chla)), aes(x=date, y=Chla, shape=Site, color=Site)) + 
  geom_point(size=3) + 
  geom_line() + 
  mytheme + 
  scale_shape_manual(values=c(15, 16, 17, 15, 16, 2), 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  scale_color_manual(values=c("black", "black", "gray60", "gray60", "gray60", "gray60") , 
                     labels=c("ODNR4", "ODNR6", "ODNR2", "ODNR1", "1163", "Bells"), 
                     guide=F) + 
  ylab (expression("Chl a ("*mu*"g L"^"-1"*")")) + 
  xlab(expression("Date (2016)")) + 
  scale_y_continuous(limits=c(0,150)) + 
  scale_x_date(date_breaks="1 month", date_labels="%b")


