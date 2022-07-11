library(readxl)
library(tidyverse)
library(cowplot)
library(mgcv)
library(MLmetrics)
library(relaimpo)
setwd("C:/Users/Cen/Desktop/ForestN2O/Drafts/Submission/For GBC/Supplementary Information") ##set working directory i.e. location of the data files
data0 <- read_xlsx("ds01_ForestN2Odataset.xlsx")
N2O.all <- cbind.data.frame(data0$Longitude, data0$Latitude, data0$EcoRegion, data0$N_Add_Std,
          data0$`RN2O(kgN ha-1 yr-1)`, data0$`MAT(K)`, data0$MAT.CV, data0$`MAP(mm)`, data0$MAP.CV, data0$`Ndepo(kgN ha-1 yr-1)`, data0$`MAN(kgN ha-1 yr-1)`, 
          data0$MAN.CV, data0$`Clay(%)`, data0$`Sand(%)`, data0$References)
colnames(N2O.all) <- c("Long", "Lati", "EcoRegion", "N_Add",  "RN2O", "MAT", "MAT.CV", "MAP", "MAP.CV", "Ndepo", "MAN", 
                       "MAN.CV", "Clay", "Sand", "Reference")
N2O.all$N_Add_adj <- N2O.all$N_Add + N2O.all$Ndepo

##Plot the overall trend
##Figure S2
N2O.NAdd <- N2O.all %>%
  group_by(Reference) %>%
  filter(any(N_Add==0)&any(N_Add>0))
ggplot(data = N2O.NAdd, aes(x = N_Add, y = RN2O))+
  geom_point(shape= 19, color="grey", cex=2)+
  scale_x_continuous(n.breaks = 7, limits = c(0,350), name = "N Input Rate")+
  scale_y_continuous(n.breaks = 7, limits = c(0,20), name ="RN2O")+
  geom_smooth(fill = "pink",color = "red", show.legend = TRUE)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )

##Calculating fLN, R0 for each site
N2O.LN <- subset(N2O.all, N2O.all$N_Add<=150)
s <- unique.data.frame(cbind(N2O.LN$Long, N2O.LN$Lati))
mod.summ <- {}
mod.valid <- {}
for(i in 1:length(s[,1])){
  temp1 <- subset(N2O.LN, N2O.LN$Long==s[i,1] & N2O.LN$Lati==s[i,2])
  if(any(temp1$N_Add==0)&any(temp1$N_Add>0)){
    mod1 <- lm(temp1$RN2O ~ temp1$N_Add_adj)
    a <- summary(mod1)
    if(!is.na(a$adj.r.squared)& a$adj.r.squared>0){
      mod.summ <- rbind(mod.summ, c(temp1$Long[1], temp1$Lati[1], temp1$EcoRegion[1], temp1$MAP[1], temp1$MAP.CV[1], 
            temp1$MAT[1], temp1$MAT.CV[1], temp1$MAN[1], temp1$MAN.CV[1], temp1$Clay[1], temp1$Sand[1],
            a$coefficients[2,1], a$coefficients[2,4], a$coefficients[1,1], a$coefficients[1,4], a$df[2]+2, a$adj.r.squared))
    }
  }
  if(all(temp1$N_Add==0)){
      mod.valid <- rbind(mod.valid, c(temp1$Long[1], temp1$Lati[1], temp1$EcoRegion[1], temp1$MAP[1], temp1$MAP.CV[1], 
                                    temp1$MAT[1], temp1$MAT.CV[1], temp1$MAN[1], temp1$MAN.CV[1], temp1$Clay[1], temp1$Sand[1],
                                    temp1$N_Add_adj[1], mean(temp1$RN2O)))
  }
}
mod.summ <- as.data.frame(mod.summ)
colnames(mod.summ) <- c("Long", "Lati", "EcoRegion", "MAP","MAP.CV", "MAT", "MAT.CV", "MAN", "MAN.CV", "Clay", "Sand", 
                       "fLN", "p_fLN", "R0", "p_R0", "n", "R2adjusted")
mod.summ$EcoRegion <- factor(mod.summ$EcoRegion, labels = levels(N2O.all$EcoRegion)[2:7])
mod.valid <- as.data.frame(mod.valid)
colnames(mod.valid) <- c("Long", "Lati", "EcoRegion", "MAP","MAP.CV", "MAT", "MAT.CV", "MAN", "MAN.CV", "Clay", "Sand",
                         "N_Add_adj", "RN2O")
mod.valid$EcoRegion <- factor(mod.valid$EcoRegion, labels = levels(N2O.all$EcoRegion)[c(1:6)])

##Check the distribution of fLN and R0
##Figure S3
dens_fLN <- ggplot(data = mod.summ)+
  geom_density(aes(fLN))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
dens_logfLN <- ggplot(data = mod.summ)+
  geom_density(aes(log(fLN)))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
dens_R0 <- ggplot(data = mod.summ)+
  geom_density(aes(R0))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
dens_sqrtR0 <- ggplot(data = mod.summ[-5,])+
  geom_density(aes(sqrt(R0)))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
plot_grid(dens_fLN, dens_logfLN, dens_R0, dens_sqrtR0, ncol=2)
shapiro.test(mod.summ$fLN)
shapiro.test(log(mod.summ$fLN))
shapiro.test(mod.summ$R0)
shapiro.test(log(mod.summ$R0[-5]))
shapiro.test(sqrt(mod.summ$R0[-5]))

##Generalized linear regression models on fLN and R0
mod.fLN <- gam(fLN ~  MAT * Sand * log(MAN.CV) * MAP
             - MAT:Sand:log(MAN.CV):MAP - MAT:log(MAN.CV):MAP
             - log(MAN.CV):MAP - Sand:log(MAN.CV):MAP - MAT:MAP
             - Sand:MAP - MAT:Sand:MAP - MAT:log(MAN.CV) - MAP
             - log(MAN.CV)
             ,data = mod.summ[-c(5,11,9),], family = "quasipoisson"
             , drop.intercept = T)
summary(mod.fLN)
mod.fLN.glm <- glm(fLN ~  MAT * Sand * log(MAN.CV) * MAP 
    - MAT:Sand:log(MAN.CV):MAP - MAT:log(MAN.CV):MAP
    - log(MAN.CV):MAP - Sand:log(MAN.CV):MAP - MAT:MAP
    - Sand:MAP - MAT:Sand:MAP - MAT:log(MAN.CV) - MAP
    - log(MAN.CV) +0, family = "quasipoisson",
    data = mod.summ[-c(5,11,9),])
summary(mod.fLN.glm)

mod.R0 <- gam(sqrt(R0) ~ MAP * MAT * Clay * MAN
             - Clay - MAT:MAN - MAT - MAP:MAT:Clay - MAT:Clay
             - MAP:MAT - MAN:Clay - MAP
             , data = mod.summ[-c(5,11,17),], family = "gaussian"
             , drop.intercept = T)
summary(mod.R0)

##Test model with mod.valid dataset
mod.test <- data.frame(mod.valid, predict.gam(mod.fLN, newdata = mod.valid, se.fit = T))
mod.test <- data.frame(mod.test, predict.gam(mod.R0, newdata = mod.valid, se.fit = T))
mod.test$RN2O.pred <- exp(mod.test$fit) * mod.test$N_Add_adj + (mod.test$fit.1)^2
RN2O.obs <- mod.test$RN2O
RN2O.pred <- mod.test$RN2O.pred
RMSE(RN2O.pred, RN2O.obs)
cor(RN2O.pred, RN2O.obs)
##Figure 3
ggplot(data = mod.test, aes(x = RN2O, y = RN2O.pred))+
  geom_point(pch = 1)+
  geom_abline(linetype = "dashed")+
  geom_smooth(aes(x = RN2O, y = RN2O.pred), method = lm,
              formula = y ~ x + 0, color = "red", se = F, lwd = 0.2)+
  ylim(c(0, 4))+
  xlim(c(0, 4))+
  theme_bw()+
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
      axis.text.y = element_text(size = 10), # Size y-axis numbers
      axis.ticks = element_blank(), # No ticks
      axis.title.x = element_text(size = 10), # No x-axis title
      axis.title.y = element_text(size = 10), # Size y-axis title
      panel.grid.minor = element_blank(), # No minor grid lines
      panel.grid.major.x = element_blank(), # No major x-axis grid lines
      panel.grid.major.y = element_blank(),
      text=element_text(size = 10)
)

##Figure S7
N2O.NAdd <- N2O.all %>%
  group_by(Long, Lati, EcoRegion, N_Add_adj) %>%
  summarize(across(where(is.numeric), mean, na.rm = T))
mod.test.NAdd <- data.frame(N2O.NAdd, predict.gam(mod.fLN, newdata = N2O.NAdd, se.fit = T))
mod.test.NAdd <- data.frame(mod.test.NAdd, predict.gam(mod.R0, newdata = mod.test.NAdd, se.fit = T))
mod.test.NAdd$RN2O.pred <- exp(mod.test.NAdd$fit) * mod.test.NAdd$N_Add_adj + (mod.test.NAdd$fit.1)^2
RMSE(c(mod.test$RN2O, mod.test.NAdd$RN2O), c(mod.test$RN2O.pred, mod.test.NAdd$RN2O.pred))
cor(c(mod.test$RN2O, mod.test.NAdd$RN2O), c(mod.test$RN2O.pred, mod.test.NAdd$RN2O.pred))
summary(lm(data = mod.test.NAdd, RN2O.pred ~ RN2O +0))

ggplot(data = mod.test.NAdd, aes(x = RN2O, y = RN2O.pred))+
  geom_point(aes(color = N_Add_adj, alpha = 0.7)) +
  scale_color_stepsn(colors= c("blue", "green", "yellow", "red"), n.breaks = 8)+
  geom_abline(linetype = "dashed")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )

##Calculating fLN, R0 and RN2O emission for each grid
data1 <- read_xlsx("ds02_EnvironFactor_grid.xlsx")
logfLN <- predict.gam(mod.fLN, newdata = data1, se.fit = T)
data1$fLN <- exp(logfLN$fit)
data1$fLN.se <- exp(logfLN$fit)* logfLN$se.fit
sqrtR0 <- predict.gam(mod.R0, newdata = data1, se.fit = T)
data1$R0 <- (sqrtR0$fit)^2
data1$R0.se <- abs(2 * sqrtR0$fit) * sqrtR0$se.fit
data1$RN2O <-  data1$fLN * data1$Ndepo + data1$R0
data1$RN2O.se <- sqrt((data1$fLN.se * data1$Ndepo)^2 + (data1$R0.se)^2)
data1$DC <- (data1$fLN * data1$Ndepo)/data1$RN2O * 100
data1$DC.se <- data1$DC * sqrt((data1$fLN.se/data1$fLN)^2 + (data1$RN2O.se/data1$RN2O)^2) 
ss <- data1 %>%
  group_by(Year) %>%
  summarize(mean = mean(RN2O, na.rm = T), mean.se = mean(RN2O.se, na.rm = T),
            sum = sum(RN2O, na.rm = T), sum.se = sum(RN2O.se, na.rm = T))
n2o.com <- as.data.frame(data1)

##Analysis on grid-level data
n2o.com$EcoRegion <- as.factor(n2o.com$EcoRegion_ras)
n2o.com$EcoRegion <- factor(n2o.com$EcoRegion, labels = c("BorealConiferousForest", "MidSubtropicalEvergreenForest", 
                                                          "NorthSubtropicalMixedForest", "SouthSubtropicalEvergreenForest", 
                                                          "TemperateMixedForest", "TropicalMonsoonRainforest",
                                                          "WarmTemperateDeciduousForest"))
n2o.com$EcoRegion <- ordered(n2o.com$EcoRegion, levels = c("BorealConiferousForest", "TemperateMixedForest",
                                                           "WarmTemperateDeciduousForest", "NorthSubtropicalMixedForest",
                                                           "MidSubtropicalEvergreenForest", "SouthSubtropicalEvergreenForest", 
                                                           "TropicalMonsoonRainforest"),
                             labels = c("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR"))
n2o.com$Year <- ordered(n2o.com$Year, levels = c("19962000", "20012005", "20062010", "20112015"), labels = c("T1", "T2", "T3", "T4"))                  
n2o9615 <- n2o.com %>%
  group_by(Longitude, Latitude, EcoRegion) %>%
  summarize(MAT = mean(MAT, na.rm = T), MAP = mean(MAP, na.rm = T), MAN = mean(MAN, na.rm = T), MAN.CV = mean(MAN.CV, na.rm = T),
            Sand = mean(Sand, na.rm = T), Clay = mean(Clay, na.rm = T),
            RN2O = mean(RN2O, na.rm = T), RN2O.se = mean(RN2O.se, na.rm = T), 
            fLN = mean(fLN, na.rm = T), fLN.se = mean(fLN.se, na.rm = T), 
            R0 = mean(R0, na.rm = T), R0.se = mean(R0.se, na.rm = T), 
            DC = mean(1-R0/RN2O, na.rm = T)*100, DC.se = mean(DC.se, na.rm = T), .groups = "drop")
##Table 1
n2o9615.summ <- n2o9615 %>%
#  group_by(EcoRegion) %>%
  summarize(RN2O = mean(RN2O, na.rm = T), RN2O.se = mean(RN2O.se, na.rm = T), 
            fLN = mean(fLN, na.rm = T), fLN.se = mean(fLN.se, na.rm = T), 
            R0 = mean(R0, na.rm = T), R0.se = mean(R0.se, na.rm = T),  
            DC = mean(DC, na.rm = T), DC.se = mean(DC.se, na.rm = T))

##ANOVA on RN2O or DC of different ecoregions
##Figure S5 & S6
s <- levels(n2o.com$EcoRegion)
temp1 <- {}
boot.mean <- {}
boot.mean <- as.data.frame(boot.mean)
for (j in 1:length(s)){
  temp1 <- subset(n2o.com, n2o.com$EcoRegion == s[j])
  for (i in (5000*(j-1)+1):(5000*j)){
    boot.sample <- sample(temp1$RN2O[!is.na(temp1$RN2O)], 20, replace = T)  ##Replace the "RN2O"s in this line with "DC"s to switch to ANOVA on DC
    boot.mean[i,1] <- mean(boot.sample)
    boot.mean[i,2] <- s[j]
  }
}
colnames(boot.mean) <- c("boot.mean", "EcoRegion")
boot.mean$EcoRegion <- factor(boot.mean$EcoRegion,
                              levels = c("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR"))
#test of normality
test.norm <- {}
for (i in 1:length(s)){
  shap <- shapiro.test(boot.mean$boot.mean[boot.mean$EcoRegion == s[i]])
  test.norm[i] <- shap$p.value
}
test.norm
#test of homoscedasticity
bartlett.test(boot.mean ~ factor(EcoRegion), data = boot.mean)
aov.mod1 <- aov(boot.mean ~ factor(EcoRegion), data = boot.mean)
TukeyHSD(aov.mod1)
par(mar=c(3,6,3,1))
plot(TukeyHSD(aov.mod1), las=1, cex.axis=0.5)

##ANOVA on RN2O of different years
t <- levels(n2o.com$Year)
temp1 <- {}
boot.mean <- {}
boot.mean <- as.data.frame(boot.mean)
for (j in 1:length(t)){
  temp1 <- subset(n2o.com, n2o.com$Year == t[j])
  for (i in (5000*(j-1)+1):(5000*j)){
    boot.sample <- sample(temp1$RN2O[!is.na(temp1$RN2O)], 20, replace = T)  ##Replace the "RN2O"s in this line with "DC"s to switch to ANOVA on DC
    boot.mean[i,1] <- mean(boot.sample)
    boot.mean[i,2] <- t[j]
  }
}
colnames(boot.mean) <- c("boot.mean", "Year")
boot.mean$Year <- factor(boot.mean$Year,
                         levels = c("T1", "T2", "T3", "T4"))
#test of normality
test.norm <- {}
for (i in 1:length(t)){
  shap <- shapiro.test(boot.mean$boot.mean[boot.mean$Year == t[i]])
  test.norm[i] <- shap$p.value
}
test.norm
#test of homoscedasticity
bartlett.test(boot.mean ~ factor(Year), data = boot.mean)
aov.mod2 <- aov(boot.mean ~ factor(Year), data = boot.mean)
par(mar=c(3,6,3,1))
plot(TukeyHSD(aov.mod2), las=1, cex.axis=0.5)

##Figure 5-b
plot.DC <- ggplot(data = n2o9615, aes(x = EcoRegion, y = DC))+
  geom_violin(aes(fill = EcoRegion, color = EcoRegion), alpha = 0.5, stat = "ydensity", position = "dodge", show.legend = F)+
  geom_boxplot(color = "black",stat = "boxplot", position = "dodge2",width = 0.2, outlier.shape = NA, show.legend = F)+
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 2)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
plot.DC.all <- ggplot(data = n2o9615, aes(x = "All", y = DC))+
  geom_violin(color = "black", fill = "grey90", stat = "ydensity", position = "dodge", show.legend = F)+
  geom_boxplot(color = "black",stat = "boxplot", position = "dodge2",width = 0.2, outlier.shape = NA, show.legend = F)+
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 2)+
  scale_color_brewer(palette = "Set2")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
n2o.summ.eco <- group_by(n2o9615, EcoRegion) %>%
  summarize(mean = mean(RN2O, na.rm = T), mean.se = mean(RN2O.se, na.rm = T),
            sum = sum(RN2O, na.rm = T)/100, sum.se = sum(RN2O.se, na.rm = T)/100,
            sum.DC = sum(RN2O*DC/100, na.rm = T)/100, sum.IDC = sum(RN2O*(1-DC/100), na.rm = T)/100, 
            DC = mean(DC, na.rm = T), 
            .groups = "drop")
n2o.summ.eco.DC <- n2o.summ.eco %>%
  gather(sum.DC, sum.IDC, key = "type", value = "value")
n2o.summ.eco.DC$type <- ordered(n2o.summ.eco.DC$type, levels = c("sum.IDC", "sum.DC"))
##Figure 5-a
plot.sum <- ggplot(data = n2o.summ.eco.DC, aes(x = EcoRegion, y = value, fill=type, alpha = 0.7))+
  geom_bar(stat = "identity", position = "stack", show.legend = F)+
  scale_fill_manual(values = c("blue", "red"))+
  scale_y_continuous(limits = c(0,150), n.breaks = 5)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10),
  )
plot.sum.all <- ggplot(data = n2o.summ.eco.DC, aes(x = "All", y = value/1000, fill=type, alpha = 0.7))+
  geom_bar(stat = "identity", position = "stack", show.legend = F)+
  scale_fill_manual(values = c("blue", "red"))+
  scale_y_continuous(limits = c(0,0.3))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10),
  )
##Figure 4-a
plot.mean <- ggplot(data = n2o9615, aes(x = EcoRegion, y = RN2O))+
  geom_violin(stat = "ydensity", aes(fill = EcoRegion, color = EcoRegion), alpha = 0.5, show.legend = F)+
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 3)+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  scale_y_continuous(limits = c(0,5))+
  geom_errorbar(data = n2o.summ.eco, aes(x = EcoRegion, y = mean, ymin = mean - mean.se, ymax = mean+ mean.se), width = 0.2)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
plot.mean.all <- ggplot(data = n2o9615, aes(x = "All", y = RN2O))+
  geom_violin(color = "black", fill = "grey90", stat = "ydensity", position = "dodge", show.legend = F)+
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 3)+
  geom_errorbar(aes(x = "All", y = mean(n2o9615$RN2O, na.rm=T), ymin = mean(n2o9615$RN2O, na.rm=T) - mean(n2o9615$RN2O.se, na.rm=T), 
                    ymax = mean(n2o9615$RN2O, na.rm=T) + mean(n2o9615$RN2O.se, na.rm=T)), width = 0.2)+
  scale_y_continuous(limits = c(0,5))+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
##Figure 4-b
n2o.summ.eco.yr <- group_by(n2o.com, Year, EcoRegion) %>%
  summarize(mean = mean(RN2O, na.rm = T), sum = sum(RN2O, na.rm = T)/100, .groups = "drop")
n2o.summ.eco.yr %>% 
  group_by(EcoRegion)%>%
  summarize(cv = sd(sum)/mean(sum))
Extrapo <- function(x){
  temp <- c(x[1]*1.4-x[2]*0.4, x[1]*1.2-x[2]*0.2,
            seq(x[1], x[2], length.out =6),
            seq(x[2], x[3], length.out =6)[-1],
            seq(x[3], x[4], length.out =6)[-1],
            x[4]*1.2-x[3]*0.2, x[4]*1.4-x[3]*0.4)
  temp
}
s <- levels(n2o.com$EcoRegion)
n2o.summ.extra <- {}
for (i in 1:length(s)){
  n2o.summ.extra <- rbind(n2o.summ.extra,cbind.data.frame(c(1996:2015), 
      s[i], Extrapo(n2o.summ.eco.yr$mean[n2o.summ.eco.yr$EcoRegion==s[i]])))
}
colnames(n2o.summ.extra) <- c("year", "EcoRegion", "mean")
n2o.summ.all.yr <- n2o.com %>%
  group_by(Year) %>%
  summarise(mean = mean(RN2O, na.rm = T))
n2o.summ.extra.all <- cbind.data.frame(c(1996:2015), Extrapo(n2o.summ.all.yr$mean))
colnames(n2o.summ.extra.all) <- c("year", "mean")
plot.eco <- ggplot(data = n2o.summ.extra, aes(x = year, y = mean))+
  geom_line(size = 1, aes(group = EcoRegion, color = EcoRegion), show.legend = F)+
  geom_line(data = n2o.summ.extra.all, aes(x = year, y = mean), color = "black", size = 1, show.legend = F)+
  scale_y_continuous(limits = c(0,2.5))+
  scale_color_brewer(palette = "Set2")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10),
    legend.position = c(0.5, 0.2),
    legend.title = element_blank(),
    legend.box.background = element_rect(size = 0.2), # Box for legend
    legend.key.size = unit(4, unit = 'mm'),
    legend.text = element_text(size = 10),
#    legend.margin = margin(1, 1, 1, 1, unit = 'mm')
  )
plot_grid(plot.sum, plot.sum.all, plot.DC, plot.DC.all, ncol = 4, rel_widths = c(7,2,7,2), align = "h")
plot_grid(plot.mean, plot.mean.all, plot.eco, ncol = 3, rel_widths = c(7,2,7), align = "hv")

##Relative importance analysis
s <- levels(n2o9615$EcoRegion)
output <- data.frame(c(1:6))
temp1 <- {}
summ <- {}
for (i in 1:length(s)){
  temp1 <- subset(n2o9615, n2o9615$EcoRegion == s[i])
  mod1 <- calc.relimp(RN2O ~ MAT + MAP + MAN + Clay + Sand + MAN.CV, 
                      type = c("lmg"),
                      data = temp1, rank = F, rela = T)
  mod1.summ <- summary(lm(RN2O ~ MAT + MAP + MAN + Clay + Sand + MAN.CV, data = temp1))
  relim <- as.data.frame(mod1@lmg)
  colnames(relim) <- s[i]
  summ <- rbind(summ, c(s[i], mod1@nobs, mod1@R2, pf(mod1.summ$fstatistic[1],           
                                                     mod1.summ$fstatistic[2],
                                                     mod1.summ$fstatistic[3],
                                                     lower.tail = FALSE)))
  output <- cbind(output, relim)
}
ord <- hclust(dist(output[,-1], method = "euclidean"), method = "complete" )
##Figure S4
plot(ord, cex=1)
reclass <- rbind.data.frame(output["MAP",]+output["MAT",], output["Sand",]
                            + output["Clay",], output["MAN",], output["MAN.CV",])
reclass[,1] <- c("Climate", "Soil", "MAN", "MAN.CV")
data2 <- reclass %>%
  gather("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR", key = "EcoRegion", value = "value")
data2 <- as.data.frame(data2)
colnames(data2) <- c("Factor", "EcoRegion", "value")
data2$Factor <- ordered(data2$Factor, levels = c("Soil", "Climate", "MAN", "MAN.CV"))
data2$EcoRegion <- ordered(data2$EcoRegion, levels = rev(c("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR")))
plot.spat <- ggplot(data = data2, aes(x = Factor, y = EcoRegion, fill = value))+
  geom_tile(stat = "identity", color = "white")+
  scale_fill_gradient(low = "#ffffff", high = "#003366")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
n2o.summ.cv <-  n2o.com%>%
  group_by(Longitude, Latitude, EcoRegion_ras)%>%
  filter(length(RN2O) == 4)%>%
  summarize(MAT = mean(MAT), MAP = mean(MAP), MAN = mean(MAN),
            MAN.CV = mean(MAN.CV), Clay = mean(Clay), Sand = mean(Sand),
            RN2O.mean = mean(RN2O), RN2O.CV = sd(RN2O)/mean(RN2O))
n2o.summ.cv$EcoRegion <- as.factor(n2o.summ.cv$EcoRegion_ras)
n2o.summ.cv$EcoRegion <- factor(n2o.summ.cv$EcoRegion, labels = c("BorealConiferousForest", "MidSubtropicalEvergreenForest", 
                                                            "NorthSubtropicalMixedForest", "SouthSubtropicalEvergreenForest", 
                                                            "TemperateMixedForest", "TropicalMonsoonRainforest",
                                                            "WarmTemperateDeciduousForest"))
n2o.summ.cv$EcoRegion <- ordered(n2o.summ.cv$EcoRegion, levels = c("BorealConiferousForest", "TemperateMixedForest",
                                                             "WarmTemperateDeciduousForest", "NorthSubtropicalMixedForest",
                                                             "MidSubtropicalEvergreenForest", "SouthSubtropicalEvergreenForest", 
                                                             "TropicalMonsoonRainforest"),
                              labels = c("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR"))
s <- levels(n2o.summ.cv$EcoRegion)
temp1 <- {}
summ <- {}
output <- data.frame(c(1:6))
for (i in 1:length(s)){
  temp1 <- subset(n2o.summ.cv, n2o.summ.cv$EcoRegion == s[i])
  mod2 <- calc.relimp(RN2O.CV ~ MAT + MAP + MAN + Clay + Sand + MAN.CV
                      , type = c("lmg"),
                      data = temp1, rank = F, rela = T)
  mod2.summ <- summary(lm(RN2O.CV ~ MAT + MAP + MAN + Clay + Sand + MAN.CV, data = temp1))
  relim <- as.data.frame(mod2@lmg)
  colnames(relim) <- s[i]
  summ <- rbind(summ, c(s[i], mod2@nobs, mod2@R2, pf(mod2.summ$fstatistic[1],           
                                                     mod2.summ$fstatistic[2],
                                                     mod2.summ$fstatistic[3],
                                                     lower.tail = FALSE)))
  output <- cbind(output, relim)
}
reclass <- rbind.data.frame(output["MAP",]+output["MAT",], output["Sand",]
                            + output["Clay",], output["MAN",], output["MAN.CV",])
reclass[,1] <- c("Climate", "Soil", "MAN", "MAN.CV")
data3 <- reclass %>%
  gather("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR", key = "EcoRegion", value = "value")
data3 <- as.data.frame(data3)
colnames(data3) <- c("Factor", "EcoRegion", "value")
data3$Factor <- ordered(data3$Factor, levels = c("Soil", "Climate", "MAN", "MAN.CV"))
data3$EcoRegion <- ordered(data3$EcoRegion, levels = rev(c("BCF", "TMF", "WDF", "NMF", "MEF", "SEF", "TMR")))
plot.temp <- ggplot(data = data3, aes(x = Factor, y = EcoRegion, fill = value))+
  geom_tile(stat = "identity", color = "white")+
  scale_fill_gradient(low = "#ffffff", high = "#003366")+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.4, vjust = 1.1, size = 10), # Angle x-axis text
    axis.text.y = element_text(size = 10), # Size y-axis numbers
    axis.ticks = element_blank(), # No ticks
    axis.title.x = element_text(size = 10), # No x-axis title
    axis.title.y = element_text(size = 10), # Size y-axis title
    panel.grid.minor = element_blank(), # No minor grid lines
    panel.grid.major.x = element_blank(), # No major x-axis grid lines
    panel.grid.major.y = element_blank(),
    text=element_text(size = 10)
  )
plot_grid(plot.spat, plot.temp, ncol = 2, align = "h")
