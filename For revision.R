library(readxl)
library(tidyverse)
library(psych)
library(mgcv)
library(MLmetrics)
library(randomForest)
setwd() ##set working directory i.e. location of the data files
data0 <- read_xlsx("ds01_ForestN2Odataset.xlsx")
N2O.all <- cbind.data.frame(data0$Longitude, data0$Latitude, data0$EcoRegion, data0$N_Add_Std,
                            data0$`RN2O(kgN ha-1 yr-1)`, data0$`MAT(K)`, data0$MAT.CV, data0$`MAP(mm)`, data0$MAP.CV, data0$`Ndepo(kgN ha-1 yr-1)`, data0$`MAN(kgN ha-1 yr-1)`, 
                            data0$MAN.CV, data0$`Clay(%)`, data0$`Sand(%)`, data0$References)
colnames(N2O.all) <- c("Long", "Lati", "EcoRegion", "N_Add",  "RN2O", "MAT", "MAT.CV", "MAP", "MAP.CV", "Ndepo", "MAN", 
                       "MAN.CV", "Clay", "Sand", "Reference")
N2O.all$N_Add_adj <- N2O.all$N_Add + N2O.all$Ndepo

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

###for comment#6
a <- 100
k <- 20
SWC <- rnorm(a*a, mean = 50, sd = 10)
pH <- rnorm(a*a, mean = 5, sd = 1)
b1 <- rnorm(a*a, mean = 0.001, sd = 0.0002)
b2 <- rnorm(a*a, mean = 0.01, sd = 0.002)
b3 <- rnorm(a*a, mean = 0.1, sd = 0.02)
space1 <- matrix(data = b1 * SWC + b2 * pH + b3, nrow = a, ncol = a)
heatmap(space1, Rowv = NA, Colv = NA)
samples <- data.frame(rep(1, k), sample(space1, k, replace = T))
colnames(samples) <- c("scale", "value")
sample.scale <- function(space1, scale){
  u <- scale
  x <- sample(c(ceiling(u/2):(a-ceiling(u/2)+1)), k, replace = T)
  y <- sample(c(ceiling(u/2):(a-ceiling(u/2)+1)), k, replace = T)
  tmp <- {}
  for (i in 1:k){
  xsub <- c((x[i] - floor(u/2)): (x[i] + floor(u/2)))
  ysub <- c((y[i] - floor(u/2)): (y[i] + floor(u/2)))
  tmp[i] <- harmonic.mean(space1[xsub, ysub])
  }
  tmp
}
for (s in 1:49){
  sample.new <- cbind(rep(2*s+1, k), sample.scale(space1, 2*s+1))
  colnames(sample.new) <- c("scale", "value")
  samples <- rbind(samples, sample.new)
}
ggplot(data = samples, aes(x = scale, y = value)) +
  geom_boxplot(aes(group = scale), color = "grey") +
  ylab("r_N2O:Ngas")+
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

###for comment#7
##Random forest
rf.data <- mod.summ[, c(4:12)]
#run random forest regression for 1000 times with different subsets as training data
k <- sample(1:10000, 1000)
output <- {}
for (i in 1:1000){
  set.seed(k[i])
  rf.train <- rf.data[-sample(c(1:nrow(rf.data)), ceiling(0.2*nrow(rf.data))), ]
  set.seed(k[i])
  rf.test <- rf.data[sample(c(1:nrow(rf.data)), ceiling(0.2*nrow(rf.data))), ]
  rf_mod <- randomForest(rf.train$fLN ~., data = rf.train,
                         mtry = 3, ntree = 1000, importance = TRUE, proximity = TRUE) 
  output <- rbind.data.frame(output, c(k[i], rf_mod$rsq[1000]))
}
#Test model with the rest 20% data
a <- output[output[,2] == max(output[,2]), 1]
set.seed(a)
rf.train <- rf.data[-sample(c(1:nrow(rf.data)), ceiling(0.2*nrow(rf.data))), ]
set.seed(a)
rf.test <- rf.data[sample(c(1:nrow(rf.data)), ceiling(0.2*nrow(rf.data))), ]
rf_mod <- randomForest(rf.train$fLN ~., data = rf.train,
                       mtry = 3, ntree = 1000, importance = TRUE, proximity = TRUE)   
print(rf_mod)
pred <- predict(rf_mod, newdata = rf.test)  
cor(pred, rf.test$fLN)  

###for comment#9
mod.fLN <- gam(fLN ~  MAT * Sand * log(MAN.CV) * MAP
               - MAT:Sand:log(MAN.CV):MAP - MAT:log(MAN.CV):MAP
               - log(MAN.CV):MAP - Sand:log(MAN.CV):MAP - MAT:MAP
               - Sand:MAP - MAT:Sand:MAP - MAT:log(MAN.CV) - MAP
               - log(MAN.CV)
               ,data = mod.summ[-c(5,11,9),], family = "quasipoisson"
               , drop.intercept = T)
mod.R0 <- gam(sqrt(R0) ~ MAP * MAT * Clay * MAN
              - Clay - MAT:MAN - MAT - MAP:MAT:Clay - MAT:Clay
              - MAP:MAT - MAN:Clay - MAP
              , data = mod.summ[-c(5,11,17),], family = "gaussian"
              , drop.intercept = T)
mod.test <- data.frame(mod.valid, predict.gam(mod.fLN, newdata = mod.valid, se.fit = T))
mod.test <- data.frame(mod.test, predict.gam(mod.R0, newdata = mod.valid, se.fit = T))
mod.test$RN2O.pred <- exp(mod.test$fit) * mod.test$N_Add_adj + (mod.test$fit.1)^2
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

###for comment#19
logfLN <- rnorm(1000, mean = -4, sd = 1)
sqrtR0 <- rnorm(1000, mean = 1, sd = 0.5)
err.rate <- rnorm(1000, mean = 0, sd = 0.5)
# err.rate <- rnorm(1000, mean = 0, sd = 0.1)
Ndepo <- rnorm(1000, 20, sd = 3)
val.tr <- exp(logfLN) * Ndepo + sqrtR0^2
val.obs <- (exp(logfLN) * Ndepo + (sqrtR0)^2) *(1 + err.rate)
dat1 <- cbind.data.frame(val.obs, val.tr)
ggplot(data = dat1, aes(x = val.obs, y = val.tr)) +
  geom_point(color = "grey") +
  geom_smooth(method = "lm", formula = "y ~ x+0", fill = "pink",color = "red")+
  geom_abline() + 
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
mean(dat1$val.obs)/mean(dat1$val.tr)
