library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
Ros1h <- read_excel("E:/Mémoire/Viab-SOx/1h/Ros1H.xlsx")

Ros2h <- read_excel("E:/Mémoire/Viab-SOx/2h/Ros2H.xlsx")

Ros4h <- read_excel("E:/Mémoire/Viab-SOx/4h/Ros4H.xlsx")

Ros6h <- read_excel("E:/Mémoire/Viab-SOx/6h/Ros6H.xlsx")

Ros24h <- read_excel("E:/Mémoire/Viab-SOx/24h/Ros24H.xlsx")

Ros48h <- read_excel("E:/Mémoire/Viab-SOx/48h/Ros48H.xlsx")

Ros30m <- read_excel("E:/Mémoire/Viab-SOx/30m/Ros30m.xlsx")

Ros1hSC <- Ros1h[-4,]
Ros1hSC <- Ros1hSC[-4,]
Ros1hSC <- Ros1hSC[-4,]

Ros2hSC <- Ros2h[-4,]
Ros2hSC <- Ros2hSC[-4,]
Ros2hSC <- Ros2hSC[-4,]

Ros4hSC <- Ros4h[-4,]
Ros4hSC <- Ros4hSC[-4,]
Ros4hSC <- Ros4hSC[-4,]

Ros6hSC <- Ros6h[-4,]
Ros6hSC <- Ros6hSC[-4,]
Ros6hSC <- Ros6hSC[-4,]

Ros24hSC <- Ros24h[-4,]
Ros24hSC <- Ros24hSC[-4,]
Ros24hSC <- Ros24hSC[-4,]

Ros48hSC <- Ros48h[-4,]
Ros48hSC <- Ros48hSC[-4,]
Ros48hSC <- Ros48hSC[-4,]

Ros30mSC <- Ros30m[-4,]
Ros30mSC <- Ros30mSC[-4,]
Ros30mSC <- Ros30mSC[-4,]

HRos1H <- bartlett.test(Ros ~ Conditions, data=Ros1hSC)
print(HRos1H)

modelRos1h <- lm(Ros ~ Conditions, data = Ros1hSC)
ggqqplot(residuals(modelRos1h))

shapiro.test(residuals(modelRos1h))

Ros1hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos1h <- Ros1hSC %>% anova_test(Ros ~ Conditions)
res.aovRos1h

mros1h <- Ros1hSC %>% tukey_hsd(Ros ~ Conditions)
mros1h


  
HRos2H <- bartlett.test(Ros ~ Conditions, data=Ros2hSC)
print(HRos2H)

modelRos2h <- lm(Ros ~ Conditions, data = Ros2hSC)
ggqqplot(residuals(modelRos2h))

shapiro.test(residuals(modelRos2h))

Ros2hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos2h <- Ros2hSC %>% anova_test(Ros ~ Conditions)
res.aovRos2h

mros2h <- Ros2hSC %>% tukey_hsd(Ros ~ Conditions)
mros2h




HRos4H <- bartlett.test(Ros ~ Conditions, data=Ros4hSC)
print(HRos4H)

modelRos4h <- lm(Ros ~ Conditions, data = Ros4hSC)
ggqqplot(residuals(modelRos4h))

shapiro.test(residuals(modelRos4h))

Ros4hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos4h <- Ros4hSC %>% anova_test(Ros ~ Conditions)
res.aovRos4h

mros4h <- Ros4hSC %>% tukey_hsd(Ros ~ Conditions)
mros4h



HRos6H <- bartlett.test(Ros ~ Conditions, data=Ros6hSC)
print(HRos6H)

modelRos6h <- lm(Ros ~ Conditions, data = Ros6hSC)
ggqqplot(residuals(modelRos6h))

shapiro.test(residuals(modelRos6h))

Ros6hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos6h <- Ros6hSC %>% anova_test(Ros ~ Conditions)
res.aovRos6h

mros6h <- Ros6hSC %>% tukey_hsd(Ros ~ Conditions)
mros6h


HRos24H <- bartlett.test(Ros ~ Conditions, data=Ros24hSC)
print(HRos24H)

modelRos24h <- lm(Ros ~ Conditions, data = Ros24hSC)
ggqqplot(residuals(modelRos24h))

shapiro.test(residuals(modelRos24h))

Ros24hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos24h <- Ros24hSC %>% anova_test(Ros ~ Conditions)
res.aovRos24h

mros24h <- Ros24hSC %>% tukey_hsd(Ros ~ Conditions)
mros24h


HRos48H <- bartlett.test(Ros ~ Conditions, data=Ros48hSC)
print(HRos48H)

modelRos48h <- lm(Ros ~ Conditions, data = Ros48hSC)
ggqqplot(residuals(modelRos48h))

shapiro.test(residuals(modelRos48h))

Ros48hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos48h <- Ros48hSC %>% anova_test(Ros ~ Conditions)
res.aovRos48h

mros48h <- Ros48hSC %>% tukey_hsd(Ros ~ Conditions)
mros48h


HRos30m <- bartlett.test(Ros ~ Conditions, data=Ros30mSC)
print(HRos30m)

modelRos30m <- lm(Ros ~ Conditions, data = Ros30mSC)
ggqqplot(residuals(modelRos30m))

shapiro.test(residuals(modelRos30m))

Ros30mSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos30m <- Ros30mSC %>% anova_test(Ros ~ Conditions)
res.aovRos30m

mros30m <- Ros30mSC %>% tukey_hsd(Ros ~ Conditions)
mros30m
