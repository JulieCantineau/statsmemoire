library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dunn.test)

HRos1H1 <- bartlett.test(Ros ~ Conditions, data=Ros1h)
print(HRos1H1)

modelRos1h1 <- lm(Ros ~ Conditions, data = Ros1h)
ggqqplot(residuals(modelRos1h1))

shapiro.test(residuals(modelRos1h))

Ros1h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos1h1 <- Ros1h %>% anova_test(Ros ~ Conditions)
res.aovRos1h1

mros1h1 <- Ros1h %>% tukey_hsd(Ros ~ Conditions)
print(n = "22", mros1h1)


HRos2H1 <- bartlett.test(Ros ~ Conditions, data=Ros2h)
print(HRos2H1)

modelRos2h1 <- lm(Ros ~ Conditions, data = Ros2h)
ggqqplot(residuals(modelRos2h1))

shapiro.test(residuals(modelRos1h))

Ros2h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

kros2h <- kruskal.test(Ros ~ Conditions, data=Ros2h)
print(kros2h)

res.aovRos2h1 <- Ros2h %>% anova_test(Ros ~ Conditions)
res.aovRos2h1

mros2h1 <- Ros2h %>% tukey_hsd(Ros ~ Conditions)
print(n = "22", mros2h1)



HRos4H1 <- bartlett.test(Ros ~ Conditions, data=Ros4h)
print(HRos4H1)

modelRos4h1 <- lm(Ros ~ Conditions, data = Ros4h)
ggqqplot(residuals(modelRos4h1))

shapiro.test(residuals(modelRos4h1))

Ros4h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos4h1 <- Ros4h %>% anova_test(Ros ~ Conditions)
res.aovRos4h1

mros4h1 <- Ros4h %>% tukey_hsd(Ros ~ Conditions)
print(n = "22", mros4h1)


HRos6H1 <- bartlett.test(Ros ~ Conditions, data=Ros6h)
print(HRos6H1)

modelRos6h1 <- lm(Ros ~ Conditions, data = Ros6h)
ggqqplot(residuals(modelRos6h1))

shapiro.test(residuals(modelRos6h1))

Ros6h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos6h1 <- Ros6h %>% anova_test(Ros ~ Conditions)
res.aovRos6h1

mros6h1 <- Ros6h %>% tukey_hsd(Ros ~ Conditions)
mros6h1
print(n = "22", mros6h1)



HRos24H1 <- bartlett.test(Ros ~ Conditions, data=Ros24h)
print(HRos24H1)

modelRos24h1 <- lm(Ros ~ Conditions, data = Ros24h)
ggqqplot(residuals(modelRos24h1))

shapiro.test(residuals(modelRos24h1))

Ros24h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos24h1 <- Ros24h %>% anova_test(Ros ~ Conditions)
res.aovRos24h1

kros24h <- kruskal.test(Ros ~ Conditions, data=Ros24h)
print(kros24h)

a <- dunn_test(Ros ~ Conditions, data = Ros24h, p.adjust.method="")
print(n = "26", a)

dunn.test(Ros24h$Ros, Ros24h$Conditions, method = "hs")

mros24h1 <- Ros24h %>% tukey_hsd(Ros ~ Conditions)
mros24h1
print(n = "22", mros24h1)


HRos48H1 <- bartlett.test(Ros ~ Conditions, data=Ros48h)
print(HRos48H1)

modelRos48h1 <- lm(Ros ~ Conditions, data = Ros48h)
ggqqplot(residuals(modelRos48h1))

shapiro.test(residuals(modelRos48h1))

Ros48h %>% 
  group_by(Conditions) %>%
  identify_outliers(Ros)

res.aovRos48h1 <- Ros48h %>% anova_test(Ros ~ Conditions)
res.aovRos48h1

mros48h1 <- Ros48h %>% tukey_hsd(Ros ~ Conditions)
mros48h1
print(n = "22", mros48h1)
