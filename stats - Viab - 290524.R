library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)

Viab1h <- read_excel("E:/Mémoire/Viab-SOx/1h/Viab1H.xlsx")

Viab2h <- read_excel("E:/Mémoire/Viab-SOx/2h/Viab2H.xlsx")

Viab4h <- read_excel("E:/Mémoire/Viab-SOx/4h/Viab4H.xlsx")

Viab6h <- read_excel("E:/Mémoire/Viab-SOx/6h/Viab6H.xlsx")

Viab24h <- read_excel("E:/Mémoire/Viab-SOx/24h/Viab24H.xlsx")

Viab48h <- read_excel("E:/Mémoire/Viab-SOx/48h/Viab48H.xlsx")

Viab30m <- read_excel("E:/Mémoire/Viab-SOx/30m/Viab30m.xlsx")

Viab1hSC <- Viab1h[-1,]
Viab1hSC <- Viab1hSC[-1,]
Viab1hSC <- Viab1hSC[-1,]

Viab2hSC <- Viab2h[-1,]
Viab2hSC <- Viab2hSC[-1,]
Viab2hSC <- Viab2hSC[-1,]

Viab4hSC <- Viab4h[-1,]
Viab4hSC <- Viab4hSC[-1,]
Viab4hSC <- Viab4hSC[-1,]

Viab6hSC <- Viab6h[-1,]
Viab6hSC <- Viab6hSC[-1,]
Viab6hSC <- Viab6hSC[-1,]

Viab24hSC <- Viab24h[-1,]
Viab24hSC <- Viab24hSC[-1,]
Viab24hSC <- Viab24hSC[-1,]

Viab24hSC <- Viab24h[-1,]
Viab24hSC <- Viab24hSC[-1,]
Viab24hSC <- Viab24hSC[-1,]

Viab48hSC <- Viab48h[-1,]
Viab48hSC <- Viab48hSC[-1,]
Viab48hSC <- Viab48hSC[-1,]

Viab30mSC <- Viab30m[-1,]
Viab30mSC <- Viab30mSC[-1,]
Viab30mSC <- Viab30mSC[-1,]

HViab1H <- bartlett.test(Viab ~ Conditions, data=Viab1hSC) #Test de Bartlett pour 
# voir si les données sont homoscédastiques (p-value < 0,5) 
print(HViab1H) #Ca c'est pour afficher les valeurs du test généré juste au dessus

modelViab1h <- lm(Viab ~ Conditions, data = Viab1hSC) #Création du modèle pour tester les résidus
ggqqplot(residuals(modelViab1h)) #Visualisation graphique de la distribution des res 

shapiro.test(residuals(modelViab1h)) #Test de Shapiro pour la normalité

Viab1hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab) #Test qui permet d'identifier les outliers

res.aovViab1h <- Viab1hSC %>% anova_test(Viab ~ Conditions) #Test ANOVA vu que c'est paramétrique
res.aovViab1h #Affichage des résultats obtenus 

mviab1h <- Viab1hSC %>% tukey_hsd(Viab ~ Conditions) #Post test de Tukey qui compare les groupes entre eux
mviab1h #Affichage du résultat


HViab2H <- bartlett.test(Viab ~ Conditions, data=Viab2hSC)
print(HViab2H)

modelViab2h <- lm(Viab ~ Conditions, data = Viab2hSC)
ggqqplot(residuals(modelViab2h))

shapiro.test(residuals(modelViab2h))

Viab2hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

kViab2h <- kruskal.test(Viab ~ Conditions, data=Viab2hSC)
print(kViab2h)

res.aovViab2h <- Viab2hSC %>% anova_test(Viab ~ Conditions)
res.aovViab2h

mviab2h <- Viab2hSC %>% tukey_hsd(Viab ~ Conditions)
mviab2h


HViab4H <- bartlett.test(Viab ~ Conditions, data=Viab4hSC)
print(HViab4H)

modelViab4h <- lm(Viab ~ Conditions, data = Viab4hSC)
ggqqplot(residuals(modelViab4h))

shapiro.test(residuals(modelViab4h))

Viab4hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

res.aovViab4h <- Viab4hSC %>% anova_test(Viab ~ Conditions)
res.aovViab4h

mviab4h <- Viab4hSC %>% tukey_hsd(Viab ~ Conditions)
mviab4h


HViab6H <- bartlett.test(Viab ~ Conditions, data=Viab6hSC)
print(HViab6H)

modelViab6h <- lm(Viab ~ Conditions, data = Viab6hSC)
ggqqplot(residuals(modelViab6h))

shapiro.test(residuals(modelViab6h))

Viab6hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

res.aovViab6h <- Viab6hSC %>% anova_test(Viab ~ Conditions)
res.aovViab6h

mviab6h <- Viab6hSC %>% tukey_hsd(Viab ~ Conditions)
mviab6h


HViab24H <- bartlett.test(Viab ~ Conditions, data=Viab24hSC)
print(HViab24H)

modelViab24h <- lm(Viab ~ Conditions, data = Viab24hSC)
ggqqplot(residuals(modelViab24h))

shapiro.test(residuals(modelViab24h))

Viab24hSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

res.aovViab24h <- Viab24hSC %>% anova_test(Viab ~ Conditions)
res.aovViab24h

mviab24h <- Viab24hSC %>% tukey_hsd(Viab ~ Conditions)
mviab24h


HViab48H <- bartlett.test(Viab ~ Conditions, data=Viab48h)
print(HViab48H)

modelViab48h <- lm(Viab ~ Conditions, data = Viab48hSC)
ggqqplot(residuals(modelViab48h))

shapiro.test(residuals(modelViab48h))

Viab48h %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

res.aovViab48h <- Viab48h %>% anova_test(Viab ~ Conditions)
res.aovViab48h

mviab48h <- Viab48h %>% tukey_hsd(Viab ~ Conditions)
mviab48h


HViab30m <- bartlett.test(Viab ~ Conditions, data=Viab30mSC)
print(HViab4H)

modelViab30m <- lm(Viab ~ Conditions, data = Viab30mSC)
ggqqplot(residuals(modelViab30m))

shapiro.test(residuals(modelViab30m))

Viab30mSC %>% 
  group_by(Conditions) %>%
  identify_outliers(Viab)

res.aovViab30m <- Viab30mSC %>% anova_test(Viab ~ Conditions)
res.aovViab30m

mviab30m <- Viab30mSC %>% tukey_hsd(Viab ~ Conditions)
mviab30m

t.test(Viab1hSC$Viab, mu = 1)

t.test(Viab2hSC$Viab, mu = 1)

t.test(Viab4hSC$Viab, mu = 1)
t.test(Viab6hSC$Viab, mu = 1)

t.test(Viab24hSC$Viab, mu = 1)

t.test(Viab48hSC$Viab, mu = 1)