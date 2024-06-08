library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dunn.test)
library(dplyr)

MECGlut <- MEC %>%
  select(Conditions, Glutamine, Glut = Glutamine)

MECGlut$Glut <- as.numeric(MECGlut$Glut)
  
Glut <- wilcox_test(Glut ~ Conditions, data = MECGlut)
Glut

MecM1 <- MEC %>%
  select(Conditions, Malate1)

MecM1$Malate1 <- as.numeric(MecM1$Malate1)

Mal1 <- wilcox_test(Malate1 ~ Conditions, data = MecM1)
Mal1

MecM2 <- MEC %>%
  select(Conditions, Malate2)

MecM2$Malate2 <- as.numeric(MecM2$Malate2)

Mal2 <- wilcox_test(Malate2 ~ Conditions, data = MecM2)
Mal2

MecM3 <- MEC %>%
  select(Conditions, Malate3)

MecM3$Malate3 <- as.numeric(MecM3$Malate3)

Mal3 <- wilcox_test(Malate3 ~ Conditions, data = MecM3)
Mal3

MecM4 <- MEC %>%
  select(Conditions, Malate4)

MecM4$Malate4 <- as.numeric(MecM4$Malate4)

Mal4 <- wilcox_test(Malate4 ~ Conditions, data = MecM4)
Mal4

MecM5 <- MEC %>%
  select(Conditions, Malate5)

MecM5$Malate5 <- as.numeric(MecM5$Malate5)

Mal5 <- wilcox_test(Malate5 ~ Conditions, data = MecM5)
Mal5



MecX1 <- MECtest %>%
  select(Conditions, X1.57)

MecX1$X1.57 <- as.numeric(MecX1$X1.57)

X1 <- wilcox_test(X1.57 ~ Conditions, data = MecX1)
X1

MecX2 <- MECtest %>%
  select(Conditions, X1.58)

MecX2$X1.58 <- as.numeric(MecX2$X1.58)

X2 <- wilcox_test(X1.58 ~ Conditions, data = MecX2)
X2
