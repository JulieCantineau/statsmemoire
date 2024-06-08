library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(dunn.test)
library(dplyr)

Metha <- read_excel("E:/Mémoire/stats-RMN-metha.xlsx", 
 col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric"))

MLac1 <- Metha %>%
  select(Conditions, Lactate)

Lac1 <- wilcox_test(Lactate ~ Conditions, data = MLac1)
Lac1

MAlanine <- Metha %>%
  select(Conditions, Alanine)

Alanine <- wilcox_test(Alanine ~ Conditions, data = MAlanine)
Alanine

MAcetate <- Metha %>%
  select(Conditions, Acétate)

Ac <- wilcox_test(Acétate ~ Conditions, data = MAcetate)
Ac


MGluT1 <- Metha %>%
  select(Conditions, Glutamate1)

GluT1 <- wilcox_test(Glutamate1 ~ Conditions, data = MGluT1)
GluT1

MGluT2 <- Metha %>%
  select(Conditions, Glutamate2)

GluT2 <- wilcox_test(Glutamate2 ~ Conditions, data = MGluT2)
GluT2

MGluTM1 <- Metha %>%
  select(Conditions, GlutamineGlutamate)

GluTM1 <- wilcox_test(GlutamineGlutamate ~ Conditions, data = MGluTM1)
GluTM1

MGluM1 <- Metha %>%
  select(Conditions, Glutamine)

GluM1 <- wilcox_test(Glutamine ~ Conditions, data = MGluM1)
GluM1

MX295 <- Metha %>%
  select(Conditions, X295)

X295 <- wilcox_test(X295 ~ Conditions, data = MX295)
X295

MCreat <- Metha %>%
  select(Conditions, Creatine)

Creat <- wilcox_test(Creatine ~ Conditions, data = MCreat)
Creat

MPCreat <- Metha %>%
  select(Conditions, Phosphocreatine)

PCreat <- wilcox_test(Phosphocreatine ~ Conditions, data = MPCreat)
PCreat

MCholine <- Metha %>%
  select(Conditions, Choline)

Choline <- wilcox_test(Choline ~ Conditions, data = MCholine)
Choline

MOPCholine <- Metha %>%
  select(Conditions, Ophosphocoline)

OPCholine <- wilcox_test(Ophosphocoline ~ Conditions, data = MOPCholine)
OPCholine

MGluc <- Metha %>%
  select(Conditions, Glucose0)

Gluc0 <- wilcox_test(Glucose0 ~ Conditions, data = MGluc)
Gluc0

MGluc1 <- Metha %>%
  select(Conditions, Glucose1)

Gluc1 <- wilcox_test(Glucose1 ~ Conditions, data = MGluc1)
Gluc1

MGluc2 <- Metha %>%
  select(Conditions, Glucose2)

Gluc2 <- wilcox_test(Glucose2 ~ Conditions, data = MGluc2)
Gluc2

MGluc3 <- Metha %>%
  select(Conditions, Glucose3)

Gluc3 <- wilcox_test(Glucose3 ~ Conditions, data = MGluc3)
Gluc3

MMI <- Metha %>%
  select(Conditions, Myoinositol)

MI1 <- wilcox_test(Myoinositol ~ Conditions, data = MMI)
MI1

MMI2 <- Metha %>%
  select(Conditions, Myoinositol2)

MI2 <- wilcox_test(Myoinositol2 ~ Conditions, data = MMI2)
MI2

MX355 <- Metha %>%
  select(Conditions, X355)

X355 <- wilcox_test(X355 ~ Conditions, data = MX355)
X355

MMI3 <- Metha %>%
  select(Conditions, Myoinositol3)

MI3 <- wilcox_test(Myoinositol3 ~ Conditions, data = MMI3)
MI3

MGluTM2 <- Metha %>%
  select(Conditions, GlutamineGlutamate2)

GluTM2 <- wilcox_test(GlutamineGlutamate2 ~ Conditions, data = MGluTM2)
GluTM2

MGluc4 <- Metha %>%
  select(Conditions, Glucose4)

Gluc4 <- wilcox_test(Glucose4 ~ Conditions, data = MGluc4)
Gluc4

MGluc5 <- Metha %>%
  select(Conditions, Glucose5)

Gluc5 <- wilcox_test(Glucose5 ~ Conditions, data = MGluc5)
Gluc5

MGluc6 <- Metha %>%
  select(Conditions, Glucose6)

Gluc6 <- wilcox_test(Glucose6 ~ Conditions, data = MGluc6)
Gluc6

MGluc7 <- Metha %>%
  select(Conditions, Glucose7)

Gluc7 <- wilcox_test(Glucose7 ~ Conditions, data = MGluc7)
Gluc7

MPCreat2 <- Metha %>%
  select(Conditions, Phosphocreatine2)

PCreat2 <- wilcox_test(Phosphocreatine2 ~ Conditions, data = MPCreat2)
PCreat2

MMI4 <- Metha %>%
  select(Conditions, Myoinositol4)

MI4 <- wilcox_test(Myoinositol4 ~ Conditions, data = MMI4)
MI4

MLac2 <- Metha %>%
  select(Conditions, Lactate2)

Lac2 <- wilcox_test(Lactate2 ~ Conditions, data = MLac2)
Lac2

MLac3 <- Metha %>%
  select(Conditions, lactate3)

Lac3 <- wilcox_test(lactate3 ~ Conditions, data = MLac3)
Lac3

MGluc8 <- Metha %>%
  select(Conditions, Glucose8)

Gluc8 <- wilcox_test(Glucose8 ~ Conditions, data = MGluc8)
Gluc8

MFA <- Metha %>%
  select(Conditions, Fumarate)

FA <- wilcox_test(Fumarate ~ Conditions, data = MFA)
FA
