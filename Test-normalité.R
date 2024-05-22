
shapiro.test(AnovaModel.Viab1h$residuals)
car::qqPlot(AnovaModel.Viab1h$residuals)

shapiro.test(AnovaModel.Viab30m$residuals)
car::qqPlot(AnovaModel.Viab30m$residuals)

shapiro.test(AnovaModel.Viab2h$residuals)
car::qqPlot(AnovaModel.Viab2h$residuals)

shapiro.test(AnovaModel.Viab4h$residuals)
car::qqPlot(AnovaModel.Viab4h$residuals)

shapiro.test(AnovaModel.Viab6h$residuals)
car::qqPlot(AnovaModel.Viab6h$residuals)

shapiro.test(AnovaModel.Viab24h$residuals)
car::qqPlot(AnovaModel.Viab24h$residuals)

shapiro.test(AnovaModel.Viab48h$residuals)
car::qqPlot(AnovaModel.Viab48h$residuals)


--------------------------------------------------
  
  
  
kruskal.test(Valeurs ~ Conditions, data = Viab48h)
dunnTest(Valeurs ~ Conditions, data=Viab48h, method="hs")