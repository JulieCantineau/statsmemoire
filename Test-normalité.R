
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
  
test <- kruskal.test(Valeurs ~ Conditions, data = Viab48h)
dunnTest(Valeurs ~ Conditions, data=Viab48h, method="hs")

summary(comp_a <- nparcomp(Valeurs ~ Conditions, data=Viab48h, type="Dunnett", control = "CTL"))
plot(comp_a)


  
shapiro.test(AnovaModel.sox1h$residuals)
car::qqPlot(AnovaModel.sox1h$residuals)

shapiro.test(AnovaModel.sox30m$residuals)
car::qqPlot(AnovaModel.sox30m$residuals)

shapiro.test(AnovaModel.sox2h$residuals)
car::qqPlot(AnovaModel.sox2h$residuals)

shapiro.test(AnovaModel.sox4h$residuals)
car::qqPlot(AnovaModel.sox4h$residuals)

shapiro.test(AnovaModel.sox6h$residuals)
car::qqPlot(AnovaModel.sox6h$residuals)

shapiro.test(AnovaModel.sox24h$residuals)
car::qqPlot(AnovaModel.sox24h$residuals)

shapiro.test(AnovaModel.sox48h$residuals)
car::qqPlot(AnovaModel.sox48h$residuals)
