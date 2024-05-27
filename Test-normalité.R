
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


  
  
library(nparcomp)
test <- kruskal.test(Valeurs ~ Conditions, data = Viab48h)
dunnTest(Valeurs ~ Conditions, data=Viab48h, method="hs")

summary(comp_a <- nparcomp(Valeurs ~ Conditions, data=Viab48h, type="Dunnett", control = "CTL"))
plot(comp_a)

summary(comp_b <- nparcomp(Valeurs ~ Conditions, data=Viab48h, type="Tukey", control = "CTL"))
plot(comp_b)


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

shapiro.test(AnovaModel.sox30mCN$residuals)
car::qqPlot(AnovaModel.sox30mCN$residuals)

shapiro.test(AnovaModel.sox1hCN$residuals)
car::qqPlot(AnovaModel.sox1hCN$residuals)

shapiro.test(AnovaModel.sox2hCN$residuals)
car::qqPlot(AnovaModel.sox2hCN$residuals)

shapiro.test(AnovaModel.sox4hCN$residuals)
car::qqPlot(AnovaModel.sox4hCN$residuals)

shapiro.test(AnovaModel.sox6hCN$residuals)
car::qqPlot(AnovaModel.sox6hCN$residuals)

shapiro.test(AnovaModel.sox24hCN$residuals)
car::qqPlot(AnovaModel.sox24hCN$residuals)

shapiro.test(AnovaModel.sox48hCN$residuals)
car::qqPlot(AnovaModel.sox48hCN$residuals)


shapiro.test(AnovaModel.sox30mSP$residuals)
car::qqPlot(AnovaModel.sox30mSP$residuals)

shapiro.test(AnovaModel.sox1hSP$residuals)
car::qqPlot(AnovaModel.sox1hSP$residuals)

shapiro.test(AnovaModel.sox2hSP$residuals)
car::qqPlot(AnovaModel.sox2hSP$residuals)

shapiro.test(AnovaModel.sox4hSP$residuals)
car::qqPlot(AnovaModel.sox4hSP$residuals)

shapiro.test(AnovaModel.sox6hSP$residuals)
car::qqPlot(AnovaModel.sox6hSP$residuals)

shapiro.test(AnovaModel.sox24hSP$residuals)
car::qqPlot(AnovaModel.sox24hSP$residuals)

shapiro.test(AnovaModel.sox48hSP$residuals)
car::qqPlot(AnovaModel.sox48hSP$residuals)


shapiro.test(AnovaModel.sox30mCNSP$residuals)
car::qqPlot(AnovaModel.sox30mCNSP$residuals)

shapiro.test(AnovaModel.sox1hCNSP$residuals)
car::qqPlot(AnovaModel.sox1hCNSP$residuals)

shapiro.test(AnovaModel.sox2hCNSP$residuals)
car::qqPlot(AnovaModel.sox2hCNSP$residuals)

shapiro.test(AnovaModel.sox4hCNSP$residuals)
car::qqPlot(AnovaModel.sox4hCNSP$residuals)

shapiro.test(AnovaModel.sox6hCNSP$residuals)
car::qqPlot(AnovaModel.sox6hCNSP$residuals)

shapiro.test(AnovaModel.sox24hCNSP$residuals)
car::qqPlot(AnovaModel.sox24hCNSP$residuals)

shapiro.test(AnovaModel.sox48hCNSP$residuals)
car::qqPlot(AnovaModel.sox48hCNSP$residuals)
