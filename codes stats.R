Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox1h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox1h)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox30m) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox30m)
library(abind, pos=23)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox2h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox2h)
AnovaModel.1 <- aov(Ros ~ Conditions, data = sox2h)
summary(AnovaModel.1)
with(sox2h, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                              'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox4h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox4h)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox6h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox6h)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox24h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox24h)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox48h) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox48h)
sox1hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/1h/nRos1h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox1hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox1hCN)
AnovaModel.sox1hCN <- aov(Ros ~ Conditions, data = sox1hCN)
summary(AnovaModel.sox1hCN)
with(sox1hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox1hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox1hCN)
sox30mCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/30m/nRos30m-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox30mCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox30mCN)
AnovaModel.sox30mCN <- aov(Ros ~ Conditions, data = sox30mCN)
summary(AnovaModel.sox30mCN)
with(sox30mCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox30mCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox30mCN)
sox2hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/2h/nRos2h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox2hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox2hCN)
AnovaModel.sox2hCN <- aov(Ros ~ Conditions, data = sox2hCN)
summary(AnovaModel.sox2hCN)
with(sox2hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox2hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox2hCN)
sox4hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/4h/nRos4h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox4hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox4hCN)
AnovaModel.sox4hCN <- aov(Ros ~ Conditions, data = sox4hCN)
summary(AnovaModel.sox4hCN)
with(sox4hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox4hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox4hCN)
sox6hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/6h/nRos6h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox6hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox6hCN)
AnovaModel.sox6hCN <- aov(Ros ~ Conditions, data = sox6hCN)
summary(AnovaModel.sox6hCN)
with(sox6hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox6hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox6hCN)
sox24hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/24h/nRos24h-CN.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox24hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox24hCN)
AnovaModel.sox24hCN <- aov(Ros ~ Conditions, data = sox24hCN)
summary(AnovaModel.sox24hCN)
with(sox24hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox24hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox24hCN)
sox48hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/48h/nRos48h-C-SN.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox48hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox48hCN)
AnovaModel.sox48hCN <- aov(Ros ~ Conditions, data = sox48hCN)
summary(AnovaModel.sox48hCN)
with(sox48hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                 'sd')))
editDataset(sox48hCN)
sox48hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/48h/nRos48h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox48hCN) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox48hCN)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox48hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox48hCN)
AnovaModel.sox48hCN <- aov(Ros ~ Conditions, data = sox48hCN)
summary(AnovaModel.sox48hCN)
with(sox48hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                 'sd')))
sox1hCN <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/1h/nRos1h-C.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox1hCN) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox1hCN)
AnovaModel.sox1hCN <- aov(Ros ~ Conditions, data = sox1hCN)
summary(AnovaModel.sox1hCN)
with(sox1hCN, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
sox1hSP <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/1h/nRos1h-sc.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox1hSP) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox1hSP)
AnovaModel.sox1hSP <- aov(Ros ~ Conditions, data = sox1hSP)
summary(AnovaModel.sox1hSP)
with(sox1hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox1hSP) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox1hSP)
sox1hCNSP <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/1h/nRos1h-C-SN.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox1hCNSP) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox1hCNSP)
AnovaModel.sox1hCNSP <- aov(Ros ~ Conditions, data = sox1hCNSP)
summary(AnovaModel.sox1hCNSP)
with(sox1hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                  'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox1hCNSP) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox1hCNSP)
sox30mSP <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/30m/nRos30msc.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox30mSP) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox30mSP)
AnovaModel.sox30mSP <- aov(Ros ~ Conditions, data = sox30mSP)
summary(AnovaModel.sox30mSP)
with(sox30mSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox30mSP) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox30mSP)
sox2hSP <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/2h/nRos2hsc.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox2hSP) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox2hSP)
AnovaModel.sox2hSP <- aov(Ros ~ Conditions, data = sox2hSP)
summary(AnovaModel.sox2hSP)
with(sox2hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox2hSP) 
# medians by group
kruskal.test(Ros ~ Conditions, data=sox2hSP)
sox2hCNSP <- 
  readXL("C:/Users/canti/OneDrive/Documents/Mémoire/2h/nRos2h-C-SN.xlsx", 
         rownames=TRUE, header=TRUE, na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox2hCNSP) 
# variances by group
bartlett.test(Ros ~ Conditions, data=sox2hCNSP)
AnovaModel.sox2hCNSP <- aov(Ros ~ Conditions, data = sox2hCNSP)
summary(AnovaModel.sox2hCNSP)
with(sox2hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 
                                                                  'sd')))
local({
  .Pairs <- glht(AnovaModel.sox2hCNSP, linfct = mcp(Conditions = "Tukey"))
  print(summary(.Pairs)) # pairwise tests
  print(confint(.Pairs, level=0.95)) # confidence intervals
  print(cld(.Pairs, level=0.05)) # compact letter display
  old.oma <- par(oma=c(0, 5, 0, 0))
  plot(confint(.Pairs))
  par(old.oma)
})
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox2hCNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox2hCNSP)
sox30CNSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/30m/nRos30m-C-SN.xlsx", rownames=TRUE, header=TRUE, 
                    na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox30CNSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox30CNSP)
AnovaModel.sox30mCNSP <- aov(Ros ~ Conditions, data = sox30CNSP)
summary(AnovaModel.sox30mCNSP)
with(sox30CNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox30CNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox30CNSP)
sox4hSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/4h/nRos4hsc.xlsx", rownames=TRUE, header=TRUE, na="", 
                  sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox4hSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox4hSP)
AnovaModel.sox4hSP <- aov(Ros ~ Conditions, data = sox4hSP)
summary(AnovaModel.sox4hSP)
with(sox4hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox4hSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox4hSP)
sox4hCNSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/4h/nRos4h-C-SN.xlsx", rownames=TRUE, header=TRUE, 
                    na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox4hCNSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox4hCNSP)
AnovaModel.sox4hCNSP <- aov(Ros ~ Conditions, data = sox4hCNSP)
summary(AnovaModel.sox4hCNSP)
with(sox4hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox4hCNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox4hCNSP)
sox6hSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/6h/nRos6hsc.xlsx", rownames=TRUE, header=TRUE, na="", 
                  sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox6hSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox6hSP)
AnovaModel.sox6hSP <- aov(Ros ~ Conditions, data = sox6hSP)
summary(AnovaModel.sox6hSP)
with(sox6hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox6hSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox6hSP)
sox6hCNSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/6h/nRos6h-C-SN.xlsx", rownames=TRUE, header=TRUE, 
                    na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox6hCNSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox6hCNSP)
AnovaModel.sox6hCNSP <- aov(Ros ~ Conditions, data = sox6hCNSP)
summary(AnovaModel.sox6hCNSP)
with(sox6hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox6hCNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox6hCNSP)
with(sox6hCNSP, plotMeans(Ros, Conditions, error.bars="se", connect=TRUE))
with(sox6hCNSP, Dotplot(Ros, by=Conditions, bin=FALSE))
Boxplot(Ros ~ Conditions, data=sox6hCNSP, id=list(method="y"))
with(sox6hCNSP, plotMeans(Ros, Conditions, error.bars="se", connect=TRUE))
sox24hSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/24h/nRos24hsc.xlsx", rownames=TRUE, header=TRUE, na="",
                   sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox24hSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox24hSP)
AnovaModel.sox24hSP <- aov(Ros ~ Conditions, data = sox24hSP)
summary(AnovaModel.sox24hSP)
with(sox24hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox24hSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox24hSP)
sox24hCNSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/24h/nRos24h-CN-SN.xlsx", rownames=TRUE, header=TRUE, 
                     na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox24hCNSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox24hCNSP)
AnovaModel.sox24hCNSP <- aov(Ros ~ Conditions, data = sox24hCNSP)
summary(AnovaModel.sox24hCNSP)
with(sox24hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox24hCNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox24hCNSP)
sox48hSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/48h/nRos48hsc.xlsx", rownames=TRUE, header=TRUE, na="",
                   sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox48hSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox48hSP)
AnovaModel.sox48hSP <- aov(Ros ~ Conditions, data = sox48hSP)
summary(AnovaModel.sox48hSP)
with(sox48hSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox48hSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox48hSP)
sox48hCNSP <- readXL("C:/Users/canti/OneDrive/Documents/Mémoire/48h/nRos48h-C-SN.xlsx", rownames=TRUE, header=TRUE, 
                     na="", sheet="Feuil1", stringsAsFactors=TRUE)
Tapply(Ros ~ Conditions, var, na.action=na.omit, data=sox48hCNSP) # variances by group
bartlett.test(Ros ~ Conditions, data=sox48hCNSP)
AnovaModel.sox48hCNSP <- aov(Ros ~ Conditions, data = sox48hCNSP)
summary(AnovaModel.sox48hCNSP)
with(sox48hCNSP, numSummary(Ros, groups = Conditions, statistics=c('mean', 'sd')))
Tapply(Ros ~ Conditions, median, na.action=na.omit, data=sox48hCNSP) # medians by group
kruskal.test(Ros ~ Conditions, data=sox48hCNSP)