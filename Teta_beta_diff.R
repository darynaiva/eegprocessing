library(tidyverse)
library(multcomp)
library(dummies)
library("cluster")
library("factoextra")
library(psych)
library(GPArotation)
library(pwr)

BetaTeta <- read.csv(
  file = "BrainloadHA.csv",
  sep = ";",
  header = FALSE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric")
)

BetaTeta[, 28:30] <- NULL

bt <- scale (BetaTeta[, 2:46])

HAdist <- get_dist(bt[, 1:2], stand = TRUE, method = "pearson")

?get_dist()
fviz_dist(HAdist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


?fviz_nbclust
fviz_nbclust(bt[, 1:2], pam, method = "gap_stat")
fviz_nbclust(bt[, 4:7], kmeans, method = "gap_stat")
fviz_nbclust(bt[, 4:12], kmeans, method = "gap_stat")

pam.res <- pam(bt[, 1:2], 2)

fviz_cluster(pam.res, data = bt, elliopse.type = "convex")+
  theme_minimal()

BetaTeta$groups <- as.numeric(pam.res$clustering)

cohen.d(BetaTeta, group = "V1")
fort <- table (c(BetaTeta$V44, BetaTeta$V45, BetaTeta$V46, BetaTeta$V48, 
          BetaTeta$V35, BetaTeta$V36, BetaTeta$V27, BetaTeta$V31, 
          BetaTeta$V32, BetaTeta$V39, BetaTeta$V42, BetaTeta$V8,
          BetaTeta$V9, BetaTeta$V13, BetaTeta$V15, BetaTeta$V22,
          BetaTeta$V23, BetaTeta$V24, BetaTeta$V1))
fort

t.test(fort, group)

?cbind
res.maov <- manova (cbind (BetaTeta$V9, BetaTeta$V10) ~ BetaTeta$groups, data = BetaTeta)
summary(res.maov)

summary(BetaTeta[ BetaTeta$groups1==1,c("V50", "V72")])
summary(BetaTeta[ BetaTeta$groups1==2,c("V50", "V72")])

sapply (BetaTeta ,function(x) t.test( BetaTeta, BetaTeta, mu=0, alternative= "greater", paired = TRUE))
?sapply
splyBT <- split(BetaTeta, f = BetaTeta$groups)

coln <- combn(colnames(BetaTeta[,]), 2)

summary(splyBT[["1"]]$V52)
summary(splyBT[["1"]]$V74)

summary(splyBT[["2"]]$V52)
summary(splyBT[["2"]]$V74)

t.test(splyBT[["1"]]$V52, splyBT[["1"]]$V74, mu=0.1, alternative= "greater", paired = TRUE)
t.test(splyBT[["2"]]$V52, splyBT[["2"]]$V74, mu=0.1, alternative= "greater", paired = TRUE)

cohen.d(BetaTeta[, 50], group = "groups1")
BetaTeta$groups1
?cohen.d

bt <- na.omit(BetaTeta[33:51])
nfactors(bt)
fm <- fa(bt, nfactors = 10, rotate = "oblimin")
fm
cor(BetaTeta[2:8], BetaTeta[31:49])
t.test( BetaTeta[,9:27], BetaTeta[,31:49], mu=0, alternative= "greater", paired = TRUE)
?by
result <- by(data, BetaTeta$V1, 
             function(x) t.test(x$V9, x$V31, mu=0, alt="two.sided", paired = TRUE, conf.level = 0.99))
before1 <- splyBT[["1"]](c("V9","V10", "V11","V12","V13","V14","V15","V16","V17","V18","V19",
                           "V20","V21","V22","V23","V24","V25","V26","V27"))
