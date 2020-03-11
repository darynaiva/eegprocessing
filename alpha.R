install.packages("xlsx")
library(NbClust)
library(pwr)
library(reshape)
library(psych)
library(Hmisc)
library("xtable")
library("factoextra")
library(NbClust)
library(cluster)
library(pastecs)
library(xlsx)

closed_eyes <- read.csv(
  file = "alpha_closed_eyes.csv",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("character", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric", "numeric"))
clear_closed <- na.omit (closed_eyes)

news1 <- read.csv(
  file = "alpha_closed_eyes.csv",
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("character", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric", "numeric"))
clear_news1 <- na.omit (news1)

cor.test(clear_closed[, 3:15], method = "spearman")
cor.test(clear_closed$HA, clear_closed$FronAs.F.3.4.alpha)
cor.test(clear_news1$HA, clear_news1$FronAs.F.3.4.gamma)
rcorr(clear_closed[, 3:15])
closed_cor <- rcorr(as.matrix(clear_closed[, 3:15]))


 cl <- as.data.frame(scale(clear_closed[, 3:15]))
 EEGdist <- get_dist(clear_closed[, 3:4], stand = FALSE, method = "pearson")
 fviz_dist(EEGdist, 
           gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) 
 fviz_nbclust(cl[, 3:4], pam, method = "gap_stat")
 
distances <- as.matrix(EEGdist)
dist_ag <- dist(cl[, 3:4], method = 'euclidean')
hc_avg <- hclust(dist_ag, method = 'average')
plot(hc_avg)
cut_avg <- cutree(hc_avg, k = 4)
plot(hc_avg)
rect.hclust(hc_avg , k = 4, border = 2:6)
abline(h = 3, col = 'red')

fviz_dist(dist_ag, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

clear_closed$group <- cut_avg


regr <- lm(clear_closed$HA ~ clear_closed$FronAs.F.3.4.alpha + clear_closed$FronAs.F.3.4.gamma)
summary(regr)
plot(clear_closed$FronAs.F.3.4.gamma, clear_closed$HA)
cor(clear_closed$HA, clear_closed$FronAs.F.3.4.gamma, method = "spearman")

ggplot(clear_closed, aes(x = FronAs.F.3.4.alpha + FronAs.F.3.4.gamma, y = HA)) + 
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) 
pam.res <- pam(clear_closed[, 3:4], 2)

fviz_cluster(pam.res, data = bt, elliopse.type = "convex")+
  theme_minimal()
clear_closed$groups <- as.numeric(pam.res$clustering)
clear_news1$groups <- as.numeric(pam.res$clustering)
cohen.d(clear_news1[, 3:16], group = "groups")
t.test(clear_news1$FronAs.F.3.4.alpha ~ clear_news1$groups)
plot(density(clear_closed$FronAs.F.3.4.gamma))
shapiro.test(clear_closed$FronAs.F.3.4.gamma)

plot(density(clear_closed$HA))
shapiro.test(clear_closed$HA)
qqnorm(clear_closed$FronAs.F.3.4.gamma)

descriptives <- round(as.data.frame(stat.desc(clear_closed[, 2:15])), 2)
write.csv(descriptives, "C:/Users/Daryna/Documents/neo_CNS/News_commercials/alpha_asym/descriptives.csv")
