library(pwr)
library(psych)
install.packages("SDMPlay")
library(Hmisc)
warnings()
library(ggplot2)
library(cluster)
library(factoextra)
library(SDMPlay)

EEG <- read.csv(
  file = "dataEEG.csv",
  sep = ";",
  header = TRUE,
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
               "numeric", "numeric", "numeric", "numeric", "numeric" 
               )
)

clearEEG <- na.omit (EEG)

ep <- as.data.frame(scale(clearEEG[, 2:43]))

poly(ep$P4tb, 3, raw = TRUE)
?poly
nonlm <- lm(E ~ poly(P4tb, 3, raw = TRUE), data = ep)
summary(nonlm)

male <- clearEEG[ which(clearEEG$sex==1), ]
female <- clearEEG[ which(clearEEG$sex==0), ]

malelm <- lm(male$E ~ male$P4tb)
nonlmm <- lm(E ~ poly(P4tb, 3, raw = TRUE), data = male)
summary(malelm)
summary(nonlmm)

femalelm <- lm(female$E ~ female$P4tb)
nonlmf <- lm(E ~ poly(P4tb, 3, raw = TRUE), data = female)
summary()
summary(femalelm)

  EEGdist <- get_dist(ep[, 41:42], stand = TRUE, method = "pearson")
  dist(ep[, 41:42], method = "manhattan")
fviz_dist(EEGdist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
fviz_nbclust(ep[, 41:42], pam, method = "gap_stat")

gap_stat <- clusGap(ep, FUN = pam,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
pam.res <- pam(ep, 2)

fviz_cluster(pam.res, data = bt, elliopse.type = "convex")+
  theme_minimal()
clearEEG$groups <- as.numeric(pam.res$clustering)
important_var_norm <- NULL
for (i in 2:43) {
  
  t <- t.test(clearEEG[, i]~clearEEG$groups)
  if (t$p.value < 0.05) {
    cat(variable.names(clearEEG[i]))
    print(t)
    important_var <- c(important_var, variable.names(clearEEG[i])) 
  }
}
important_var

nclEEG <- log(clearEEG[,3:40])
nclEEG$sex <- clearEEG$sex
nclEEG$E <- clearEEG$E
nclEEG$N <- clearEEG$N
sigCor <- corr.test(nclEEG)
corrmat <- flattenCorrMatrix(sigCor$r, sigCor$p)
corrmat[, 3:4] <- round(corrmat[, 3:4], digits = 3)

nEEGdist <- get_dist(nclEEG, stand = TRUE, method = "pearson")
fviz_dist(nEEGdist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
gap_stat <- clusGap(nclEEG, FUN = pam,
                    K.max = 10, B = 10)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)
pam.res <- pam(nclEEG, 3)
fviz_cluster(pam.res, data = nclEEG, elliopse.type = "convex")+
  theme_minimal()
nclEEG$groups <- as.numeric(pam.res$clustering)

ngroup1 <- nclEEG[ which(nclEEG$groups==1), ]
ngroup1 <- ngroup1[, -42]
ngroup2 <- nclEEG[ which(nclEEG$groups==2), ]
ngroup2 <- ngroup2[, -42]
ngroup3 <- nclEEG[ which(nclEEG$groups==3), ]
ngroup3 <- ngroup3[, -42]

corng1 <- corr.test(ngroup1)
fcorng1 <- flattenCorrMatrix(corng1$r, corng1$p)

corng2 <- corr.test(ngroup2)
fcorng2 <- flattenCorrMatrix(corng2$r, corng2$p)

corng3 <- corr.test(ngroup3)
fcorng3 <- flattenCorrMatrix(corng3$r, corng3$p)

important_var_norm <- NULL
for (i in 2:43) {
  
  fit <- aov(nclEEG[, i] ~ nclEEG$groups, data=nclEEG)
  if (fit$ < 0.05) {
    cat(variable.names(clearEEG[i]))
    print(t)
    important_var <- c(important_var, variable.names(clearEEG[i])) 
  }
}

fit <- aov(nclEEG$E ~ nclEEG$groups, data=nclEEG)
summary(fit)
important_var
pam.res


group1 <- clearEEG[ which(clearEEG$groups==1), ]
g1c <- corr.test(group1[2:43])
g1c2 <- flattenCorrMatrix(g1c$r, g1c$p)
g1c2[, 3:4] <- round(g1c2[, 3:4], 3)

group2 <- clearEEG[ which(clearEEG$groups==2), ]
sd(group1$N)
t.test(group1$sex, group2$sex)
mean(clearEEG$E)
sd(group1$P4tb)
mean(group2$P4tb)
t.test(group2$E, clearEEG$E)
t.test(group2$N, clearEEG$N)
scatter.smooth(x=clearEEG$E, clearEEG$P4bt, main="Extraversion ~ P4 theta / bets")
EP4corr <- corr.test(x=clearEEG$E, y = clearEEG$P4tb, method = "spearman")
NP4corr <- corr.test(x=clearEEG$N, y = clearEEG$P4tb, method = "spearman")
ENcorr <- cor.test(x=clearEEG$N, y = clearEEG$E, method = "spearman")
print(ENcorr, short=FALSE)
print(NP4corr, short=FALSE)
cor.test(x=clearEEG$N, y = clearEEG$P4tb, method = "spearman")
LinearMod <- lm(clearEEG$P4tb ~ clearEEG$E)
print (LinearMod)
summary (LinearMod)

qqnorm(normal,main="QQ plot of normal data",pch=19)
qqnorm(clearEEG$E,main="QQ plot of normal data",pch=19)
qqline(normal)
ks.test(clearEEG$E,"pnorm", mean = 31.22, sd = 3.86,  exact = TRUE)
t.test(clearEEG$N ~ clearEEG$groups)

cEEG <- as.matrix(clearEEG)
sigCor <- rcorr(cEEG, type = "spearman")
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
pr <- flattenCorrMatrix(sigCor$r, sigCor$P)
write.table(pr, "c:\\Users\\Daryna\\Documents\\neo_CNS\\News_commercials\\corr.txt", sep="\t")

P4tbs <- lm(ep$E ~ ep$P4tb)
cor(clearEEG$E, clearEEG$P4tb, method = 'spearman')                 


plot(P4tbs)
ggplot(ep, aes(x = P4tb, y = E)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) 
plot(clea)
ggPredict(P4tbs,se=TRUE,interactive=TRUE)


linmod <-lm(clearEEG$E ~ clearEEG$P4tb)
plot (linmod)

ep <- ep[-120,]

linmod <-lm(clearEEG$E ~ nP4)
summary(linmod)
plot(linmod)

P4Fz <- lm(ep$E ~ ep$Fzdb)
summary (P4Fz)
plot(P4Fz)
plot(density(ep$P4tb))

nP4 <- log(clearEEG$P4tb)
plot(density(nP4))
cor(nP4, clearEEG$E)
plot(nP4, clearEEG$E)
clearEEG$nP4 <- nP4

group1 <- na.omit(nclEEG[ which(nclEEG$sex==1), ])
group2 <- nclEEG[ which(nclEEG$sex==0), ]

lfemale <- lm(group2$E ~ group2$P4tb)
summary(lfemale)
plot(lfemale)
ggplot(group2, aes(x = P4tb, y = E)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) 
plot(clea)

lmale <- lm(group1$E ~ group1$P4tb)
summary(lmale)
plot(lmale)
ggplot(group1, aes(x = P4tb, y = E)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) 
plot(clea)

nclEEG <- log(clearEEG[,3:40])
nclEEG$sex <- clearEEG$sex
nclEEG$E <- clearEEG$E
nclEEG$N <- clearEEG$N
sigCor <- corr.test(nclEEG)
corrmat <- flattenCorrMatrix(sigCor$r, sigCor$p)
corrmat[, 3:4] <- round(corrmat[, 3:4], digits = 3)

lmn <- lm(nclEEG$E ~ nclEEG$P4tb + nclEEG$Fp2tb)
summary(lmn)
ggplot(nclEEG, aes(x = P4tb, y = E)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) 
plot(clea)