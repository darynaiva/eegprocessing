library(psych)
library(GPArotation)
library(pwr)

df <- read.csv(
  file = "BrainloadHA.csv",
  sep = ";",
  header = FALSE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "character", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric" 
               )
  )

df[, ]  <- as.numeric(df[, ])
    
summary(df[ , 1:5])

m <- t.test(V36~V55, mu = 0.1,
            alternative = 'two.sided', data = df)

str(m)
pwr.t.test (d=0.2, sig.level = 0.5, power = 0.8)

cohen.d(df, group = df$V55)
?cohen.d

cor(df$V17, df$V36)
cor.test(df$V17, df$V36)

cor(df$V29, df$V36)
cor.test(df$V29, df$V36)

cor(df$V23, df$V36)
cor.test(df$V23, df$V36)

cor(df$V4, df$V36)
cor.test(df$V4, df$V36)

cor(df$V4, df$V36)
cor.test(df$V7, df$V36)
