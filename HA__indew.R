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
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric")
  )

df[, ]  <- as.numeric(df[, ])
    
summary(df[ , 1:5])

str(df)
m <- t.test(V36~V55, mu = 0.2,
            alternative = 'two.sided', data = df)