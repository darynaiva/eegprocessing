library(eegUtils)
library(eegkit)
library(ggplot2)


rm(list = ls())
setwd('D:/Beehevior/eye tracking')
dt <- read.csv(file = 'dt.csv',
               header = T,
               encoding = 'UTF-8',
               sep = ';')

setwd('D:/Beehevior/BDF/BDF')
name.list <- c('Bogomaz.Rostislav', 'Cherniuk',
               'Ivaskevych', 'Karelskij.Andrej', 
               'Karnauch.Alexandra', 'Kravchenko.Alexandr',
               'Plyusch.Sofia', 'Sharnin.Dmitrij',
               'Stanislavskij.Yan', 'Tukaiev.Sergii',
               'Turitsina.Katerina', 'Vajtishin.Valentin')
delta <- read.csv(
  file = 'delta.csv',
  encoding = 'UTF-8',
  sep = ';',
  dec = '.',
  colClasses = c('character', 'numeric', 'numeric'),
  header = T
)

#BDF
n<- length(name.list)
i<-1
for (i in 1:n) {
  s.name <- paste(name.list[[i]],'script.bdf', sep = '_')
  script <- import_raw(s.name)
  script <- eeg_reference(script, ref_chans = 'average',
                          exclude = c('GyroX', 'GyroY', 'GyroZ',
                                      'M1', 'M2'))
  script <- select_elecs(script, electrode = c('Fp1', 'Fp2', 'F3', 'F4', 'F7', 'F8', 'T7', 'T8', 'Fz', 'Pz'))
  script.eeg <- as.data.frame(cbind(script$signals, script$timings$time))
  colnames(script.eeg) <- c(colnames(script$signals), 'time')
  assign(paste(name.list[[i]], 's', sep = '.'), script.eeg)
  
  m.name <- paste(name.list[[i]],'movie.bdf', sep = '_')
  movie <- import_raw(m.name)
  movie <- eeg_reference(movie, ref_chans = 'average',
                          exclude = c('GyroX', 'GyroY', 'GyroZ',
                                      'M1', 'M2'))
  movie <- select_elecs(movie, electrode = c('Fp1', 'Fp2', 'F3', 'F4', 'F7', 'F8', 'T7', 'T8', 'Fz', 'Pz'))
  movie.eeg <- as.data.frame(cbind(movie$signals, movie$timings$time))
  colnames(movie.eeg) <- c(colnames(movie$signals), 'time')
  assign(paste(name.list[[i]], 'm', sep = '.'), movie.eeg)
}

eegpsd(Ivaskevych.m[,1:10], Fs = 500, lower = 4, upper = 7.4)
eegpsd(Ivaskevych.s[,1:10], Fs = 500, lower = 4, upper = 7.4)
eegpsd(Karnauch.Alexandra.m[,1:10], Fs = 500, lower = 7.5, upper = 13)
eegpsd(Karnauch.Alexandra.s[,1:10], Fs = 500, lower = 7.5, upper = 13)
eegpsd(Tukaiev.Sergii.m[,1:10], Fs = 500, lower = 7.5, upper = 13)
eegpsd(Tukaiev.Sergii.s[,1:10], Fs = 500, lower = 7.5, upper = 13)

people <- length(name.list)
asFp12 <- list()
asF34 <- list()
asF78 <- list()
asT78 <- list()

gsFp12 <- list()
gsF34 <- list()
gsF78 <- list()
gsT78 <- list()

BrLs <- list()


amFp12 <- list()
amF34 <- list()
amF78 <- list()
amT78 <- list()

gmFp12 <- list()
gmF34 <- list()
gmF78 <- list()
gmT78 <- list()

BrLm <- list()
x <- 1
for (x in 1:people){
#  script.eeg <- get(paste(name.list[[x]], 's', sep = '.'))
#  m <- 1
#  times <- floor(max(script.eeg$time))
#  script.eeg$time <- floor(script.eeg$time)
#  a.asymmetry <- as.data.frame(matrix(ncol = 4, nrow = times+1))
#  g.asymmetry <- as.data.frame(matrix(ncol = 4, nrow = times+1))
#  BLI <- as.data.frame(matrix(ncol = 1, nrow = times+1))
#  colnames(a.asymmetry) <- c('Fp12', 'F34', 'F78', 'T78')
#  colnames(g.asymmetry) <- c('Fp12', 'F34', 'F78', 'T78')
#  s<-0
#    for (s in 0:times) {
#      sec <- script.eeg[which(script.eeg[,11]==s),]
#      sec <- sec[, -11]
#      a.fft <- eegfft(sec, Fs = script$srate, lower = 7.5, upper = 13)
#      a.asymmetry[s+1, 1] <- sum(log(a.fft$strength[,1]) - log(a.fft$strength[,2]))
#      a.asymmetry[s+1, 2] <- sum(log(a.fft$strength[,3]) - log(a.fft$strength[,4]))
#      a.asymmetry[s+1, 3] <- sum(log(a.fft$strength[,5]) - log(a.fft$strength[,6]))
#      a.asymmetry[s+1, 4] <- sum(log(a.fft$strength[,7]) - log(a.fft$strength[,8]))
      
#      g.fft <- eegfft(sec, Fs = script$srate, lower = 35, upper = 49)
#      g.asymmetry[s+1, 1] <- sum(log(g.fft$strength[,1]) - log(g.fft$strength[,2]))
#      g.asymmetry[s+1, 2] <- sum(log(g.fft$strength[,3]) - log(g.fft$strength[,4]))
#      g.asymmetry[s+1, 3] <- sum(log(g.fft$strength[,5]) - log(g.fft$strength[,6]))
#      g.asymmetry[s+1, 4] <- sum(log(g.fft$strength[,7]) - log(g.fft$strength[,8]))
#      
#      theta <- eegfft(sec, Fs = script$srate, lower = 5, upper = 7.4)
#      BLI[s+1,1] <- sum(theta$strength[,9])/sum(a.fft$strength[,10])
#    }
#  a.asymmetry$time <- 0:times
#  g.asymmetry$time <- 0:times
#  BLI$time <- 0:times
#  dTs <- delta[which(delta[,1]==name.list[[x]]), 2]
#  if (dTs != 0){
#    a.asymmetry <- a.asymmetry[-c(1:dTs),]
#    a.asymmetry$time <- 1:nrow(a.asymmetry)
#    
#    g.asymmetry <- g.asymmetry[-c(1:dTs),]
#    g.asymmetry$time <- 1:nrow(g.asymmetry)
#    
#    BLI <- BLI[-c(1:dTs),]
#    BLI$time <- 1:nrow(BLI)
#  }
#  chan.list <- c('Fp12', 'F34', 'F78', 'T78')
#  num.chan <- length(chan.list)
#  n<- 1
#  wddata <- getwd()
#  setwd('D:/Beehevior/BDF/BDF/images')
#  dir.n <- gsub('.', '', name.list[[x]])
#  dir.create(paste('D:/Beehevior/BDF/BDF/images/script/', name.list[[x]], sep = ''))
#  setwd(paste('D:/Beehevior/BDF/BDF/images/script/', name.list[[x]], sep = ''))
#  for (n in 1:num.chan){
#    a.chan.sd <- sd(a.asymmetry[,n])
#    a.chan.mean <- mean(a.asymmetry[,n])
#    g.chan.sd <- sd(g.asymmetry[,n])
#    g.chan.mean <- mean(g.asymmetry[,n])
#    png( filename = paste(name.list[[x]], chan.list[[n]],'alphaAsymmetryS.png', sep = ''), width = 800, height = 800, units = "px")
#    print(
#      ggplot(a.asymmetry, aes(x=a.asymmetry$time, y=a.asymmetry[,n]))+
#        geom_line(aes(colour = ''))+
#        geom_line(aes(y = a.chan.mean + 2*a.chan.sd), colour = 'blue')+
#        geom_line(aes(y = a.chan.mean-2*a.chan.sd), colour = 'blue')+
#        geom_line(aes(y = a.chan.mean), colour = 'dark green')+
#        ggtitle(paste("Alpha asymmetry", chan.list[[n]])) + xlab('Time, s') + ylab('Power')+
#        theme_minimal()+
#        theme(legend.position = 'none')
#    )
#    dev.off()
#    
#    png( filename = paste(name.list[[x]], chan.list[[n]],'gammaAsymmetryS.png', sep = ''), width = 800, height = 800, units = "px")
#    print(
#      ggplot(a.asymmetry, aes(x=g.asymmetry$time, y=g.asymmetry[,n]))+
#        geom_line(aes(colour = ''))+
#        geom_line(aes(y = g.chan.mean + 2*g.chan.sd), colour = 'blue')+
#        geom_line(aes(y = g.chan.mean-2*g.chan.sd), colour = 'blue')+
#        geom_line(aes(y = g.chan.mean), colour = 'dark green')+
#        ggtitle(paste("Gamma asymmetry", chan.list[[n]],  sep = ', ')) + xlab('Time, s') + ylab('Power')+
#        theme_minimal()+
#        theme(legend.position = 'none')
#    )
#    dev.off()
#  }
#  png( filename = paste(name.list[[x]], chan.list[[n]],'bliS.png', sep = ''), width = 800, height = 800, units = "px")
#  print(
#    ggplot(BLI, aes(x=g.asymmetry$time, y=g.asymmetry[,n]))+
 ##     geom_line(aes(colour = ''))+
##      geom_line(aes(y = g.chan.mean + 2*g.chan.sd), colour = 'blue')+
#      geom_line(aes(y = g.chan.mean-2*g.chan.sd), colour = 'blue')+
#      geom_line(aes(y = g.chan.mean), colour = 'dark green')+
#      ggtitle(paste("BLI", name.list[[x]])) + xlab('Time, s') + ylab('Units')+
#      theme_minimal()+
#      theme(legend.position = 'none')
#  )
#  dev.off()
  
#  a.asymmetry$time <- sapply(1:nrow(a.asymmetry), function(x,y) (y[x,5]/max(y[,5]))*100, y = a.asymmetry)
#  g.asymmetry$time <- sapply(1:nrow(g.asymmetry), function(x,y) (y[x,5]/max(y[,5]))*100, y = g.asymmetry)
#  BLI$time <- sapply(1:nrow(BLI), function(x,y) (y[x,2]/max(y[,2]))*100, y = BLI)
  
#  assign(paste(name.list[[x]], 'aFp12', sep = ''), cbind(a.asymmetry$Fp12, a.asymmetry$time))
#  assign(paste(name.list[[x]], 'aF34', sep = ''),cbind(a.asymmetry$F34, a.asymmetry$time))
#  assign(paste(name.list[[x]], 'aF78' , sep = ''),cbind(a.asymmetry$F78, a.asymmetry$time))
#  assign(paste(name.list[[x]], 'aT78', sep = ''),cbind(a.asymmetry$T78, a.asymmetry$time))
#  
#  assign(paste(name.list[[x]], 'gFp12', sep = ''),cbind(g.asymmetry$Fp12, g.asymmetry$time))
#  assign(paste(name.list[[x]], 'gF34', sep = ''),cbind(g.asymmetry$F34, g.asymmetry$time))
#  assign(paste(name.list[[x]], 'gF78', sep = ''),cbind(g.asymmetry$F78, g.asymmetry$time))
#  assign(paste(name.list[[x]], 'gT78', sep = ''),cbind(g.asymmetry$T78, g.asymmetry$time))
#  
#  assign(paste(name.list[[x]], 'BLI', sep = ''), BLI)
#  
#  asFp12[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aFp12', sep = '')))
#  asF34[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aF34', sep = '')))
#  asF78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aF78', sep = '')))
#  asT78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aT78', sep = '')))
#  
#  gsFp12[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gFp12', sep = '')))
#  gsF34[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gF34', sep = '')))
#  gsF78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gF78', sep = '')))
#  gsT78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gT78', sep = '')))
#  
#  BrLs[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'BLI', sep = '')))
#  
#  
#  to_remove <- c('a.asymmetry', 'g.asymmetry', 
#                 paste(name.list[[x]], 'gFp12', sep = ''),
#                 paste(name.list[[x]], 'gF34', sep = ''),
#                 paste(name.list[[x]], 'gF78', sep = ''),
#                 paste(name.list[[x]], 'gT78', sep = ''),
#                 paste(name.list[[x]], 'aFp12', sep = ''),
#                 paste(name.list[[x]], 'aF34', sep = ''),
#                 paste(name.list[[x]], 'aF78', sep = ''),
#                 paste(name.list[[x]], 'aT78', sep = ''),
#                 'sec', 'a.fft', 'g.fft', 'theta', 'BLI',
#                 paste(name.list[[x]], 'BLI', sep = ''))
#  rm(list = to_remove)
#  
#  
#  
#  
#  
  movie.eeg <- get(paste(name.list[[x]], 'm', sep = '.'))
  m <- 1
  times <- floor(max(movie.eeg$time))
  movie.eeg$time <- floor(movie.eeg$time)
  
  
  
  a.asymmetry <- as.data.frame(matrix(ncol = 4, nrow = times+1))
  g.asymmetry <- as.data.frame(matrix(ncol = 4, nrow = times+1))
  BLI <- as.data.frame(matrix(ncol = 1, nrow = times+1))
  colnames(a.asymmetry) <- c('Fp12', 'F34', 'F78', 'T78')
  colnames(g.asymmetry) <- c('Fp12', 'F34', 'F78', 'T78')
  s<-0
  # Count asymmetry and BLI
  for (s in 0:times) {
    sec <- movie.eeg[which(movie.eeg[,11]==s),]
    sec <- sec[, -11]
    a.fft <- eegfft(sec, Fs = movie$srate, lower = 7.5, upper = 13)
    a.asymmetry[s+1, 1] <- sum(log(a.fft$strength[,1]) - log(a.fft$strength[,2]))
    a.asymmetry[s+1, 2] <- sum(log(a.fft$strength[,3]) - log(a.fft$strength[,4]))
    a.asymmetry[s+1, 3] <- sum(log(a.fft$strength[,5]) - log(a.fft$strength[,6]))
    a.asymmetry[s+1, 4] <- sum(log(a.fft$strength[,7]) - log(a.fft$strength[,8]))
    
    g.fft <- eegfft(sec, Fs = script$srate, lower = 35, upper = 49)
    g.asymmetry[s+1, 1] <- sum(log(g.fft$strength[,1]) - log(g.fft$strength[,2]))
    g.asymmetry[s+1, 2] <- sum(log(g.fft$strength[,3]) - log(g.fft$strength[,4]))
    g.asymmetry[s+1, 3] <- sum(log(g.fft$strength[,5]) - log(g.fft$strength[,6]))
    g.asymmetry[s+1, 4] <- sum(log(g.fft$strength[,7]) - log(g.fft$strength[,8]))
    
    theta <- eegfft(sec, Fs = script$srate, lower = 5, upper = 7.4)
    BLI[s+1,1] <- sum(theta$strength[,9])/sum(a.fft$strength[,10])
  }
  
  # Edit time scale
  a.asymmetry$time <- 0:times
  g.asymmetry$time <- 0:times
  BLI$time <- 0:times
  dTs <- delta[which(delta[,1]==name.list[[x]]), 2]
  if (dTs != 0){
    a.asymmetry <- a.asymmetry[-c(1:dTs),]
    a.asymmetry$time <- 1:nrow(a.asymmetry)
    
    g.asymmetry <- g.asymmetry[-c(1:dTs),]
    g.asymmetry$time <- 1:nrow(g.asymmetry)
    
    BLI <- BLI[-c(1:dTs),]
    BLI$time <- 1:nrow(BLI)
  }
  
  # Asymmetry plot
  chan.list <- c('Fp12', 'F34', 'F78', 'T78')
  num.chan <- length(chan.list)
  n<- 1
  wddata <- getwd()
  setwd('D:/Beehevior/BDF/BDF/images')
  dir.n <- gsub('.', '', name.list[[x]])
  dir.create(paste('D:/Beehevior/BDF/BDF/images/movie/', name.list[[x]], sep = ''))
  setwd(paste('D:/Beehevior/BDF/BDF/images/movie/', name.list[[x]], sep = ''))
  for (n in 1:num.chan){
    a.chan.sd <- sd(a.asymmetry[,n])
    a.chan.mean <- mean(a.asymmetry[,n])
    g.chan.sd <- sd(g.asymmetry[,n])
    g.chan.mean <- mean(g.asymmetry[,n])
    png( filename = paste(name.list[[x]], chan.list[[n]],'alphaAsymmetry.png', sep = ''), width = 800, height = 800, units = "px")
    print(
      ggplot(a.asymmetry, aes(x=a.asymmetry$time, y=a.asymmetry[,n]))+
        geom_line(aes(colour = ''))+
        geom_line(aes(y = a.chan.mean + 2*a.chan.sd), colour = 'blue')+
        geom_line(aes(y = a.chan.mean-2*a.chan.sd), colour = 'blue')+
        geom_line(aes(y = a.chan.mean), colour = 'dark green')+
        ggtitle(paste("Alpha asymmetry", chan.list[[n]])) + xlab('Time, s') + ylab('Power')+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
        scale_x_continuous(breaks=a.asymmetry$time)
    )
    dev.off()
    
    png( filename = paste(name.list[[x]], chan.list[[n]],'gammaAsymmetry.png', sep = ''), width = 800, height = 800, units = "px")
    print(
      ggplot(g.asymmetry, aes(x=g.asymmetry$time, y=g.asymmetry[,n]))+
        geom_line(aes(colour = ''))+
        geom_line(aes(y = g.chan.mean + 2*g.chan.sd), colour = 'blue')+
        geom_line(aes(y = g.chan.mean-2*g.chan.sd), colour = 'blue')+
        geom_line(aes(y = g.chan.mean), colour = 'dark green')+
        ggtitle(paste("Gamma asymmetry", chan.list[[n]],  sep = ', ')) + xlab('Time, s') + ylab('Power')+
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
        scale_x_continuous(breaks=g.asymmetry$time)
    )
    dev.off()
  }
  
  BLI.sd <- sd(BLI[,1])
  BLI.mean <- mean(BLI[,1])
  png( filename = paste(name.list[[x]], 'bli.png', sep = ''), width = 800, height = 800, units = "px")
  print(
    ggplot(BLI, aes(x=BLI$time, y=BLI[,1]))+
      geom_line(aes(colour = ''))+
      geom_line(aes(y = BLI.mean + 2*BLI.sd), colour = 'blue')+
      geom_line(aes(y = BLI.mean-2*BLI.sd), colour = 'blue')+
      geom_line(aes(y = BLI.mean), colour = 'dark green')+
      ggtitle(paste("BLI", name.list[[x]])) + xlab('Time, s') + ylab('Units')+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
      scale_x_continuous(breaks=BLI$time)
  )
  dev.off()
  
#  a.asymmetry$time <- sapply(1:nrow(a.asymmetry), function(x,y) (y[x,5]/max(y[,5]))*100, y = a.asymmetry)
#  g.asymmetry$time <- sapply(1:nrow(g.asymmetry), function(x,y) (y[x,5]/max(y[,5]))*100, y = g.asymmetry)
#  BLI$time <- sapply(1:nrow(BLI), function(x,y) (y[x,2]/max(y[,2]))*100, y = BLI)
  
  assign(paste(name.list[[x]], 'aFp12', sep = ''), cbind(a.asymmetry$Fp12, a.asymmetry$time))
  assign(paste(name.list[[x]], 'aF34', sep = ''),cbind(a.asymmetry$F34, a.asymmetry$time))
  assign(paste(name.list[[x]], 'aF78' , sep = ''),cbind(a.asymmetry$F78, a.asymmetry$time))
  assign(paste(name.list[[x]], 'aT78', sep = ''),cbind(a.asymmetry$T78, a.asymmetry$time))
  
  assign(paste(name.list[[x]], 'gFp12', sep = ''),cbind(g.asymmetry$Fp12, g.asymmetry$time))
  assign(paste(name.list[[x]], 'gF34', sep = ''),cbind(g.asymmetry$F34, g.asymmetry$time))
  assign(paste(name.list[[x]], 'gF78', sep = ''),cbind(g.asymmetry$F78, g.asymmetry$time))
  assign(paste(name.list[[x]], 'gT78', sep = ''),cbind(g.asymmetry$T78, g.asymmetry$time))
  
  assign(paste(name.list[[x]], 'BLI', sep = ''), BLI)
  
  amFp12[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aFp12', sep = '')))
  amF34[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aF34', sep = '')))
  amF78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aF78', sep = '')))
  amT78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'aT78', sep = '')))
  
  gmFp12[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gFp12', sep = '')))
  gmF34[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gF34', sep = '')))
  gmF78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gF78', sep = '')))
  gmT78[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'gT78', sep = '')))
  
  BrLm[[name.list[[x]]]] <- as.data.frame(get(paste(name.list[[x]], 'BLI', sep = '')))
  
  to_remove <- c('a.asymmetry', 'g.asymmetry', 
                 paste(name.list[[x]], 'gFp12', sep = ''),
                 paste(name.list[[x]], 'gF34', sep = ''),
                 paste(name.list[[x]], 'gF78', sep = ''),
                 paste(name.list[[x]], 'gT78', sep = ''),
                 paste(name.list[[x]], 'aFp12', sep = ''),
                 paste(name.list[[x]], 'aF34', sep = ''),
                 paste(name.list[[x]], 'aF78', sep = ''),
                 paste(name.list[[x]], 'aT78', sep = ''),
                 'sec', 'a.fft', 'g.fft', 'theta', 'BLI',
                 paste(name.list[[x]], 'BLI', sep = ''))
  rm(list = to_remove)
}
setwd('D:/Beehevior/BDF/BDF/images')

total <- 0
i<-1
n <- length(name.list)
for (i in 1:n) {
  total <- total + nrow(amFp12[[name.list[[i]]]])
}

#D<-data.frame()
D <- as.data.frame(matrix(nrow = total, ncol = 3))
count <- 1
i <- 1
for (i in 1:n){
  a <- amFp12[[name.list[[i]]]]
  last <- count + 65-1
  D[count:last, 1] <- a[1:65, 1]
  D[count:last, 2] <- a[1:65, 2]
  D[count:last, 3] <- paste(name.list[[i]])
  count <- last +1
}
colnames(D) <- c('Fp12', 'time', 'respondent')

finalS <- min(nrow(amFp12$Bogomaz.Rostislav),
              nrow(amFp12$Cherniuk),
              nrow(amFp12$Ivaskevych),
              nrow(amFp12$Karelskij.Andrej),
              nrow(amFp12$Karnauch.Alexandra),
              nrow(amFp12$Kravchenko.Alexandr),
              nrow(amFp12$Plyusch.Sofia),
              nrow(amFp12$Sharnin.Dmitrij),
              nrow(amFp12$Stanislavskij.Yan),
              nrow(amFp12$Tukaiev.Sergii),
              nrow(amFp12$Turitsina.Katerina),
              nrow(amFp12$Vajtishin.Valentin))


groupResFp12 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResFp12[,j] <- amFp12[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResF34 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResF34[,j] <- amF34[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResF78 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResF78[,j] <- amF78[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResT78 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResT78[,j] <- amF34[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResBLI<- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResBLI[,j] <- BrLm[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResFp12$median <- apply(groupResFp12, 1, median)
groupResF34$median <- apply(groupResF34, 1, median)
groupResF78$median <- apply(groupResF78, 1, median)
groupResT78$median <- apply(groupResT78, 1, median)
groupResBLI$median <- apply(groupResBLI, 1, median)
groupResBLI$time <- 1:finalS

viz <- as.data.frame(matrix(nrow = 4*finalS, ncol = 3))
colnames(viz) <- c('median', 'time', 'channels')
viz$median <- c(groupResFp12$median,
                groupResF34$median,
                groupResF78$median,
                groupResT78$median)
viz$time <- 1:finalS
viz$channels <- c(rep('Fp12', finalS),
                  rep('F34', finalS),
                  rep('F78', finalS),
                  rep('T78', finalS))

setwd('D:/Beehevior/BDF/BDF/images/movie')
png(filename = 'Group_alpha.png', width = 800, height = 800, units = "px")
print(
  ggplot(viz, aes(y = viz$median, x = viz$time, group = viz$channels))+
    geom_line(aes(colour = channels))+
    ggtitle('Alpha asymmetry, group results') + xlab('Time, s') + ylab('Power')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
    scale_x_continuous(breaks=viz$time)
)
dev.off()

png(filename = 'Group_BLI.png', width = 800, height = 800, units = "px")
print(
  ggplot(groupResBLI, aes(y = groupResBLI$median, x = groupResBLI$time))+
    geom_line(aes(colour = 'red'))+
    ggtitle('BLI, group results') + xlab('Time, s') + ylab('Units')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
    scale_x_continuous(breaks=groupResBLI$time)
)
dev.off()


groupResFp12 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResFp12[,j] <- gmFp12[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResF34 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResF34[,j] <- gmF34[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResF78 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResF78[,j] <- gmF78[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResT78 <- as.data.frame(matrix(ncol = people, nrow = finalS))
j <- 1
s <- data.frame()
for (j in 1:people){
  groupResT78[,j] <- gmF34[[paste(name.list[[j]])]][1:finalS, 1]
}

groupResFp12$median <- apply(groupResFp12, 1, median)
groupResF34$median <- apply(groupResF34, 1, median)
groupResF78$median <- apply(groupResF78, 1, median)
groupResT78$median <- apply(groupResT78, 1, median)


viz <- as.data.frame(matrix(nrow = 4*finalS, ncol = 3))
colnames(viz) <- c('median', 'time', 'channels')
viz$median <- c(groupResFp12$median,
                groupResF34$median,
                groupResF78$median,
                groupResT78$median)
viz$time <- 1:finalS
viz$channels <- c(rep('Fp12', finalS),
                  rep('F34', finalS),
                  rep('F78', finalS),
                  rep('T78', finalS))

setwd('D:/Beehevior/BDF/BDF/images/movie')
png(filename = 'Group_gamma.png', width = 800, height = 800, units = "px")
print(
  ggplot(viz, aes(y = viz$median, x = viz$time, group = viz$channels))+
    geom_line(aes(colour = channels))+
    ggtitle('Gamma asymmetry, group results') + xlab('Time, s') + ylab('Power')+
    theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = 'none')+
    scale_x_continuous(breaks=viz$time)
)
dev.off()

