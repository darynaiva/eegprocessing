library(eegUtils)
library(eegkit)
library(ggplot2)
library(signal)
library(oce)
setwd('D:/Beehevior/BDF/BDF')

movie <- import_raw('Cherniuk_movie.bdf')
movie <- eeg_reference(movie, ref_chans = 'average',
                       exclude = c('GyroX', 'GyroY', 'GyroZ',
                                   'M1', 'M2'))
movie <- select_elecs(movie, electrode = c('Fp1', 'Fp2', 'F3', 'F4', 'F7', 'F8', 'T7', 'T8', 'Fz', 'Pz'))
movie.eeg <- as.data.frame(cbind(movie$signals, movie$timings$time))
colnames(movie.eeg) <- c(colnames(movie$signals), 'time')

