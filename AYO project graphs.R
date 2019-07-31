library(dplyr)
library(ggplot2)
BAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/bai_official.csv", sep = ",", header = TRUE)
BAI1 <- filter(BAI, Annual_Precip < 1032.574)
BAI2 <- filter(BAI, 1032.54 <= Annual_Precip & Annual_Precip < 1177.400)
BAI3 <- filter(BAI, 1177.400 <= Annual_Precip & Annual_Precip < 1542.004)
BAI4 <- filter(BAI, 1542.004 <= Annual_Precip)
BAI5 <- rbind(BAI1, BAI2, BAI3, BAI4)
BAI5[,25] = 0
BAI5[1:5,25] = "0-25%"
BAI5[6:10,25] = "25-50%"
BAI5[11:16,25] = "50-75%"
BAI5[16:22,25] = "75-100%"
BAI5 <- group_by(BAI5, V25)
BAI6 <- summarize(BAI5, N_Anomaly = mean(N_oak), S_Anomaly = mean(S_oak), V_Anomaly = mean(V_oak), sd_N = sd(N_oak), sd_S = sd(S_oak), sd_V = sd(V_oak))
BAI7 <- select(BAI6, sd_N, sd_S, sd_V)
#Cleaned up Excel for Better Graph
BAI8 <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/TreeAnomaly.csv", sep = ",", header = TRUE)
BAI8$Anomaly <- BAI8$Anomaly - 1
X <- 0.7
ggplot(BAI8, aes(x = ï..V25, y = Anomaly, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anomaly-sd, ymax=Anomaly+sd), position = position_dodge(X))+
  ylim(-1,1)+
  xlab("Percentile")+
  ylab("Anomaly")