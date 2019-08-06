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

MBAI <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined1.csv", sep = ",", header = TRUE)
MBAI1 <- filter(MBAI, Annual_Precip < 1032.574)
MBAI2 <- filter(MBAI, 1032.54 <= Annual_Precip & Annual_Precip < 1177.400)
MBAI3 <- filter(MBAI, 1177.400 <= Annual_Precip & Annual_Precip < 1542.004)
MBAI4 <- filter(MBAI, 1542.004 <= Annual_Precip)
MBAI5 <- rbind(MBAI1, MBAI2, MBAI3, MBAI4)
MBAI5[,6] = 0
MBAI5[1:5,6] = "0-25%"
MBAI5[6:10,6] = "25-50%"
MBAI5[11:16,6] = "50-75%"
MBAI5[17:24,6] = "75-100%"
MBAI6 <- group_by(MBAI5, V6)
MBAI7 <- summarize(MBAI6, N_Anom = mean(N_Anomaly), S_Anom = mean(S_Anomaly), V_Anom = mean(V_Anomaly), sd_N = sd(N_Anomaly), sd_S = sd(S_Anomaly), sd_V = sd(V_Anomaly))
#Cleaned up Excel for Better Graph
MBAI8 <- read.csv("C:/Users/epica/Desktop/CUNY ASRC/Excel Activity/MapleDataRefined2.csv", sep = ",", header = TRUE)
MBAI8 <- na.omit(MBAI8)
a1 <- MBAI8[1:5,3] / sqrt(5)
MBAI8[1:5,3] = a1
a2 <- MBAI8[6:10,3] / sqrt(5)
MBAI8[6:10,3] = a2
a3 <- MBAI8[11:16,3] / sqrt(6)
MBAI8[11:16,3] = a3
a4 <- MBAI8[17:24,3] / sqrt(8)
MBAI8[17:24,3] = a4
MBAI8 <- na.omit(MBAI8)
MBAI8$Anom <- MBAI8$Anom - 1
X <- 0.7
ggplot(MBAI8, aes(x = ï..V6, y = Anom, color = Location))+
  geom_point(position = position_dodge(X))+
  geom_errorbar(aes(ymin=Anom-Sd, ymax=Anom+Sd), position = position_dodge(X))+
  ylim(-1,1)+
  theme(axis.text.x = element_text(vjust = c(0,0.65)), axis.title.x = element_text(margin = margin(t = -65)))+
  xlab("Percentile")+
  ylab("Anomaly")

