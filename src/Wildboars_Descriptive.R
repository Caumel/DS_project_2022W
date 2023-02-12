setwd("C:/Users/hanne/Desktop/Data Analysis Project")
getwd()

library(ggplot2)
library(reshape2)

final_closest <- read.csv2("final_closest_v2.csv", sep = ",")
head(final_closest)
data_fc <- final_closest
data_fc$date <- substr(data_fc$Date, 1, 10)
data_fc$time <- substr(data_fc$Date, 12, 19)
data_fc$year <- as.numeric(substr(data_fc$Date, 1, 4))
data_fc$month <- as.numeric(substr(data_fc$Date, 6, 7))
data_fc$day_night <- ifelse(as.numeric(substr(data_fc$time, 1, 2)) %in% c(5:18), "Day", "Night")
data_fc$season <- NULL
data_fc$season[data_fc$month %in% 3:5] <- "Spring"
data_fc$season[data_fc$month %in% 6:8] <- "Summer"
data_fc$season[data_fc$month %in% 9:11] <- "Autumn"
data_fc$season[data_fc$month %in% c(12, 1:2)] <- "Winter"
data_fc$animal_id <- data_fc$ID
data_fc$temp_subcut <- as.numeric(data_fc$temp.sc)
data_fc$temp_intrafixe <- as.numeric(data_fc$temp.ip)
data_fc$temp_intrafree <- as.numeric(data_fc$temp.if)
data_fc$temp_ear <- as.numeric(data_fc$temp.et)
data_fc$heartrate <- as.numeric(data_fc$hr)
data_fc$temp_main <- as.numeric(data_fc$TempMain)
temp_main_labels <- c("less than -10", "-10 to -5", "0-5 to 0", "0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25", "25 to 30", "30 to 35", "more than 35")
data_fc$temp_main_cat <- cut(data_fc$temp_main, c(-Inf, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, Inf), labels = temp_main_labels, na.rm = TRUE)
data_fc$temp_blackbulb <- as.numeric(data_fc$TempBB)
data_fc$temp_forest <- as.numeric(data_fc$TempForest)
temp_forest_labels <- c("less than -10", "-10 to -5", "0-5 to 0", "0 to 5", "5 to 10", "10 to 15", "15 to 20", "20 to 25", "25 to 30", "more than 30")
data_fc$temp_forest_cat <- cut(data_fc$temp_forest, c(-Inf, -10, -5, 0, 5, 10, 15, 20, 25, 30, Inf), labels = temp_forest_labels, na.rm = TRUE)
data_fc$hum_main <- as.numeric(data_fc$HumMain)
hum_main_labels <- c("less than 30%", "30%-40%", "40%-50%", "50%-60%", "60%-70%", "70%-80%", "80%-90%", "90%-100%")
data_fc$hum_main_cat <- cut(data_fc$hum_main, c(-Inf, seq(30, 90, 10), Inf), labels = hum_main_labels, na.rm = TRUE)
data_fc$hum_forest <- as.numeric(data_fc$HumForest)
data_fc$hum_forest_cat <- cut(data_fc$hum_forest, c(-Inf, seq(30, 90, 10), Inf), labels = hum_main_labels, na.rm = TRUE)
data_fc$wind <- as.numeric(data_fc$Wind)
wind_labels <- c("0", "0.1-3", "3.1-6", "6.1-9", "9.1-12", "12.1-15", "15.1-18", "18.1-21", "21.1-24", "24.1-27", "more than 27")
data_fc$wind_cat <- cut(data_fc$wind, c(-Inf, 0 , 3, seq(6, 27, 3), Inf), labels = wind_labels, na.rm = TRUE)
data_fc$solar <- as.numeric(data_fc$Solar)
data_fc$solar_cat <- ifelse(data_fc$solar > 0, "Yes", "No")
data_fc$rain <- as.numeric(data_fc$Rain)
data_fc$rain_cat <- ifelse(data_fc$rain > 0, "Yes", "No")
data_fc$baro <- as.numeric(data_fc$BaroPressure)
baro_labels <- c("less than 1020hPa", "1020-1030hPa", "1030-1040hPa", "1040-1050hPa", "more than 1050hPa")
data_fc$baro_cat <- cut(data_fc$baro, c(-Inf, seq(1020, 1050, 10), Inf), labels = baro_labels)
data_fc <- data_fc[, -(1:46)]

data_fc <- data_fc[(data_fc$temp_ear > 0 & data_fc$temp_ear < 50) | is.na(data_fc$temp_ear),]
data_fc <- data_fc[(data_fc$temp_subcut > 20 & data_fc$temp_subcut < 45) | is.na(data_fc$temp_subcut),]
data_fc <- data_fc[(data_fc$temp_intrafixe > 30 & data_fc$temp_intrafixe < 45) | is.na(data_fc$temp_intrafixe),]
data_fc <- data_fc[(data_fc$temp_intrafree > 30 & data_fc$temp_intrafree < 45) | is.na(data_fc$temp_intrafree),]
data_fc <- data_fc[(data_fc$heartrate > 30 & data_fc$heartrate < 220) | is.na(data_fc$heartrate),]

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")

heartrate_season <- round(tapply(data_fc$heartrate, data_fc$season, mean, na.rm = TRUE))
heartrate_season <- data.frame(x=names(heartrate_season), y=heartrate_season)
heartrate_month <- round(tapply(data_fc$heartrate, data_fc$month, mean, na.rm = TRUE))
heartrate_month <- data.frame(x=months, y=heartrate_month)
heartrate_month$x <- factor(heartrate_month$x, levels = heartrate_month$x)
heartrate_dn <- round(tapply(data_fc$heartrate, data_fc$day_night, mean, na.rm = TRUE))
heartrate_dn <- data.frame(x=names(heartrate_dn), y=heartrate_dn)
jpeg(file="1.jpeg")
ggplot(heartrate_season, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heartrate_season$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Season")+
  ylab("Mean Heartrate")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()
jpeg(file="2.jpeg")
ggplot(heartrate_month, aes(x,y,group=1))+
  geom_line(color = "royalblue")+
  geom_hline(yintercept = mean(heartrate_month$y), color = "red")+
  coord_cartesian(ylim=c(50,85))+
  ggtitle("Heartrate/ Month")+
  ylab("Mean Heartrate")+
  xlab("")
dev.off()
jpeg(file="3.jpeg")
ggplot(heartrate_dn, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate")+
  ylab("Mean Heartrate")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

subcut_season <- round(tapply(data_fc$temp_subcut, data_fc$season, mean, na.rm = TRUE), 2)
subcut_season <- data.frame(x=names(subcut_season), y=subcut_season)
subcut_month <- round(tapply(data_fc$temp_subcut, data_fc$month, mean, na.rm = TRUE), 2)
subcut_month <- data.frame(x=months, y=subcut_month)
subcut_month$x <- factor(subcut_month$x, levels = subcut_month$x)
subcut_dn <- round(tapply(data_fc$temp_subcut, data_fc$day_night, mean, na.rm = TRUE), 2)
subcut_dn <- data.frame(x=names(subcut_dn), y=subcut_dn)
jpeg(file="4.jpeg")
ggplot(subcut_season, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_season$y), color = "red")+
  coord_cartesian(ylim=c(34.5,37))+
  ggtitle("Subcut Temperature/ Season")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()
jpeg(file="5.jpeg")
ggplot(subcut_month, aes(x,y,group=1))+
  geom_line(color = "royalblue")+
  geom_hline(yintercept = mean(subcut_month$y), color = "red")+
  coord_cartesian(ylim=c(33,37))+
  ggtitle("Subcut Temperature/ Month")+
  ylab("Mean Temperature")+
  xlab("")
dev.off()
jpeg(file="6.jpeg")
ggplot(subcut_dn, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  coord_cartesian(ylim=c(35,36.5))+
  ggtitle("Subcut Temperature")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

ear_season <- round(tapply(data_fc$temp_ear, data_fc$season, mean, na.rm = TRUE),2)
ear_season <- data.frame(x=names(ear_season), y=ear_season)
ear_month <- round(tapply(data_fc$temp_ear, data_fc$month, mean, na.rm = TRUE),2)
ear_month <- data.frame(x=months, y=ear_month)
ear_month$x <- factor(ear_month$x, levels = ear_month$x)
ear_dn <- round(tapply(data_fc$temp_ear, data_fc$day_night, mean, na.rm = TRUE),2)
ear_dn <- data.frame(x=names(ear_dn), y=ear_dn)
jpeg(file="7.jpeg")
ggplot(ear_season, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_season$y), color = "red")+
  coord_cartesian(ylim=c(15,35))+
  ggtitle("Ear Tag Temperature/ Season")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()
jpeg(file="8.jpeg")
ggplot(ear_month, aes(x,y,group=1))+
  geom_line(color = "royalblue")+
  geom_hline(yintercept = mean(ear_month$y), color = "red")+
  coord_cartesian(ylim=c(15,35))+
  ggtitle("Ear Tag Temperature/ Month")+
  ylab("Mean Temperature")+
  xlab("")
dev.off()
jpeg(file="9.jpeg")
ggplot(ear_dn, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  coord_cartesian(ylim=c(15,30))+
  ggtitle("Ear Tag Temperature")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafixe_season <- round(tapply(data_fc$temp_intrafixe, data_fc$season, mean, na.rm = TRUE),2)
intrafixe_season <- data.frame(x=names(intrafixe_season), y=intrafixe_season)
intrafixe_month <- round(tapply(data_fc$temp_intrafixe, data_fc$month, mean, na.rm = TRUE),2)
intrafixe_month <- data.frame(x=months, y=intrafixe_month)
intrafixe_month$x <- factor(intrafixe_month$x, levels = intrafixe_month$x)
intrafixe_dn <- round(tapply(data_fc$temp_intrafixe, data_fc$day_night, mean, na.rm = TRUE),2)
intrafixe_dn <- data.frame(x=names(intrafixe_dn), y=intrafixe_dn)
jpeg(file="10.jpeg")
ggplot(intrafixe_season, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_season$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Season")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()
jpeg(file="11.jpeg")
ggplot(intrafixe_month, aes(x,y,group=1))+
  geom_line(color = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_month$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Month")+
  ylab("Mean Temperature")+
  xlab("")
dev.off()
jpeg(file="12.jpeg")
ggplot(intrafixe_dn, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafree_season <- round(tapply(data_fc$temp_intrafree, data_fc$season, mean, na.rm = TRUE),2)
intrafree_season <- data.frame(x=names(intrafree_season), y=intrafree_season)
intrafree_month <- round(tapply(data_fc$temp_intrafree, data_fc$month, mean, na.rm = TRUE),2)
intrafree_month <- data.frame(x=months, y=intrafree_month)
intrafree_month$x <- factor(intrafree_month$x, levels = intrafree_month$x)
intrafree_dn <- round(tapply(data_fc$temp_intrafree, data_fc$day_night, mean, na.rm = TRUE),2)
intrafree_dn <- data.frame(x=names(intrafree_dn), y=intrafree_dn)
jpeg(file="13.jpeg")
ggplot(intrafree_season, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_season$y), color = "red")+
  coord_cartesian(ylim=c(37,39.5))+
  ggtitle("Intrafree Temperature/ Season")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()
jpeg(file="14.jpeg")
ggplot(intrafree_month, aes(x,y,group=1))+
  geom_line(color = "royalblue")+
  geom_hline(yintercept = mean(intrafree_month$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Month")+
  ylab("Mean Temperature")+
  xlab("")
dev.off()
jpeg(file="15.jpeg")
ggplot(intrafree_dn, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

heart_animal <- round(tapply(data_fc$heartrate, data_fc$animal_id, mean, na.rm = TRUE)[!is.na(tapply(data_fc$heartrate, data_fc$animal_id, mean, na.rm = TRUE))])
heart_animal <- data.frame(x=names(heart_animal), y=heart_animal)
jpeg(file="16.jpeg")
ggplot(heart_animal, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_animal$y), color = "red")+
  coord_cartesian(ylim=c(40,90))+
  ggtitle("Heartrate/ Animal")+
  ylab("Mean Heartrate")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_animal <- round(tapply(data_fc$temp_ear, data_fc$animal_id, mean, na.rm = TRUE)[!is.na(tapply(data_fc$temp_ear, data_fc$animal_id, mean, na.rm = TRUE))],2)
ear_animal <- data.frame(x=names(ear_animal), y=ear_animal)
jpeg(file="17.jpeg")
ggplot(ear_animal, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_animal$y), color = "red")+
  coord_cartesian(ylim=c(0,35))+
  ggtitle("Ear Tag Temperature/ Animal")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


subcut_animal <- round(tapply(data_fc$temp_subcut, data_fc$animal_id, mean, na.rm = TRUE)[!is.na(tapply(data_fc$temp_subcut, data_fc$animal_id, mean, na.rm = TRUE))],2)
subcut_animal <- data.frame(x=names(subcut_animal), y=subcut_animal)
jpeg(file="18.jpeg")
ggplot(subcut_animal, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_animal$y), color = "red")+
  coord_cartesian(ylim=c(33,38))+
  ggtitle("Subcut Temperature/ Animal")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_animal <- round(tapply(data_fc$temp_intrafixe, data_fc$animal_id, mean, na.rm = TRUE)[!is.na(tapply(data_fc$temp_intrafixe, data_fc$animal_id, mean, na.rm = TRUE))],2)
intrafixe_animal <- data.frame(x=names(intrafixe_animal), y=intrafixe_animal)
jpeg(file="19.jpeg")
ggplot(intrafixe_animal, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_animal$y), color = "red")+
  coord_cartesian(ylim=c(37,39.5))+
  ggtitle("Intrafixe Temperature/ Animal")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_animal <- round(tapply(data_fc$temp_intrafree, data_fc$animal_id, mean, na.rm = TRUE)[!is.na(tapply(data_fc$temp_intrafree, data_fc$animal_id, mean, na.rm = TRUE))],2)
intrafree_animal <- data.frame(x=names(intrafree_animal), y=intrafree_animal)
jpeg(file="20.jpeg")
ggplot(intrafree_animal, aes(reorder(x,-y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_animal$y), color = "red")+
  coord_cartesian(ylim=c(38,39.5))+
  ggtitle("Intrafree Temperature/ Animal")+
  ylab("Mean Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

heart_temp <- round(tapply(data_fc$heartrate, data_fc$temp_main_cat, mean, na.rm = TRUE))
heart_temp <- data.frame(x=names(heart_temp), y=heart_temp)
heart_temp$x <- factor(heart_temp$x, levels = heart_temp$x)
jpeg(file="21.jpeg")
ggplot(heart_temp, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_temp$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Air Temperature (Main Station)")+
  ylab("Mean Heartrate")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_temp <- round(tapply(data_fc$temp_subcut, data_fc$temp_main_cat, mean, na.rm = TRUE),2)
subcut_temp <- data.frame(x=names(subcut_temp), y=subcut_temp)
subcut_temp$x <- factor(subcut_temp$x, levels = subcut_temp$x)
jpeg(file="22.jpeg")
ggplot(subcut_temp, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_temp$y), color = "red")+
  coord_cartesian(ylim=c(30,40))+
  ggtitle("Subcut Temperature/ Air Temperature (Main Station)")+
  ylab("Mean Subcut Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_temp <- round(tapply(data_fc$temp_ear, data_fc$temp_main_cat, mean, na.rm = TRUE),2)
ear_temp <- data.frame(x=names(ear_temp), y=ear_temp)
ear_temp$x <- factor(ear_temp$x, levels = ear_temp$x)
jpeg(file="23.jpeg")
ggplot(ear_temp, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_temp$y), color = "red")+
  coord_cartesian(ylim=c(10,40))+
  ggtitle("Ear Tag Temperature/ Air Temperature (Main Station)")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_temp <- round(tapply(data_fc$temp_intrafixe, data_fc$temp_main_cat, mean, na.rm = TRUE),2)
intrafixe_temp <- data.frame(x=names(intrafixe_temp), y=intrafixe_temp)
intrafixe_temp$x <- factor(intrafixe_temp$x, levels = intrafixe_temp$x)
jpeg(file="24.jpeg")
ggplot(intrafixe_temp, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_temp$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Air Temperature (Main Station)")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_temp <- round(tapply(data_fc$temp_intrafree, data_fc$temp_main_cat, mean, na.rm = TRUE),2)
intrafree_temp <- data.frame(x=names(intrafree_temp), y=intrafree_temp)
intrafree_temp$x <- factor(intrafree_temp$x, levels = intrafree_temp$x)
jpeg(file="25.jpeg")
ggplot(intrafree_temp, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_temp$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Air Temperature (Main Station)")+
  ylab("Mean Intrafree Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


heart_temp_f <- round(tapply(data_fc$heartrate, data_fc$temp_forest_cat, mean, na.rm = TRUE))
heart_temp_f <- data.frame(x=names(heart_temp_f), y=heart_temp_f)
heart_temp_f$x <- factor(heart_temp_f$x, levels = heart_temp_f$x)
jpeg(file="26.jpeg")
ggplot(heart_temp_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_temp_f$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Air Temperature (Forest Station)")+
  ylab("Mean Heartrate")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_temp_f <- round(tapply(data_fc$temp_subcut, data_fc$temp_forest_cat, mean, na.rm = TRUE),2)
subcut_temp_f <- data.frame(x=names(subcut_temp_f), y=subcut_temp_f)
subcut_temp_f$x <- factor(subcut_temp_f$x, levels = subcut_temp_f$x)
jpeg(file="27.jpeg")
ggplot(subcut_temp_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_temp_f$y), color = "red")+
  coord_cartesian(ylim=c(30,39))+
  ggtitle("Subcut Temperature/ Air Temperature (Forest Station)")+
  ylab("Mean Subcut Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_temp_f <- round(tapply(data_fc$temp_ear, data_fc$temp_forest_cat, mean, na.rm = TRUE),2)
ear_temp_f <- data.frame(x=names(ear_temp_f), y=ear_temp_f)
ear_temp_f$x <- factor(ear_temp_f$x, levels = ear_temp_f$x)
jpeg(file="28.jpeg")
ggplot(ear_temp_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_temp_f$y), color = "red")+
  coord_cartesian(ylim=c(10,37))+
  ggtitle("Ear Tag Temperature/ Air Temperature (Forest Station)")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_temp_f <- round(tapply(data_fc$temp_intrafixe, data_fc$temp_forest_cat, mean, na.rm = TRUE),2)
intrafixe_temp_f <- data.frame(x=names(intrafixe_temp_f), y=intrafixe_temp_f)
intrafixe_temp_f$x <- factor(intrafixe_temp_f$x, levels = intrafixe_temp_f$x)
jpeg(file="29.jpeg")
ggplot(intrafixe_temp_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_temp_f$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Air Temperature (Forest Station)")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_temp_f <- round(tapply(data_fc$temp_intrafree, data_fc$temp_forest_cat, mean, na.rm = TRUE),2)
intrafree_temp_f <- data.frame(x=names(intrafree_temp_f), y=intrafree_temp_f)
intrafree_temp_f$x <- factor(intrafree_temp_f$x, levels = intrafree_temp_f$x)
jpeg(file="30.jpeg")
ggplot(intrafree_temp_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_temp_f$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Air Temperature (Forest Station)")+
  ylab("Mean Intrafree Temperature")+
  xlab("Air Temperature °C")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

heart_hum <- round(tapply(data_fc$heartrate, data_fc$hum_main_cat, mean, na.rm = TRUE))
heart_hum <- data.frame(x=names(heart_hum), y=heart_hum)
heart_hum$x <- factor(heart_hum$x, levels = heart_hum$x)
jpeg(file="31.jpeg")
ggplot(heart_hum, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_hum$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Humidity (Main Station)")+
  ylab("Mean Heartrate")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_hum <- round(tapply(data_fc$temp_subcut, data_fc$hum_main_cat, mean, na.rm = TRUE),2)
subcut_hum <- data.frame(x=names(subcut_hum), y=subcut_hum)
subcut_hum$x <- factor(subcut_hum$x, levels = subcut_hum$x)
jpeg(file="32.jpeg")
ggplot(subcut_hum, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_hum$y), color = "red")+
  coord_cartesian(ylim=c(30,38))+
  ggtitle("Subcut Temperature/ Humidity (Main Station)")+
  ylab("Mean Subcut Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


ear_hum <- round(tapply(data_fc$temp_ear, data_fc$hum_main_cat, mean, na.rm = TRUE),2)
ear_hum <- data.frame(x=names(ear_hum), y=ear_hum)
ear_hum$x <- factor(ear_hum$x, levels = ear_hum$x)
jpeg(file="33.jpeg")
ggplot(ear_hum, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_hum$y), color = "red")+
  coord_cartesian(ylim=c(10,35))+
  ggtitle("Ear Tag Temperature/ Humidity (Main Station)")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_hum <- round(tapply(data_fc$temp_intrafixe, data_fc$hum_main_cat, mean, na.rm = TRUE),2)
intrafixe_hum <- data.frame(x=names(intrafixe_hum), y=intrafixe_hum)
intrafixe_hum$x <- factor(intrafixe_hum$x, levels = intrafixe_hum$x)
jpeg(file="34.jpeg")
ggplot(intrafixe_hum, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_hum$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Humidity (Main Station)")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_hum <- round(tapply(data_fc$temp_intrafree, data_fc$hum_main_cat, mean, na.rm = TRUE),2)
intrafree_hum <- data.frame(x=names(intrafree_hum), y=intrafree_hum)
intrafree_hum$x <- factor(intrafree_hum$x, levels = intrafree_hum$x)
jpeg(file="35.jpeg")
ggplot(intrafree_hum, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_hum$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Humidity (Main Station)")+
  ylab("Mean Intrafree Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

heart_hum_f <- round(tapply(data_fc$heartrate, data_fc$hum_forest_cat, mean, na.rm = TRUE),2)
heart_hum_f <- data.frame(x=names(heart_hum_f), y=heart_hum_f)
heart_hum_f$x <- factor(heart_hum_f$x, levels = heart_hum_f$x)
jpeg(file="36.jpeg")
ggplot(heart_hum_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_hum$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Humidity (Forest Station)")+
  ylab("Mean Heartrate")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_hum_f <- round(tapply(data_fc$temp_subcut, data_fc$hum_forest_cat, mean, na.rm = TRUE),2)
subcut_hum_f <- data.frame(x=names(subcut_hum_f), y=subcut_hum_f)
subcut_hum_f$x <- factor(subcut_hum_f$x, levels = subcut_hum_f$x)
jpeg(file="37.jpeg")
ggplot(subcut_hum_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_hum_f$y), color = "red")+
  coord_cartesian(ylim=c(30,38))+
  ggtitle("Subcut Temperature/ Humidity (Forest Station)")+
  ylab("Mean Subcut Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_hum_f <- round(tapply(data_fc$temp_ear, data_fc$hum_forest_cat, mean, na.rm = TRUE),2)
ear_hum_f <- data.frame(x=names(ear_hum_f), y=ear_hum_f)
ear_hum_f$x <- factor(ear_hum_f$x, levels = ear_hum_f$x)
jpeg(file="38.jpeg")
ggplot(ear_hum_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_hum_f$y), color = "red")+
  coord_cartesian(ylim=c(10,35))+
  ggtitle("Ear Tag Temperature/ Humidity (Forest Station)")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_hum_f <- round(tapply(data_fc$temp_intrafixe, data_fc$hum_forest_cat, mean, na.rm = TRUE),2)
intrafixe_hum_f <- data.frame(x=names(intrafixe_hum_f), y=intrafixe_hum_f)
intrafixe_hum_f$x <- factor(intrafixe_hum_f$x, levels = intrafixe_hum_f$x)
jpeg(file="39.jpeg")
ggplot(intrafixe_hum_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_hum_f$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Humidity (Forest Station)")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()


intrafree_hum_f <- round(tapply(data_fc$temp_intrafree, data_fc$hum_forest_cat, mean, na.rm = TRUE),2)
intrafree_hum_f <- data.frame(x=names(intrafree_hum_f), y=intrafree_hum_f)
intrafree_hum_f$x <- factor(intrafree_hum_f$x, levels = intrafree_hum_f$x)
jpeg(file="40.jpeg")
ggplot(intrafree_hum_f, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_hum_f$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Humidity (Forest Station)")+
  ylab("Mean Intrafree Temperature")+
  xlab("Humidity")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

heart_wind <- round(tapply(data_fc$heartrate, data_fc$wind_cat, mean, na.rm = TRUE))
heart_wind <- data.frame(x=names(heart_wind), y=heart_wind)
heart_wind$x <- factor(heart_wind$x, levels = heart_wind$x)
jpeg(file="41.jpeg")
ggplot(heart_wind, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_wind$y), color = "red")+
  coord_cartesian(ylim=c(40,100))+
  ggtitle("Heartrate/ Wind Speed")+
  ylab("Mean Heartrate")+
  xlab("Wind Speed in km/h")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_wind <- round(tapply(data_fc$temp_subcut, data_fc$wind_cat, mean, na.rm = TRUE),2)
subcut_wind <- data.frame(x=names(subcut_wind), y=subcut_wind)
subcut_wind$x <- factor(subcut_wind$x, levels = subcut_wind$x)
jpeg(file="42.jpeg")
ggplot(subcut_wind, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_wind$y), color = "red")+
  coord_cartesian(ylim=c(32,37))+
  ggtitle("Subcut Temperature/ Wind Speed")+
  ylab("Mean Subcut Temperature")+
  xlab("Wind Speed in km/h")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_wind <- round(tapply(data_fc$temp_ear, data_fc$wind_cat, mean, na.rm = TRUE),2)
ear_wind <- data.frame(x=names(ear_wind), y=ear_wind)
ear_wind$x <- factor(ear_wind$x, levels = ear_wind$x)
jpeg(file="43.jpeg")
ggplot(ear_wind, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_wind$y), color = "red")+
  coord_cartesian(ylim=c(15,28))+
  ggtitle("Ear Tag Temperature/ Wind Speed")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Wind Speed in km/h")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_wind <- round(tapply(data_fc$temp_intrafixe, data_fc$wind_cat, mean, na.rm = TRUE),2)
intrafixe_wind <- data.frame(x=names(intrafixe_wind), y=intrafixe_wind)
intrafixe_wind$x <- factor(intrafixe_wind$x, levels = intrafixe_wind$x)
jpeg(file="44.jpeg")
ggplot(intrafixe_wind, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_wind$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Wind Speed")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Wind Speed in km/h")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_wind <- round(tapply(data_fc$temp_intrafree, data_fc$wind_cat, mean, na.rm = TRUE),2)
intrafree_wind <- data.frame(x=names(intrafree_wind), y=intrafree_wind)
intrafree_wind$x <- factor(intrafree_wind$x, levels = intrafree_wind$x)
jpeg(file="45.jpeg")
ggplot(intrafree_wind, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_wind$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Wind Speed")+
  ylab("Mean Intrafree Temperature")+
  xlab("Wind Speed in km/h")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

heart_rain <- round(tapply(data_fc$heartrate, data_fc$rain_cat, mean, na.rm = TRUE))
heart_rain <- data.frame(x=names(heart_rain), y=heart_rain)
jpeg(file="46.jpeg")
ggplot(heart_rain, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_rain$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Rain")+
  ylab("Mean Heartrate")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

subcut_rain <- round(tapply(data_fc$temp_subcut, data_fc$rain_cat, mean, na.rm = TRUE),2)
subcut_rain <- data.frame(x=names(subcut_rain), y=subcut_rain)
jpeg(file="47.jpeg")
ggplot(subcut_rain, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_rain$y), color = "red")+
  coord_cartesian(ylim=c(32,37))+
  ggtitle("Subcut Temperature/ Rain")+
  ylab("Mean Subcut Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

ear_rain <- round(tapply(data_fc$temp_ear, data_fc$rain_cat, mean, na.rm = TRUE),2)
ear_rain <- data.frame(x=names(ear_rain), y=ear_rain)
jpeg(file="48.jpeg")
ggplot(ear_rain, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_rain$y), color = "red")+
  coord_cartesian(ylim=c(15,28))+
  ggtitle("Ear Tag Temperature/ Rain")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafixe_rain <- round(tapply(data_fc$temp_intrafixe, data_fc$rain_cat, mean, na.rm = TRUE),2)
intrafixe_rain <- data.frame(x=names(intrafixe_rain), y=intrafixe_rain)
jpeg(file="49.jpeg")
ggplot(intrafixe_rain, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_rain$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Rain")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafree_rain <- round(tapply(data_fc$temp_intrafree, data_fc$rain_cat, mean, na.rm = TRUE),2)
intrafree_rain <- data.frame(x=names(intrafree_rain), y=intrafree_rain)
jpeg(file="50.jpeg")
ggplot(intrafree_rain, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_rain$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Rain")+
  ylab("Mean Intrafree Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

heart_baro <- round(tapply(data_fc$heartrate, data_fc$baro_cat, mean, na.rm = TRUE))
heart_baro <- data.frame(x=names(heart_baro), y=heart_baro)
heart_baro$x <- factor(heart_baro$x, levels = heart_baro$x)
jpeg(file="51.jpeg")
ggplot(heart_baro, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_baro$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Air Pressure")+
  ylab("Mean Heartrate")+
  xlab("Air Pressure")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

subcut_baro <- round(tapply(data_fc$temp_subcut, data_fc$baro_cat, mean, na.rm = TRUE),2)
subcut_baro <- data.frame(x=names(subcut_baro), y=subcut_baro)
subcut_baro$x <- factor(subcut_baro$x, levels = subcut_baro$x)
jpeg(file="52.jpeg")
ggplot(subcut_baro, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_baro$y), color = "red")+
  coord_cartesian(ylim=c(32,37))+
  ggtitle("Subcut Temperature/ Air Pressure")+
  ylab("Mean Subcut Temperature")+
  xlab("Air Pressure")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

ear_baro <- round(tapply(data_fc$temp_ear, data_fc$baro_cat, mean, na.rm = TRUE),2)
ear_baro <- data.frame(x=names(ear_baro), y=ear_baro)
ear_baro$x <- factor(ear_baro$x, levels = ear_baro$x)
jpeg(file="53.jpeg")
ggplot(ear_baro, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_baro$y), color = "red")+
  coord_cartesian(ylim=c(15,28))+
  ggtitle("Ear Tag Temperature/ Air Pressure")+
  ylab("Mean Ear Tag Temperature")+
  xlab("Air Pressure")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafixe_baro <- round(tapply(data_fc$temp_intrafixe, data_fc$baro_cat, mean, na.rm = TRUE),2)
intrafixe_baro <- data.frame(x=names(intrafixe_baro), y=intrafixe_baro)
intrafixe_baro$x <- factor(intrafixe_baro$x, levels = intrafixe_baro$x)
jpeg(file="54.jpeg")
ggplot(intrafixe_baro, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_baro$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Air Pressure")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Air Pressure")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

intrafree_baro <- round(tapply(data_fc$temp_intrafree, data_fc$baro_cat, mean, na.rm = TRUE),2)
intrafree_baro <- data.frame(x=names(intrafree_baro), y=intrafree_baro)
intrafree_baro$x <- factor(intrafree_baro$x, levels = intrafree_baro$x)
jpeg(file="55.jpeg")
ggplot(intrafree_baro, aes(x,y,group=1))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_baro$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Air Pressure")+
  ylab("Mean Intrafree Temperature")+
  xlab("Air Pressure")+
  geom_text(aes(label=y), vjust=-0.3)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

heart_solar <- round(tapply(data_fc$heartrate, data_fc$solar_cat, mean, na.rm = TRUE))
heart_solar <- data.frame(x=names(heart_solar), y=heart_solar)
jpeg(file="56.jpeg")
ggplot(heart_solar, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(heart_solar$y), color = "red")+
  coord_cartesian(ylim=c(40,80))+
  ggtitle("Heartrate/ Sun")+
  ylab("Mean Heartrate")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

subcut_solar <- round(tapply(data_fc$temp_subcut, data_fc$solar_cat, mean, na.rm = TRUE),2)
subcut_solar <- data.frame(x=names(subcut_solar), y=subcut_solar)
jpeg(file="57.jpeg")
ggplot(subcut_solar, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(subcut_solar$y), color = "red")+
  coord_cartesian(ylim=c(32,37))+
  ggtitle("Subcut Temperature/ Sun")+
  ylab("Mean Subcut Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

ear_solar <- round(tapply(data_fc$temp_ear, data_fc$solar_cat, mean, na.rm = TRUE),2)
ear_solar <- data.frame(x=names(ear_solar), y=ear_solar)
jpeg(file="58.jpeg")
ggplot(ear_solar, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(ear_solar$y), color = "red")+
  coord_cartesian(ylim=c(15,30))+
  ggtitle("Ear Tag Temperature/ Sun")+
  ylab("Mean Ear Tag Temperature")+
  xlab("")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafixe_solar <- round(tapply(data_fc$temp_intrafixe, data_fc$solar_cat, mean, na.rm = TRUE),2)
intrafixe_solar <- data.frame(x=names(intrafixe_solar), y=intrafixe_solar)
jpeg(file="59.jpeg")
ggplot(intrafixe_solar, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafixe_solar$y), color = "red")+
  coord_cartesian(ylim=c(37,39))+
  ggtitle("Intrafixe Temperature/ Sun")+
  ylab("Mean Intrafixe Temperature")+
  xlab("Sun")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()

intrafree_solar <- round(tapply(data_fc$temp_intrafree, data_fc$solar_cat, mean, na.rm = TRUE),2)
intrafree_solar <- data.frame(x=names(intrafree_solar), y=intrafree_solar)
jpeg(file="60.jpeg")
ggplot(intrafree_solar, aes(reorder(x,y),y))+
  geom_bar(stat = "identity", fill = "royalblue")+
  geom_hline(yintercept = mean(intrafree_solar$y), color = "red")+
  coord_cartesian(ylim=c(37.5,39.5))+
  ggtitle("Intrafree Temperature/ Sun")+
  ylab("Mean Intrafree Temperature")+
  xlab("Rain")+
  geom_text(aes(label=y), vjust=-0.3)
dev.off()


data_fc_hr <- data_fc[, c("animal_id", "heartrate")]
data_fc_hr <- data_fc_hr[!is.na(data_fc_hr$heartrate),]
jpeg(file="61.jpeg")
ggplot(data_fc_hr, aes(x = animal_id, y = heartrate)) +            
  geom_boxplot()+
  xlab("Animal")+
  ylab("Heartrate")+
  ggtitle("Heartrate/Animal")
dev.off()

data_fc_ear <- data_fc[, c("animal_id", "temp_ear")]
data_fc_ear <- data_fc_ear[!is.na(data_fc_ear$temp_ear),]
jpeg(file="62.jpeg")
ggplot(data_fc_ear, aes(x = animal_id, y = temp_ear)) +            
  geom_boxplot()+
  xlab("Animal")+
  ylab("Ear Tag Temperature")+
  ggtitle("Ear Tag Temperature/Animal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

data_fc_subcut <- data_fc[, c("animal_id", "temp_subcut")]
data_fc_subcut <- data_fc_subcut[!is.na(data_fc_subcut$temp_subcut),]
jpeg(file="63.jpeg")
ggplot(data_fc_subcut, aes(x = animal_id, y = temp_subcut)) +            
  geom_boxplot()+
  xlab("Animal")+
  ylab("Subcut Temperature")+
  ggtitle("Subcut Temperature/Animal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

data_fc_intrafixe <- data_fc[, c("animal_id", "temp_intrafixe")]
data_fc_intrafixe <- data_fc_intrafixe[!is.na(data_fc_intrafixe$temp_intrafixe),]
jpeg(file="64.jpeg")
ggplot(data_fc_intrafixe, aes(x = animal_id, y = temp_intrafixe)) +            
  geom_boxplot()+
  xlab("Animal")+
  ylab("Intrafixe Temperature")+
  ggtitle("Intrafixe Temperature/Animal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

data_fc_intrafree <- data_fc[, c("animal_id", "temp_intrafree")]
data_fc_intrafree <- data_fc_intrafree[!is.na(data_fc_intrafree$temp_intrafree),]
jpeg(file="65.jpeg")
ggplot(data_fc_intrafree, aes(x = animal_id, y = temp_intrafree)) +            
  geom_boxplot()+
  xlab("Animal")+
  ylab("Intrafree Temperature")+
  ggtitle("Intrafree Temperature/Animal")
dev.off()


data_fc_temp <- data_fc[, c("season", "heartrate")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$heartrate),]
jpeg(file="66.jpeg")
ggplot(data_fc_temp, aes(x = season, y = heartrate)) +            
  geom_boxplot()+
  xlab("Season")+
  ylab("Heartrate")+
  ggtitle("Heartrate/Season (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("month", "heartrate")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$heartrate),]
jpeg(file="67.jpeg")
ggplot(data_fc_temp, aes(x = as.factor(month), y = heartrate)) +            
  geom_boxplot()+
  xlab("Month")+
  ylab("Heartrate")+
  ggtitle("Heartrate/Month (All animals)")
dev.off()

num <- 68
for(id in c("DE.2011.14", "DE.2011.23", "DE.2011.25", "IS.2011.01", "IS.2011.04", "IS.2011.11", "IS.2011.12")){
  data_fc_temp <- data_fc[data_fc$animal_id == id, c("season", "heartrate")]
  data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$heartrate),]
  jpeg(file = paste0(num, ".jpeg"))
  print(ggplot(data_fc_temp, aes(x = season, y = heartrate)) +            
    geom_boxplot()+
    xlab("Season")+
    ylab("Heartrate")+
    ggtitle(paste0("Heartrate/Season (", id, ")")))
  dev.off()
  num <- num + 1
  
  data_fc_temp <- data_fc[data_fc$animal_id == id, c("month", "heartrate")]
  data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$heartrate),]
  jpeg(file = paste0(num, ".jpeg"))
  print(ggplot(data_fc_temp, aes(x = as.factor(month), y = heartrate)) +            
          geom_boxplot()+
          xlab("Month")+
          ylab("Heartrate")+
          ggtitle(paste0("Heartrate/Month (", id, ")")))
  dev.off()
  num <- num + 1
  
  data_fc_temp <- data_fc[data_fc$animal_id == id, c("temp_main_cat", "heartrate")]
  data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_main_cat)&!is.na(data_fc_temp$heartrate),]
  data_fc_temp <- data_fc_temp[data_fc_temp$temp_main_cat %in% names(table(data_fc_temp$temp_main_cat)[table(data_fc_temp$temp_main_cat) > 10]),]
  jpeg(file = paste0(num, ".jpeg"))
  print(ggplot(data_fc_temp, aes(x = temp_main_cat, y = heartrate)) +            
          geom_boxplot()+
          xlab("Air Temperature (Main Station)")+
          ylab("Heartrate")+
          ggtitle(paste0("Heartrate/Air Temperature (", id, ")"))+
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
  dev.off()
  num <- num + 1
}


data_fc_temp <- data_fc[, c("season", "temp_subcut")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_subcut),]
jpeg(file="89.jpeg")
ggplot(data_fc_temp, aes(x = season, y = temp_subcut)) +            
  geom_boxplot()+
  xlab("Season")+
  ylab("Subcut Temperature")+
  ggtitle("Subcut Temperature/Season (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("month", "temp_subcut")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_subcut),]
jpeg(file="90.jpeg")
ggplot(data_fc_temp, aes(x = as.factor(month), y = temp_subcut)) +            
  geom_boxplot()+
  xlab("Month")+
  ylab("Subcut Temperature")+
  ggtitle("Subcut Temperature/Month (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("season", "temp_ear")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_ear),]
jpeg(file="91.jpeg")
ggplot(data_fc_temp, aes(x = season, y = temp_ear)) +            
  geom_boxplot()+
  xlab("Season")+
  ylab("Ear Tag Temperature")+
  ggtitle("Ear Tag Temperature/Season (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("month", "temp_ear")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_ear),]
jpeg(file="92.jpeg")
ggplot(data_fc_temp, aes(x = as.factor(month), y = temp_ear)) +            
  geom_boxplot()+
  xlab("Month")+
  ylab("Ear Tag Temperature")+
  ggtitle("Ear Tag Temperature/Month (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("month", "temp_intrafixe")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_intrafixe),]
jpeg(file="93.jpeg")
ggplot(data_fc_temp, aes(x = as.factor(month), y = temp_intrafixe)) +            
  geom_boxplot()+
  xlab("Month")+
  ylab("Intrafixe Temperature")+
  ggtitle("Intrafixe Temperature/Month (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("month", "temp_intrafree")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_intrafree),]
jpeg(file="94.jpeg")
ggplot(data_fc_temp, aes(x = as.factor(month), y = temp_intrafree)) +            
  geom_boxplot()+
  xlab("Month")+
  ylab("Intrafree Temperature")+
  ggtitle("Intrafree Temperature/Month (All animals)")
dev.off()

data_fc_temp <- data_fc[, c("animal_id", "month", "heartrate")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$heartrate),]
temp <- data.frame("animal" = NULL, "month" = NULL, "hr" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$heartrate, temp_id$month, mean)) == 12){
    hr <- tapply(temp_id$heartrate, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, hr)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("95.jpeg"))
ggplot(temp, aes(x = months, y = hr, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Heartrate")+
  ggtitle("Heartrate/Month (Animals compared)")
dev.off()

data_fc_temp <- data_fc[, c("animal_id", "month", "temp_subcut")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_subcut),]
temp <- data.frame("animal" = NULL, "month" = NULL, "ts" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$temp_subcut, temp_id$month, mean)) == 12){
    ts <- tapply(temp_id$temp_subcut, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, ts)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("96.jpeg"))
ggplot(temp, aes(x = months, y = ts, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Subcut Temperature")+
  ggtitle("Subcut Temperature/Month (Animals compared)")+
  scale_color_manual(values = c("cyan3", "lightblue", "blue", "green", "darkgreen", "yellow", "orange", "purple", "violet", "brown", "red", "black", "darkgrey"))
dev.off()

data_fc_temp <- data_fc[, c("animal_id", "month", "temp_subcut")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_subcut),]
temp <- data.frame("animal" = NULL, "month" = NULL, "ts" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$temp_subcut, temp_id$month, mean)) == 12){
    ts <- tapply(temp_id$temp_subcut, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, ts)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("96.jpeg"))
ggplot(temp, aes(x = months, y = ts, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Subcut Temperature")+
  ggtitle("Subcut Temperature/Month (Animals compared)")+
  scale_color_manual(values = c("cyan3", "lightblue", "blue", "green", "darkgreen", "yellow", "orange", "purple", "violet", "brown", "red", "black", "darkgrey"))
dev.off()

data_fc_temp <- data_fc[, c("animal_id", "month", "temp_ear")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_ear),]
temp <- data.frame("animal" = NULL, "month" = NULL, "te" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$temp_ear, temp_id$month, mean)) == 12){
    te <- tapply(temp_id$temp_ear, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, te)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("97.jpeg"))
ggplot(temp, aes(x = months, y = te, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Ear Tag Temperature")+
  ggtitle("Ear Tag Temperature/Month (Animals compared)")+
  scale_color_manual(values = c("blue", "green", "darkgreen", "yellow", "orange", "purple", "red", "black"))
dev.off()

data_fc_temp <- data_fc[, c("animal_id", "month", "temp_intrafixe")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_intrafixe),]
temp <- data.frame("animal" = NULL, "month" = NULL, "ti" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$temp_intrafixe, temp_id$month, mean)) == 12){
    ti <- tapply(temp_id$temp_intrafixe, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, ti)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("98.jpeg"))
ggplot(temp, aes(x = months, y = ti, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Intrafixe Temperature")+
  ggtitle("Intrafixe Temperature/Month (Animals compared)")+
  scale_color_manual(values = c("cyan3", "blue", "green", "darkgreen", "yellow", "orange", "purple", "violet", "brown", "red", "black"))
dev.off()


data_fc_temp <- data_fc[, c("animal_id", "month", "temp_intrafree")]
data_fc_temp <- data_fc_temp[!is.na(data_fc_temp$temp_intrafree),]
temp <- data.frame("animal" = NULL, "month" = NULL, "ti" = NULL)
for (id in unique(data_fc_temp$animal_id)){
  temp_id <- data_fc_temp[data_fc_temp$animal_id == id,]
  if (length(tapply(temp_id$temp_intrafree, temp_id$month, mean)) == 12){
    ti <- tapply(temp_id$temp_intrafree, temp_id$month, mean)
    animal <- rep(id, length(months))
    df <- data.frame(animal, months, ti)
    df$months <- factor(df$months, levels = df$months)
    temp <- rbind(temp, df)
  }
}
jpeg(file=paste0("99.jpeg"))
ggplot(temp, aes(x = months, y = ti, group = animal, color = animal)) +            
  geom_line()+
  xlab("Month")+
  ylab("Mean Intrafree Temperature")+
  ggtitle("Intrafree Temperature/Month (Animals compared)")
dev.off()

