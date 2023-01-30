setwd("C:/Users/hanne/Desktop/binary data")
getwd()

load("Accelerationdata.RData")
acs <- Acceleration
range(acs$timestampms)
head(acs)
table(acs$tagmac)
acs_means <- tapply(acs$absolute_acc, acs$tagmac, mean)

load("Loggers and Climate_one dataframe_closest time.RData")
all <- AllData
tail(all)
all$date <- substr(all$MergeTime, 1, 10)
all$time <- substr(all$MergeTime, 12, 19)
all$year <- as.numeric(substr(all$MergeTime, 1, 4))
all$month <- as.numeric(substr(all$MergeTime, 6, 7))
all$day_night <- ifelse(as.numeric(substr(all$time, 1, 2)) %in% c(5:18), "Day", "Night")
all$season <- NULL
all$season[all$month %in% 3:5] <- "Spring"
all$season[all$month %in% 6:8] <- "Summer"
all$season[all$month %in% 9:11] <- "Autumn"
all$season[all$month %in% c(12, 1:2)] <- "Winter"
all$animal_id <- all$ID
all$temp_subcut <- all$temp.sc
all$temp_intrafixe <- all$temp.ip
all$temp_intrafree <- all$temp.if
all$temp_ear <- all$temp.et
all$heartrate <- all$hr
all$temp_main <- all$TempMain
all$temp_main_cat <- cut(all$temp_main, c(-Inf, -10, -5, 0, 5, 10, 15, 20, 25, 30, 35, Inf), na.rm = TRUE)
all$temp_blackbulb <- all$TempBB
all$temp_forest <- all$TempForest
all$temp_forest_cat <- cut(all$temp_forest, c(-Inf, -10, -5, 0, 5, 10, 15, 20, 25, 30, Inf), na.rm = TRUE)
all$hum_main <- all$HumMain
all$hum_main_cat <- cut(all$hum_main, c(-Inf, seq(30, 90, 10), Inf), na.rm = TRUE)
all$hum_forest <- all$HumForest
all$hum_forest_cat <- cut(all$hum_forest, c(-Inf, seq(30, 90, 10), Inf), na.rm = TRUE)
all$wind <- all$Wind
all$wind_cat <- cut(all$wind, c(-Inf, 0.1, 3, seq(6, 27, 3), Inf), na.rm = TRUE)
all$solar <- all$Solar
all$solar_cat <- ifelse(all$solar > 0, "Yes", "No")
all$rain <- all$Rain
all$rain_cat <- ifelse(all$rain > 0, "Yes", "No")
all$baro <- all$BaroPressure
all$baro_cat <- cut(all$baro, c(-Inf, seq(1020, 1050, 10), Inf))
all <- all[, -(1:25)]

all <- all[(all$temp_ear > 10 & all$temp_ear < 40) | is.na(all$temp_ear),]
all <- all[(all$temp_subcut > 30 & all$temp_subcut < 40) | is.na(all$temp_subcut),]
all <- all[(all$temp_intrafixe > 36 & all$temp_intrafixe < 40) | is.na(all$temp_intrafixe),]
all <- all[(all$temp_intrafree > 37 & all$temp_intrafree < 40) | is.na(all$temp_intrafree),]
all <- all[(all$heartrate > 30 & all$heartrate < 220) | is.na(all$heartrate),]

subcut_season <- tapply(all$temp_subcut, all$season, median, na.rm = TRUE)
subcut_month <- tapply(all$temp_subcut, all$month, median, na.rm = TRUE)
subcut_dn <- tapply(all$temp_subcut, all$day_night, median, na.rm = TRUE)

ear_season <- tapply(all$temp_ear, all$season, median, na.rm = TRUE)
ear_month <- tapply(all$temp_ear, all$month, median, na.rm = TRUE)
ear_dn <- tapply(all$temp_ear, all$day_night, median, na.rm = TRUE)

intrafixe_season <- tapply(all$temp_intrafixe, all$season, median, na.rm = TRUE)
intrafixe_month <- tapply(all$temp_intrafixe, all$month, median, na.rm = TRUE)
intrafixe_dn <- tapply(all$temp_intrafixe, all$day_night, median, na.rm = TRUE)

intrafree_season <- tapply(all$temp_intrafree, all$season, median, na.rm = TRUE)
intrafree_month <- tapply(all$temp_intrafree, all$month, median, na.rm = TRUE)
intrafree_dn <- tapply(all$temp_intrafree, all$day_night, median, na.rm = TRUE)

ear_animal <- tapply(all$temp_ear, all$animal_id, median, na.rm = TRUE)[!is.na(tapply(all$temp_ear, all$animal_id, median, na.rm = TRUE))]
subcut_animal <- tapply(all$temp_subcut, all$animal_id, median, na.rm = TRUE)[!is.na(tapply(all$temp_subcut, all$animal_id, median, na.rm = TRUE))]
intrafixe_animal <- tapply(all$temp_intrafixe, all$animal_id, median, na.rm = TRUE)[!is.na(tapply(all$temp_intrafixe, all$animal_id, median, na.rm = TRUE))]
intrafree_animal <- tapply(all$temp_intrafree, all$animal_id, median, na.rm = TRUE)[!is.na(tapply(all$temp_intrafree, all$animal_id, median, na.rm = TRUE))]
heart_animal <-  tapply(all$heartrate, all$animal_id, median, na.rm = TRUE)[!is.na(tapply(all$heartrate, all$animal_id, median, na.rm = TRUE))]

heart_season <- tapply(all$heartrate, all$season, median, na.rm = TRUE)
heart_month <- tapply(all$heartrate, all$month, median, na.rm = TRUE)
heart_dn <- tapply(all$heartrate, all$day_night, median, na.rm = TRUE)

heart_temp <-  tapply(all$heartrate, all$temp_main_cat, median, na.rm = TRUE)
subcut_temp <- tapply(all$temp_subcut, all$temp_main_cat, median, na.rm = TRUE)
ear_temp <- tapply(all$temp_ear, all$temp_main_cat, median, na.rm = TRUE)
intrafixe_temp <- tapply(all$temp_intrafixe, all$temp_main_cat, median, na.rm = TRUE)
intrafree_temp <- tapply(all$temp_intrafree, all$temp_main_cat, median, na.rm = TRUE)

heart_temp_f <-  tapply(all$heartrate, all$temp_forest_cat, median, na.rm = TRUE)
subcut_temp_f <- tapply(all$temp_subcut, all$temp_forest_cat, median, na.rm = TRUE)
ear_temp_f <- tapply(all$temp_ear, all$temp_forest_cat, median, na.rm = TRUE)
intrafixe_temp_f <- tapply(all$temp_intrafixe, all$temp_forest_cat, median, na.rm = TRUE)
intrafree_temp_f <- tapply(all$temp_intrafree, all$temp_forest_cat, median, na.rm = TRUE)

heart_hum <-  tapply(all$heartrate, all$hum_main_cat, median, na.rm = TRUE)
subcut_hum <- tapply(all$temp_subcut, all$hum_main_cat, median, na.rm = TRUE)
ear_hum <- tapply(all$temp_ear, all$hum_main_cat, median, na.rm = TRUE)
intrafixe_hum <- tapply(all$temp_intrafixe, all$hum_main_cat, median, na.rm = TRUE)
intrafree_hum <- tapply(all$temp_intrafree, all$hum_main_cat, median, na.rm = TRUE)

heart_hum_f <-  tapply(all$heartrate, all$hum_forest_cat, median, na.rm = TRUE)
subcut_hum_f <- tapply(all$temp_subcut, all$hum_forest_cat, median, na.rm = TRUE)
ear_hum_f <- tapply(all$temp_ear, all$hum_forest_cat, median, na.rm = TRUE)
intrafixe_hum_f <- tapply(all$temp_intrafixe, all$hum_forest_cat, median, na.rm = TRUE)
intrafree_hum_f <- tapply(all$temp_intrafree, all$hum_forest_cat, median, na.rm = TRUE)

heart_wind <-  tapply(all$heartrate, all$wind_cat, median, na.rm = TRUE)
subcut_wind <- tapply(all$temp_subcut, all$wind_cat, median, na.rm = TRUE)
ear_wind <- tapply(all$temp_ear, all$wind_cat, median, na.rm = TRUE)
intrafixe_wind <- tapply(all$temp_intrafixe, all$wind_cat, median, na.rm = TRUE)
intrafree_wind <- tapply(all$temp_intrafree, all$wind_cat, median, na.rm = TRUE)

heart_rain <-  tapply(all$heartrate, all$rain_cat, median, na.rm = TRUE)
subcut_rain <- tapply(all$temp_subcut, all$rain_cat, median, na.rm = TRUE)
ear_rain <- tapply(all$temp_ear, all$rain_cat, median, na.rm = TRUE)
intrafixe_rain <- tapply(all$temp_intrafixe, all$rain_cat, median, na.rm = TRUE)
intrafree_rain <- tapply(all$temp_intrafree, all$rain_cat, median, na.rm = TRUE)

heart_baro <-  tapply(all$heartrate, all$baro_cat, median, na.rm = TRUE)
subcut_baro <- tapply(all$temp_subcut, all$baro_cat, median, na.rm = TRUE)
ear_baro <- tapply(all$temp_ear, all$baro_cat, median, na.rm = TRUE)
intrafixe_baro <- tapply(all$temp_intrafixe, all$baro_cat, median, na.rm = TRUE)
intrafree_baro <- tapply(all$temp_intrafree, all$baro_cat, median, na.rm = TRUE)

heart_solar <-  tapply(all$heartrate, all$solar_cat, median, na.rm = TRUE)
subcut_solar <- tapply(all$temp_subcut, all$solar_cat, median, na.rm = TRUE)
ear_solar <- tapply(all$temp_ear, all$solar_cat, median, na.rm = TRUE)
intrafixe_solar <- tapply(all$temp_intrafixe, all$solar_cat, median, na.rm = TRUE)
intrafree_solar <- tapply(all$temp_intrafree, all$solar_cat, median, na.rm = TRUE)


all_2016 <- all[all$year == 2016,]
all_2017 <- all[all$year == 2017,]
all_2018 <- all[all$year == 2018,]

#load("Positionsdata_2017.RData")
#load("Positionsdata_2018.RData")
#load("Positionsdata_2019.RData")


load("TelemetrieFiwigatter.RData")
load("TelemetrieJagdgatter.RData")
load("WaterTemperatures.RData")

tele_fiwi <- Tel.Fiwi
tele_fiwi$date <- substr(tele_fiwi$start, 1, 10)
tele_fiwi$time <- substr(tele_fiwi$start, 12, 19)
tele_fiwi$year <- as.numeric(substr(tele_fiwi$start, 1, 4))
tele_fiwi$month <- as.numeric(substr(tele_fiwi$start, 6, 7))
tele_fiwi$day_night <- ifelse(as.numeric(substr(tele_fiwi$time, 1, 2)) %in% c(5:18), "Day", "Night")
tele_fiwi$season <- NULL
tele_fiwi$season[tele_fiwi$month %in% 3:5] <- "Spring"
tele_fiwi$season[tele_fiwi$month %in% 6:8] <- "Summer"
tele_fiwi$season[tele_fiwi$month %in% 9:11] <- "Autumn"
tele_fiwi$season[tele_fiwi$month %in% c(12, 1:2)] <- "Winter"
tele_fiwi$animal_id <- tele_fiwi$ID
tele_fiwi$duration <- tele_fiwi$duration
tele_fiwi <- tele_fiwi[-(1:5), ]

tele_jg <- Tel.JG
tele_jg$date <- substr(tele_jg$start, 1, 10)
tele_jg$time <- substr(tele_jg$start, 12, 19)
tele_jg$year <- as.numeric(substr(tele_jg$start, 1, 4))
tele_jg$month <- as.numeric(substr(tele_jg$start, 6, 7))
tele_jg$day_night <- ifelse(as.numeric(substr(tele_jg$time, 1, 2)) %in% c(5:18), "Day", "Night")
tele_jg$season <- NULL
tele_jg$season[tele_jg$month %in% 3:5] <- "Spring"
tele_jg$season[tele_jg$month %in% 6:8] <- "Summer"
tele_jg$season[tele_jg$month %in% 9:11] <- "Autumn"
tele_jg$season[tele_jg$month %in% c(12, 1:2)] <- "Winter"
tele_jg$animal_id <- tele_jg$ID
tele_jg$location <- tele_jg$location
tele_jg$duration <- tele_jg$duration
tele_jg <- tele_jg[-(1:5), ]

