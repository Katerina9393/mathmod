# Читаем файл
eddy = read.csv("C:/CO2/CO2/eddypro.csv", skip = 1)[-1, 1:77]# до столбца h2o cef[-1, c("date", "time", "daytime", "co2_flux")]
# Удаляем некорректные данные
eddy = eddy[which(!is.na(eddy$daytime) & (eddy$rand_err_Tau != "-9999.0")), ]
# Приводим столбцы к нужным типам
eddy$H = as.double(eddy$H)
eddy$co2_flux = as.double(eddy$co2_flux)
eddy$h2o_flux = as.double(eddy$h2o_flux)
eddy$qc_H = as.numeric(eddy$qc_H)
eddy$rand_err_H = as.numeric(eddy$rand_err_H)
# - Приводим к типу logical колонку daytime
eddy$daytime = as.logical(eddy$daytime)
# Май, 5 месяц. Дневное/ночное время = T/F
eddy = subset(eddy, as.Date(date) >= as.Date("2013-05-01") & as.Date(date) <= as.Date("2013-05-31") & daytime == T)

round(cor(x = eddy[ , c("H", "qc_H", "rand_err_H", "co2_flux", "h2o_flux")]), 2)
library(ggplot2)
# Это типо модель
mod = lm(
  co2_flux 
  ~
    h2o_flux + as.numeric(sonic_temperature) + as.numeric(air_temperature) + as.numeric(air_pressure) + as.numeric(wind_speed), # 
  data = eddy
)
coef(mod)


#Графики
plot(x = mod)
anova(mod)

mod <- m(lco2_flux ~ h2o_flux + as.numeric(sonic_temperature) + as.numeric(air_temperature) + as.numeric(air_pressure) + as.numeric(wind_speed) + h2o_flux:as.numeric(sonic_temperature):as.numeric(air_temperature):as.numeric(air_pressure):as.numeric(wind_speed),   data = eddy)












