# Read station code
station_code <- read.csv("data/station_code.csv")



# The general format of the URL of the weather data is:
# https://www.weather.gov.sg/files/dailydata/DAILYDATA_STATIONCODE_YYYYMM.csv
yy <- "2018"
mths <- sprintf("%02d", 1:12)
stations <- station_code$stn_code
root_s <- "http://www.weather.gov.sg/files/dailydata/"
for(s in stations) {
  for (mm in mths) {
    uu <- paste(root_s, 
                "DAILYDATA_", s, "_",
                yy, mm, ".csv", sep ="")
    outname <- paste("data/2018/", yy, mm, "_", s, ".csv", sep ="")
    cat(outname, "\n")
    try(download.file(uu, outname))
  }
}

yy <- "2019"
mths <- sprintf("%02d", 1:12)
stations <- station_code$stn_code
root_s <- "http://www.weather.gov.sg/files/dailydata/"
for(s in stations) {
  for (mm in mths) {
    uu <- paste(root_s, 
                "DAILYDATA_", s, "_",
                yy, mm, ".csv", sep ="")
    outname <- paste("data/2019/", yy, mm, "_", s, ".csv", sep ="")
    cat(outname, "\n")
    try(download.file(uu, outname))
  }
}

yy <- "2020"
mths <- sprintf("%02d", 1:12)
stations <- station_code$stn_code
root_s <- "http://www.weather.gov.sg/files/dailydata/"

for(s in stations) {
  for (mm in mths) {
    uu <- paste(root_s, 
                "DAILYDATA_", s, "_",
                yy, mm, ".csv", sep ="")
    outname <- paste("data/2020/", yy, mm, "_", s, ".csv", sep ="")
    cat(outname, "\n")
    try(download.file(uu, outname))
  }
}

yy <- "2021"
mths <- sprintf("%02d", 1:12)
stations <- station_code$stn_code
root_s <- "http://www.weather.gov.sg/files/dailydata/"

for(s in stations) {
  for (mm in mths) {
    uu <- paste(root_s, 
                "DAILYDATA_", s, "_",
                yy, mm, ".csv", sep ="")
    outname <- paste("data/2021/", yy, mm, "_", s, ".csv", sep ="")
    cat(outname, "\n")
    try(download.file(uu, outname))
  }
}



# Loop through files to combine data for each year
files <- list.files(path = "data/2018/", pattern = "*.csv")
df_total = data.frame()
for (i in files) {
  uu <- paste("data/2018/", i, sep = "")
  temp <- read.csv(uu, skip = 1, header = FALSE, na.strings = '-')
  df_total <- rbind(df_total, temp)
}
# save df_total in csv
write.csv(df_total, "data/df_2018.csv", row.names = FALSE)

files <- list.files(path = "data/2019/", pattern = "*.csv")
df_total = data.frame()
for (i in files) {
  uu <- paste("data/2019/", i, sep = "")
  temp <- read.csv(uu, skip = 1, header = FALSE, na.strings = '-')
  df_total <- rbind(df_total, temp)
}
# save df_total in csv
write.csv(df_total, "data/df_2019.csv", row.names = FALSE)

files <- list.files(path = "data/2020/", pattern = "*.csv")
df_total = data.frame()
for (i in files) {
  uu <- paste("data/2020/", i, sep = "")
  temp <- read.csv(uu, skip = 1, header = FALSE, na.strings = '-')
  df_total <- rbind(df_total, temp)
}
# save df_total in csv
write.csv(df_total, "data/df_2020.csv", row.names = FALSE)

files <- list.files(path = "data/2021/", pattern = "*.csv")
df_total = data.frame()
for (i in files) {
  uu <- paste("data/2021/", i, sep = "")
  temp <- read.csv(uu, skip = 1, header = FALSE, na.strings = '-')
  df_total <- rbind(df_total, temp)
}
# save df_total in csv
write.csv(df_total, "data/df_2021.csv", row.names = FALSE)