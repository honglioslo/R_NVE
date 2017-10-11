library(zoo)
DataPath <- "A:/holi/GitHub/hbvNVE/ex1/"
tem_files <- c("99840_Svalbard_airport_TAM_1948-2017.txt", "99910_Ny-Aalesund_TAM_1934-2017.txt")
pre_files <- c("99840_Svalbard_airport_RR_1975-2017.txt", "99910_Ny-Aalesund_RR_1974-2017.txt")
Dates <- seq(as.Date("1975-08-01"), as.Date("2016-12-31"), by = "day")
Data <- matrix(nrow = length(Dates), ncol = length(c(tem_files, pre_files)))


for (ipre in seq(length(pre_files))) {
  temp <- read.table(paste(DataPath, pre_files[ipre], sep = ""), header = TRUE, sep = "\t")
  temp_zoo <- zoo(temp[,3], as.Date(temp$Dato, format= "%d.%m.%Y" ))
  temp_zoo <- temp_zoo[index(temp_zoo) %in% Dates]
  Data[, ipre] <- temp_zoo
}
for (item in seq(length(tem_files))) {
  temp <- read.table(paste(DataPath, tem_files[item], sep = ""), header = TRUE, sep = "\t")
  temp_zoo <- zoo(temp[,4], as.Date(sprintf("%04d-%02d-%02d", temp$AAR, temp$MND, temp$DAG)))
  temp_zoo <- temp_zoo[index(temp_zoo) %in% Dates]
  Data[, item + length(pre_files)] <- temp_zoo
}

DataZoo <- zoo(Data, Dates)
rownames(DataZoo) <- format(Dates, "%Y%m%d/1200")
colnames(DataZoo) <- c(paste("pre_", seq(length(pre_files)), sep = ""), paste("tem_", seq(length(tem_files)), sep = ""))

fileName <- "../hbvNVE/ex1/input_data.txt"

writeHBV_metinput(DataZoo, fileName = fileName)
