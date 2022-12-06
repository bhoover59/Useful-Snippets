
# Example Data ------------------------------------------------------------


test1 = data.frame(Time1 = c("0:00", "1:00", "3:00", "4:00", "5:05", "7:00", "7:57", "9:00", "11:00", "13:00", "14:01", "15:00", "17:00", "17:55", "18:15", "19:00", "21:00", "23:00"), 
                   data1 = c(0,1,3,4,5,7,8,9,11,13,14,15,17,18,18.2,19,21,23))

test2 = data.frame(Time2 = c("0:00", "1:05", "2:00", "4:00", "6:05", "7:00", "8:57", "10:00", "12:00", "13:00", "14:01", "16:00", "18:00", "19:55", "20:15", "21:00", "22:00", "23:00"), 
                   data2 = c(0,1,2,4,6,7,9,10,12,13,14,16,18,20,20.2,21,22,23))

test1$Time1 = as.POSIXct(test1$Time1, format = "%H:%M")
test2$Time2 = as.POSIXct(test2$Time2, format = "%H:%M")


# findclose(x,y) ----------------------------------------------------------

# Returns an index of which values in y are nearest to the values in x
# Output has every value in x, may miss or duplicate values in y

findclose = function(x, y) {
  output = c()
  x = as.numeric(x)
  y = as.numeric(y)
  for (i in 1:length(x)){
    output[i] = which.min(abs(x[i]-y))
  }
  return(output)
}

## Example

test1$Time2 = test2$Time2[findclose(test1$Time1, test2$Time2)]
test1$data2 = test2$data2[findclose(test1$Time1, test2$Time2)]


# match_avgs(x,y,xdata,ydata,t) -------------------------------------------

# When you have two time series with the same time resolution (i.e. both 30m averages or 1m average)
# and both data sets have occasional missing values
# Returns a new data frame with all time steps and NAs for missing values

# x, y = time column of data, respectively
# xdata, ydata = data you want to merge
# t = time step, in minutes

match_avgs = function(x, y, xdata, ydata, t){
  # if ("dplyr" %in% (.packages()) == FALSE){
  #   library(dplyr)
  # }
  # if ("lubridate" %in% (.packages()) == FALSE){
  #   library(lubridate)
  # }
  t_sec = t * 60
  all = c(x, y)
  time_min = min(all)
  unit = paste(t, "minutes")
  time_min = floor_date(time_min, unit = unit)
  time_max = max(all)
  time_max = ceiling_date(time_max, unit = unit)
  time_series = seq.POSIXt(time_min, time_max, by = t_sec)
  idx = data.frame(matrix(nrow = length(time_series), ncol = 3))
  colnames(idx) = c("time_series", "x", "y")
  idx$time_series = time_series
  for (i in 1:length(x)){
    idx$x[which.min(abs(x[i] - time_series))] = i
  }
  for (i in 1:length(y)){
    idx$y[which.min(abs(y[i] - time_series))] = i
  }
  colnames = c("time_series", names(xdata), names(ydata))
  lengthx = ncol(xdata)
  output = data.frame(matrix(nrow = length(time_series), ncol = length(colnames)))
  colnames(output) = colnames
  output$time_series = time_series
  for (i in 1:nrow(idx)){
    for (j in 1:ncol(xdata)){
      output[i, j + 1] = xdata[idx[i, 2], j]
    }
    for (j in 1:ncol(ydata)){
      output[i, j + ncol(xdata) + 1] = ydata[idx[i, 3], j]
    }
  }
  return(output)
}

# Example

output = match_avgs(test1$Time1, test2$Time2, test1, test2, 60)


# Replacing missing with diurnal averages ---------------------------------

#This code relies on the binaverage function, which is included below

#Define flag for when data is missing (when timestamp of one data set is far from another)
all$flag = 0
all$TimeDiff = abs(as.numeric(all$DateTimeO3) - as.numeric(all$Binstart))
all$flag[all$TimeDiff > 5400] = 1
#This cutoff value is in seconds, since POSIXct is seconds since 01-01-1970


if (any(all$flag == 1)){
  #Take original data set and add a time column for the diurnal average
  O3_avg$Time = as.POSIXct(paste("08/08/2021", format(O3_avg$DateTimeO3, "%H:%M:%S")), format = "%m/%d/%Y %H:%M:%S")
  O3_diurnal = binaverage(O3_avg, timecol = "Time", x = 120)
  O3_diurnal = O3_diurnal[ ,c("Group.1", "Time", "O3", "JNO2", "Temp1", "P_atm", "JHONO")]
  colnames(O3_diurnal)[1] = "Binstart"
  #for any flagged row, replace the values with diurnal average measurement from the same hour
  for (i in 1:nrow(all)){
    if (all$flag[i] == 1){
      all$P_atm[i] = O3_diurnal$P_atm[which(hour(O3_diurnal$Binstart) == hour(all$Binstart[i]))]
      all$O3[i] = O3_diurnal$O3[which(hour(O3_diurnal$Binstart) == hour(all$Binstart[i]))]
      all$JNO2[i] = O3_diurnal$JNO2[which(hour(O3_diurnal$Binstart) == hour(all$Binstart[i]))]
      all$Temp1[i] = O3_diurnal$Temp1[which(hour(O3_diurnal$Binstart) == hour(all$Binstart[i]))]
      all$JHONO[i] = O3_diurnal$JHONO[which(hour(O3_diurnal$Binstart) == hour(all$Binstart[i]))]
    }
  }
}



# binaverage(df, timecol, x, sd_count, centered) --------------------------

#' df = dataframe you want to average
#' timecol = in quotation marks, the name of the column with datetime data
#' x = number of minutes to average
#' ignore_text = T/F, keep true because I haven't coded it yet
#'      theoretically, if false, it will try to match text data to nearest timestamp
#' sd_count = T/F, if true, returns stdev and # of points for each data column
#' centered = T/F, changes where the middle of the bin is. If true, the binstart column 
#'      is messed up, but I haven't figured that out yet, but the averaged timecol is correct

binaverage <- function(df, timecol = "datetime", x = 5, ignore_text=TRUE, sd_count = FALSE, centered = FALSE){
  class_test = lapply(df, class)
  idx <- rep(NA, ncol(df))
  for (i in 1:length(idx)){
    if (class_test[[i]][1] == "character" | class_test[[i]][1] == "factor"){
      idx[i] = 0
    } else{
      idx[i] = 1
    }
  }
  if (centered == TRUE){
    Timefloor <- as.POSIXct(floor(as.numeric(df[ ,timecol]-(x*30)) / (x * 60)) * (x * 60) , origin = "1970-01-01")
  } else{
    Timefloor <- as.POSIXct(floor(as.numeric(df[ ,timecol]) / (x * 60)) * (x * 60), origin = "1970-01-01")
  }
  
  output <- aggregate(df[,which(idx == 1)], list(Timefloor), mean, na.action = na.omit)
  if (sd_count == TRUE){
    stdev <- aggregate(df[,which(idx == 1)], list(Timefloor), sd)
    count <- aggregate(df[, which(idx == 1)], list(Timefloor), length)
    names(stdev) = paste0(names(stdev), "SD")
    names(count) = paste0(names(count), "Count")
    output = cbind(output, stdev, count)
  }
  
  if (ignore_text == FALSE & any(idx == 0)){
    textdata <- df[ ,which(idx == 0)]
    textnames <- names(df)[idx == 0]
    timedata <- df[,timecol]
    textdata <- data.frame(timedata, textdata)
    textnames <- append("datetime", textnames)
    names(textdata) <-textnames
    setDT(textdata)
    setkey(textdata, datetime)
    setDT(output)
    setkey(output, datetime)
    output = textdata[output, roll = "nearest"]
  }
  
  return(output)
}
