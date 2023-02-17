# Compute hourly photolysis rate of NO2 and HONO using TUV model
# Must repeat for each day then use rbind to combine runs
# Maybe in future make a for loop to run through list of dates
rm(list = ls()) # CLEARS MEMORY
tuv_core = function(wStart = 280, 
                    wStop = 420, 
                    wIntervals = 140, 
                    inputMode = 0, 
                    latitude = 0, 
                    longitude = 0, 
                    date = 20220625, 
                    timeStamp = "12:00:00",  
                    zenith = 0, 
                    ozone = 300, 
                    albedo = 0.1, 
                    gAltitude = 0, 
                    mAltitude = 0, 
                    taucld = 0.00, 
                    zbase = 4.00, 
                    ztop = 5.00, 
                    tauaer = 0.235, 
                    ssaaer = 0.990, 
                    alpha = 1.000, 
                    time = 12, 
                    outputMode = 2, 
                    nStreams = -2, 
                    dirsun = 1.0, 
                    difdn = 1.0, 
                    difup = NA){
  if(is.na(difup) & (outputMode == 2 | outputMode == 4)){
    difup = 1.0
  }else if(is.na(difup) & (outputMode == 3 | outputMode == 5)){
    difup = 0.0
  }
  url <- paste0(c("https://www.acom.ucar.edu/cgi-bin/acom/TUV/V5.3/tuv?wStart=", wStart, "&wStop=", wStop, "&wIntervals=", wIntervals, "&inputMode=", inputMode, "&latitude=", latitude, "&longitude=", longitude, "&date=", date, "&timeStamp=", timeStamp,  "&zenith=", zenith, "&ozone=", ozone, "&albedo=", albedo, "&gAltitude=", gAltitude, "&mAltitude=", mAltitude, "&taucld=", taucld, "&zbase=", zbase, "&ztop=", ztop, "&tauaer=", tauaer, "&ssaaer=", ssaaer, "&alpha=", alpha, "&time=", time, "&outputMode=", outputMode, "&nStreams=", nStreams, "&dirsun=", dirsun, "&difdn=", difdn, "&difup=", difup), collapse='')
  download.file(url, "file.txt", quiet = TRUE)
  filetext = read.delim("file.txt")
  if(outputMode == 2){
    # PHOTOLYSIS RATES (1/sec)
    strow = which(grepl("PHOTOLYSIS RATES", filetext[,1]))+1
    edrow = nrow(filetext)
    photolysis_rates = filetext[strow:edrow,]
    phlydf = data.frame(do.call(rbind, strsplit(photolysis_rates, ' (?=[^ ]+$)', perl=TRUE)))
    phlydf = phlydf[complete.cases(phlydf),]
    phlydf_value = data.frame(t(phlydf[,2]))
    names(phlydf_value) = phlydf[,1]
    return(phlydf_value)
  }else if(outputMode == 3){
    # WEIGHTED IRRADIANCES (W m-2)
    strow = which(grepl("with normalized action spectra", filetext[,1]))+1
    edrow = nrow(filetext)
    weighted_irradiances = filetext[strow:edrow,]
    weirdf = do.call(rbind.data.frame,strsplit(weighted_irradiances, "\\s{2,}"))
    weirdf_value = data.frame(t(weirdf[,3]))
    names(weirdf_value) = weirdf[,2]
    return(weirdf_value)
  }else if(outputMode == 4){
    # ACTINIC FLUX (# photons/sec/nm/cm2)
    strow=which(grepl("LOWER WVL  UPPER WVL  DIRECT", filetext[,1]))+1
    edrow=nrow(filetext)	   
    actinic_flux = filetext[strow:edrow,]
    acfldf_value = do.call(rbind.data.frame,strsplit(actinic_flux, "\\s{2,}"))
    names(acfldf_value) = unlist(strsplit(filetext[strow-1,], "\\s{2,}"))
    return(acfldf_value)
  }else if(outputMode == 5){
    # SPECTRAL IRRADIANCE (W m-2 nm-1)
    strow = which(grepl("LOWER WVL  UPPER WVL  DIRECT", filetext[,1]))+1
    edrow = nrow(filetext)	   
    spectral_irradiance = filetext[strow:edrow,]
    spirdf_value=do.call(rbind.data.frame,strsplit(spectral_irradiance, "\\s{2,}"))
    names(spirdf_value) = unlist(strsplit(filetext[strow-1,], "\\s{2,}"))
    return(spirdf_value)
  }
}
hour <- c("00", "01", "02",
          "03", "04", "05",
          "06", "07", "08",
          "09", "10", "11",
          "12", "13", "14",
          "15", "16", "17",
          "18", "19", "20",
          "21", "22", "23")
dates <- c(20220725:20220731, 20220801:20220812)
NO2_photolysis <- data.frame(matrix(1:24, ncol = 1))
HONO_photolysis <- data.frame(matrix(1:24, ncol = 1))
Ndays <- 19
for(i in 1:Ndays){
  # TUV runs ------------------------------------------------------------
  TUV1 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[1])
  TUV2 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[2])
  TUV3 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[3])
  TUV4 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[4])
  TUV5 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[5])
  TUV6 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[6])
  TUV7 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[7])
  TUV8 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[8])
  TUV9 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[9])
  TUV10 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[10])
  TUV11 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[11])
  TUV12 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[12])
  TUV13 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[13])
  TUV14 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[14])
  TUV15 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[15])
  TUV16 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[16])
  TUV17 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[17])
  TUV18 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[18])
  TUV19 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[19])
  TUV20 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[20])
  TUV21 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[21])
  TUV22 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[22])
  TUV23 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[23])
  TUV24 <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = dates[i], time = hour[24])
  NO2_photolysis[1,] <- TUV1[6] # NO2 photolysis
  HONO_photolysis[1,] <- TUV1[12] # HONO photolysis
  NO2_photolysis[2,] <- TUV2[6] # NO2 photolysis
  HONO_photolysis[2,] <- TUV2[12] # HONO photolysis
  NO2_photolysis[3,] <- TUV3[6] # NO2 photolysis
  HONO_photolysis[3,] <- TUV3[12] # HONO photolysis
  NO2_photolysis[4,] <- TUV4[6] # NO2 photolysis
  HONO_photolysis[4,] <- TUV4[12] # HONO photolysis
  NO2_photolysis[5,] <- TUV5[6] # NO2 photolysis
  HONO_photolysis[5,] <- TUV5[12] # HONO photolysis# Must satisfy: gAltitude < mAltitude
  NO2_photolysis[6,] <- TUV6[6] # NO2 photolysis
  HONO_photolysis[6,] <- TUV6[12] # HONO photolysis
  NO2_photolysis[7,] <- TUV7[6] # NO2 photolysis
  HONO_photolysis[7,] <- TUV7[12] # HONO photolysis
  NO2_photolysis[8,] <- TUV8[6] # NO2 photolysis
  HONO_photolysis[8,] <- TUV8[12] # HONO photolysis
  NO2_photolysis[9,] <- TUV9[6] # NO2 photolysis
  HONO_photolysis[9,] <- TUV9[12] # HONO photolysisresult[6] # NO2 -> NO + O
  NO2_photolysis[10,] <- TUV10[6] # NO2 photolysis
  HONO_photolysis[10,] <- TUV10[12] # HONO photolysisresult[12] # HONO -> OH +NO
  NO2_photolysis[11,] <- TUV11[6] # NO2 photolysis
  HONO_photolysis[11,] <- TUV11[12] # HONO photolysis
  NO2_photolysis[12,] <- TUV12[6] # NO2 photolysis
  HONO_photolysis[12,] <- TUV12[12] # HONO photolysis
  NO2_photolysis[13,] <- TUV13[6] # NO2 photolysis
  HONO_photolysis[13,] <- TUV13[12] # HONO photolysis
  NO2_photolysis[14,] <- TUV14[6] # NO2 photolysis
  HONO_photolysis[14,] <- TUV14[12] # HONO photolysis
  NO2_photolysis[15,] <- TUV15[6] # NO2 photolysis
  HONO_photolysis[15,] <- TUV15[12] # HONO photolysis
  NO2_photolysis[16,] <- TUV16[6] # NO2 photolysis
  HONO_photolysis[16,] <- TUV16[12] # HONO photolysis
  NO2_photolysis[17,] <- TUV17[6] # NO2 photolysis
  HONO_photolysis[17,] <- TUV17[12] # HONO photolysis
  NO2_photolysis[18,] <- TUV18[6] # NO2 photolysis
  HONO_photolysis[18,] <- TUV18[12] # HONO photolysis
  NO2_photolysis[19,] <- TUV19[6] # NO2 photolysis
  HONO_photolysis[19,] <- TUV19[12] # HONO photolysis
  NO2_photolysis[20,] <- TUV20[6] # NO2 photolysis
  HONO_photolysis[20,] <- TUV20[12] # HONO photolysis
  NO2_photolysis[21,] <- TUV21[6] # NO2 photolysis
  HONO_photolysis[21,] <- TUV21[12] # HONO photolysis
  NO2_photolysis[22,] <- TUV22[6] # NO2 photolysis
  HONO_photolysis[22,] <- TUV22[12] # HONO photolysis
  NO2_photolysis[23,] <- TUV23[6] # NO2 photolysis
  HONO_photolysis[23,] <- TUV23[12] # HONO photolysis
  NO2_photolysis[24,] <- TUV24[6] # NO2 photolysis
  HONO_photolysis[24,] <- TUV24[12] # HONO photolysis
  # Merge runs ------------------------------------------------------------
  NO2_photolysis$Time <- 1:24
  HONO_photolysis$Time <- 1:24
  Photolysis <- merge(NO2_photolysis, HONO_photolysis, by = "Time")
  Photolysis$Date <- dates[i]
  colnames(Photolysis) <- c("Time", "JNO2", "JHONO", "Date")
  if(i == 1){
    Photolysis1 <- Photolysis
  }
  if(i == 2){
    Photolysis2 <- Photolysis
  }
  if(i == 3){
    Photolysis3 <- Photolysis
  }
  if(i == 4){
    Photolysis4 <- Photolysis
  }
  if(i == 5){
    Photolysis5 <- Photolysis
  }
  if(i == 6){
    Photolysis6 <- Photolysis
  }
  if(i == 7){
    Photolysis7 <- Photolysis
  }
  if(i == 8){
    Photolysis8 <- Photolysis
  }
  if(i == 9){
    Photolysis9 <- Photolysis
  }
  if(i == 10){
    Photolysis10 <- Photolysis
  }
  if(i == 11){
    Photolysis11 <- Photolysis
  }
  if(i == 12){
    Photolysis12 <- Photolysis
  }
  if(i == 13){
    Photolysis13 <- Photolysis
  }
  if(i == 14){
    Photolysis14 <- Photolysis
  }
  if(i == 15){
    Photolysis15 <- Photolysis
  }
  if(i == 16){
    Photolysis16 <- Photolysis
  }
  if(i == 17){
    Photolysis17 <- Photolysis
  }
  if(i == 18){
    Photolysis18 <- Photolysis
  }
  if(i == 19){
    Photolysis19 <- Photolysis
  }
  
  }
TotalPhotolysis <- rbind(Photolysis1, Photolysis2, Photolysis3,
                         Photolysis4, Photolysis5, Photolysis6,
                         Photolysis7, Photolysis8, Photolysis9,
                         Photolysis10, Photolysis11, Photolysis12,
                         Photolysis13, Photolysis14, Photolysis15,
                         Photolysis16, Photolysis17, Photolysis18, Photolysis19)
View(TotalPhotolysis)
# Convert time from GMT to local EST, GMT = EST + 5 hours
TotalPhotolysis$Time <- as.numeric(TotalPhotolysis$Time)
TotalPhotolysis$Time <- TotalPhotolysis$Time - 5
for(i in 1:length(TotalPhotolysis)){
  if(TotalPhotolysis$Time[i] == -4){TotalPhotolysis$Time[i] = 20}
  if(TotalPhotolysis$Time[i] == -3){TotalPhotolysis$Time[i] = 21}
  if(TotalPhotolysis$Time[i] == -2){TotalPhotolysis$Time[i] = 22}
  if(TotalPhotolysis$Time[i] == -1){TotalPhotolysis$Time[i] = 23}
  if(TotalPhotolysis$Time[i] == 0){TotalPhotolysis$Time[i] = 24}
}
TotalPhotolysis$Hour <- TotalPhotolysis$Time
TotalPhotolysis$Year <- substrLeft(TotalPhotolysis$Date, 4)
TotalPhotolysis$Day <- substrRight(substrRight(TotalPhotolysis$Date, 4), 2)
TotalPhotolysis$Month <- substrLeft(substrRight(TotalPhotolysis$Date, 4), 2)
TotalPhotolysis$DateTime <- paste(TotalPhotolysis$Month, "/", TotalPhotolysis$Day, "/", TotalPhotolysis$Year, sep = "")
TotalPhotolysis$Time <- paste(TotalPhotolysis$Time, ":00", sep = "")
TotalPhotolysis$DateTime <- paste(TotalPhotolysis$DateTime, TotalPhotolysis$Time)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1,n)
}
# Combine runs ------------------------------------------------------------
Photolysis_July28 <- Photolysis
View(Photolysis_July28)
TotalPhotolysis <- rbind(Photolysis_July25,Photolysis_July26, Photolysis_July27,
                         Photolysis_July28,Photolysis_July29, Photolysis_July30,
                         Photolysis_July31,Photolysis_JAugust1, Photolysis_August2,
                         Photolysis_August3, Photolysis_August4, Photolysis_August5,
                         Photolysis_August6, Photolysis_August7, Photolysis_August8,
                         Photolysis_August9, Photolysis_August10, Photolysis_August11,Photolysis_August12)

# Attempting for loop version ------------------------------------------------------------
Ndays <- 19 # length of field campaign
NO2_photolysis <- data.frame(matrix(1:24, ncol = 1))
HONO_photolysis <- data.frame(matrix(1:24, ncol = 1))
for (i in 1:Ndays){
  # July 25-31
  if(i <= 6){
    date <- 20220725 + i - 1
    TUV <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = date)
    NO2_photolysis[i,] <- TUV[6] # NO2 photolysis
    HONO_photolysis[i,] <- TUV[12] # HONO photolysis
  }
  # August 1-12
  else if(i > 6){
    date <- 20220725 + i - 1 + 70 # get to 20220801
    TUV <- tuv_core(latitude = 45.568100, longitude = -84.682869, gAltitude = 0.2667, mAltitude = 0.26715, date = date)
    NO2_photolysis[i,] <- TUV[6] # NO2 photolysis
    HONO_photolysis[i,] <- TUV[12] # HONO photolysis
  }
  NO2_photolysis$Number <- 1:19
  HONO_photolysis$Number <- 1:19
  colnames(NO2_photolysis) <- c("JNO2", "Number")
  colnames(HONO_photolysis) <- c("JHONO", "Number")
  Photolysis <- merge(NO2_photolysis, HONO_photolysis, by = "Number")
  Photolysis <- Photolysis[,!names(Photolysis) %in% c("Number")]
}
View(HONO_photolysis)
View(Photolysis)

# 875 ft = 0.2667 km 
# measured 45cm above ground mAltitude = gAltitude + measurement


# Description of function
# Source: https://rdrr.io/cran/foqat/src/R/tuv_core.R
#' Calculate TUV Online
#'
#' This function runs TUV online by reading the input parameters, and summarizes the results to the new dataframe. \cr
#'
#' @param inputMode The default value is 0. InputMode 0: User-specified geographic location and time/date. The code computes the appropriate solar zenith angle and Earth-Sun distance.  InputMode 1: User specifies the solar zenith angle, and the annual average Earth-Sun distance is used. To avoid inconsistencies (e.g. overhead sun at the poles), options 1 and 2 cannot be invoked at the same time.   
#' @param outputMode The default value is 2.  OutputMode 2: Molecular photolysis frequencies (109 photoreactions). OutputMode 3: Weighted irradiance (27 weighting functions). OutputMode 4: Spectral actinic flux. OutputMode 5: Spectral irradiance.
#' @param nStreams The default value is -2.   NStreams -2: Pseudo-spherical 2 streams (faster, less accurate). NStreams 4: Pseudo-spherical discrete ordinate 4 streams (slower, more accurate).
#' @param wStart Shortest wavelength. The default value is 280. \cr  
#' @param wStop Longest wavelength.  The default value is 420. \cr
#' @param wIntervals Number of equal-sized subdivisions of the range End-Start. The default value is 140. \cr 
#' @param latitude Latitudes: positive North of equator, negative South of equator. The default value is 0. \cr
#' @param longitude Longitudes: positive East of the Greenwich meridian, negative West of the Greenwich meridian. The default value is 0. \cr  
#' @param zenith Solar zenith angle (deg). The default value is 0. \cr
#' @param ozone Ozone column, in Dobson Units (du), vertical, from ground (even if above sea level) to space. The US Standard Atmosphere O3 is used to specify the shape of the vertical profile but the total column is re-scaled to the value selected here by the user. The default value is 300. \cr
#' @param albedo Surface albedo: Assumes a Lambertian reflection (isotropic radiance) Values for snow can reach 0.90-0.99, but otherwise values at UV wavelengths are in the range 0.02-0.20 depending on the precise surface.  The default value is 0.1.
#' @param gAltitude Ground elevation: The elevation of the ground, in km above mean sea level.  The default value is 0. \cr
#' @param mAltitude Measurement altitude: The altitude in the atmosphere for which results are requested. This should not be confused with the ground elevation. For example, if you have measurements made from an airplane, flying at 6 km above the ground, and the surface is at 1.5 km, then you will want to request results for a measurement altitude of 7.5 km asl.   The default value is 0. \cr
#' @param taucld Cloud Optical Depth: vertical optical depth of the cloud. The default value is 0.00.  \cr
#' @param zbase Cloud base: base of cloud, in km (asl). The default value is 4.00. \cr  
#' @param ztop Cloud top:  top of cloud, in km (asl). The default value is 5.00. \cr  
#' @param tauaer Optical Depth: total extinction (absorption + scattering) at 550 nm, vertical, from ground to space. The default value is 0.235. \cr
#' @param ssaaer Single Scattering Albedo (S-S alb), assumed independent of wavelength. The default value is 0.990. \cr
#' @param alpha Alpha (Angstrom exponent), gives wavelength dependence of optical depth, by multiplying the 550 nm value by (550 nm/wavelength, nm)**alpha. The default value is 1.000. \cr
#' @param dirsun Direct beam, direct solar beam. The default value is 1.0. \cr 
#' @param difdn Diffuse down, down-ward propagating scattered radiation (diffuse sky light). The default value is 1.0. \cr  
#' @param difup Diffuse up, up-ward propagating scattered radiation (diffuse light from below). The default value is NA. \cr  
#' @param date Date (format: (YYYYMMDD, GMT). The default value is 20150630. \cr  
#' @param timeStamp -> Timestamp (format: hh:mm:ss, GMT). The default value is "12:00:00". \cr 
#' @param time Hour. The default value is 12. \cr 
#' @return a dataframe. The contents of dataframe are diterminated by OutputMode. \cr 
#' OutputMode 2: Molecular photolysis frequencies (109 photoreactions). \cr
#' OutputMode 3: Weighted irradiance (27 weighting functions). \cr
#' OutputMode 4: Spectral actinic flux. \cr
#' OutputMode 5: Spectral irradiance. \cr
#' @importFrom stats complete.cases 
