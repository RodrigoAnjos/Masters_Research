#####################################@
## ---- Initialization ----
#####################################@
library(signal)
library(readr)
library(stringr)
library(patchwork)
library(ggplot2)
library(pracma)

#####################################@
## ---- Constants Declaration ----
epsilon0_const  <- 8.850e-12  # Electric Permittivity [F/m]
L_e             <- 0.800      # Line Length           [m]
V_p             <- 2e8        # Propagation Velocity  [m/s]
tau             <- L_e / V_p  # Electrical Delay      [s]

minor_break     <- 0.025
major_break     <- 0.200

#####################################@
## ---- Dataframe Acquisition ----
get_dataSet <- function(path, filename, colnames, delim = ",", skip = 0){
  df = data.frame()
  
  tryCatch({
    
    
    df <- read_delim(paste(path,filename,sep = "/"),
                     delim          = delim,
                     col_names      = colnames,
                     comment        = "%",
                     show_col_types = FALSE,
                     trim_ws        = TRUE,
                     skip           = skip)
    
    if(ncol(df) == 1){
      #message("Unexpected number of columns. Attempting to read as table...")
      df <- read_table(paste(path,filename,sep = "/"),
                       col_names      = colnames,
                       comment        = "#",
                       show_col_types = FALSE,
                       skip           = skip)
    }
    
    },
    error = function(e){
      message("Returning NA - ", e)
      return(data.frame())
    }
  )

  if(ncol(df) != length(colnames)){
    message("Dataframe does not match expected input columns. Returning NULL")
    return(NULL)
  }
  
  return(df)
  
}
get_multiple_dataSet <- function(path, filename, colnames, delim = ",", skip=0, sampling_time=30){
  
  # Check the total number of dataset to be merged
  filename          <- paste(path,filename,sep = "/")
  directory         <- dirname(filename)
  base_name         <- basename(filename)
  base_name_escaped <- gsub("([\\^$.|?*+(){}\\[\\]])", "\\\\\\1", base_name)               # Escape special characters in the base_name for use in regex
  
  pattern        <- paste0("^", base_name_escaped, "_(\\d+)\\.S2P$")                       # Build pattern to match files like "base_name_#.S2P"
  file_list      <- list.files(path = directory, pattern = pattern, full.names = FALSE)    # List files in the directory
  matches        <- regmatches(file_list, regexec(pattern, file_list))                     # Extract numeric indices
  indices        <- as.integer(sapply(matches, function(x) x[2]))
  indices        <- indices[!is.na(indices)]                                               # Clean up NAs

  # Get initial point
  tinitial <- get_time_from_S2P(directory, paste0(base_name_escaped, "_", 0, ".S2P"))
  
  # Iterate through each dataset and store data in main dataset
  df_list <- list()
  for (i in sort(indices)) {
    #print(paste0(base_name_escaped, "_", i, ".S2P"))
    
    df <- data.frame()
    df <- get_dataSet(directory, paste0(base_name_escaped, "_", i, ".S2P"), colnames, ",", 10)
    df <- process_MEAS_Data(df, directory,paste0(base_name_escaped, "_", i, ".S2P"))
    df$absTime <- tinitial + i*30
    df$index <- i
    df_list[[i+1]] <- df
    
    cat(sprintf("\rExtracting S2P Data [%d/%d]", i, max(indices)))
    flush.console()
    
  }
  cat("\nDone Extracting S2P Data from folder ...\n")
  
  df_agregate <- as.data.frame(do.call(rbind, df_list))
  
  return(df_agregate)
}
get_time_from_S2P <- function(path, filename){
  filepath <- paste(path,filename,sep = "/")
  
  # Open the file connection
  con <- file(filepath, open = "r", encoding = "latin1")
  
  tinitial <- NA  # Default to NA
  
  # Read line by line
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # Remove problematic non-ASCII characters safely
    line_clean <- iconv(line, from = "", to = "UTF-8", sub = "byte")
    
    #print(line)

    # Check if line contains the exact pattern
    if (grepl("! Time and date: ", line, fixed = TRUE)) {
      
      # Extract date-time string using regex: dd/mm/YYYY HH:MM
      match <- regmatches(line, regexpr("\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}:\\d{2}", line))
      
      if (length(match) > 0) {
        # Convert to POSIXct
        tinitial <- as.POSIXct(match, format = "%d/%m/%Y %H:%M", tz="Etc/GMT+3")
        
      }
      
      # Stop reading after finding the relevant line
      break
    }
  }
  
  # Close the connection
  close(con)
  cat(as.character(tinitial))
  
  return(tinitial)
}
get_mag_unit_from_S2P <- function(path, filename){
  filepath <- paste(path, filename, sep = "/")
  
  # Open the file connection
  con <- file(filepath, open = "r", encoding = "latin1")
  
  default_unit <- NA  # Default to NA
  
  # Read line by line
  while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
    
    # Remove problematic non-ASCII characters safely
    line_clean <- iconv(line, from = "", to = "UTF-8", sub = "byte")
    
    # Check if line contains the exact pattern
    if (grepl("HZ  S  DB", line, fixed = TRUE)) {
      close(con)
      # Stop reading after finding the relevant line
      return("DB")
    } else if (grepl("HZ  S  MA", line, fixed = TRUE)){
      close(con)
      # Stop reading after finding the relevant line
      return("LIN")
    }
  }
  
  # Close the connection
  close(con)
  return("ERR")
}
get_filenames <- function(path){
  cat("Retrieving filenames from folder:", as.character(path))
  # List all files in folder
  files <- list.files(path, full.names = TRUE)
  
  # Find first matching "sensor*" (no extension enforced)
  sensor_match <- files[grepl("^sensor", basename(files))]
  sensor_file <- if (length(sensor_match) > 0) basename(sensor_match[1]) else NA
  
  # Find first matching "stimulus*" (no extension enforced)
  stimulus_match <- files[grepl("^stimulus", basename(files))]
  stimulus_file <- if (length(stimulus_match) > 0) basename(stimulus_match[1]) else NA
  
  # Find first matching "*_0.S2P" (case insensitive)
  s2p_match <- files[grepl("_0\\.S2P$", basename(files), ignore.case = TRUE)]
  s2p_file <- if (length(s2p_match) > 0) {
    s2p_base <- basename(s2p_match[1])
    sub("_0\\.S2P$", "", s2p_base, ignore.case = TRUE)  # Remove "_0.S2P"
  } else {
    NA
  }
  
  cat("\n\nFound: \n[SENSOR FILENAME]:\t",sensor_file,"\n[STIMULUS FILENAME]:\t",stimulus_file,"\n[S2P FILENAME]:\t\t",s2p_file,"\n")
  
  filename_list <- list(
    sensor_file   = sensor_file,
    stimulus_file = stimulus_file,
    s2p_file      = s2p_file
  )
  
  # Return result as named list
  return(filename_list)
}
get_experiment <- function(path){
  
}

## ---- Dataframe Processing ----
process_ADS_Data     <- function(df){
  if(is.null(df)){
    return(NULL)
  }
  ## Clean frequency units
  df$Freq <- gsub(" GHz","e9",df$Freq)
  df$Freq <- gsub(" MHz","e6",df$Freq)
  df$Freq <- gsub(" kHz","e3",df$Freq)
  
  ## Convert frequency data to numeric
  df$Freq <- as.numeric(df$Freq)
  
  SParam <- c("S11", "S21", "S12", "S22")
  
  ## Check S-Parameter coordinate system
  df$S11 <- convert_polar_to_complex(df$S11)
  df$S21 <- convert_polar_to_complex(df$S21)
  
  #df[ , SParam] <- lapply(df[ , SParam], function(col) sapply(col, convert_polar_to_complex))
  
  names(df$S21) <- NULL
  
  #df$S21 <- electrical_delay_compensation(df$S21, df$Freq)
  #df$S11 <- electrical_delay_compensation(df$S11, df$Freq)

  
  return(df)
}
process_COMSOL_Data  <- function(df){
  if(is.null(df)){
    return(NULL)
  }
  ## Convert frequency data to numeric
  df$Freq <- as.numeric(df$Freq)*1e9
  
  ## Convert S-Parameter data to imaginary numbers
  df$S11 <- as.complex(df$S11)
  df$S21 <- as.complex(df$S21)
  
  df$Z  <- Mod(50 * (1 + df$S11) / (1 - df$S11))
  df$Z  <- round(df$Z / 10) * 10
  
  
  return(df)
}
process_MEAS_Data    <- function(df, path, filename, EDelay=NULL, EDelayRef=NULL){
  if(is.null(df)){
    return(NULL)
  }
  ## Check S-Parameter coordinate system
  #print(names(df))
  
  unit <- get_mag_unit_from_S2P(path, filename)
  
  if(unit == "DB"){
    df$MagS11_lin <- 10^(df$MagS11/20)
    df$MagS21_lin <- 10^(df$MagS21/20)
    
    df$PhaseS11_rad <- df$PhaseS11 * pi / 180
    df$PhaseS21_rad <- df$PhaseS21 * pi / 180
    
    df$S11 <- df$MagS11_lin * cos(df$PhaseS11_rad) + 1i * df$MagS11_lin * sin(df$PhaseS11_rad)
    df$S21 <- df$MagS21_lin * cos(df$PhaseS21_rad) + 1i * df$MagS21_lin * sin(df$PhaseS21_rad)
    
    df$MagS11 <- df$PhaseS11 <- NULL
    df$MagS21 <- df$PhaseS21 <- NULL
    df$MagS11_lin <- df$PhaseS11_rad <- NULL
    df$MagS21_lin <- df$PhaseS21_rad <- NULL
    
  } else if (unit == "LIN"){
    df$S11 <- df$MagS11 * exp(1i * df$PhaseS11 * pi / 180)
    df$S21 <- df$MagS21 * exp(1i * df$PhaseS21 * pi / 180)
    
    df$MagS11 <- df$PhaseS11 <- NULL
    df$MagS21 <- df$PhaseS21 <- NULL
  } else {
    return(NULL)
  }
  
  if(!is.null(EDelay) & !is.null(EDelayRef)){
    df$unwrappedPhase <- get_phase_from_electrical_delay(df, EDelayRef, EDelay)
  }
  
  


  
  #df$MagS12 <- df$PhaseS12 <- NULL
  #df$MagS22 <- df$PhaseS22 <- NULL
  
  
  
  return(df)
}
process_RH_Data      <- function(df){
  
  if(is.null(df)){
    message("Error NULL RH Dataframe to be processed")
    return(NULL)
  }
  
  ## Convert Date/Time format
  df$absTime  <- as.POSIXct(paste(df$date, df$time), format = "%Y-%m-%d %H:%M:%S", tz="Etc/GMT+3")   # Create POSIX compatible auxiliary time column
  df$date     <- NULL                                                                            # Delete column date
  df$time     <- NULL                                                                            # Delete column time
  
  ## Extract Relative Time and remove duplicates
  df$relTime <- as.numeric(df$absTime - min(df$absTime, na.rm = TRUE))                           # Create relative time from the start of the experiment
  df          <- df[!duplicated(df$relTime), ]                                                   # Remove duplicate values and maintain only the first value
  
  
  ## Extract Initial and Final Timestamps
  t_init   <- as.POSIXct(min(df$absTime, na.rm = TRUE), tz="Etc/GMT+3")
  t_final  <- as.POSIXct(max(df$absTime, na.rm = TRUE), tz="Etc/GMT+3")
  Ts       <- 1
  t_len    <- length(seq(from = t_init, to = t_final, by = Ts))
  
  ## 
  
  
  proc_df <- data.frame(matrix(ncol = 0, nrow = t_len))                                  # Create empty data frame
  excluded_cols <- c("absTime", "relTime")                                                       # Create excluded columns from interpolation function

  if(ncol(df) != 0){
    proc_df$absTime <- seq(from = t_init, to = t_final, by = Ts)         # Interpolate Time Ts = 1 sec
    proc_df$relTime <- as.numeric(proc_df$absTime - min(proc_df$absTime))                        # Create relative time from the start of the experiment

    for (col in setdiff(colnames(df), excluded_cols)) {
      proc_df[[col]] <- (approx(df$relTime,df[[col]],proc_df$relTime))$y                         # Interpolate the Y value
    }
  }
  return(proc_df)
}
process_STIM_Data    <- function(df){
  if(is.null(df)){
    return(NULL)
  }
  ## Add Relative Pressure Column
  df$RS       <- df$MFC1*0.0 + df$MFC3*1.0*df$MFC4
  
  ## Convert Date/Time format
  df$absTime  <- as.POSIXct(paste(df$date, df$time), format = "%d/%m/%Y %H:%M:%S", tz="Etc/GMT+3")               # Create POSIX compatible auxiliary time column
  df$date     <- NULL                                                                            # Delete column date
  df$time     <- NULL                                                                            # Delete column time
  
  ## Extract Relative Time and remove duplicates
  df$relTime  <- as.numeric(df$absTime - min(df$absTime))                                        # Create relative time from the start of the experiment
  df          <- df[!duplicated(df$relTime), ]                                                   # Remove duplicate values and maintain only the first value
  
  ## Extract Initial and Final Timestamps
  t_init   <- as.POSIXct(min(df$absTime), tz="Etc/GMT+3")
  t_final  <- as.POSIXct(max(df$absTime), tz="Etc/GMT+3")
  Ts       <- 1
  t_len    <- length(seq(from = t_init, to = t_final, by = Ts))

  ## Interpolate Data
  proc_df <- data.frame(matrix(ncol = 0, nrow = t_len))                                          # Create empty data frame
  excluded_cols <- c("absTime", "relTime")                                                       # Create excluded columns from interpolation function
  
  if(ncol(df) != 0){
    proc_df$absTime <- seq(from = t_init, to = t_final, by = Ts)                                 # Interpolate Time Ts = 1 sec
    proc_df$relTime <- as.numeric(proc_df$absTime - min(proc_df$absTime))                        # Create relative time from the start of the experiment
    
    for (col in setdiff(colnames(df), excluded_cols)) {
      proc_df[[col]] <- (approx(df$relTime,df[[col]],proc_df$relTime))$y                         # Interpolate the Y value
    }
  }

  return(proc_df)
}

get_start_time       <- function(df_list){
  t_absolute_initial <- list()
  
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]
    if(is.null(df)){
      cat("Dataframe", i, "- Rows: NULL \tStart Time @ NULL\n")
    } else {
      t_absolute_initial[[i]] = min(df$absTime)
      cat("Dataframe", i, "- Rows:", nrow(df), "\tStart Time @", as.character(t_absolute_initial[[i]]) , "\n")
    }
    
  }
  
  min_abs_time = as.POSIXct(max(unlist(t_absolute_initial)))
  
  cat("Common Absolute Initial Time found is:", as.character(min_abs_time),"\n\n")
  
  return(min_abs_time)
}
get_end_time         <- function(df_list){
  t_absolute_final <- list()
  
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]
    if(is.null(df)){
      cat("Dataframe", i, "- Rows: NULL \tEnd Time @ NULL\n")
    } else {
      t_absolute_final[[i]] = max(df$absTime, na.rm = TRUE)
      cat("Dataframe", i, "- Rows:", nrow(df), "\tEnd Time @", as.character(t_absolute_final[[i]]) , "\n")
    }
  }
  
  max_abs_time = as.POSIXct(min(unlist(t_absolute_final)), na.rm = TRUE)
  
  cat("Common Absolute Final Time found is:", as.character(max_abs_time),"\n\n")
  
  return(max_abs_time)
}
get_endRel_time      <- function(df_list){
  t_relative_final <- list()
  
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]
    if(is.null(df)){
      cat("Dataframe", i, "- Max Index: NULL \tEnd Time @ NULL\n")
    } else {
      t_relative_final[[i]] = max(df$relTime)
      cat("Dataframe", i, "- Max Index:", t_relative_final[[i]], "\tEnd Time @", as.character(t_relative_final[[i]]) , "\n")
    }
  }
  
  max_rel_time = min(unlist(t_relative_final))
  
  cat("Common Relative Final Time found is:", as.character(max_rel_time),"\n\n")
  
  return(max_rel_time)
}
cut_df               <- function(df, tinitial, tfinal){
  if(is.null(df)){
    return(NULL)
  }
  df <- subset(df, absTime <= tfinal & absTime >= tinitial)
  df$relTime <- as.numeric(df$absTime - min(df$absTime))
  
  return(df)
}
zero_crossing        <- function(df, epsilon){
  if(is.null(df)){
    return(NULL)
  }
  df_neg <- subset(df, PlaneX < 0)
  df_pos <- subset(df, PlaneX > 0)
  
  
  # Compute Quadratic model for E and E^2 for negative portion of the dataset
  
  neg_Enorm_model  <- lm(Enorm  ~ PlaneX + I(PlaneX^2), data = tail(df_neg, 3))
  neg_E2norm_model <- lm(E2norm ~ PlaneX + I(PlaneX^2), data = tail(df_neg, 3))
  neg_y_Enorm   <- predict(neg_Enorm_model,  newdata = data.frame(PlaneX = -epsilon))
  neg_y_E2norm  <- predict(neg_E2norm_model, newdata = data.frame(PlaneX = -epsilon))
  neg_point     <- data.frame(Freq   = NA,
                              PlaneX = (-epsilon),
                              Enorm  = neg_y_Enorm,
                              E2norm = neg_y_E2norm,
                              S11    = NA,
                              S21    = NA)
  df_neg <- rbind(df_neg, neg_point)
  
  # Compute Quadratic model for E and E^2 for positive portion of the dataset
  
  pos_Enorm_model  <- lm(Enorm  ~ PlaneX + I(PlaneX^2), data = head(df_pos, 3))
  pos_E2norm_model <- lm(E2norm ~ PlaneX + I(PlaneX^2), data = head(df_pos, 3))
  pos_y_Enorm   <- predict(pos_Enorm_model,  newdata = data.frame(PlaneX =  epsilon))
  pos_y_E2norm  <- predict(pos_E2norm_model, newdata = data.frame(PlaneX =  epsilon))
  pos_point     <- data.frame(Freq   = NA,
                              PlaneX = ( epsilon),
                              Enorm  = pos_y_Enorm,
                              E2norm = pos_y_E2norm,
                              S11    = NA,
                              S21    = NA)
  df_pos <- rbind(pos_point, df_pos)
  
  df <- rbind(df_neg, df_pos)
  
  
  return(df)
}
zero_padding         <- function(df, xlim){
  if(is.null(df)){
    return(NULL)
  }
  neg_lim <- xlim[1]
  pos_lim <- xlim[2]
  
  p1      <- data.frame(Freq   = NA,
                        PlaneX = neg_lim,
                        Enorm  = 0,
                        E2norm = 0,
                        S11    = NA,
                        S21    = NA)
  
  p2      <- data.frame(Freq   = NA,
                        PlaneX = df[which.min(df$PlaneX), ]$PlaneX,
                        Enorm  = 0,
                        E2norm = 0,
                        S11    = NA,
                        S21    = NA)
  
  p3      <- data.frame(Freq   = NA,
                        PlaneX = df[which.max(df$PlaneX), ]$PlaneX,
                        Enorm  = 0,
                        E2norm = 0,
                        S11    = NA,
                        S21    = NA)
  
  p4      <- data.frame(Freq   = NA,
                        PlaneX = pos_lim,
                        Enorm  = 0,
                        E2norm = 0,
                        S11    = NA,
                        S21    = NA)

  df <- rbind(p1, p2, df, p3, p4)
  
  return(df)
}
get_MUT_sensibility  <- function(df){
  max_y <- max(get_phase(subset(df, df$Freq == 5e9)$S21, "unwrapped", "relative", "degrees"))
  min_y <- min(get_phase(subset(df, df$Freq == 5e9)$S21, "unwrapped", "relative", "degrees"))
  max_x <- max(subset(df, df$Freq == 5e9)$Er)
  min_x <- min(subset(df, df$Freq == 5e9)$Er)
  
  delta_y <- max_y - min_y
  delta_x <- max_x - min_x
  
  sensibility <- delta_y/delta_x
  
  return(sensibility)
}
get_cum_We           <- function(df, e_mut, e_sub){
  # Compute We (energy density per unit volume)
  
  df_pos          <- df[df$PlaneX > 0, ]
  df_pos$We       <- e_mut * epsilon0_const * df_pos$E2norm / 4
  df_pos$plane_We <- numeric(length(df_pos$PlaneX))
  df_pos$cum_We   <- numeric(length(df_pos$PlaneX))
  df_pos$dz       <- numeric(length(df_pos$PlaneX))
  
  dfl <- length(df_pos$PlaneX)
  
  for (j in seq(1,dfl)){
    if(j == dfl){
      df_pos$dz[j]       <- 0
      df_pos$plane_We[j] <- 0
    } else {
      df_pos$dz[j]       <- (df_pos$PlaneX[j+1] - df_pos$PlaneX[j])*0.001
      df_pos$plane_We[j] <- (df_pos$We    [j+1] + df_pos$We    [j])*df_pos$dz[j]/2
    }
    
    
    if(j == 1){
      df_pos$cum_We[j] <- df_pos$plane_We[j]
    } else {
      df_pos$cum_We[j] <- df_pos$plane_We[j] + df_pos$cum_We[j-1]
    }
  }
  
  df_neg          <- df[df$PlaneX < 0, ]
  df_neg$We       <- e_sub * epsilon0_const * df_neg$E2norm / 4
  df_neg$plane_We <- numeric(length(df_neg$PlaneX))
  df_neg$cum_We   <- numeric(length(df_neg$PlaneX))
  df_neg$dz       <- numeric(length(df_neg$PlaneX))
  
  dfl <- length(df_neg$PlaneX)
  
  for (j in seq(dfl, 1)){
    
    if(j == 1){
      df_neg$dz[j]       <- 0
      df_neg$plane_We[j] <- 0
    } else {
      df_neg$dz[j]       <- (df_neg$PlaneX[j] - df_neg$PlaneX[j-1])*0.001
      df_neg$plane_We[j] <- (df_neg$We    [j] + df_neg$We    [j-1])*df_neg$dz[j]/2
    }
    
    if(j == dfl){
      df_neg$cum_We[j] <- df_neg$plane_We[j]
    } else {
      df_neg$cum_We[j] <- df_neg$plane_We[j] + df_neg$cum_We[j+1]
    }
  }

  # print(head(df_pos[, c("We", "PlaneX", "dz", "plane_We", "cum_We")], 5))
  # 
  # print(tail(df_pos[, c("We", "PlaneX", "dz", "plane_We", "cum_We")], 5))
  
  df_result <- rbind(df_pos, df_neg)
  
  return(df_result)
}

#####################################@
## ---- S-Parameters Functions ----
get_mag                       <- function(data, unit=""){
  # Check if data already is in a compatible format
  if(!is.complex(data)){
    data <- as.complex(gsub("i", "i", data))
  }
  
  # Calculate S11 Magnitude in V/V
  data <- Mod(data)
  
  if(unit == "dB"){
    data  <- 20*log10(data)                     # Transform to degrees
  }
  
  return(data)
}
get_phase                     <- function(data, wrapping="NA", mode="NA", unit="NA"){
  # Check if data already is in a compatible format
  if(!is.complex(data)){
    data <- as.complex(gsub("i", "i", data))
  }
  
  # Calculate S21 Phase in Radians
  phase  <- Arg(data)
  
  if(wrapping == "unwrapped"){
    phase  <- unwrap(phase)                         # Unwrap phase in radians
  }
  
  if(unit == "degrees"){
    phase  <- phase*180/(pi)                      # Transform to degrees
  }
  
  if(mode == "relative"){
    phase  <- phase - phase[1]    # subtract 
  }
  
  return(phase)
}
convert_polar_to_complex      <- function(data) {
  vapply(data, function(value) {
    if (str_detect(value, "^-?\\d*\\.?\\d+\\s*/\\s*-?\\d*\\.?\\d+$")) {
      # Extract magnitude and phase
      parts <- as.numeric(unlist(str_split(value, "\\s*/\\s*")))
      magnitude <- parts[1]
      phase_deg <- parts[2]
      
      # Convert to complex number
      return(magnitude * exp(1i * phase_deg * pi / 180))
    } else {
      num <- suppressWarnings(as.numeric(value))
      if (is.na(num)) return(NA_complex_) else return(num + 0i)
    }
  }, FUN.VALUE = complex(1))
}
electrical_delay_compensation <- function(data, freq, tau){
  data <- data * exp(1i * 2 * pi * freq * tau)
  return(data)
}
get_phase_from_electrical_delay <- function(data, delay_freq, tau){
  phase_raw <- Arg(data$S21) - 2 * pi * data$Freq * tau + 2 * pi * delay_freq * tau  # Add back delay
  phase_deg <- (phase_raw * 180) / pi           # Convert to degrees
  
  
  return(phase_deg)
}

#####################################@
## ---- Config  Plot Functions  ---- 

library("viridis")

add_We_line        <- function(ggplot_obj, df, e_mut, e_sub, color) {
  ggplot_obj <- ggplot_obj + 
    geom_line(
      data      = subset(df, PlaneX > 0),
      aes(x     = PlaneX,
          y     = e_mut * epsilon0_const * E2norm / 4,
          color = color),
      linewidth = 1,
      linetype  = "solid")
  
  ggplot_obj <- ggplot_obj + 
    geom_line(
      data      = subset(df, PlaneX < 0),
      aes(x     = PlaneX,
          y     = e_sub * epsilon0_const * E2norm / 4,
          color = color),
      linewidth = 1,
      linetype  = "solid")
  
  return(ggplot_obj)
}
add_CumWe_line     <- function(ggplot_obj, df, e_mut, e_sub, color) {
  
  
  df_filtered <- get_cum_We(df, e_mut, e_sub)

  ggplot_obj <- ggplot_obj + 
    geom_line(
      data      = subset(df_filtered, PlaneX > 0),
      aes(x     = PlaneX,
          y     = cum_We,
          color = color),
      linewidth = 1,
      linetype  = "solid")
  
  ggplot_obj <- ggplot_obj +
    geom_line(
      data      = subset(df_filtered, PlaneX < 0),
      aes(x     = PlaneX,
          y     = cum_We,
          color = color),
      linewidth = 1,
      linetype  = "solid")
  
  return(ggplot_obj)
}
add_SMag_line      <- function(ggplot_obj, df, dataset, data="Both"){
  if (data == "Both" | data == "S11"){
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df),
        aes(x     = Freq/1e9,
            y     = get_mag(df$S11, "dB"),
            color = dataset,
            linetype  = "S11"),
        linewidth = 1,
      )
  }
  if (data == "Both" | data == "S21"){
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df),
        aes(x     = Freq/1e9,
            y     = get_mag(df$S21, "dB"),
            color = dataset,
            linetype  = "S21"),
        linewidth = 1)
  }
  return(ggplot_obj)
}
add_SMag_Er_line   <- function(ggplot_obj, df, dataset, Er_seq)     {

  # ggplot_obj <- ggplot_obj + 
  #   geom_line(
  #     data      = subset(df, abs(Er - Er_seq[1]) > 1e-6 & abs(Er - Er_seq[2]) > 1e-6),
  #     aes(x     = Freq/1e9,
  #         y     = get_mag(S21, "dB"),
  #         group = Er),
  #     linetype  = "dashed",
  #     color = "lightgrey",
  #     linewidth = 1)
  
  if(dataset == "ADS-MoM"){
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df, abs(Er - Er_seq[1]) < 1e-6),
        aes(x     = Freq/1e9,
            y     = get_mag(S21, "dB"),
            group = Er),
        linetype  = "solid",
        color = "lightpink",
        linewidth = 1)
    
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df, abs(Er - Er_seq[2]) < 1e-6),
        aes(x     = Freq/1e9,
            y     = get_mag(S21, "dB"),
            group = Er),
        linetype  = "solid",
        color = "darkred",
        linewidth = 1)
  }
  else if(dataset == "ADS-FEM"){
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df, abs(Er - Er_seq[1]) < 1e-6),
        aes(x     = Freq/1e9,
            y     = get_mag(S21, "dB"),
            group = Er),
        linetype  = "solid",
        color = "#D95319",
        linewidth = 1)
    
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data      = subset(df, abs(Er - Er_seq[2]) < 1e-6),
        aes(x     = Freq/1e9,
            y     = get_mag(S21, "dB"),
            group = Er),
        linetype  = "solid",
        color = "#0072BD",
        linewidth = 1)
  }
  
  
  
  
  
  return(ggplot_obj)

}
add_MSMag_line     <- function(ggplot_obj, df, dataset, freqs=c(5)) {
  if(is.null(df)){
    return(ggplot_obj)
  }
  if      (dataset == "Freq")   {
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data          = subset(df),
        aes(x         = Freq/1e9,
            y         = get_mag(S21, "dB"),
            color     = index,
            linetype  = "S21",
            group     = index),
        linewidth     = 1)
  }
  else if (dataset == "index")  {
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data          = subset(df, sapply(Freq/1e9, function(f) any(abs(f - freqs) < 1e-6))),
        aes(x         = index,
            y         = get_mag(S21, "dB"),
            color     = Freq,
            linetype  = "S21",
            group     = Freq),
        linewidth     = 1)
  }
  else if (dataset == "relTime"){
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data          = subset(df, sapply(Freq/1e9, function(f) any(abs(f - freqs) < 1e-6))),
        aes(x         = relTime,
            y         = get_mag(S21, "dB"),
            color     = Freq,
            linetype  = "S21",
            group     = Freq),
        linewidth     = 1)
  } 
  else                          {
    
  }
  
  
  
  return(ggplot_obj)
}
add_SPhase_line    <- function(ggplot_obj, df, dataset)             {
  if(hasName(df, "unwrappedPhase")){
    ggplot_obj <- ggplot_obj +
      geom_line(
        data      = subset(df),
        aes(x     = Freq/1e9,
            y     = unwrappedPhase,
            color = dataset,
            linetype  = "S21"),
        linewidth = 1)
  } else {
    ggplot_obj <- ggplot_obj +
      geom_line(
        data      = subset(df),
        aes(x     = Freq/1e9,
            y     = get_phase(S21, "unwrapped", "absolute", "degrees"),
            color = dataset,
            linetype  = "S21"),
        linewidth = 1)
  }
  return(ggplot_obj)
}
add_MUT_Er_line    <- function(ggplot_obj, df, linetype, simulator) {
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, df$Freq == 5e9),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = linetype,
          linetype  = simulator),
      linewidth = 1)
  
  return(ggplot_obj)
}
add_RHT_line       <- function(ggplot_obj, df, dataset, RH_, T_)    {
  if(is.null(df)){
    return(ggplot_obj)
  }
  if      (dataset == "Relative-Humidity") {
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data          = subset(df),
        aes(x         = relTime,
            y         = RH,
            color     = dataset),
        linewidth = 1,
      )
    return(ggplot_obj)
  } 
  else if (dataset == "Temperature")       {
    scale_factor <- diff(RH_) / diff(T_)
    offset       <- RH_[1] - T_[1] * scale_factor
    
    ggplot_obj <- ggplot_obj + 
      geom_line(
        data          = subset(df),
        aes(x         = relTime,
            y         = T * scale_factor + offset,
            color     = dataset),
        linewidth = 1,
      )
    return(ggplot_obj)
  } 
  else                                     {
    # Do nothing
    return(ggplot_obj)
  }
}
add_STIM_line      <- function(ggplot_obj, df, dataset)             {
  if(is.null(df)){
    return(ggplot_obj)
  }
  # ggplot_obj <- ggplot_obj + 
  #   geom_line(
  #     data      = subset(df),
  #     aes(x     = relTime,
  #         y     = MFC1,
  #         color = "MFC1"),
  #     linetype  = "solid",
  #     linewidth = 1,
  #   )
  
  # ggplot_obj <- ggplot_obj + 
  #   geom_line(
  #     data      = subset(df),
  #     aes(x     = relTime,
  #         y     = MFC3,
  #         color = "MFC3"),
  #     linetype  = "solid",
  #     linewidth = 1)
  
  ggplot_obj <- ggplot_obj +
    geom_line(
      data      = subset(df),
      aes(x     = relTime,
          y     = RS,
          color = "MFC1"),
      linetype  = "solid",
      linewidth = 1,
    )
  return(ggplot_obj)
}
add_Z_line         <- function(ggplot_obj, df, linetype, simulator) {
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, Z==30),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = as.factor(Z),
          linetype  = simulator),
      linewidth = 1)
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, Z==40),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = as.factor(Z),
          linetype  = simulator),
      linewidth = 1)
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, Z==50),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = as.factor(Z),
          linetype  = simulator),
      linewidth = 1)
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, Z==60),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = as.factor(Z),
          linetype  = simulator),
      linewidth = 1)
  ggplot_obj <- ggplot_obj +
    geom_line(
      data          = subset(df, Z==70),
      aes(x         = Er,
          y         = get_phase(S21, "unwrapped", "relative", "degrees"),
          color     = as.factor(Z),
          linetype  = simulator),
      linewidth = 1)
  return(ggplot_obj)
}

add_MPts_line      <- function(ggplot_obj, df, dataset, freqs=c(5)) {
  if(is.null(df)){
    return(ggplot_obj)
  }

  p <- findpeaks(-get_mag(subset(df, relTime == 0)$S21, "dB"),
                 minpeakdistance = 10,
                 threshold = 0.1)
  
  v_idx <- sort(p[,2])
  v <- df[v_idx, ]
  v_p <- df[v_idx+1,]
  
  ggplot_obj <- ggplot_obj + 
    geom_line(
      data          = subset(df),
      aes(x         = Freq/1e9,
          y         = get_mag(S21, "dB"),
          color     = index,
          linetype  = "S21",
          group     = index),
      linewidth     = 1)

  
  ggplot_obj <- ggplot_obj +
    geom_point(
      data          = subset(v),
      aes(x         = Freq/1e9, 
          y         = get_mag(S21, "dB")),
          color     = "red",
          size      = 2)
  
  ggplot_obj <- ggplot_obj +
    geom_point(
      data          = subset(v_p),
      aes(x         = Freq/1e9, 
          y         = get_mag(S21, "dB")),
      color     = "green",
      size      = 2)

  return(ggplot_obj)
}
add_Sens_line      <- function(ggplot_obj, df, dataset, freqs=c(5)) {
  if(is.null(df)){
    return(ggplot_obj)
  }
  
  ggplot_obj <- ggplot_obj + 
    geom_line(
      data          = subset(df, sapply(Freq/1e9, function(f) any(abs(f - freqs) < 1e-6))),
      aes(x         = relTime,
          y         = get_mag(S21, "dB"),
          color     = Freq,
          linetype  = "S21",
          group     = Freq),
      linewidth     = 1)
  
  return(ggplot_obj)
}




config_We_plot     <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Transmission Line Topology",
                       values = c("Microstrip"         = "red",
                                  "Stripline"          = "blue",
                                  "Coplanar Waveguide" = "green")) + 
    
    labs(
      title = "Electric Energy (We) stored in Dielectric Material",
      x     = "Integration Plane Z Coordinate [mm]",
      y     = "Plane Energy [J]"
    ) + 
    
    scale_x_continuous(minor_breaks = c(seq(0, min(xlim), by = -minor_break), 
                                        seq(0, max(xlim), by =  minor_break)),
                       breaks       = c(seq(0, min(xlim), by = -major_break), 
                                        seq(0, max(xlim), by =  major_break))) +
    
    scale_y_log10(
      limits = c(min(ylim), max(ylim)),
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x)),
      minor_breaks = NULL  # or define your own
    ) + 
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 14), 
          legend.text      = element_text(size = 12),
          legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_CumWe_plot  <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Transmission Line Topology",
                       values = c("Microstrip"         = "red",
                                  "Stripline"          = "blue",
                                  "Coplanar Waveguide" = "green")) + 
    
    labs(
      title = "Cumulative Electric Energy (We) stored in Dielectric Material",
      x     = "Integration Plane Z Coordinate [mm]",
      y     = "Volume Energy [J]"
    ) + 
    
    scale_x_continuous(minor_breaks = c(seq(0, min(xlim), by = -minor_break), 
                                        seq(0, max(xlim), by =  minor_break)),
                       breaks       = c(seq(0, min(xlim), by = -major_break), 
                                        seq(0, max(xlim), by =  major_break))) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                       breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 16),
          axis.text        = element_text(size = 14),
          legend.title     = element_text(size = 16), 
          legend.text      = element_text(size = 14),
          legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_SMag_plot   <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Datasource",
                       values = c("ADS-MoM" = "#D95319",
                                  "ADS-FEM" = "#0072BD",
                                  "MEAS"    = "black")) + 
    
    scale_linetype_manual(name   = "S-Parameter",
                          values = c("S11" = "solid",
                                     "S21" = "dashed")) + 
    
    labs(
      title = title,
      x     = "",
      y     = "Magnitude [dB]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 0.1),
                       breaks       = seq(min(xlim), max(xlim), by = 0.5)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                       breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/5)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title       = element_text(size = 16),
          axis.text        = element_text(size = 16),
          legend.title     = element_text(size = 16), 
          legend.text      = element_text(size = 14),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_SMag_Er_plot<- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    labs(
      title = title,
      x     = "",
      y     = "Magnitude [dB]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/20),
                       breaks       = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/10)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                       breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/5)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title       = element_text(size = 16),
          axis.text        = element_text(size = 16),
          legend.title     = element_text(size = 16), 
          legend.text      = element_text(size = 14),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_MSMag_plot  <- function(ggplot_obj, xlim, ylim, title="", m){
  if      (m == "Freq")   {
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Frequency [GHz]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/50),
                         breaks       = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/10)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/50),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else if (m == "index")  {
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Observation [index]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 1),
                         breaks       = seq(min(xlim), max(xlim), by = 10)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/50),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else if (m == "relTime"){
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Relative Time [s]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 600),
                         breaks       = seq(min(xlim), max(xlim), by = 1800)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else                    {
    
  }
  
}
config_SPhase_plot <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Datasource",
                       values = c("ADS-MoM" = "#D95319",
                                  "ADS-FEM" = "#0072BD",
                                  "MEAS"    = "black")) + 
    
    scale_linetype_manual(name   = "S-Parameter",
                       values = c("S11" = "solid",
                                  "S21" = "dashed")) +
    
    labs(
      title = title,
      x     = "Frequency [GHz]",
      y     = "Phase [°]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 0.1),
                       breaks       = seq(min(xlim), max(xlim), by = 0.5)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                       breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.title       = element_text(size = 16),
          axis.text        = element_text(size = 16),
          legend.title     = element_text(size = 16), 
          legend.text      = element_text(size = 14),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_MUT_Er_plot <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_linetype_manual(name   = "Simulator",
                       values = c("ADS-MoM"    = "dotted",
                                  "ADS-FEM"    = "dashed",
                                  "COMSOL-EWM" = "dotdash",
                                  "MEAS"       = "solid")) +
    
    scale_color_manual(name   = "Line Topology",
                          values = c("MSL"     = "orange",
                                     "STRL"    = "blue",
                                     "CPWG"    = "darkgreen")) +

    labs(
      title = title,
      x     = "Relative MUT Permittivity [ ]",
      y     = "Normalized Relative Phase [°]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 0.001),
                       breaks       = seq(min(xlim), max(xlim), by = 0.01)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = 0.05),
                       breaks       = seq(min(ylim), max(ylim), by = 0.1)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 12), 
          legend.text      = element_text(size = 10),
          #legend.position  = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_Z_plot      <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_linetype_manual(name   = "Simulator",
                          values = c("ADS-MoM"    = "dotted",
                                     "ADS-FEM"    = "dashed",
                                     "COMSOL-EWM" = "dotdash",
                                     "MEAS"       = "solid")) +
    
    # scale_color_manual(name   = "Line Topology",
    #                    values = c("Microstrip"           = "red",
    #                               "Stripline"            = "blue",
    #                               "Coplanar G Waveguide" = "green")) +
      
    scale_color_manual(
      name = "Z [Ω]",
      values = c(
        "30" = "blue",
        "40" = "green",
        "50" = "cyan",
        "60" = "orange",
        "70" = "red"
      ))+
    
    
    labs(
      title = title,
      x     = "MUT Relative Permittivity [ ]",
      y     = "Phase [°]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 0.001),
                       breaks       = seq(min(xlim), max(xlim), by = 0.01)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = 0.05),
                       breaks       = seq(min(ylim), max(ylim), by = 0.1)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 12), 
          legend.text      = element_text(size = 10),
          #legend.position  = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}

config_RHT_plot    <- function(ggplot_obj, xlim, ylim, ylim2=NULL, title="")   {
  if(length(ggplot_obj$layers)==0){
    return(ggplot_obj)
  }
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Datasource",
                       values = c("Relative-Humidity" = "black",
                                  "Temperature" = "red")) + 
    
    labs(
      title = title,
      x     = "Relative Time [s]",
      y     = "Relative Humidity [%]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 600),
                       breaks       = seq(min(xlim), max(xlim), by = 1800)) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 12), 
          legend.text      = element_text(size = 10),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
  
  if (is.null(ylim2)) {
    # Single Y-axis
    ggplot_obj <- ggplot_obj + 
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = max(ylim)/20),
                         breaks       = seq(min(ylim), max(ylim), by = max(ylim)/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim)
  } 
  else                {
    # Dual Y-axis with secondary axis transformation
    # We'll assume temperature values were scaled before plotting to align visually with RH scale
    scale_factor <- diff(ylim) / diff(ylim2)
    offset <- ylim[1] - ylim2[1] * scale_factor
    
    
    
    ggplot_obj   <- ggplot_obj +
    
      coord_cartesian(xlim = xlim, ylim = ylim) +
        
      scale_y_continuous(
        name         = "Relative Humidity [%]",
        limits       = ylim,
        minor_breaks = seq(min(ylim), max(ylim), by = max(ylim)/20),
        breaks       = seq(min(ylim), max(ylim), by = max(ylim)/10),
        sec.axis     = sec_axis(~ (. - offset) / scale_factor,
                                name = "Temperature [°C]")
      )
  }
  
  return(ggplot_obj)
  
}
config_T_plot      <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Datasource",
                       values = c("AHT-10" = "black")) + 
    
    labs(
      title = title,
      x     = "Relative Time [s]",
      y     = "Temperature [°C]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 300),
                       breaks       = seq(min(xlim), max(xlim), by = 600)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = 0.5),
                       breaks       = seq(min(ylim), max(ylim), by = 2)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 12), 
          legend.text      = element_text(size = 10),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}
config_STIM_plot   <- function(ggplot_obj, xlim, ylim, title="")   {
  
  ggplot_obj <- ggplot_obj + 
    
    scale_color_manual(name   = "Datasource",
                       values = c("MFC1" = "black",
                                  "MFC2" = "black",
                                  "MFC3" = "blue",
                                  "MFC4" = "red")) + 
    
    labs(
      title = title,
      x     = "Relative Time [s]",
      y     = "Gas Flow [ml/min]"
    ) + 
    
    scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 600),
                       breaks       = seq(min(xlim), max(xlim), by = 1800)) +
    
    scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                       breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
    
    coord_cartesian(xlim = xlim, ylim = ylim) +
    
    theme(panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.ontop      = FALSE,
          plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title       = element_text(size = 14),
          axis.text        = element_text(size = 12),
          legend.title     = element_text(size = 12), 
          legend.text      = element_text(size = 10),
          #legend.position = c(0.95, 0.95),  
          legend.justification = c(1, 1))
}

config_Sens_plot  <- function(ggplot_obj, xlim, ylim, title="", m){
  if      (m == "Freq")   {
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Frequency [GHz]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/50),
                         breaks       = seq(min(xlim), max(xlim), by = (max(xlim)-min(xlim))/10)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/50),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else if (m == "index")  {
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Observation [index]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 1),
                         breaks       = seq(min(xlim), max(xlim), by = 10)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/50),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else if (m == "relTime"){
    ggplot_obj <- ggplot_obj + 
      
      scale_linetype_manual(name   = "S-Parameter",
                            values = c("S11" = "solid",
                                       "S21" = "solid")) + 
      
      labs(
        title = title,
        x     = "Relative Time [s]",
        y     = "Magnitude [dB]"
      ) + 
      
      scale_x_continuous(minor_breaks = seq(min(xlim), max(xlim), by = 600),
                         breaks       = seq(min(xlim), max(xlim), by = 1800)) +
      
      scale_y_continuous(minor_breaks = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/20),
                         breaks       = seq(min(ylim), max(ylim), by = (max(ylim)-min(ylim))/10)) +
      
      coord_cartesian(xlim = xlim, ylim = ylim) +
      
      theme(panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            panel.grid.minor = element_line(colour = "grey95"),
            panel.ontop      = FALSE,
            plot.title       = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title       = element_text(size = 14),
            axis.text        = element_text(size = 12),
            legend.title     = element_text(size = 12), 
            legend.text      = element_text(size = 10),
            #legend.position = c(0.95, 0.95),  
            legend.justification = c(1, 1))
  }
  else                    {
    
  }
  
}

#####################################@
## ---- Experiment Functions ----
get_experiment_data<- function(current_path, experiment_path, meas_col, RH_range, temp_range, meas_range, meas_freqs){
  ## Get Experiment path
  datapath <- paste(current_path, experiment_path, sep = "/")
  
  ExperimentGlobalPath <- ""
  
  if(exists("ExperimentPath", envir = .GlobalEnv)){
    ExperimentGlobalPath <- get("ExperimentPath", envir = .GlobalEnv)
  }
  
  ## Check if the experiment is already loaded into memory
  if( is.null(ExperimentGlobalPath) | ExperimentGlobalPath != datapath){
    message("New experiment being loaded into memory!")
    # Clean core variables
    sensor_data   <- data.frame()
    stimulus_data <- data.frame()
    mut_data      <- data.frame()

    ## Get filenames from sensor, stimulus and mut datasets
    filename_list <- get_filenames       (datapath)

    # ## Get data from stimulus, commercial sensor & from MUT
    sensor_data   <- get_dataSet         (datapath, filename_list[[1]], colnames=AHT_col)
    sensor_data   <- process_RH_Data     (sensor_data)
    stimulus_data <- get_dataSet         (datapath, filename_list[[2]], colnames=STIM_col)
    stimulus_data <- process_STIM_Data   (stimulus_data)
    mut_data      <- get_multiple_dataSet(datapath, filename_list[[3]], colnames=meas_col, ",", 8)

    ## Get starting/ending time for data frames
    start_time    <- get_start_time (list(sensor_data, stimulus_data, mut_data))
    end_time      <- get_end_time   (list(sensor_data, stimulus_data, mut_data))

    ## Align dataframes in absolute time
    sensor_data   <- cut_df(sensor_data  , start_time, end_time)
    stimulus_data <- cut_df(stimulus_data, start_time, end_time)
    mut_data      <- cut_df(mut_data     , start_time, end_time)

    assign("sensor_data",   sensor_data,   envir = .GlobalEnv)
    assign("stimulus_data", stimulus_data, envir = .GlobalEnv)
    assign("mut_data",      mut_data,      envir = .GlobalEnv)
    assign("ExperimentPath",datapath,      envir = .GlobalEnv)
  } else {
    message("This experiment is already loaded into memory. Skipping data gathering fase!")
    sensor_data   <- get("sensor_data",   envir = .GlobalEnv)
    stimulus_data <- get("stimulus_data", envir = .GlobalEnv)
    mut_data      <- get("mut_data",      envir = .GlobalEnv)
  }
  ## Calculate vector count
  end_rel_time  <- get_endRel_time(list(sensor_data, stimulus_data, mut_data))
  T_range <- c(0, end_rel_time)

  ## Plot Humidity & Temperature data
  Fig_RHT  <- ggplot()
  Fig_RHT  <- add_RHT_line     (Fig_RHT , sensor_data, "Relative-Humidity", RH_range, temp_range)
  Fig_RHT  <- add_RHT_line     (Fig_RHT , sensor_data, "Temperature", RH_range, temp_range)
  Fig_RHT  <- config_RHT_plot  (Fig_RHT , T_range, RH_range, temp_range, "AHT-10 Comercial Sensor Humidity/Temperature Data")

  ## Plot stimulus data
  Fig_STIM <- ggplot()
  Fig_STIM <- add_STIM_line    (Fig_STIM, stimulus_data, "Stimulus")
  Fig_STIM <- config_STIM_plot (Fig_STIM, T_range, RH_range, "Gas Injection Stimulus")

  ## Plot Measured data
  Fig_TMAG <- ggplot()
  Fig_TMAG <- add_MSMag_line   (Fig_TMAG, mut_data, "relTime", meas_freqs)
  Fig_TMAG <- config_MSMag_plot(Fig_TMAG, T_range, meas_range, "Microstrip Meander Line Measurements", "relTime")

  Fig_SMag <- ggplot()
  Fig_SMag <- add_MSMag_line   (Fig_SMag, mut_data, "Freq")
  Fig_SMag <- config_MSMag_plot(Fig_SMag, c(3, 7), c(-20, 0), "Microstrip Meander Line Measurements", "Freq")
  
  
  
  
  Fig_MPts <- ggplot()
  Fig_MPts <- add_MPts_line    (Fig_MPts, mut_data, "Freq")
  #Fig_MPts <- config_MPts_plot (Fig_MPts, c(3, 7), c(-20, 0), "Microstrip Meander Line Measurements", "Freq")
  
  fanalyze <- mut_data[v_idx+1, ]$Freq/1e9
  
  Fig_Sens <- ggplot()
  Fig_Sens <- add_Sens_line    (Fig_Sens, mut_data, "relTime", fanalyze)
  Fig_Sens <- config_Sens_plot (Fig_Sens, T_range, meas_range, "Microstrip Meander Line Measurements", "relTime")
  
  assign("fanalyze",   fanalyze,   envir = .GlobalEnv)
  
  
  #print(Fig_SMag)
  print(Fig_MPts / Fig_Sens)
  #print(Fig_STIM / Fig_RHT / Fig_TMAG)
  
}
#####################################@
#####################################@