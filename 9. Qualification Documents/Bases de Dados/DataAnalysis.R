##############################@
## ---- INITIALIZATION ----
##############################@
current_path    <- dirname(rstudioapi::getSourceEditorContext()$path)   # Get current path
functions_path  <- paste(current_path,"functions.R", sep="/")           # Get utility functions path

meas_col <- c("Freq","MagS11", "PhaseS11","MagS21", "PhaseS21","MagS12", "PhaseS12","MagS22", "PhaseS22")
meas_col_reduc <- c("Freq","MagS11", "PhaseS11","MagS21", "PhaseS21")
AHT_col  <- c("date","time","T","RH")
ads_col  <- c("Freq", "S11", "S21", "S12", "S22")
ads_col_Er  <- c("Er","Freq", "S11", "S21", "S12", "S22")
STIM_col = c("MFC1","MFC2","MFC3","MFC4","date", "time")
Z_analysis_header = c("W", "Er", "Freq", "S11", "S21")

source(functions_path)                                                  # Source utility functions

## initialize Physical Constants

epsilon_MUT     = 1         # []
epsilon_SUB     = 4.38      # []

##########################################@
## ---- 1. Fase vs. Permissividade MUT ----
## Frequência: 3 GHz ~ 7 GHz (0.01 GHz)
## Permissividade: 1.00 ~ 3.00 (0.1)
##########################################@

# ## Import Data
# # Microstrip Line
# MSL_MUT_COMSOL    <- get_dataSet(current_path, "MSL_MUT_COMSOL_EMW.csv" , c("Er", "Freq", "S11", "S21")              )      # Get COMSOL dataframe
# MSL_MUT_ADS_MOM   <- get_dataSet(current_path, "MSL_MUT_ADS_MOM.csv"    , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS MoM dataframe
# MSL_MUT_ADS_FEM   <- get_dataSet(current_path, "MSL_MUT_ADS_FEM.csv"    , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS FEM dataframe
# MSL_MUT_ADS_MOM   <- process_ADS_Data(MSL_MUT_ADS_MOM)
# MSL_MUT_ADS_FEM   <- process_ADS_Data(MSL_MUT_ADS_FEM)
# print(get_MUT_sensibility(MSL_MUT_COMSOL))
# print(get_MUT_sensibility(MSL_MUT_ADS_MOM))
# print(get_MUT_sensibility(MSL_MUT_ADS_FEM))
# 
# # Coplanar Waveguide
# CPWG_MUT_COMSOL   <- get_dataSet(current_path, "CPWG_MUT_COMSOL_EMW.csv", c("Er", "Freq", "S11", "S21"))                    # Get COMSOL dataframe
# CPWG_MUT_ADS_MOM  <- get_dataSet(current_path, "CPWG_MUT_ADS_MOM.csv"   , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS MoM dataframe
# CPWG_MUT_ADS_FEM  <- get_dataSet(current_path, "CPWG_MUT_ADS_FEM.csv"   , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS FEM dataframe
# CPWG_MUT_ADS_MOM  <- process_ADS_Data(CPWG_MUT_ADS_MOM)
# CPWG_MUT_ADS_FEM  <- process_ADS_Data(CPWG_MUT_ADS_FEM)
# print(get_MUT_sensibility(CPWG_MUT_COMSOL))
# print(get_MUT_sensibility(CPWG_MUT_ADS_MOM))
# print(get_MUT_sensibility(CPWG_MUT_ADS_FEM))
# 
# # Stripline
# STRL_MUT_COMSOL   <- get_dataSet(current_path, "STRL_MUT_COMSOL_EMW.csv", c("Er", "Freq", "S11", "S21"))                    # Get COMSOL dataframe
# STRL_MUT_ADS_MOM  <- get_dataSet(current_path, "STRL_MUT_ADS_MOM.csv"   , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS MoM dataframe
# STRL_MUT_ADS_FEM  <- get_dataSet(current_path, "STRL_MUT_ADS_FEM.csv"   , c("Er", "Freq", "S11", "S21", "S12", "S22"))      # Get ADS FEM dataframe
# STRL_MUT_ADS_MOM  <- process_ADS_Data(STRL_MUT_ADS_MOM)
# STRL_MUT_ADS_FEM  <- process_ADS_Data(STRL_MUT_ADS_FEM)
# print(get_MUT_sensibility(STRL_MUT_COMSOL))
# print(get_MUT_sensibility(STRL_MUT_ADS_MOM))
# print(get_MUT_sensibility(STRL_MUT_ADS_FEM))
# 
# ## Plot Figure
# Fig_MUT_Er <- ggplot()
# 
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, MSL_MUT_COMSOL ,  "MSL" , "COMSOL-EWM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, MSL_MUT_ADS_MOM,  "MSL" , "ADS-MoM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, MSL_MUT_ADS_FEM,  "MSL" , "ADS-FEM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, CPWG_MUT_COMSOL,  "CPWG", "COMSOL-EWM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, CPWG_MUT_ADS_MOM, "CPWG", "ADS-MoM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, CPWG_MUT_ADS_FEM, "CPWG", "ADS-FEM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, STRL_MUT_COMSOL , "STRL", "COMSOL-EWM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, STRL_MUT_ADS_MOM, "STRL", "ADS-MoM")
# Fig_MUT_Er <- add_MUT_Er_line(Fig_MUT_Er, STRL_MUT_ADS_FEM, "STRL", "ADS-FEM")
# 
# Fig_MUT_Er <- config_MUT_Er_plot (Fig_MUT_Er, c(1, 1.1), c(-1, 0), "Relative Normalized Phase Deviation")
# 
# print(Fig_MUT_Er)

## ---- 2. Fase vs. Frequência ----
# Fig2 <- ggplot() +
#   geom_line(data=subset(STRL_MUT_COMSOL, Er == 1), aes(Freq, Phase-Phase[1], group = Er)) + 
#   geom_line(data=subset(STRL_MUT_COMSOL, Er == 2), aes(Freq, Phase-Phase[1], group = Er)) + 
#   geom_line(data=subset(STRL_MUT_COMSOL, Er == 3), aes(Freq, Phase-Phase[1], group = Er))
# 
# print(Fig2)

## ---- 3. Fase vs. Impedância ----

# Import Data
# Microstrip Line
# MSL_ZTL_COMSOL    <- get_dataSet(current_path, "MSL_ZTL_COMSOL_EMW.csv" , Z_analysis_header)      # Get COMSOL dataframe
# MSL_ZTL_COMSOL    <- process_COMSOL_Data(MSL_ZTL_COMSOL)
# 
# 
# # MSL_ZTL_ADS_MOM   <- get_dataSet(current_path, "MSL_ZTL_ADS_MOM.csv"    , Z_analysis_header)      # Get ADS MoM dataframe
# # MSL_ZTL_ADS_FEM   <- get_dataSet(current_path, "MSL_ZTL_ADS_FEM.csv"    , Z_analysis_header)      # Get ADS FEM dataframe
# # 
# # # Coplanar Waveguide
# # CPWG_ZTL_COMSOL   <- get_dataSet(current_path, "CPWG_ZTL_COMSOL_EMW.csv", Z_analysis_header)      # Get COMSOL dataframe
# # CPWG_ZTL_ADS_MOM  <- get_dataSet(current_path, "CPWG_ZTL_ADS_MOM.csv"   , Z_analysis_header)      # Get ADS MoM dataframe
# # CPWG_ZTL_ADS_FEM  <- get_dataSet(current_path, "CPWG_ZTL_ADS_FEM.csv"   , Z_analysis_header)      # Get ADS FEM dataframe
# # 
# # # Stripline
# # STRL_ZTL_COMSOL   <- get_dataSet(current_path, "STRL_ZTL_COMSOL_EMW.csv", Z_analysis_header)      # Get COMSOL dataframe
# # STRL_ZTL_ADS_MOM  <- get_dataSet(current_path, "STRL_ZTL_ADS_MOM.csv"   , Z_analysis_header)      # Get ADS MoM dataframe
# # STRL_ZTL_ADS_FEM  <- get_dataSet(current_path, "STRL_ZTL_ADS_FEM.csv"   , Z_analysis_header)      # Get ADS FEM dataframe
# 
# Fig_Zcomp <- ggplot()
# Fig_Zcomp <- add_Z_line (Fig_Zcomp, MSL_ZTL_COMSOL, "Microstrip", "COMSOL-EWM")
# 
# Fig_Zcomp <- config_Z_plot(Fig_Zcomp, c(1, 1.1), c(-1, 0), "Transmission Line Phase Deviation x Line Impedance")
# 
# print(Fig_Zcomp)

##########################################@
## ---- 4. Fase vs. Volume       ----
## Frequência: 5 GHz
## Permissividade: 1.00
## Plane X: -0.708 mm ~ 10 mm (0.01 mm)
##########################################@

# # Import Data
# # Microstrip Line
# MSL_VOL_COMSOL    <- get_dataSet(current_path, "MSL_VOL_COMSOL_EMW.csv", c("Freq", "PlaneX", "Enorm", "E2norm", "S11", "S21"))      # Get COMSOL dataframe
# MSL_VOL_COMSOL    <- zero_crossing(MSL_VOL_COMSOL, 1e-6)
# 
# # Coplanar Waveguide
# CPWG_VOL_COMSOL   <- get_dataSet(current_path, "CPWG_VOL_COMSOL_EMW.csv", c("Freq", "PlaneX", "Enorm", "E2norm", "S11", "S21"))      # Get COMSOL dataframe
# CPWG_VOL_COMSOL   <- zero_crossing(CPWG_VOL_COMSOL, 1e-6)
# 
# # Stripline
# STRL_VOL_COMSOL   <- get_dataSet(current_path, "STRL_VOL_COMSOL_EMW.csv", c("Freq", "PlaneX", "Enorm", "E2norm", "S11", "S21"))      # Get COMSOL dataframe
# STRL_VOL_COMSOL   <- zero_crossing(STRL_VOL_COMSOL, 1e-6)
# STRL_VOL_COMSOL   <- zero_padding(STRL_VOL_COMSOL, c(-0.708, 10))
# 
# # Plot Figure
# # Fig_We <- ggplot()
# # Fig_We <- add_We_line   (Fig_We, MSL_VOL_COMSOL , epsilon_MUT, epsilon_SUB, "Microstrip")
# # Fig_We <- add_We_line   (Fig_We, STRL_VOL_COMSOL, epsilon_MUT, epsilon_SUB, "Stripline")
# # Fig_We <- add_We_line   (Fig_We, CPWG_VOL_COMSOL, epsilon_MUT, epsilon_SUB, "Coplanar Waveguide")
# # Fig_We <- config_We_plot(Fig_We, c(-0.708, 1.4), c(1e-10, 1e-6))
# 
# # Plot Figure
# Fig_CumWe <- ggplot()
# Fig_CumWe <- add_CumWe_line   (Fig_CumWe, MSL_VOL_COMSOL , epsilon_MUT, epsilon_SUB, "Microstrip")
# Fig_CumWe <- add_CumWe_line   (Fig_CumWe, STRL_VOL_COMSOL, epsilon_MUT, epsilon_SUB, "Stripline")
# Fig_CumWe <- add_CumWe_line   (Fig_CumWe, CPWG_VOL_COMSOL, epsilon_MUT, epsilon_SUB, "Coplanar Waveguide")
# Fig_CumWe <- config_CumWe_plot(Fig_CumWe, c(-0.8, 1), c(0, 3e-11))
# 
# # print(Fig_We)
# print(Fig_CumWe)

## ---- 5. Linha de Transmissão ----
## Frequência: 4-6 GHz
## Permissividade: 1.00
##########################################@

# MSL_Meander_MoM       <- get_dataSet(current_path, "MSL_MEANDER_ADSMoM.csv", ads_col)      # Get ADS dataframe
# MSL_Meander_MoM       <- process_ADS_Data(MSL_Meander_MoM)
# 
# MSL_Meander_FEM       <- get_dataSet(current_path, "MSL_MEANDER_ADSFEM.csv", ads_col)      # Get ADS dataframe
# MSL_Meander_FEM       <- process_ADS_Data(MSL_Meander_FEM)
# 
# MSL_Meander_meas  <- get_dataSet(current_path,"Meandro_3GHZ7GHZ_electricalLength.S2P", meas_col_reduc)      # Get S2P dataframe
# MSL_Meander_meas  <- process_MEAS_Data(MSL_Meander_meas, current_path,"Meandro_3GHZ7GHZ_electricalLength.S2P", EDelay=3.9729e-9, EDelayRef=4e9)
# 
# Fig_S11Mag <- ggplot()
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_Meander_MoM, "ADS-MoM", "S11")
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_Meander_FEM, "ADS-FEM", "S11")
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_Meander_meas,"MEAS"   , "S11")
# Fig_S11Mag <- config_SMag_plot (Fig_S11Mag, c(4, 6), c(-40, 0), "Reflection Parameters Magnitude (S11)")
# 
# 
# Fig_S21Mag <- ggplot()
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_Meander_MoM, "ADS-MoM", "S21")
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_Meander_FEM, "ADS-FEM", "S21")
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_Meander_meas,"MEAS"   , "S21")
# Fig_S21Mag <- config_SMag_plot (Fig_S21Mag, c(4, 6), c(-12, 0), "Transmission Parameters Magnitude (S21)")
# 
# Fig_SPhase <- ggplot()
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_Meander_MoM, "ADS-MoM")
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_Meander_FEM, "ADS-FEM")
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_Meander_meas, "MEAS")
# Fig_SPhase <- config_SPhase_plot (Fig_SPhase, c(4, 6), c(-3000, 0), "Transmission Parameters Phase (S21)")
# 
# print(Fig_S11Mag / Fig_S21Mag / Fig_SPhase)

## ---- 5. Linha de Transmissão + Combinador & Divisor de Wilkinson ----
## Frequência: 4-6 GHz
## Permissividade: 1.00
##########################################@





# MSL_Meander_meas_  <- get_dataSet(current_path,"Meandro_4G8HZ5G2HZ.S2P", meas_col_reduc)      # Get S2P dataframe
# MSL_Meander_meas_  <- process_MEAS_Data(MSL_Meander_meas_, current_path,"Meandro_4G8HZ5G2HZ.S2P")




# 
# Fig_SMag <- add_SMag_line    (Fig_SMag, MSL_Meander_meas_, "MEAS")
# 
# 

# 
# 


# print(Fig_SMag / Fig_SPhase)

# MSL_DK2AM_MoM       <- get_dataSet(current_path, "MSL_DK2AM_ADSMoM.csv", ads_col)      # Get ADS dataframe
# MSL_DK2AM_MoM       <- process_ADS_Data(MSL_DK2AM_MoM)
# 
# MSL_DK2AM_FEM       <- get_dataSet(current_path, "MSL_DK2AM_ADSFEM.csv", ads_col)      # Get ADS dataframe
# MSL_DK2AM_FEM       <- process_ADS_Data(MSL_DK2AM_FEM)
# 
# MSL_Meander_meas  <- get_dataSet(current_path,"2025-03-26_Linha&Divisores_3-7_RH27_T23C.S2P", meas_col)      # Get S2P dataframe
# MSL_Meander_meas  <- process_MEAS_Data(MSL_Meander_meas, current_path,"2025-03-26_Linha&Divisores_3-7_RH27_T23C.S2P")
# 
# Fig_S11Mag <- ggplot()
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_DK2AM_MoM, "ADS-MoM", "S11")
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_DK2AM_FEM, "ADS-FEM", "S11")
# Fig_S11Mag <- add_SMag_line    (Fig_S11Mag, MSL_Meander_meas,"MEAS"   , "S11")
# Fig_S11Mag <- config_SMag_plot (Fig_S11Mag, c(4, 6), c(-40, 0), "Reflection Parameters Magnitude (S11)")
# 
# Fig_S21Mag <- ggplot()
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_DK2AM_MoM, "ADS-MoM", "S21")
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_DK2AM_FEM, "ADS-FEM", "S21")
# Fig_S21Mag <- add_SMag_line    (Fig_S21Mag, MSL_Meander_meas,"MEAS"   , "S21")
# Fig_S21Mag <- config_SMag_plot (Fig_S21Mag, c(4, 6), c(-20, 0), "Transmission Parameters Magnitude (S21)")
# 
# Fig_SPhase <- ggplot()
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_DK2AM_MoM, "ADS-MoM")
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_DK2AM_FEM, "ADS-FEM")
# Fig_SPhase <- add_SPhase_line    (Fig_SPhase, MSL_Meander_meas, "MEAS")
# Fig_SPhase <- config_SPhase_plot (Fig_SPhase, c(4, 6), c(-900, 200), "Transmission Parameters Phase (S21)")
# 
# print(Fig_S11Mag / Fig_S21Mag / Fig_SPhase)

##########################################################################################################################

# MSL_DK2AM_MoM_Er       <- get_dataSet(current_path, "MSL_DK2AM_ADSMoM_Er.csv", ads_col_Er)      # Get ADS dataframe
# MSL_DK2AM_MoM_Er       <- process_ADS_Data(MSL_DK2AM_MoM_Er)
# 
# MSL_DK2AM_FEM_Er       <- get_dataSet(current_path, "MSL_DK2AM_ADSFEM_Er.csv", ads_col_Er)      # Get ADS dataframe
# MSL_DK2AM_FEM_Er       <- process_ADS_Data(MSL_DK2AM_FEM_Er)
# 
# Ftest <- 5.0
# 
# Fig_SMag_Er_1 <- ggplot()
# # Fig_SMag_Er_1 <- add_SMag_Er_line    (Fig_SMag_Er_1, MSL_DK2AM_MoM_Er, "ADS-MoM", c(1.000538, 1.000842717))
# Fig_SMag_Er_1 <- add_SMag_Er_line    (Fig_SMag_Er_1, MSL_DK2AM_FEM_Er, "ADS-FEM", c(1.000538, 1.000842717))
# Fig_SMag_Er_1 <- config_SMag_Er_plot (Fig_SMag_Er_1, c(4.8, 5.2), c(-20, 0), "Dk_MUT = 1.000842 [0% -> 0.1%]")
# print(get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.000538000) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB")-
#       get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.000842717) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB"))
# 
# Fig_SMag_Er_2 <- ggplot()
# # Fig_SMag_Er_2 <- add_SMag_Er_line    (Fig_SMag_Er_2, MSL_DK2AM_MoM_Er, "ADS-MoM", c(1.000538, 1.00359))
# Fig_SMag_Er_2 <- add_SMag_Er_line    (Fig_SMag_Er_2, MSL_DK2AM_FEM_Er, "ADS-FEM", c(1.000538, 1.00359))
# Fig_SMag_Er_2 <- config_SMag_Er_plot (Fig_SMag_Er_2, c(4.8, 5.2), c(-20, 0), "Dk_MUT = 1.003590 [0% -> 1.0%]")
# print(get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.000538000) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB")-
#       get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.003590000) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB"))
# 
# Fig_SMag_Er_3 <- ggplot()
# # Fig_SMag_Er_3 <- add_SMag_Er_line    (Fig_SMag_Er_3, MSL_DK2AM_MoM_Er, "ADS-MoM", c(1.000538, 1.031338708))
# Fig_SMag_Er_3 <- add_SMag_Er_line    (Fig_SMag_Er_3, MSL_DK2AM_FEM_Er, "ADS-FEM", c(1.000538, 1.031338708))
# Fig_SMag_Er_3 <- config_SMag_Er_plot (Fig_SMag_Er_3, c(4.8, 5.2), c(-20, 0), "Dk_MUT = 1.031338 [0% -> 10.0%]")
# print(get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.000538000) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB")-
#       get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.031338708) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB"))
# 
# Fig_SMag_Er_4 <- ggplot()
# # Fig_SMag_Er_4 <- add_SMag_Er_line    (Fig_SMag_Er_4, MSL_DK2AM_MoM_Er, "ADS-MoM", c(1.000538, 1.337622790))
# Fig_SMag_Er_4 <- add_SMag_Er_line    (Fig_SMag_Er_4, MSL_DK2AM_FEM_Er, "ADS-FEM", c(1.000538, 1.337622790))
# Fig_SMag_Er_4 <- config_SMag_Er_plot (Fig_SMag_Er_4, c(4.8, 5.2), c(-20, 0), "Dk_MUT = 1.337622 [0% -> 100.0%]")
# print(get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.000538000) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB")-
#       get_mag(subset(MSL_DK2AM_FEM_Er, abs(Er - 1.337622790) < 1e-6 & abs(Freq/1e9 - Ftest) < 1e-6)$S21, "dB"))
# 
# print(Fig_SMag_Er_1 / Fig_SMag_Er_2 / Fig_SMag_Er_3 / Fig_SMag_Er_4)




## ---- 4. Teste RH 20 %       ----
##         Frequência: 4.8~5.2 GHz
##         Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_1", 
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"), 
#                     RH_range        = c(0  , 100), 
#                     temp_range      = c(20 , 25 ), 
#                     meas_range      = c(0, -15))
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_1", 
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"), 
#                     RH_range        = c(0  , 30), 
#                     temp_range      = c(23 , 24 ), 
#                     meas_range      = c(-12.9, -12.8))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_3",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(20 , 25 ),
#                     meas_range      = c(-20, 0  ))
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_3",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(22 , 23 ),
#                     meas_range      = c(-14, -6  ))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_4",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(20 , 25 ),
#                     meas_range      = c(-7.5, -6),
#                     meas_freqs      = c(4.9, 5.152, 5.200))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_5",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(20 , 25 ),
#                     meas_range      = c(-9.25, -8.75  ))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_6",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(20 , 25 ),
#                     meas_range      = c(-10.5, -9.5  ))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_7",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS12","PhaseS12","MagS21","PhaseS21","MagS22","PhaseS22"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(20 , 25 ),
#                     meas_range      = c(-7.5, -6  ))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_9",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS21","PhaseS21"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(22.5 , 23.5 ),
#                     meas_range      = c(-6.2 , -6.0  ),
#                     meas_freqs      = c(4.97 , 5.02, 5.116))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa: 0~20 %
##########################################@
# get_experiment_data(current_path    = current_path,
#                     experiment_path = "AquisicaoCamaraDeGases/TesteAquisicaoContinua_10",
#                     meas_col        = c("Freq","MagS11","PhaseS11","MagS21","PhaseS21"),
#                     RH_range        = c(0  , 100),
#                     temp_range      = c(22.5 , 23.2 ),
#                     meas_range      = c(-8, -7.5  ))
## ---- ##################################@
## ---- 4. Teste RH 20 %       ----
## Frequência: 4.8~5.2 GHz
## Humidade Relativa ??~? %
##########################################@
get_experiment_data(current_path    = current_path,
                    experiment_path = "AquisicaoCamaraDeGases/ExperimentoCNT_3_29_09_2025",
                    meas_col        = c("Freq","MagS11","PhaseS11","MagS21","PhaseS21"),
                    RH_range        = c(0  , 100),
                    temp_range      = c(22.5 , 23.2),
                    meas_range      = c(-16, -10),
                    meas_freqs      = c(3.28, 3.56, 3.86, 4.16, 4.42, 4.70, 4.98, 5.26, 5.56, 5.84, 6.10, 6.38, 6.68, 6.96))
## ---- ##################################@