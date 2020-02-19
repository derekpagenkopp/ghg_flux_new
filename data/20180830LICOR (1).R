Sys.setenv(LANG = "en")
library(readxl)
library(rsq)
library(tidyverse)
#Import data from excel and name as GHGoriginal
GHGoriginal <- read_xlsx("C:/Users/pc/documents/data/Copy of 20190830 GHG formatted.xlsx",
          sheet = "LICOR")

#Name the field data as GHGfield
GHGfield <- GHGoriginal[, 1:3]

#Calculate the Concentration
Concentration <- data.frame(GHGfield$Area*31.689-102.46)


#Merge GHGfield with Concentration to GHGfield2
GHGfield2 <- cbind(GHGfield, Concentration)
                   
#Change the Column names
names(GHGfield2) <- c("Chamber", "time", "Area", "Concentration")

#In order to calculate R^2 easier, I seperate the dataframe
AStotal <- filter(GHGfield2, GHGfield2$time == "AS")
Sitetotal <- filter(GHGfield2, GHGfield2$time != "AS")

#Change data from character to numeric
Sitetotal$time <- as.numeric(unlist(Sitetotal$time))
Sitetotal$Area <- as.numeric(Sitetotal$Area)
Sitetotal$Concentration <- as.numeric(unlist(Sitetotal$Concentration))

#Function for Rsqure
Rsqure <- function(x, y) {
  Cor <- cor(x, y)
  R2 <- Cor^2
  return(R2)
}

#Calculate R^2 for each site
NNHA <- filter(Sitetotal, Sitetotal$Chamber == "NNHA") 
NNHA_R2 <- Rsqure(NNHA$time, NNHA$Area)

NNHB <- filter(Sitetotal, Sitetotal$Chamber == "NNHB")
NNHB_R2 <- Rsqure(NNHB$time, NNHB$Area)

NNHC <- filter(Sitetotal, Sitetotal$Chamber == "NNHC")
NNHC_R2 <- Rsqure(NNHC$time, NNHC$Area)

NNLA <- filter(Sitetotal, Sitetotal$Chamber == "NNLA")
NNLA_R2 <- Rsqure(NNLA$time, NNLA$Area)

NNLB <- filter(Sitetotal, Sitetotal$Chamber == "NNLB")
NNLB_R2 <- Rsqure(NNLB$time, NNLB$Area)

NNLC <- filter(Sitetotal, Sitetotal$Chamber == "NNLC")
NNLC_R2 <- Rsqure(NNLC$time, NNLC$Area)

NEHA <- filter(Sitetotal, Sitetotal$Chamber == "NEHA")
NEHA_R2 <- Rsqure(NEHA$time, NEHA$Area)

NEHB <- filter(Sitetotal, Sitetotal$Chamber == "NEHB")
NEHB_R2 <- Rsqure(NEHB$time, NEHB$Area)

NEHC <- filter(Sitetotal, Sitetotal$Chamber == "NEHC")
NEHC_R2 <- Rsqure(NEHC$time, NEHC$Area)

NELA <- filter(Sitetotal, Sitetotal$Chamber == "NELA")
NELA_R2 <- Rsqure(NELA$time, NELA$Area)

NELB <- filter(Sitetotal, Sitetotal$Chamber == "NELB")
NELB_R2 <- Rsqure(NELB$time, NELB$Area)

NELC <- filter(Sitetotal, Sitetotal$Chamber == "NELC")
NELC_R2 <- Rsqure(NELC$time, NELC$Area)

CNHA <- filter(Sitetotal, Sitetotal$Chamber == "CNHA")
CNHA_R2 <- Rsqure(CNHA$time, CNHA$Area)

CNHB <- filter(Sitetotal, Sitetotal$Chamber == "CNHB")
CNHB_R2 <- Rsqure(CNHB$time, CNHB$Area)

CNHC <- filter(Sitetotal, Sitetotal$Chamber == "CNHC")
CNHC_R2 <- Rsqure(CNHC$time, CNHC$Area)

CNLA <- filter(Sitetotal, Sitetotal$Chamber == "CNLA")
CNLA_R2 <- Rsqure(CNLA$time, CNLA$Area)

CNLB <- filter(Sitetotal, Sitetotal$Chamber == "CNLB")
CNLB_R2 <- Rsqure(CNLB$time, CNLB$Area)

CNLC <- filter(Sitetotal, Sitetotal$Chamber == "CNLC")
CNLC_R2 <- Rsqure(CNLC$time, CNLC$Area)

CSHA <- filter(Sitetotal, Sitetotal$Chamber == "CSHA")
CSHA_R2 <- Rsqure(CSHA$time, CSHA$Area)

CSHB <- filter(Sitetotal, Sitetotal$Chamber == "CSHB")
CSHB_R2 <- Rsqure(CSHB$time, CSHB$Area)

CSHC <- filter(Sitetotal, Sitetotal$Chamber == "CSHC")
CSHC_R2 <- Rsqure(CSHC$time, CSHC$Area)

CSLA <- filter(Sitetotal, Sitetotal$Chamber == "CSLA")
CSLA_R2 <- Rsqure(CSLA$time, CSLA$Area)

CSLB <- filter(Sitetotal, Sitetotal$Chamber == "CSLB")
CSLB_R2 <- Rsqure(CSLB$time, CSLB$Area)

CSLC <- filter(Sitetotal, Sitetotal$Chamber == "CSLC")
CSLC_R2 <- Rsqure(CSLC$time, CSLC$Area)

#Subset a new dataframe to lm() calculation
GHGnew_1 <- GHGoriginal[, 8:14]
GHGnew <- na.omit(GHGnew_1)

#Calculation part 1
TotalChamberVolumn <- data.frame(GHGnew$`Chamber Top Volume` + 
                      GHGnew$`Chamber Base Volume`)

Tcorr<- data.frame(GHGnew$`Air T`/GHGnew$`Lab T`)

Ngas<- data.frame((TotalChamberVolumn/22.4)*Tcorr)

#CO2 slope Function
Slope <- function(x, y) {
  CO2slope <- (y - x) / 45 / 1000000
  return(CO2slope)
}

#Calculate CO2 slop for each site
NNHACslope <- Slope(NNHA[1, 4], NNHA[4, 4])
NNHBCslope <- Slope(NNHB[1, 4], NNHB[4, 4])
NNHCCslope <- Slope(NNHC[1, 4], NNHC[4, 4])
NNLACslope <- Slope(NNLA[1, 4], NNLA[4, 4])
NNLBCslope <- Slope(NNLB[1, 4], NNLB[4, 4])
NNLCCslope <- Slope(NNLC[1, 4], NNLC[4, 4])
NEHACslope <- Slope(NEHA[1, 4], NEHA[4, 4])
NEHBCslope <- Slope(NEHB[1, 4], NEHB[4, 4])
NEHCCslope <- Slope(NEHC[1, 4], NEHC[4, 4])
NELACslope <- Slope(NELA[1, 4], NELA[4, 4])
NELBCslope <- Slope(NELB[1, 4], NELA[4, 4])
NELCCslope <- Slope(NELC[1, 4], NELC[4, 4])
CNHACslope <- Slope(CNHA[1, 4], CNHA[4, 4])
CNHBCslope <- Slope(CNHB[1, 4], CNHB[4, 4])
CNHCCslope <- Slope(CNHC[1, 4], CNHC[4, 4])
CNLACslope <- Slope(CNLA[1, 4], CNLA[4, 4])
CNLBCslope <- Slope(CNLB[1, 4], CNLB[4, 4])
CNLCCslope <- Slope(CNLC[1, 4], CNLC[4, 4])
CSHACslope <- Slope(CSHA[1, 4], CSHA[4, 4])
CSHBCslope <- Slope(CSHB[1, 4], CSHB[4, 4])
CSHCCslope <- Slope(CSHC[1, 4], CSHC[4, 4])
CSLACslope <- Slope(CSLA[1, 4], CSLA[4, 4])
CSLBCslope <- Slope(CSLB[1, 4], CSLB[4, 4])
CSLCCslope <- Slope(CSLC[1, 4], CSLC[4, 4])

CO2_Slope <- data.frame(c(NNHACslope, NNHBCslope, NNHCCslope,
                          NNLACslope, NNLBCslope, NNLCCslope,
                          NEHACslope, NEHBCslope, NEHCCslope,
                          NELACslope, NELBCslope, NELCCslope,
                          CNHACslope, CSHBCslope, CSHCCslope,
                          CNLACslope, CNLBCslope, CNLCCslope,
                          CSHACslope, CSHBCslope, CSHCCslope,
                          CSLACslope, CSLBCslope, CSLCCslope))

Cal2 <- cbind(GHGnew, TotalChamberVolumn, Tcorr, Ngas, CO2_Slope)

#Calculation part 2
CO2flux<- CO2_Slope*Ngas*60/0.075625

CO2_flux2 <- CO2flux*44*1000

#Final table
Finaltable <- cbind(Cal2, CO2flux, CO2_flux2)

#Rename Column names
names(Finaltable) <- c("date", 
                       "Chamber",
                       "Air T",
                       "Soil T",
                       "Lab T",
                       "Chamber Top Volume",
                       "Chamber Base Volume",
                       "Total Chamber Volume ",
                       "Tcorr",
                       "Ngas",
                       "CO2 slope",
                       "CO2 flux (mol/m2/min)",
                       "CO2 flux (mg/m2/hour)")

