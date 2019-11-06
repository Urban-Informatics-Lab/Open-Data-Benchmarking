# NYC.R: This file contains the code used for importing, cleaning, and preparing the public data
# for this city. The output of this file is the dataset used for modeling in the Lasso_RandomFores.Rmd script.

# Copyright (C) 2018-2019 Jonathan Roth, Benjamin Lim, Rishee K. Jain     
# This program is free software: you can redistribute it and/or modify it under the terms of the 
# GNU Affero General Public License as published by the Free Software Foundation, either version 
# 3 of the License, or (at your option) any later version.      
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU Affero General Public License for more details. You should have received a copy of 
# the GNU Affero General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.


library('glmnet')
library('mice')
library('rqPen')
library('hqreg')
library('readr')
library(randomForest)

NYC_Energy = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/Energy_and_Water_Data_Disclosure_for_Local_Law__2014_.csv", stringsAsFactors = FALSE)
Pluto_Mn = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/nyc_pluto_15v1/Mn.csv", stringsAsFactors = FALSE)
Pluto_BK = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/nyc_pluto_15v1/BK.csv", stringsAsFactors = FALSE)
Pluto_BX = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/nyc_pluto_15v1/BX.csv", stringsAsFactors = FALSE)
Pluto_QN = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/nyc_pluto_15v1/QN.csv", stringsAsFactors = FALSE)
Pluto_SI = read.csv("/Users/jonathanroth/Documents/Research/NYC dataset/nyc_pluto_15v1/SI.csv", stringsAsFactors = FALSE)

names(NYC_Energy)[names(NYC_Energy)=="NYC.Borough..Block..and.Lot..BBL."] <- "BBL"
names(NYC_Energy)[names(NYC_Energy)=="DOF.Property.Floor.Area..Buildngs.and.Parking..ft2."] <- "DOF_Property_Area"

# Merge datasets from each borough with Energy data set and then combine them all
test_mn = merge(NYC_Energy, Pluto_Mn, by="BBL")
test_bk = merge(NYC_Energy, Pluto_BK, by="BBL")
test_bx = merge(NYC_Energy, Pluto_BX, by="BBL")
test_qn = merge(NYC_Energy, Pluto_QN, by="BBL")
test_si = merge(NYC_Energy, Pluto_SI, by="BBL")
Combined = rbind(test_mn, test_bx, test_bk, test_qn, test_si)

# See that 62 observations were not included in the combined set (these 62 have secondary BBLs reported and contain no information in any column)
setdiff(NYC_Energy$BBL, Combined$BBL)
# Condense building class to one type
for(i in 1:length(Combined$BldgClass)) {
  tempStr = Combined$BldgClass[i]
  Combined$BldgClass[i] = substr(tempStr, 1, 1)
}

# Change to numeric and factor type where necessary
Combined$Source.EUI.kBtu.ft2. = as.numeric(Combined$Source.EUI.kBtu.ft2.)
Combined$Site.EUI.kBtu.ft2. = as.numeric(Combined$Site.EUI.kBtu.ft2.)
Combined$Block = as.numeric(Combined$Block)
Combined$BldgArea = as.numeric(Combined$BldgArea)
Combined$ComArea = as.numeric(Combined$ComArea)
Combined$NumFloors = as.numeric(Combined$NumFloors)
Combined$NumBldgs = as.numeric(Combined$NumBldgs)
Combined$YearBuilt = as.numeric(Combined$YearBuilt)
Combined$YearAlter1 = as.numeric(Combined$YearAlter1)

Combined$Reported.Property.Floor.Area..Building.s....ft.. = as.numeric(Combined$Reported.Property.Floor.Area..Building.s....ft..)
Combined$Primary.Property.Type...Self.Selected = as.factor(Combined$Primary.Property.Type...Self.Selected)
Combined$BldgClass = as.factor(Combined$BldgClass)
Combined$LandUse = as.factor(Combined$LandUse)
Combined$LotArea = as.numeric(Combined$LotArea)
Combined$BuiltFAR = as.numeric(Combined$BuiltFAR)

Combined$DOF_Property_Area = as.numeric(Combined$DOF_Property_Area)
Combined$DOF.Number.of.Buildings = as.numeric(Combined$DOF.Number.of.Buildings)
Combined$LotType = as.factor(Combined$LotType)
Combined$Municipally.Supplied.Potable.Water...Indoor.Intensity..gal.ft.. = as.numeric(Combined$Municipally.Supplied.Potable.Water...Indoor.Intensity..gal.ft..)
Combined$ResArea = as.numeric(Combined$ResArea)
Combined$OfficeArea = as.numeric(Combined$OfficeArea)
Combined$RetailArea = as.numeric(Combined$RetailArea)
Combined$GarageArea = as.numeric(Combined$GarageArea)
Combined$StrgeArea = as.numeric(Combined$StrgeArea)
Combined$FactryArea = as.numeric(Combined$FactryArea)
Combined$OtherArea = as.numeric(Combined$OtherArea)
Combined$UnitsRes = as.numeric(Combined$UnitsRes)
Combined$UnitsTotal = as.numeric(Combined$UnitsTotal)
Combined$YearAlter2 = as.numeric(Combined$YearAlter2)
Combined$Lot = as.numeric(Combined$Lot)
Combined$PolicePrct = as.numeric(Combined$PolicePrct)

###### ADD FEATURES
Combined$kBTU = (Combined$Source.EUI.kBtu.ft2.)*(Combined$DOF_Property_Area)
Combined$logkBTU = log(Combined$kBTU)
Combined$logPropertyArea = log(Combined$DOF_Property_Area)
Combined$logAssessTot = log(Combined$AssessTot)
Combined$logBldgArea = log(Combined$BldgArea)
Combined$logComArea = log(Combined$ComArea)
Combined$logOfficeArea = log(Combined$OfficeArea)
Combined$logResArea = log(Combined$ResArea)
Combined$logGarage = log(Combined$GarageArea)
Combined$logStrge = log(Combined$StrgeArea)
Combined$logFactory = log(Combined$FactryArea)
Combined$logOther = log(Combined$OtherArea)
Combined$logRetail = log(Combined$RetailArea)

Combined$PricePerSqFt = Combined$AssessLand/Combined$DOF_Property_Area
Combined$BuildPricePerSqFt = Combined$AssessTot/Combined$BldgArea

Combined$PercentOffice = Combined$OfficeArea/Combined$BldgArea
Combined$PercentGarage = Combined$GarageArea/Combined$BldgArea
Combined$PercentRes = Combined$ResArea/Combined$BldgArea
Combined$PercentCom = Combined$ComArea/Combined$BldgArea
Combined$PercentStrge = Combined$StrgeArea/Combined$BldgArea
Combined$PercentFactory = Combined$FactryArea/Combined$BldgArea
Combined$PercentOther = Combined$OtherArea/Combined$BldgArea
Combined$PercentRetail = Combined$RetailArea/Combined$BldgArea

# Turn zeros in certain columns to "NA" so that regression tree can make appropriate surrogate splits 
Combined$BldgArea[Combined$BldgArea == 0] = NA
Combined$NumBldgs[Combined$NumBldgs == 0] = NA
Combined$NumFloors[Combined$NumFloors == 0] = NA
Combined$LotFront[Combined$LotFront == 0] = NA
Combined$LotDepth[Combined$LotDepth == 0] = NA
Combined$BldgFront[Combined$BldgFront == 0] = NA
Combined$BldgDepth[Combined$BldgDepth == 0] = NA
Combined$AssessLand[Combined$AssessLand == 0] = NA
Combined$AssessTot[Combined$AssessTot == 0] = NA
Combined$YearBuilt[Combined$YearBuilt == 0] = NA
Combined$Source.EUI.kBtu.ft2.[Combined$Source.EUI.kBtu.ft2. == 0] = NA

Combined$logkBTU[Combined$logkBTU == 0] = NA
Combined$logPropertyArea[Combined$logPropertyArea == 0] = NA
Combined$logAssessTot[Combined$logAssessTot == 0] = NA
Combined$logBldgArea[Combined$logBldgArea == 0] = NA
Combined$logComArea[Combined$logComArea == 0] = NA
Combined$logOfficeArea[Combined$logOfficeArea == 0] = NA
Combined$logResArea[Combined$logResArea == 0] = NA

# Eliminate Source EUI that are too large (unrealistic - probably due to inpur error), create extra column, and new dataframe.
# Combine building classes
Energy = Combined[Combined$Source.EUI.kBtu.ft2. < 1000,]
Energy = Energy[Energy$Source.EUI.kBtu.ft2. > 1,]
Energy = Energy[!is.na(Energy$BBL),]
write.csv("/Users/jonathanroth/Documents/Research/NYC dataset/NYC_Final_Fixed.csv", stringsAsFactors = FALSE)


# Import NYC Dataset
Energy = read_csv("/Users/jonathanroth/Documents/Side Projects/OSM/NYC/NYC_Final.csv", col_types = cols(BBL = col_character()))
Energy = Energy[!is.na(Energy$BBL),]
Energy$kBTU = (Energy$`Site_EUI_(kBtu/ft_)`)*(Energy$BldgArea)

########  RUN MODEL USING LOG TRANSFORMATION AND INCLUDE MORE FEATURES ########
features = c('Largest_Property_Use_Type','Largest_Property_Use_Type_-_Gross_Floor_Area_(ft_)',
             'Year_Built','Number_of_Buildings_-_Self-reported',
             'Occupancy','LandUse','BldgArea','LotArea','ComArea','ResArea',
             'OfficeArea','RetailArea','GarageArea','StrgeArea','FactryArea','OtherArea','Easements','NumBldgs',
             'NumFloors','UnitsRes','UnitsTotal','LotFront','LotDepth','BldgFront','BldgDepth','LotType','BsmtCode',
             'AssessLand','AssessTot','YearBuilt','YearAlter1','YearAlter2','BuiltFAR','ResidFAR','CommFAR','FacilFAR',
             'BldgClass2','logPropertyArea','logAssessTot','logBldgArea','logComArea','logOfficeArea','logResArea',
             'logGarage','logStrge','logFactory','logOther','logRetail','PricePerSqFt','BuildPricePerSqFt','PercentOffice',
             'PercentGarage','PercentRes','PercentCom','PercentStrge','PercentFactory','PercentOther','PercentRetail',
             'BBL','Primary_Property_Type_-_Self_Selected','Property_GFA_-_Self-Reported_(ft_)') 
#### ONLY INCLUDE LAST ROW FOR FEATURES THAT ARE NEEDED IN ML MODEL FOR EDF PROJECT!!!

x2 = Energy[,c(features,'kBTU')] # y = Energy[,c('kBTU')]
x2$logkBTU = log(x2$kBTU)
x2 = x2[!is.na(x2$logkBTU),] # Eliminated 200 obs! Total = 14,283
x2 = x2[!is.infinite(x2$logkBTU),] # Eliminated 42 obs! Total = 14,241

# IMPUTE MISSING DATA
log = c('logPropertyArea','logAssessTot','logBldgArea','logComArea','logOfficeArea','logResArea',
        'logGarage','logStrge','logFactory','logOther','logRetail')
x2[,log][is.na(x2[,log])] = 0 # Do not impute log features -- set NA values to zero

mi = mice(x2[,1:59],m=3,maxit=3,method='cart') # Excluse 'kBTU' and 'logkBTU' !!!
x2[,1:59] = complete(mi)

# PREPARE DATA
y2 = x2[,c('logkBTU')]

x2$Largest_Property_Use_Type = as.factor(as.integer(as.factor(x2$Largest_Property_Use_Type)))
x2$LandUse = as.factor(as.integer(as.factor(x2$LandUse)))
x2$LotType = as.factor(as.integer(as.factor(x2$LotType)))
x2$BsmtCode = as.factor(as.integer(as.factor(x2$BsmtCode)))
x2$BldgClass2 = as.factor(as.integer(as.factor(x2$BldgClass2)))
x2$BldgArea_squared = x2$BldgArea^2

##################################
# SAVE DATA FILE THEN ELIMINATE KBTU BASED FEATURES
write.csv(x2, file = "/Users/jonathanroth/Documents/Side Projects/OSM/NYC/NYC_Imputed.csv")
# x2 = read_csv("/Users/jonathanroth/Documents/Side Projects/OSM/NYC/NYC_Final_Fixed.csv", col_types = cols(BBL = col_character()))
x2 = x2[!is.na(x2$BBL),]

x2 = x2[,c(2:65)]
y = x2$kBTU
y2 = x2['logkBTU']
x2 = x2[,c(1:61,64)]

# ols1 = cv.glmnet(data.matrix(x2), data.matrix(y), alpha = 1, type.measure="mae")
# coef(ols1, s = "lambda.min")
# 
# ols2 = cv.glmnet(data.matrix(x2), data.matrix(y), alpha = 1, type.measure="mse")
# coef(ols2, s = "lambda.min")

##############
# RUN OLS MODELS 
log_ols1 = cv.glmnet(data.matrix(x2), data.matrix(y2), alpha = 1, type.measure="mae")
coef(log_ols1, s = "lambda.1se")

log_ols2 = cv.glmnet(data.matrix(x2), data.matrix(y2), alpha = 1, type.measure="mse")
coef(log_ols2, s = "lambda.1se")

log_ols3 = cv.glmnet(data.matrix(x2), data.matrix(y2), alpha = 1, type.measure="mse")
coef(log_ols3, s = "lambda.1se")

plot(log_ols1)

#######
NYC_Electricity$Date = as.POSIXct(NYC_Electricity$`Service Start Date`,format = '%m/%d/%y')
NYC_Electricity$Year = substr(NYC_Electricity$Date,0,4)
NYC_2016 = NYC_Electricity[NYC_Electricity$Year == '2016',]
