# CBECS_Analysis.R: This file contains the main analysis for the CBECS dataset. 
# Several iterations of models (lasso and random forests) were constructed on different 
# subsets of data (e.g., by building type, census region).

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

###### EXAMINE CBECS DATASET ###### 

CBECS_2012 <- read_csv("Documents/Research/City_Benchmarking/CBECS_2012.csv")
CBECS_imputed = CBECS_2012
CBECS_imputed[is.na(CBECS_imputed)] <- 0

# CBECS selected features from paper: SQFT, NWKER,WKHRS,ZMFBTU,MONUSE,NGUSED,HDD65,HEATP,CDD65,NWKERC)
# CBECS features that MAY be possible to collect
cbecs_features = c('REGION','CENDIV','PBA','FREESTN','SQFT','SQFTC','WLCNS','RFCNS','RFTILT','BLDSHP','GLSSPC','EQGLSS','NFLOOR','BASEMNT',
                   'FLCEILHT','NELVTR','NESLTR','YRCON','YRCONC','RENOV','RENADD','RENRFF','RENWLL','RENWIN','RENHVC','RENLGT','RENINS','ACT1','ACT2','ACT3',
                   'ACT1PCT','ACT2PCT','ACT3PCT','PBAPLUS','VACANT','CUBE','CUBEC','FDSEAT','LODGRM','COURT','FACIL','FACACT','GOVTYP','OWNTYPE','NOCC','NOCCAT','MONUSE',
                   'OCCUPYP','LODOCCP','OPEN24','WKHRS','WKHRSC','NWKER','NWKERC','HEATP','MAINHT','COOLP','MAINCL','HWRDHT','HWRDCL','WTHTEQ','AMIMETER','ENRGYPLN','CONFSPP',
                   'MEDEQP','RFGWIN','HDD65','CDD65','PUBCLIM','EMCS','WINTYP','POOL')
                  
# Variables to log transform
to_log = c('SQFT','NWKER','HDD65','CDD65')

# Variables to change into factors
to_factor = c('REGION','CENDIV','PBA','FREESTN','SQFTC','WLCNS','RFCNS','RFTILT','BLDSHP','GLSSPC','EQGLSS','YRCONC','RENOV','RENADD','RENRFF',
              'RENWLL','RENWIN','RENHVC','RENLGT','RENINS','ACT1','ACT2','ACT3','PBAPLUS','VACANT','CUBE','CUBEC','COURT','FACIL','FACACT',
              'GOVTYP','OWNTYPE','NOCCAT','WKHRSC','NWKERC','MAINHT','MAINCL','HWRDHT','HWRDCL','WTHTEQ','AMIMETER','ENRGYPLN','MEDEQP','PUBCLIM','EMCS','WINTYP','POOL')

# MFBTU, ELBTU, NGBTU, FKBTU, DHBTU
to_total_BTU = c('MFBTU','ELBTU','NGBTU','FKBTU','DHBTU')

# Prepare data
CBECS_x = CBECS_imputed[,cbecs_features]
CBECS_x$LOG_SQFT = log(CBECS_x$SQFT)
CBECS_x$LOG_NWKER = log(CBECS_x$NWKER)
CBECS_x$LOG_HDD65 = log(CBECS_x$HDD65)
CBECS_x$LOG_CDD65 = log(CBECS_x$CDD65)
CBECS_x[to_factor] = lapply(CBECS_x[to_factor], factor)
CBECS_x[] = lapply(CBECS_x, function(i) if(is.numeric(i)) ifelse(is.infinite(i), 0, i) else i)

CBECS_y = log(rowSums(CBECS_imputed[,to_total_BTU]))
CBECS_y = log(CBECS_imputed$MFBTU)
CBECS_y[is.infinite(CBECS_y)] = 0 
CBECS = cbind(CBECS_x,CBECS_y)

# LASSO
# x_NYC_1hot=model.matrix( ~ .-1, x_NYC)

CBECS_ols1 = cv.glmnet(model.matrix(CBECS_y ~. , CBECS), CBECS$CBECS_y, alpha = 1, type.measure="mse")
coef(CBECS_ols3, s = "lambda.1se")
CBECS_ols2 = cv.glmnet(data.matrix(CBECS_x), data.matrix(CBECS_y), alpha = 1, type.measure="mse") 
CBECS_ols3 = cv.glmnet(model.matrix( ~. -1, CBECS_x), CBECS$CBECS_y, alpha = 1, type.measure="mse")

# BY TYPE:
cbecs_types = as.character(levels(unique(CBECS_x$PBA)))

models = list()
lowest_cv = rep(0,20)

models_rf = list()
lowest_cv_rf = rep(0,20)
for(i in 1:length(cbecs_types)){
  print(i)
  CBECS_x2 = CBECS_x[CBECS_x$PBA == cbecs_types[i],]
  CBECS_y2 = CBECS$CBECS_y[CBECS$PBA == cbecs_types[i]]
  
  #CBECS_ols3_02 = cv.glmnet(model.matrix( ~. -1, CBECS_x2), CBECS_y2, alpha = 1, type.measure="mse")
  #print(CBECS_ols3_02$cvm)
  #lowest_cv[i] = min(CBECS_ols3_02$cvm)
  #models = append(models,CBECS_ols3_02)
  #models[[length(models)+1]] = CBECS_ols3_02
  
  CBECS_rf2_02 <- randomForest(CBECS_y2 ~ AMIMETER + PBAPLUS + PBA + LOG_SQFT + SQFT + WKHRS + SQFTC + LOG_NWKER + RFGWIN + OCCUPYP + FDSEAT + ENRGYPLN +
                              NWKER + OWNTYPE + NELVTR + CENDIV + HWRDCL + NWKERC + EMCS + LOG_HDD65 + HDD65 + WKHRSC + POOL + NFLOOR,
                            data=CBECS_x2, importance=TRUE, na.action=na.omit)
  print(CBECS_rf2_02$mse)
  lowest_cv_rf[i] = min(CBECS_rf2_02$mse)
  models_rf[[length(models_rf)+1]] = CBECS_rf2_02
}

# BY REGION 
cbecs_regions = as.character(levels(unique(CBECS_x$CENDIV)))

models2 = list()
lowest_cv2 = rep(0,9)

models2_rf = list()
lowest_cv2_rf = rep(0,9)
for(i in 1:length(cbecs_regions)){
  print(i)
  CBECS_x2 = CBECS_x[CBECS_x$PBA == cbecs_types[i],]
  CBECS_y2 = CBECS$CBECS_y[CBECS$PBA == cbecs_types[i]]
  
  #CBECS_ols3_02 = cv.glmnet(model.matrix( ~. -1, CBECS_x2), CBECS_y2, alpha = 1, type.measure="mse")
  #print(CBECS_ols3_02$cvm)
  #lowest_cv2[i] = min(CBECS_ols3_02$cvm)
  #models = append(models,CBECS_ols3_02)
  #models2[[length(models2)+1]] = CBECS_ols3_02
  
  CBECS_rf2_03 <- randomForest(CBECS_y2 ~ AMIMETER + PBAPLUS + PBA + LOG_SQFT + SQFT + WKHRS + SQFTC + LOG_NWKER + RFGWIN + OCCUPYP + FDSEAT + ENRGYPLN +
                                 NWKER + OWNTYPE + NELVTR + CENDIV + HWRDCL + NWKERC + EMCS + LOG_HDD65 + HDD65 + WKHRSC + POOL + NFLOOR,
                               data=CBECS_x2, importance=TRUE, na.action=na.omit)
  print(CBECS_rf2_03$mse)
  lowest_cv2_rf[i] = min(CBECS_rf2_03$mse)
  models2_rf[[length(models2_rf)+1]] = CBECS_rf2_03
}


CBECS_x2 = CBECS_x[CBECS_x$PBA == '02',]
CBECS_y2 = CBECS$CBECS_y[CBECS$PBA == '02']

CBECS_ols3_02 = cv.glmnet(model.matrix( ~. -1, CBECS_x2), CBECS_y2, alpha = 1, type.measure="mse")




# RANDOM FORESTS
CBECS_rf1 <- randomForest(CBECS_y ~. , data=CBECS, importance=TRUE, na.action=na.omit)
varImpPlot(CBECS_rf1)

CBECS_rf2 <- randomForest(CBECS_y ~ AMIMETER + PBAPLUS + PBA + LOG_SQFT + SQFT + WKHRS + SQFTC + LOG_NWKER + RFGWIN + OCCUPYP + FDSEAT + ENRGYPLN +
                          NWKER + OWNTYPE + NELVTR + CENDIV + HWRDCL + NWKERC + EMCS + LOG_HDD65 + HDD65 + WKHRSC + POOL + NFLOOR,
                          data=CBECS, importance=TRUE, na.action=na.omit)

# USING "MFBTU" AS THE Y VARIABLE
CBECS_rf3 <- randomForest(CBECS_y ~. , data=CBECS, importance=TRUE, na.action=na.omit)
CBECS_rf4 <- randomForest(CBECS_y ~. , data=CBECS[CBECS$REGION == 1,], importance=TRUE, na.action=na.omit)

final_variables = c('SQFT','NWKER','WKHRS','ZMFBTU','MONUSE','NGUSED','HDD65','HEATP','CDD65','NWKERC')
CBECS_rf5 <- randomForest(CBECS_imputed$MFBTU ~. , data=CBECS_imputed[,final_variables], importance=TRUE, na.action=na.omit)
CBECS_rf6 <- randomForest(CBECS_y ~. , data=CBECS[CBECS$PBA == '02',], importance=TRUE, na.action=na.omit)

CBECS_rf7 <- randomForest(CBECS_y ~ AMIMETER + PBAPLUS + SQFT + WKHRS + SQFTC + LOG_NWKER + RFGWIN + OCCUPYP + FDSEAT + ENRGYPLN +
                            NWKER + OWNTYPE + NELVTR + CENDIV + HWRDCL + NWKERC + EMCS + LOG_HDD65 + HDD65 + WKHRSC + POOL + NFLOOR,
                          data=CBECS, importance=TRUE, na.action=na.omit)


cor(CBECS_rf2$predicted, CBECS$CBECS_y)^2
cor(CBECS_rf2$predicted, CBECS$CBECS_y)^2

