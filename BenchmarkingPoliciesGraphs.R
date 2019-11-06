# BenchmarkingPoliciesGraph.R: This file contains the code to build several of the figures in the paper. 
# The URL for the data used to build figure 1 is at the top of the script. Several iterations were constructed to build Figure 2, 
# where plot #6 in the script is the code for the final version of this plot.

# Copyright (C) 2018-2019 Jonathan Roth, Benjamin Lim, Rishee K. Jain     
# This program is free software: you can redistribute it and/or modify it under the terms of the 
# GNU Affero General Public License as published by the Free Software Foundation, either version 
# 3 of the License, or (at your option) any later version.      
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
# See the GNU Affero General Public License for more details. You should have received a copy of 
# the GNU Affero General Public License along with this program.  If not, see <http://www.gnu.org/licenses/>.



library(ggplot2)
theme_set(theme_classic())
library(stringr)
library(dplyr)
library(tidyr)
library(viridis)
library(reshape2)

# Policies per year by governmental level (https://www.energystar.gov/sites/default/files/tools/Benchmarking%20Programs%20and%20Policies%20Factsheet_10162019_Final.pdf)
Government = c('National','National','National','State','State','State','State','State','State','State','State','State','State','State','State','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','City','County','County','County')
Organization = c('Executive Office', 'US Congress','US Congress','California', 'Connecticut', 'Delaware','Hawaii','Michigan', 'New Jersey', 'New York', 'Oklahoma', 'Oregon', 'Texas', 'Washington', 'Washington','Atlanta, GA','Austin, TX','Berkeley, CA','Boston, MA','Boulder, CO','Cambridge, MA','Chicago, IL','Denver, CO','Denver, CO','Denver, CO','Evanston, IL','Fort Collins, CO','Kansas City, MO','Los Angeles, CA','Minneapolis, MN','New York City, NY','Orlando, FL','Philadelphia, PA','Pittsburgh, PA','Portland, ME','Portland, OR','Reno, NV','Rockville, MD','Roswell, GA','Salt Lake City, UT','Salt Lake City, UT','San Diego, CA','San Francisco, CA','San Jose, CA','Seattle, WA','South Portland, ME','St. Louis, MO','Washington DC','Washington DC','Washington DC','Cook County, IL','Montgomery County, MD','Borough of West Chester, PA')
Year = c(2015,2007,2007,2015,2011,2010,2006,2005,2018,2012,2014,2012,2017,2009,2018,2015,2008,2015,2013,2015,2014,2013,2007,2016,2018,2016,2019,2015,2016,2013,2009,2016,2012,2016,2016,2015,2018,2016,2009,2017,2015,2019,2011,2018,2010,2017,2017,2006,2008,2018,2014,2014,2008)

df = data.frame(Government, Organization, Year)
df$Government = factor(df$Government, levels = c("National","State","County","City"))
df = df[df$Year != 2019,]
df$City = "Other"
df$City[df$Government == "City"] = "City"
df$City = factor(df$City, levels = c("Other","City"))

ggplot(df, aes(Year)) + 
  scale_fill_manual("Government",values = c("grey85",'#2c7bb6'))+
  geom_bar(aes(fill=City), col="transparent", size=.1, alpha = 0.8, width = 0.8) +   # change number of bins
  scale_x_continuous(breaks = pretty(df$Year, n = 7))+
  geom_line(data = EnergyStar4, aes(x=Year, y=scaled)) +
  theme(legend.position = c(0.1,0.9)) +
  scale_y_continuous("Number of Benchmarking Policies",expand=c(0,0),sec.axis = sec_axis(trans =  ~.*(max(EnergyStar4$x)/8/10^9),expression("Newly EnergyStar Certified [Billion "*ft^2*"]",breaks=20)))

ggsave(filename = "/Users/jonathanroth/Documents/Research/City_Benchmarking/EnergyStar001.png", plot = last_plot(), width = 6, height = 4, units = "in")
# EnergyStar2 - ???
EnergyStar <- read_csv("Downloads/labelbuildingregistry.csv", 
                                  col_types = cols(`Property/Plant ID` = col_character(),
                                                   Zip = col_character()))

colnames(EnergyStar)[12] = 'Certification_Years'
colnames(EnergyStar)[11] = 'Number_Years_Certified'
colnames(EnergyStar)[10] = 'Gross_Floor_Area'


years = c(1999:2019)
years = paste(years, sep=", ")
EnergyStar2 = EnergyStar %>% separate('Certification_Years',  years)
colnames(EnergyStar2)[12] = 'First_Year'

ggplot(EnergyStar2, aes(Certification_Years)) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=Government), bins=15, col="black", size=.1) +   # change number of bins
  labs(title="Number of Benchmarking Policies", subtitle="2005-2019") 

# EnergyStar3 - Certifications per year
EnergyStar3 = EnergyStar[grep("1999", EnergyStar[['Certification_Years']]),]
EnergyStar3$Certification_Years = "1999"
for(i in 2:21) { # Grab all buildings that are certified for each year
  EnergyStar_temp = EnergyStar[grep(years[i], EnergyStar[['Certification_Years']]),]
  EnergyStar_temp$Certification_Years = years[i]
  print(EnergyStar_temp$Certification_Years[2])
  EnergyStar3 = rbind(EnergyStar3,EnergyStar_temp)
}
EnergyStar3$Certification_Years = as.numeric(EnergyStar3$Certification_Years)
EnergyStar3$Number_Years_Certified = as.factor(EnergyStar3$Number_Years_Certified)

ggplot(EnergyStar3, aes(Certification_Years)) + 
  scale_fill_brewer(palette = "Spectral") + 
  geom_histogram(aes(fill=Number_Years_Certified), bins=21, col="transparent", size=.1) +   # change number of bins
  labs(title="EnergyStar Certifications", subtitle="1999-2019") 

# EnergyStar4 - Squarefootage certified per year
test = EnergyStar3[,c("Gross_Floor_Area","Certification_Years")]
test[is.na(test)] <- 0
EnergyStar4 = aggregate(test$Gross_Floor_Area, 
                        by=list(Category=test$Certification_Years), 
                        FUN=sum)
EnergyStar4 = EnergyStar4[-c(1:6,21),] # Eliminate 2019 data
EnergyStar4$scaled = EnergyStar4$x/(max(EnergyStar4$x)/8)
colnames(EnergyStar4)[1] = "Year"

ggplot(EnergyStar4, aes(x=Category, y=x)) +
  geom_bar(stat="identity")   # change number of bins
  labs(title="EnergyStar Certifications", subtitle="1999-2019")


#### MSE BY CITY
cbecs_rf_mean_se = mean_se(lowest_cv2_rf)
cbecs_mean_se = mean_se(lowest_cv2)

cbecs_rf_mean_se_type = mean_se(lowest_cv_rf)
cbecs_mean_se_type = mean_se(lowest_cv)

City = c("New York City", "Chicago", "Boston", "San Francisco", "Philadelphia", "Seattle", "Minneapolis", "Washington DC", "Los Angeles", "London", "CBECS", "CBECS_ByCensus", "CBECS_ByType")  
RF = c(0.280,0.245,0.579,0.633,0.500,0.273,0.247,0.263,0.273,0.140,0.591, cbecs_rf_mean_se$y, cbecs_rf_mean_se_type$y)
Lasso = c(0.285,0.228,0.644,0.534,0.478,0.266,0.232,0.234,0.304,0.149,0.591, cbecs_mean_se$y, cbecs_mean_se_type$y)
Results = data.frame(City, RF, Lasso)
Results = melt(Results, id.vars = "City",measure.vars = c("RF","Lasso"))
Results$City = factor(Results$City, levels = c("London", "Chicago", "Minneapolis", "Washington DC", "Seattle", "New York City", "Los Angeles","Philadelphia", "San Francisco", "CBECS", "Boston", "CBECS_ByCensus", "CBECS_ByType"))
Results$sd = NA
Results$sd[Results$City == "CBECS_ByCensus" & Results$variable == "RF"] = cbecs_rf_mean_se$y-cbecs_rf_mean_se$ymin
Results$sd[Results$City == "CBECS_ByCensus" & Results$variable == "Lasso"] = cbecs_mean_se$y-cbecs_mean_se$ymin
Results$sd[Results$City == "CBECS_ByType" & Results$variable == "RF"] = cbecs_rf_mean_se_type$y-cbecs_rf_mean_se_type$ymin
Results$sd[Results$City == "CBECS_ByType" & Results$variable == "Lasso"] = cbecs_mean_se_type$y-cbecs_mean_se_type$ymin

ggplot(Results, aes(x=City, y=value, fill=variable)) +
  scale_fill_manual("",values = c('#869FB6','#8EB6A8'),labels=c("Random Forests", "Lasso")) +
  scale_y_continuous(limits=c(0,1.25), breaks=seq(0,1.5, 0.25), expand = c(0,0))+
  ylab("Mean Square Error (MSE)")+
  geom_bar(stat="identity", position="dodge",alpha=1, color = "grey30",width = 0.8) +
  theme(legend.position = c(0.15,0.935), legend.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.y = element_line(colour = "gray70"), 
        panel.ontop = FALSE) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9), colour = "gray30") # change number of bins

ggsave(filename = "/Users/jonathanroth/Documents/Research/City_Benchmarking/MSE_plot003.png", plot = last_plot(), width = 6, height = 4, units = "in")



