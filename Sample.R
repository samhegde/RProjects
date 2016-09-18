# Additional problems
# ======
# ======
# Read the HVAC file and the Building file which contains the information about the HVAC systems of offices in different countries 
# There are different brands of HVAC systems. 
# 1. Merge the files to get a data frame of all the columns.
# 2. Find the temperature difference between actual and target temperature of all readings.
# 3. Categorize the temperature as HOT , COLD and NORMAL (use +5 , -5 degrees as the differentiating criteria)
# 4. Categorize the temperature as Exterme and Non Exterme depending on whether the temperature is Hot/Cold and Normal respectively.
# 5. Create an interactive graph to get the count of Cold, Hot and Normal working HAVC systems of different brand.
# 6. Create an interactive graph to check which HVAC system is best among all 
# 7. Create an interactive graph to get the count of Cold, Hot and Normal working HAVC systems in different countries.

library(lubridate)
library(ggvis)
library(sqldf)
library(dplyr)

options(sqldf.driver = "SQLite")

#Read HVAC data
url1 <- "https://dl.dropboxusercontent.com/u/69272712/HVAC.csv"
hvac <- read.csv(url1, header=T, sep=',');
head(hvac)

#Read Building Data
url2 <- "https://dl.dropboxusercontent.com/u/69272712/building.csv"
b <- read.csv(url2, header=T, sep=',');
head(b)

#Merge the File
merge <- sqldf("select Date,Time,TargetTemp,ActualTemp,System,SystemAge,hvac.BuildingID,BuildingMgr,BuildingAge,HVACproduct,Country from hvac,b where hvac.BuildingID=b.BuildingID;")
head(merge)

#Find Extreme temperatures
merge$TempDiff <- merge$TargetTemp - merge$ActualTemp
head(merge)

#Mark Temperature Range
merge$TempRange <- 'NORMAL'
merge$TempRange[merge$TempDiff > 5] <- 'COLD'
merge$TempRange[merge$TempDiff < -5] <- 'HOT'
head(merge)


#Mark the HOT and COLD as EXTREME
merge$ExtremeTemp <- 'Non Extreme'
merge$ExtremeTemp [merge$TempRange == 'HOT'| merge$TempRange =='COLD'] <- 'Extreme'
head(merge)

merge$Brand <-   as.factor(merge$HVACproduct)

#create an interative graph to get the count of Cot, Hot and Normal working 
#HAVC systems of different brand.
select_range <- input_select(label='Range',c("COLD", "HOT", "NORMAL")) 
merge %>% ggvis(~HVACproduct) %>% filter(TempRange == eval(select_range)) %>%layer_bars(fill:='blue')
%>% add_axis('x',title='HVAC Product') %>% add_axis('y',title='Count')

#Create an interactive graph to check which HVAC system is best among all.
select_range1 <- input_select(label='Status',c("Extreme", "Non Extreme")) 
merge %>% ggvis(~HVACproduct) %>% filter(ExtremeTemp == eval(select_range1)) %>%layer_bars(fill:='blue')
%>% add_axis('x',title='HVAC Product') %>% add_axis('y',title='Count')

#create an interative graph to get the count of Cot, Hot and Normal working 
#HAVC systems in different countries.
select_range3 <- input_select(label='Range',c("COLD", "HOT", "NORMAL")) 
merge %>% ggvis(~Country) %>% filter(TempRange == eval(select_range3)) %>%layer_bars(fill:='choclate')
%>% add_axis('x',title='Country') %>% add_axis('y',title='Count')

#Create an interactive graph to check which country office has least defective HVAC system.
select_range2 <- input_select(label='Status',c("Extreme", "Non Extreme")) 
merge %>% ggvis(~Country) %>% filter(ExtremeTemp == eval(select_range2)) %>%layer_bars(fill:='red')
%>% add_axis('x',title='Country') %>% add_axis('y',title='Count')

#====================================================================
# PRIMARY ENERGY CONSUMPTION, BY SOURCE 1993 TO 2012 from Hawaii State 
# Department of Business, Economic Development & Tourism, Strategic Industries
# Division, Energy Planning & Policy Branch, records.(PEC_1993_2012.csv)
#
# 1. Find the Correlation between Total energy consumed and Solar Energy
# 2. Plot the consumption from Solar against Year and provide your observation.
# 3. Plot the consumption from Petroleum against Year and provide your observation.
# 4. Plot the consumption from Coal against Year and provide your observation.


url1 <- "https://dl.dropboxusercontent.com/u/69272712/PEC_1993_2012.csv"
pec <- read.csv(url1, header=T, sep=',');
head(pec)

#1
cor.test(pec$Total,pec$Solar)
#not Significant and weakly correlated. 

#2 
pec %>%  ggvis(~Year,~Solar) %>% 
  layer_lines(stroke:='blue') %>% 
  layer_points(fill:='red',size:=6)%>%
  scale_numeric('y',zero=T) %>% 
  add_axis('x', title = 'Year', format="####") %>% 
  add_axis('y',title='Consumption in mWh', title_offset=50)
#usage has been increasing [ a positive sign :) ]

#3
pec %>%  ggvis(~Year,~Petroleum) %>% 
  layer_lines(stroke:='blue') %>% 
  layer_points(fill:='red',size:=6)%>%
  scale_numeric('y',zero=T) %>% 
  add_axis('x', title = 'Year', format="####") %>% 
  add_axis('y',title='Consumption in mWh', title_offset=50)
#The plot demonstrates that the trend of Petroleum consumption increase from Year1993 to Year1994
#then decreases to 1997, increases a little bit in 1998, then decreases a little bit in 1999
#then increases till 2007, but decreases a lot in 2008,then almost flat with little flutuation

#4
pec %>%  ggvis(~Year,~Coal) %>% 
  layer_lines(stroke:='blue') %>% 
  layer_points(fill:='red',size:=6)%>%
  scale_numeric('y',zero=T) %>% 
  add_axis('x', title = 'Year', format="####") %>% 
  add_axis('y',title='Consumption in mWh', title_offset=50)
# The plot demonstrates that the coal consumption flutuate between 15.5 to 20.5 
# from 1993 to 2012
