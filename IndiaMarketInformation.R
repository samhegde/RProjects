#Indian market information
#*************************
# Version: 1.7 - Added Try catch block for easy use of fetching data from tables.
# Author : SAMHEGDE
#*************************
#Basic console settings to increase the display lines.
options(max.print=999999)

# load packages
library(RCurl)
library(XML)
library(plyr)
library(sqldf)
library(stringr)
options(sqldf.driver = "SQLite")

#--------------------------------------------------
#create an empty data frame with required columns
#--------------------------------------------------
x1 <- data.frame(city=NA,year=NA, population_served=NA,num_village=NA,status=NA,year_regulation=NA,name_of_market_legislation=NA,apmc=NA,num_cs=NA,cs_cap=NA)
head(x1)
#+++++++++++++++++++++End of Section +++++++++++++++++++

#--------------------------------------------------------------------------- 
#Scraping all the HTML links within the main page using the <a href HTML tag
# The result is stored in a vector whose length is j. Each vector element
# is then concatened with the first part of http address within the loop.
#---------------------------------------------------------------------------
url <- "http://agmarknet.nic.in/profile/profile_online/combo10.asp"
html <- paste(readLines(url), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"")
 links <- matched[[1]][, 2]
 head(links)
j<- length(links)
#+++++++++++++++++++++End of Section++++++++++++++++++++++++++++++++++++++++

#--------------------------------------------------------------------------------------
#This is where the loop begins. The loop is needed to get data from individual links.
#Some operations within this loop are nested under a try-catch block for error handling.
#This might considerably put some load on the execution time but nevertheless makes the 
# program more dynamic in nature.
#---------------------------------------------------------------------------------------
for (i in 1:j){

#------Concateneate the address within try-catch block---------------#
result <- try(
link<- paste("http://agmarknet.nic.in/profile/profile_online/",links[i], sep="")
);if(class(result) == "try-error") next;

#---------------parse html---------------------------------
doc <-htmlParse(link)
#doc
#tables <-readHTMLTable(doc)

#------------ Read General Information Heading-------------------------
result <- try(
gih <- readHTMLTable(doc,which=2,trim=T,as.data.frame =T,stringsAsFactors = FALSE)
);if(class(result) == "try-error") next;


#------------  Read the data from gih table ---------------------------
result <- try(
city <- sqldf("select V3 from gih where V1 ='(i)'")
);if(class(result) == "try-error") next;

city<- iconv(city, to='ASCII', sub='')
year <- sqldf("select V3 from gih where V1 ='(v)'")
year<- iconv(year, to='ASCII', sub='')
population_served <- sqldf("select V3 from gih where V1 ='(vi)'")
population_served <- iconv(population_served, to='ASCII', sub='')
num_village <- sqldf("select V3 from gih where V1 ='(vii)'")
num_village<- iconv(num_village, to='ASCII', sub='')
#----------------------------------------------------------------------


#---------------Read Administration heading -----------------------------------------
result <- try(
admin <-readHTMLTable(doc,which=3,trim=T,as.data.frame =T,stringsAsFactors = FALSE)
);if(class(result) == "try-error") next;
#admin

#--------- Read the data from admin table -----------------------------
result <- try(
status <- sqldf("select V3 from admin where V1 ='(i)'")
);if(class(result) == "try-error") next;

status<- iconv(status, to='ASCII', sub='')
year_regulation <- sqldf("select V3 from admin where V1 ='(ii)'")
year_regulation <- iconv(year_regulation, to='ASCII', sub='')
name_of_market_legislation <- sqldf("select V3 from admin where V1 ='(iii)'")
name_of_market_legislation<- iconv(name_of_market_legislation, to='ASCII', sub='')
apmc <- sqldf("select V3 from admin where V1 ='(iv)'")
apmc<- iconv(apmc, to='ASCII', sub='')
#---------------------------------------------------------------------

#-----------Read Market Area heading ---------------------------------------------
result <- try(
ma <-readHTMLTable(doc,which=7,trim=T,as.data.frame =T,stringsAsFactors = FALSE)
);if(class(result) == "try-error") next;
#ma

#---------- Read the data from ma table -----------------------------------
result <- try(
num_cs<- sqldf("select V3 from ma where V1 ='(vi)'")
);if(class(result) == "try-error") next;

num_cs<- iconv(num_cs, to='ASCII', sub='')
cs_cap <- sqldf("select V3 from ma where V1 ='(vii)'")
cs_cap<- iconv(cs_cap, to='ASCII', sub='')
#-----------------------------------------------------------------------

#-------------------------------------------------------------------------
#Create a new row with data obtained from tables above 
# Bind them to the data frame x1
#-----------------------------------------------------------------------
newrow <- c(city,year,population_served,num_village,status,year_regulation,name_of_market_legislation,apmc,num_cs,cs_cap)
x1 <- rbind(x1,newrow)
#head(x1)

#----------------------------------------------------------------------
}

#---------END OF FOR LOOP ------------------------------------------#


#-------------Dump the CSV data into storage-------------------------
write.csv(x1,file="IndiaMarketInfo.csv")
#+++++++++++++++++++++End of Program ++++++++++++++++++++++++++++++++++++++++

# Version control using GIT
