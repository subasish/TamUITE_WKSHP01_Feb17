### Indiana Crash Data
### Example Data Cleaning
setwd("/Users/subasishdas1/Desktop/TAMU_ITE_WKSHP1_Feb17/Data")
### list.files()
mydata0 <- read.csv("Indiana_2006_Sample.csv")
### mydata2 <- mydata[sample(nrow(mydata), 10000), ]
dim(mydata0)
table(mydata0$DIRFROMPOINTCDE)
table(mydata0$RDWYCLASSCDE)

In_code <- read.csv("Indiana_Code.csv")
head(In_code)

for (each_column in intersect(colnames(mydata0), In_code$Column)){
    curr_dict = In_code$Column %in% each_column
    code = In_code$Code[curr_dict]
    descr = In_code$Desc[curr_dict]
    mydata0[[each_column]] = descr[match(mydata0[[each_column]], code)]
}


## write.csv(mydata0, "Indiana_2006_Sample_Desc.csv")

mydata2 <- read.csv("Indiana_2006_Sample_Desc.csv")
table(mydata2$STATUSCDE)
table(mydata2$DIRFROMPOINTCDE)
table(mydata2$RDWYCLASSCDE)
table(mydata2$LOCALITYCDE)
table(mydata2$CONSTRUCTTYPECDE)
table(mydata2$LIGHTCONDCDE)
table(mydata2$WEATHERCDE)
table(mydata2$SURFACECONDCDE)
table(mydata2$MEDIANTYPECDE)
table(mydata2$RDWYJUNCTIONCDE)
table(mydata2$RDWYCHARCDE)
table(mydata2$PRIMARYFACTORCDE)
table(mydata2$MANNERCOLLCDE)
table(mydata2$TRAFFICCNTRLCDE)



#### Rename

setwd("/Users/subasishdas1/Desktop/TAMU_ITE_WKSHP1_Feb17/Data")
### list.files()
mydata0 <- read.csv("Indiana_2006_Sample.csv")
names(mydata0)

head(mydata0)

library(stringr)
mydata0$Year <- str_sub(mydata0$COLLDTE,1,4)
mydata0$Month <- str_sub(mydata0$COLLDTE,5,6)
table(mydata0$Year)
table(mydata0$Month)
hist(mydata0$Month)
mydata0$Month <- as.numeric(mydata0$Month)
hist(mydata0$Month)

head(mydata0)

table(mydata0$LOCALITYCDE)
library(reshape2)
library(car)
library(plyr)
mydata0$LOCALITYCDE <- revalue(mydata0$LOCALITYCDE, c("R"="Rural","U"="Urban"))
table(mydata0$LOCALITYCDE)


table(mydata0$CONSTRUCTTYPECDE)
mydata0$CONSTRUCTTYPECDE <- recode(mydata0$CONSTRUCTTYPECDE, "1= 'Lane Closure'; 
2= 'X-Over/Lane Shift'; 3='Work on Shoulder'; 4='Intermittent or Moving Work';")
table(mydata0$CONSTRUCTTYPECDE)


