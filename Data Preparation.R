
library(matlib)
library(gam)
library(ape)
library(car)
library(ellipse)
library(faraway)
library(leaps)
library(MASS)
library(GGally)
library(BAS)
options(rgl.debug = TRUE)
library(rgl)
library(corrplot)
library( Matrix )
library(RColorBrewer)
library(geosphere)
library(lctools)
library(lmPerm)
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(mgcv)
library(pbapply)
library(spdep)
library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat)        ## Geostatisticr
library(geoR) 
library(readxl) #Read Excel
library(tidyverse) #String manipulation
library(lubridate) #Date conversion
library(MASS)
library(rgl)
library(DepthProc)
library(hexbin)
library(aplpack)
library(robustbase)
library(MDBED)
library(spatialreg)
library(ggplot2)


########### Dataset loading

df2 <-  read.csv("us_county_sociohealth_data.csv")
zones    <-  st_read("github_albersusa.json")
votes16  <-  read.csv("2016_US_County_Level_Presidential_Results.csv")
votes20  <-  read.csv("2020_US_County_Level_Presidential_Results.csv")
#df_big <-  read.csv("US_counties_COVID19_health_weather_data.csv")
CHR16 <-  read.csv("dati_2016abbb.csv", sep=";")
CDC16 <-  read.csv("SVI_2016_US_county.csv") 



################################################################################
################################################################################
########################    PRELIMINARY OPERATIONS      ########################
################################################################################
################################################################################
colnames(votes16)[11] <- "fips"
colnames(CDC16)[5] <- "fips"
colnames(CHR16)[1] <- "fips"

df2<-df2[,c(1,4,5)]
#colnames(votes20)[2] <- "fips"

df2$fips <- as.numeric(df2$fips)
zones$fips <- as.numeric(zones$fips)
votes16$fips <- as.numeric(votes16$fips)
CDC16$fips <- as.numeric(CDC16$fips)
CHR16$fips <- as.numeric(CHR16$fips)

#votes20$fips <- as.character(votes20$fips)

df2<- df2[order(df2$fips),]
zones<- zones[order(zones$fips),]
votes16 <- votes16[order(votes16$fips),]
CDC16<- CDC16[order(CDC16$fips),]
CHR16<- CHR16[order(CHR16$fips),]

#votes20 <- votes20[order(votes20$fips),]

zones <- zones[-which(zones$state=="Alaska"),]
zones <- zones[-which(zones$state=="Hawaii"),]
#df[which(!(df$fips %in% zones$fips)),c(2,3)]
#zones[which(!(zones$fips %in% df$fips)),c(8,6)]
#df1[-which(),]
# df <- merge(df,votes16[,-c(1,9,10)],by="fips")
# zones<- zones[-which(!(zones$fips %in% df$fips)),]
# 
# df$dem_win <- 0
# df$dem_win[which(df$per_gop >df$per_dem)] <- 1
# df$target_diff <- df$per_gop - df$per_dem



###########################################
#####  MERGING DATASETS  ######################################################
###########################################

#### manual check of some incorrected fips
CDC16[which(!(CDC16$fips %in% CHR16$fips )),c(2,4,5)]
CHR16[which(!(CHR16$fips %in% CDC16$fips )),c(2,3,1)]


#"shannon" became "Oglala Lakota" in 2010, new fips=46102
zones[which(zones$fips == 46113),c(2,5)] <- c(46102, "Oglala Lakota")
CHR16[which(CHR16$fips == 46113),c(1,3)] <- c(46102, "Oglala Lakota")
votes16[which(votes16$fips == 46113),c(11,10)] <- c(46102, "Oglala Lakota")

CHR16[which(CHR16$fips == 46102),1]
CDC16[which(CDC16$fips == 46102),1]
zones[which(zones$fips == 46102),1]
votes16[which(votes16$fips == 46102),1]

#### Merging
df <- merge(CDC16,CHR16,by="fips") 
df <- merge(df,votes16,by="fips") 
df <- merge(df,df2,by="fips")


if (any(!(df$fips %in% zones$fips)))
  df<-df[-which(!(df$fips %in% zones$fips)),]

if (any(!(zones$fips %in% df$fips)))
  zones<- zones[-which(!(zones$fips %in% df$fips)),]

####
df$fips <- as.character(df$fips)
zones$fips <- as.character(zones$fips)
df<- df[order(df$fips),]
zones<- zones[order(zones$fips),]

all(df$fips==zones$fips)


#### removing useless columns
rem_col  <- which(grepl("X95", colnames(df), fixed = TRUE))
rem_col <- c(rem_col,c(2,4,124,125,215,216))

df<- df[,-rem_col]



###########################################
#####  Columns_modification  ######################################################
###########################################
which(!(zones$fips==df$fips))

##### Diff% in the result
df$target_diff <- df$per_dem-df$per_gop

df$dem_win <- 0
df$dem_win[which(df$target_diff>0)] <- 1


##### logit of normalized result
#Calcolo percentuale ignorando voti nulli
true_perc_dem_votes <- (df$votes_dem)/((df$votes_dem)+(df$votes_gop))
true_perc_gop_votes <- 1-true_perc_dem_votes
#Applico logit function alla true_perc_dem_votes per mappare valori su retta reale
df$target_logit <- log(true_perc_dem_votes / (1 - true_perc_dem_votes))

# inverse tranform
# tttt <- exp(df$target_logit)/(1+exp(df$target_logit))
# sum(true_perc_dem_votes-tttt)/3135


##### astensionism%
df$astensionism <- 1 - df$total_votes/(df$E_TOTPOP-df$E_AGE17)

# # Votes for alaska are not by county
# df$astensionism[which(df$STATE=="ALASKA")] = 1 - (246588/sum(df$E_TOTPOP[which(df$STATE=="ALASKA")]
#                                                              -
#                                                              df$E_AGE17[which(df$STATE=="ALASKA")]))
# other errors
df$astensionism[df$astensionism<0] <- mean(df$astensionism)
df$logit_astensionism <- logit(df$astensionism)
##### NA filling
for (j in 1:dim(df)[2]){
  
  if (is.numeric(df[,j]))
    df[which(is.na(df[,j])),j] <- mean(df[,j], na.rm =T)
}



######## ddplot
# df <- df[which(df$dem_win==1),c(155,150,148,130,63,55,37,38,41,135)]
# df2 <- df[which(df$dem_win==0),c(155,150,148,130,63,55,37,38,41,135)]
# 
# x11()
# ddPlot(x = df,y = df2,depth_params = list(method='Mahalanobis'))











########### Dataset loading

df2 <-  read.csv("us_county_sociohealth_data.csv")
#zones    <-  st_read("github_albersusa.json")
#votes16  <-  read.csv("2016_US_County_Level_Presidential_Results.csv")
votes20  <-  read.csv("2020_US_County_Level_Presidential_Results.csv")
#df_big <-  read.csv("US_counties_COVID19_health_weather_data.csv")
#CHR16 <-  read.csv("dati_2016abbb.csv", sep=";")
#CDC16 <-  read.csv("SVI_2016_US_county.csv") 
#2020 Data 
CDC20 <-  read.csv("SVI_2020_US_county.csv") 
#CHR20 <-  read.csv("analytic_data2020_0.csv")
#Delete first row, as it is data description
#CHR20 <- CHR20[-1,]


################################################################################
################################################################################
########################    PRELIMINARY OPERATIONS      ########################
################################################################################
################################################################################
#colnames(votes16)[11] <- "fips"
#colnames(CDC16)[5] <- "fips"
#colnames(CHR16)[1] <- "fips"
#2020 Data
colnames(CDC20)[6] <- "fips"
#colnames(CHR20)[3] <- "fips"
colnames(votes20)[2] <- "fips"



df2<-df2[,c(1,4,5)]
#colnames(votes20)[2] <- "fips"

df2$fips <- as.numeric(df2$fips)
zones$fips <- as.numeric(zones$fips)
#votes16$fips <- as.numeric(votes16$fips)
#CDC16$fips <- as.numeric(CDC16$fips)
#CHR16$fips <- as.numeric(CHR16$fips)
#2020 Data
CDC20$fips <- as.numeric(CDC20$fips)
#CHR20$fips <- as.numeric(CHR20$fips)
votes20$fips <- as.numeric(votes20$fips)

#votes20$fips <- as.character(votes20$fips)

df2<- df2[order(df2$fips),]
zones<- zones[order(zones$fips),]
#votes16 <- votes16[order(votes16$fips),]
#CDC16<- CDC16[order(CDC16$fips),]
#CHR16<- CHR16[order(CHR16$fips),]
#2020 Data
CDC20<- CDC20[order(CDC20$fips),]
#CHR20<- CHR20[order(CHR20$fips),]
votes20 <- votes20[order(votes20$fips),]

#votes20 <- votes20[order(votes20$fips),]
#zones <- zones[-which(zones$state=="Alaska"),]
#zones <- zones[-which(zones$state=="Hawaii"),]

#df[which(!(df$fips %in% zones$fips)),c(2,3)]
#zones[which(!(zones$fips %in% df$fips)),c(8,6)]
#df1[-which(),]
# df <- merge(df,votes16[,-c(1,9,10)],by="fips")
# zones<- zones[-which(!(zones$fips %in% df$fips)),]
# 
# df$dem_win <- 0
# df$dem_win[which(df$per_gop >df$per_dem)] <- 1
# df$target_diff <- df$per_gop - df$per_dem



###########################################
#####  MERGING DATASETS  ######################################################
###########################################

#### manual check of some incorrected fips $$ HELP
CDC20[which(!(CDC20$fips %in% votes20$fips )),c(2,5,6)]
#CHR16[which(!(CHR16$fips %in% CDC16$fips )),c(2,3,1)]
votes20[which(!(votes20$fips %in% CDC20$fips )),c(1,3,2)]

#"shannon" became "Oglala Lakota" in 2010, new fips=46102
zones[which(zones$fips == 46113),c(2,5)] <- c(46102, "Oglala Lakota")
#CHR16[which(CHR16$fips == 46113),c(1,3)] <- c(46102, "Oglala Lakota")
#votes16[which(votes16$fips == 46113),c(11,10)] <- c(46102, "Oglala Lakota")

#CHR16[which(CHR16$fips == 46102),1]
#CDC16[which(CDC16$fips == 46102),1]
#zones[which(zones$fips == 46102),1]
#votes16[which(votes16$fips == 46102),1]

#### Merging
#df <- merge(CDC16,CHR16,by="fips")
##include 2020
#df <- merge(CDC20,CHR20,by="fips")
df20 <- merge(CDC20,votes20,by="fips")
#df <- merge(df,votes16,by="fips") 
df20 <- merge(df20,df2,by="fips")


if (any(!(df20$fips %in% zones$fips)))
  df20<-df20[-which(!(df20$fips %in% zones$fips)),]

if (any(!(zones$fips %in% df20$fips)))
  zones<- zones[-which(!(zones$fips %in% df20$fips)),]

#zones[which(!(zones$fips %in% df20$fips)),]
#df20[which(!(df20$fips %in% zones$fips)),c(1,3,6)]


####
df20$fips <- as.character(df20$fips)
zones$fips <- as.character(zones$fips)
df20<- df20[order(df20$fips),]
zones<- zones[order(zones$fips),]

all(df20$fips==zones$fips)



nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)


###########################################
#####  Columns_modification  ######################################################
###########################################
which(!(zones$fips==df20$fips))

##### Diff% in the result
df20$target_diff <- df20$per_dem-df20$per_gop

df20$dem_win <- 0
df20$dem_win[which(df20$target_diff>0)] <- 1


##### logit of normalized result
#Calcolo percentuale ignorando voti nulli
true_perc_dem_votes <- (df20$votes_dem)/((df20$votes_dem)+(df20$votes_gop))
true_perc_gop_votes <- 1-true_perc_dem_votes
#Applico logit function alla true_perc_dem_votes per mappare valori su retta reale
df20$target_logit <- log(true_perc_dem_votes / (1 - true_perc_dem_votes))

all(df20$fips==df$fips)
df20$STATE <- df$STATE  # to have ALABAMA and not Alabama


df20$astensionism <- 1 - df20$total_votes/(df20$E_TOTPOP-df20$E_AGE17)

# # Votes for alaska are not by county
# df$astensionism[which(df$STATE=="ALASKA")] = 1 - (246588/sum(df$E_TOTPOP[which(df$STATE=="ALASKA")]
#                                                              -
#                                                              df$E_AGE17[which(df$STATE=="ALASKA")]))
# other errors
df20$astensionism[df20$astensionism<0] <- mean(df20$astensionism)
df20$logit_astensionism <- logit(df20$astensionism)



df$EP_H



