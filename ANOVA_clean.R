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


# Loading libraries
library(MASS)
library(rgl)
library(DepthProc)
library(Matrix)
library(hexbin)
library(aplpack)
library(robustbase)
library(MDBED)
library(ISLR2)
library(car)
library(survival)
library(survminer)
library(dplyr) 
library(ggplot2)
library(knitr)
library(broom)
library(tidyr)
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(HoRM)
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)
library(dplyr) 
library(ggplot2)
library(knitr)
library(broom)
library(tidyr)
library(progress)
library(pbapply)
pboptions(type='none')
library(dbscan)
library(gridExtra)
library(pbapply)
library(parallel)
# devtools::install_github(repo="ryantibs/conformal", subdir="conformalInference")
library(conformalInference)
#install.packages('conformalInference.multi')
library(conformalInference.multi)
library(rgl)
library(geometry)
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)
library(magrittr)
library(KernSmooth)
library(aplpack)
library(tinytex)
library(spdep)  # Load the spdep package

library(ggplot2)
library(sf)



setwd("C:/Users/marco/Desktop/NPS/Progetto/US")

########### Dataset loading

df2 <-  read.csv("us_county_sociohealth_data.csv")
zones    <-  st_read("github_albersusa.json")
votes16  <-  read.csv("2016_US_County_Level_Presidential_Results.csv")
votes20  <-  read.csv("2020_US_County_Level_Presidential_Results.csv")
#df_big <-  read.csv("US_counties_COVID19_health_weather_data.csv")
CHR16 <-  read.csv("dati_2016abbb.csv", sep=";")
CDC16 <-  read.csv("SVI_2016_US_county.csv") 

head(df2)
head(zones)
head(votes16)
head(votes20)
head(CHR16)
head(CDC16)
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
  df[-which(!(df$fips %in% zones$fips)),]

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


head(df$total_votes)
head(df$E_TOTPOP)
head(df$E_AGE17)
head(df$E_TOTPOP-df$E_AGE17)
head(1-df$total_votes/(df$E_TOTPOP-df$E_AGE17))

colnames(df)
# E_TOTPOP
# E_AGE17
# 

##### astensionism%
df$astensionism <- 1 - df$total_votes/(df$E_TOTPOP-df$E_AGE17)

# Votes for alaska are not by county
df$astensionism[which(df$STATE=="ALASKA")] = 1 - (246588/sum(df$E_TOTPOP[which(df$STATE=="ALASKA")]
                                                             -
                                                               df$E_AGE17[which(df$STATE=="ALASKA")]))
# other errors
df$astensionism[df$astensionism<0] <- mean(df$astensionism)

##### NA filling
for (j in 1:dim(df)[2]){
  if (is.numeric(df[,j]))
    df[which(is.na(df[,j])),j] <- mean(df[,j], na.rm =T)
}


### ### ### ### ### ### ### ### ### ### ### 
### ANOVA - WITHOUT SPATIAL AUTOCORRELATION ### ### 
### ### ### ### ### ### ### ### ### ### ### 



values <- mod2$residuals

nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)
set.seed(2023)

mod_clean <- lagsarlm(values ~ 1, data = df,
                    listw = listw1, zero.policy = T)
#y=ρWy+β+ε

beta_0 = mod_clean$coefficients

# removing correlation
W <- nb2mat(nb1, zero.policy = T)
W <- as.matrix(W)

X_beta <- beta_0

length(residuals(mod_clean))

values_clean <-  residuals(mod_clean) + beta_0
values_clean <- as.vector(values_clean)
#values_clean <- as.vector(exp(values_clean)/(1+exp(values_clean)))

head(values)
head(values_clean)

sim <- moran.mc(x = mod2$residuals, listw=listw1, nsim=999 , alternative="two.sided")
sim




# Permutational ANOVA
############################
###### INITIALIZATION #########################################################
############################
### MODIFY HERE

data.cont =  values_clean       #continuous variable column
var.names =   'values'     #continuous variable name
factor1 =  as.factor(df$dem_win)               #groups columns
groups.names =  'dem_win'     #group name 

B = 1e3
seed = 26111992


### NOT MODIFY
n       <- length(factor1)      # total number of obs.
p       <- 1                 # number of variables
ng      <- table(factor1)       # number of obs. in each group
treat   <- levels(factor1)      # levels of the treatment
g       <- length(treat)     # number of levels (i.e., of groups)


### INITIAL PLOTS
# Group-wise boxplot #### Already here we can see qualitatively that there is a difference in mean

x11()
plot(factor1, data.cont, xlab=groups.names, ylab=var.names, col=rainbow(g),
     main='ANOVA on abstentionism')
abline(h=mean(data.cont))
#dev.off()



#############################
###### TEST  ############################################################
#############################

#statistic of the data
fit <- aov(data.cont~ factor1)
summary(fit)
T0 <- T0 <- summary(fit)[[1]][1,4]
#permutation statistics
T_stat <- numeric(B) 
set.seed(seed)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  data_perm <- data.cont[permutation]
  fit_perm <- aov(data_perm ~ factor1)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


### pval
p_val <- sum(T_stat>=T0)/B
p_val





#############################
###### PLOTS  ############################################################
#############################

x11()
par(mfrow = c(1,2))
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)























#################################################################################
#################################################################################
#############  Anova Swing
nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)


##################### Cleaning selected col
Values_to_clean <- df20[,c(47,49,51,73,66)]
Values_to_clean$EP_MINRTY <- df20$EP_MINRTY
Values_to_clean$EP_NOHSDP <- df20$EP_NOHSDP
Values_to_clean$EP_POV150 <- df20$EP_POV150
Values_to_clean$LogPop <- log(df20$E_TOTPOP)

Values_cleaned <- Values_to_clean

moran.check <-rep(0,dim(Values_to_clean)[2])
for(j in 1:dim(Values_to_clean)[2]){
  values <- Values_to_clean[,j]
  
  mod_clean <- lagsarlm(values~ 1, data = df,
                        listw = listw1, zero.policy = T)
  
  beta_0 = mod_clean$coefficients
  
  # removing correlation
  W <- nb2mat(nb1, zero.policy = T)
  W <- as.matrix(W)
  beta_0
  
  length(residuals(mod_clean))
  values_clean <-  residuals(mod_clean) + beta_0
  values_clean <- as.vector(values_clean)
  Values_cleaned[,j] <- values_clean 
  
  sim <- moran.mc(x = values_clean, listw=listw1, nsim=999 , alternative="two.sided")
  moran.check[j] <- sim$statistic
  print(j)
}


write.csv(Values_cleaned, file = "Cleaned_values.csv")


# Permutational ANOVA
############################
###### INITIALIZATION #########################################################
############################
### MODIFY HERE
SWING1 <- 
c( "ARIZONA", "DELAWARE" , "FLORIDA" , "GEORGIA" , "MAINE" , "NEVADA" , "NEW HAMPSHIRE" ,
   "NORTH CAROLINA" , "OREGON" , "PENNSYLVANIA" , "SOUTH CAROLINA" , "WISCONSIN")


Values_cleaned_swing <- Values_cleaned[which(df20$STATE %in% SWING1),]
factor.col <- df20$STATE[which(df20$STATE %in% SWING1)]

data.cont =  Values_cleaned_swing[,1]      #continuous variable column
var.names =   colnames(Values_cleaned_swing)[1]    #continuous variable name
factor1 =  as.factor(factor.col)               #groups columns
groups.names =  'state'     #group name 

B = 1e3
seed = 26111992


### NOT MODIFY
n       <- length(factor1)      # total number of obs.
p       <- 1                 # number of variables
ng      <- table(factor1)       # number of obs. in each group
treat   <- levels(factor1)      # levels of the treatment
g       <- length(treat)     # number of levels (i.e., of groups)


### INITIAL PLOTS
# Group-wise boxplot #### Already here we can see qualitatively that there is a difference in mean

x11()
plot(factor1, data.cont, xlab=NA, ylab=NA, col=rainbow(g),
     main=var.names)
abline(h=mean(data.cont))
#dev.off()



#############################
###### TEST  ############################################################
#############################

#statistic of the data
fit <- aov(data.cont~ factor1)
summary(fit)
T0 <- T0 <- summary(fit)[[1]][1,4]
#permutation statistics
T_stat <- numeric(B) 
set.seed(seed)
for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  data_perm <- data.cont[permutation]
  fit_perm <- aov(data_perm ~ factor1)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


### pval
p_val <- sum(T_stat>=T0)/B
p_val




#############################
###### PLOTS  ############################################################
#############################

x11()
par(mfrow = c(1,2))
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)












