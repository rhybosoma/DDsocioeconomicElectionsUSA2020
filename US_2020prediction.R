




################################################################################
#############################   2020 Prediction
################################################################################
##### Base gam

#####  gam + tp
# mod2 = gam(target_logit ~ 
#              #   EP_MINRTY
#              s(EP_MINRTY,bs = "cr")
#            + s(EP_NOHSDP,bs = "cr")
#            #+ s(log(E_TOTPOP),bs = "cr")
#            #+ STATE  to reduce corr 0.3->0.2
#            + s(lon,lat, bs="tp")
#            
#            , data = df)
# summary(mod2)


y_pred1<- predict(mod1,df20)

predi <- as.vector(y_pred1)# predict(mod2,df20)



y_pred <- as.vector(predi)
predicted_percent <- exp(y_pred)/(1+exp(y_pred))




electoral_votes_data <- data.frame(
  state = c("ALABAMA", #"ALASKA",
            "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", 
            "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", #"HAWAII",
            "IDAHO", "ILLINOIS", "INDIANA", "IOWA", 
            "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", 
            "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", 
            "NEW YORK", "NORTH CAROLINA", "NORTH_DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", 
            "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON", 
            "WEST VIRGINIA", "WISCONSIN", "WYOMING"),
  GE = c(9, #3,
         11, 6, 54, 10, 7, 3, 3, 30, 16, #4, 
         4, 19, 11, 6, 6, 8, 8, 4, 10, 11, 15, 10, 6, 10, 4, 
         5, 6, 4, 14, 5, 28, 16, 3, 17, 7, 8, 19, 4, 9, 3, 11, 40, 6, 3, 13, 12, 4, 10, 3)
)

#FOR 2016-2020


US_states <- sort(unique(df$STATE))
votes_matrix <- data.frame(state="aaa",pop=0,votes=0,winner="aaa",GE=0,GE_DEM=0,GE_GOP=0)
for(i in 1:length(US_states)){
  index1 <- which(df$STATE==US_states[i])
  index2 <- which(electoral_votes_data==US_states[i])
  win <- NULL
  temp <-NULL
  state_pop = 0
  dist1_pop = 0
  dist2_pop = 0
  dist3_pop = 0
  state_dem_votes = 0
  dist1_dem_votes = 0
  dist2_dem_votes = 0
  dist3_dem_votes = 0
  GE_DEMaux=0
  GE_GOPaux=0
  
  
  for (j in index1) {
    state_pop = state_pop + df$E_TOTPOP[j]
    state_dem_votes  = state_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
  }
  
  if (US_states[i]=="MAINE"){
    for (j in index1) {
      
      #dist 1
      if (df$fips[j] == 23005 || df$fips[j] == 23013 || df$fips[j] == 23015 || df$fips[j] == 23023 || df$fips[j] == 23031 || df$fips[j] == 23011){
        dist1_pop = state_pop + df$E_TOTPOP[j]
        dist1_dem_votes = dist1_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
        #print(predicted_percent[j])
        #print(df$fips[j])
        #print(j)
      }
      #dist 2
      else{
        
        dist2_pop = state_pop + df$E_TOTPOP[j]
        dist2_dem_votes = dist2_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
        #print(predicted_percent[j])
        #print(df$fips[j])
      }
      
      #There are 4 General electors distributed as follows
      #Votes for general majortity (2 out of 2)
      if( state_dem_votes>(0.5*state_pop) ){
        GE_DEMaux = 2
      }
      else{
        GE_GOPaux = 2
      }
      
      #Votes for dist 1 (1 out of 1)
      if( dist1_dem_votes>(0.5*dist1_pop) ){
        GE_DEMaux = GE_DEMaux + 1 
      }
      else{
        GE_GOPaux = GE_GOPaux + 1
      }
      
      #Votes for dist 2 (1 out of 1)
      if( dist2_dem_votes>(0.5*dist2_pop) ){
        GE_DEMaux = GE_DEMuax + 1 
      }
      else{
        GE_GOPaux = GE_GOPaux + 1
      }
    }
    
    GE_DEM <- GE_DEMaux
    GE_GOP <- GE_GOPaux
    #print(GE_GOP)
    #print(GE_DEM)
    
    
  }
  
  #For Nebraska
  if (US_states[i]=="NEBRASKA"){
    for (j in index1) {
      
      #dist 1
      if (df$fips[j] == 31021 || df$fips[j] == 31023 || df$fips[j] == 31025 || df$fips[j] == 31037 || df$fips[j] == 31039 || df$fips[j] == 31053 || df$fips[j] == 31109 || df$fips[j] == 31119 || df$fips[j] == 31131 || df$fips[j] == 31141 || df$fips[j] == 31155 || df$fips[j] == 31159 || df$fips[j] == 31167 || df$fips[j] == 31173){
        dist1_pop = state_pop + df$E_TOTPOP[j]
        dist1_dem_votes = dist1_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
        print(predicted_percent[j])
        print(df$fips[j])
        #print(j)
      }
      #dist 2
      else if (df$fips[j] == 31055 || df$fips[j] == 31153)
      {
        
        dist2_pop = state_pop + df$E_TOTPOP[j]
        dist2_dem_votes = dist2_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
        #print(predicted_percent[j])
        #print(df$fips[j])
      }
      else{
        dist3_pop = state_pop + df$E_TOTPOP[j]
        dist3_dem_votes = dist3_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
        #print(predicted_percent[j])
        #print(df$fips[j])
      }
      #There are 4 General electors distributed as follows
      #Votes for general majortity (2 out of 2)
      if( state_dem_votes>(0.5*state_pop) ){
        GE_DEMaux = 2
      }
      else{
        GE_GOPaux = 2
      }
      
      #Votes for dist 1 (1 out of 1)
      if( dist1_dem_votes>(0.5*dist1_pop) ){
        GE_DEMaux = GE_DEMaux + 1 
      }
      else{
        GE_GOPaux = GE_GOPaux + 1
      }
      
      #Votes for dist 2 (1 out of 1)
      if( dist2_dem_votes>(0.5*dist2_pop) ){
        GE_DEMaux = GE_DEMuax + 1 
      }
      else{
        GE_GOPaux = GE_GOPaux + 1
      }
      
      #Votes for dist 3 (1 out of 1)
      if( dist3_dem_votes>(0.5*dist3_pop) ){
        GE_DEMaux = GE_DEMuax + 1 
      }
      else{
        GE_GOPaux = GE_GOPaux + 1
      }
    }
    
    GE_DEM <- GE_DEMaux
    GE_GOP <- GE_GOPaux
    #print(GE_GOP)
    #print(GE_DEM)
    
    
  }
  
  ##For the rest of states
  else{
    if( state_dem_votes>(0.5*state_pop) ){
      GE_DEM <- electoral_votes_data$GE[i]
      GE_GOP <- 0
    }
    else{
      GE_DEM <- 0
      GE_GOP <- electoral_votes_data$GE[i]
      
    }
    
  }
  
  if( state_dem_votes>(0.5*state_pop) ){
    win <- "DEM"
    GE <- electoral_votes_data$GE[i]
    
  }
  else{
    win <- "GOP"
    GE <- electoral_votes_data$GE[i]
    
  }
  
  temp <- data.frame(state = US_states[i],
                     pop = state_pop,
                     votes = state_dem_votes,
                     winner= win,
                     GE,GE_DEM,GE_GOP)
  
  votes_matrix <- rbind(votes_matrix, temp)
}

votes_matrix<- votes_matrix[-1,]


################################
#  Alaska ->   3 GOP
#  Hawayii ->  4 DEM
mean(abs(y_pred-df20$target_logit))

moran.mc(mod1$fitted.values-df20$target_logit, listw = listw1, nsim=999)

sum(votes_matrix$GE_DEM) + 4
sum(votes_matrix$GE_GOP) + 3


########################################
#######  result   on 2020

#          MAE    MORAN  MORAN-P  DEM   GOP 
# exact     0       0       1     306   232

# mod1    0.3888  0.577     0     296   242                 

# mod2    0.2968  0.371     0     277   261     

# mod2.r  0.2977  0.370     0     299   239 

# mod2.w  0.2987  0.367     0     283   255 

# mod2.s  0.2796  0.276     0     303   235

# mod3    0.4212  0.601     0     344   194                 

# mod4    0.4554  0.203     0     312   226

