
####################################################
###########  backward selection
####################################################


##### Base gam
mod1 = gam(df$target_diff ~ 
             s(EP_MINRTY,bs = "cr")
           + s(EP_NOHSDP,bs = "cr")
           #+ s(EP_POV,bs = "cr")
           #+ s(Homicide.Rate,bs = "cr")
           #+ s(EP_CROWD,bs = "cr")
           #+ s(EP_UNEMP,bs = "cr")
           #+ s(E_PCI,bs = "cr")
           + s(log(E_TOTPOP),bs = "cr")
           ###+ s(lon,lat, bs="tp")
           , data = df)
summary(mod1)

# step-wise backward:
#      R2adj   Aic  
# 0°: 0.677 | i remove: Homicide.Rate
# 1°: 0.673 | i remove: EP_CROWD
# 2°: 0.657 | i remove: EP_UNEMP
# 3°: 0.645 | i remove: EP_POV
# 4°: 0.610 | i remove: E_PCI
# 5°: 0.593 | if i remove: EP_NOHSD  => 0.508 
# => i stop at s(EP_MINRTY) + s(EP_NOHSDP) + s(log(E_TOTPOP))


#####  gam + tp
mod2 = gam(target_logit ~ 
            s(EP_MINRTY,bs = "cr")
           + s(EP_NOHSDP,bs = "cr")
           #+ s(log(E_TOTPOP),bs = "cr")
           #+ STATE  to reduce corr 0.3->0.2
           + s(lon,lat, bs="tp")
           + STATE
           , data = df ,  # weights = log(df$E_TOTPOP)
)
summary(mod2)
#  removing log(tot_pop)  i go 0.782 => 0.77    so i remove it
#
x11()
par(mfrow=c(1,3))
plot(mod2)

# BEST ONE - ASTENSIONISM
logit_astensionism <- logit(df$astensionism)
mod_asten = gam(logit_astensionism ~ 
                  
                  s(EP_NOHSDP,bs = "cr")
                + s(log(E_TOTPOP),bs = "cr")
                +s(lon,lat, bs="tp")
                , data = df)
summary(mod_asten)

# step-wise backward:
#      R2adj   Aic  
# 0°: 0.603 | i remove: EP_UNEMP
# 1°: 0.603 | i remove: EP_CROWD
# 2°: 0.601 | i remove: Homicide.Rate
# 3°: 0.598 | i remove: EP_MINRTY
# 4°: 0.587 | i remove: EP_POV
# 5°: 0.552 | i remove:   stop  or drop to 0.44
# 6°: 0.599 => stop  (or drop to 0.4 removing coordinates)





######################## DATA PREPARATION

nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)
W <- nb2mat(nb1, zero.policy = T)
W <- as.matrix(W)
qi <- c(0.25,0.5,0.75)


EP_MINRTY_bs <- bs(df$EP_MINRTY,    knots = quantile(df$EP_MINRTY,qi)) 
EP_NOHSDP_bs <- bs(df$EP_NOHSDP,    knots = quantile(df$EP_NOHSDP,qi))
E_LOGPOP_bs  <- bs(log(df$E_TOTPOP),    knots = quantile(log(df$E_TOTPOP),qi))

###### vote
df_bs<- as.data.frame(cbind(EP_MINRTY_bs,EP_NOHSDP_bs))
colnames(df_bs) <- c("M1","M2","M3","M4","M5","M6",
                     "ND1","ND2","ND3","ND4","ND5","ND6")
df_bs$target_logit <- df$target_logit
row.names(df_bs)<- (as.integer(attr(listw1, "region.id"))) 

######  abstensionism 
df_bsA<- as.data.frame(cbind(EP_NOHSDP_bs,E_LOGPOP_bs))
colnames(df_bsA) <- c("ND1","ND2","ND3","ND4","ND5","ND6",
                     "LP1","LP2","LP3","LP4","LP5","LP6")
df_bsA$targe_ast <- logit(df$astensionism)
row.names(df_bsA)<- (as.integer(attr(listw1, "region.id"))) 


############################################
#for  2020 prediction

#a <-predict(EP_MINRTY_bs, newx = df$EP_MINRTY)
EP_MINRTY_bs20 <- predict(EP_MINRTY_bs, newx = df20$EP_MINRTY)
EP_NOHSDP_bs20 <- predict(EP_NOHSDP_bs, newx = df20$EP_NOHSDP)
E_LOGPOP_bs20  <- predict(E_LOGPOP_bs, newx = log(df20$E_TOTPOP))

###### vote
df_bs20<- as.data.frame(cbind(EP_MINRTY_bs20,EP_NOHSDP_bs20))
colnames(df_bs20) <- c("M1","M2","M3","M4","M5","M6",
                       "ND1","ND2","ND3","ND4","ND5","ND6")
df_bs20$target_logit <- df20$target_logit
row.names(df_bs20)<- (as.integer(attr(listw1, "region.id"))) 

######  abstensionism 
df_bs20A<- as.data.frame(cbind(EP_NOHSDP_bs20,E_LOGPOP_bs20))
colnames(df_bs20A) <- c("ND1","ND2","ND3","ND4","ND5","ND6",
                       "LP1","LP2","LP3","LP4","LP5","LP6")
df_bs20A$targe_ast <- logit(df20$astensionism)
row.names(df_bs20A)<- (as.integer(attr(listw1, "region.id"))) 


### check 
mod0 <- lm(target_logit ~., data = df_bs)
summary(mod0)

y <- predict(mod0,df_bs20)

mean(abs(y-df_bs20$target_logit))  # 0.3892117 comparable -> ok 







##############################################################################
##########    MODELS VOTE
#############################################################################
{
# BEST ONE 
  #####  gam + tp
  mod1 = gam(target_logit ~ 
               s(EP_MINRTY,bs = "cr")
             + s(EP_NOHSDP,bs = "cr")
             #+ s(log(E_TOTPOP),bs = "cr")
             + s(lon,lat, bs="tp")
             + STATE
             , data = df ,  # weights = log(df$E_TOTPOP)
  )
  summary(mod1)

 
  
  #####  Spatial lag model
  mod2 <- lagsarlm(target_logit ~.
                   , data = df_bs, listw = listw1, zero.policy = T)
  summary(mod2)
  
  
  #####  Spatial error model
  
  #start_time <- Sys.time()
  #end_time <- Sys.time()
  #end_time - start_time
  mod3 <- errorsarlm(target_logit ~.
                     , data = df_bs, listw = listw1, zero.policy = T)
  summary(mod3)


  ####################### comparison on 2016
  
  #start_time <- Sys.time()
  #end_time <- Sys.time()
  #end_time - start_time
  a <- data.frame("R2"=0,"MAE"=0,"AIC"=0,"MoranI"=0,"Moran-p"=0)
  for (i in 1:3) {
    modi <- get(paste("mod", i,sep = ""))
    y_pred <- modi$fitted.values
    res   <-  modi$residuals
    
    # Compute R-squared
    ssr <- sum((df$target_logit - y_pred)^2)
    sst <- sum((df$target_logit - mean(df$target_logit))^2)
    r_squaredi <- 1 - (ssr / sst)
    r_squaredi
    ## Compute MSE
    #msei <- mean((df$target_logit - y_pred)^2)
    # Compute MAE
    maei <- mean(abs(df$target_logit - y_pred))
    # Moran 
    set.seed(2023)
    simi <- moran.mc(x = res, listw=listw1, nsim=999 , alternative="two.sided")
    a <- rbind(a,c(r_squaredi,maei,AIC(modi),simi$statistic,simi$p.value))
    print(i)
  }
  a<-a[-1,]
  row.names(a)<-c("gam","gam-lag","gam-err")
  a
  # R2       MAE      AIC      MoranI Moran.p
  # gam     0.4691740 0.4197991 5490.248  0.29075423   0.000
  # gam-lag 0.7156677 0.3126058 3708.928  0.14060332   0.000
  # gam-err 0.8379695 0.2277622 2301.974 -0.08393847   0.002
 
  
  
  
  ###########################  PREDICTION on 2020
  
  # gam
  y_pred <- predict(mod1,df20)
  # lag
  y_pred <- predict(mod2, newdata=df_bs20[,-13], listw= listw1,
                     pred.type = "TC", zero.policy=T, legacy=F, all.data=T)
  # err
  y_pred <- predict(mod3, newdata=df_bs20[,-13], listw= listw1,
                     pred.type = "TS", zero.policy=T)
  
  
  res <- df$target_logit - y_pred
  mae <- mean(abs(res))
  # Moran 
  set.seed(2023)
  simi <- moran.mc(x = res, listw=listw1, nsim=99 , alternative="two.sided")
  
  mae
  simi
  
#  MODEL      MAE        MORAN
#  gam    0.2867649      0.22068
#  sar    0.5592369      0.70284
#  sae    0.4946132      0.74126
#

}





##############################################################################
##########    MODELS ABS
#############################################################################
{
  # BEST ONE - ASTENSIONISM
  #####  gam + tp
  mod1= gam(logit_astensionism ~ 
                    
                    s(EP_NOHSDP,bs = "cr")
                  + s(log(E_TOTPOP),bs = "cr")
                  +s(lon,lat, bs="tp")
                  , data = df)

  summary(mod1)

  
  #####  Spatial lag model
  mod2 <- lagsarlm(targe_ast  ~.
                   , data = df_bsA, listw = listw1, zero.policy = T)
  summary(mod2)
  
  
  #####  Spatial error model
  
  #start_time <- Sys.time()
  #end_time <- Sys.time()
  #end_time - start_time
  mod3 <- errorsarlm(targe_ast ~.
                     , data = df_bsA, listw = listw1, zero.policy = T)
  summary(mod3)
  
  
  ####################### comparison on 2016
  
  #start_time <- Sys.time()
  #end_time <- Sys.time()
  #end_time - start_time
  a <- data.frame("R2"=0,"MAE"=0,"AIC"=0,"MoranI"=0,"Moran-p"=0)
  for (i in 1:3) {
    modi <- get(paste("mod", i,sep = ""))
    y_pred <- modi$fitted.values
    res   <-  modi$residuals
    
    # Compute R-squared
    ssr <- sum((df$logit_astensionism - y_pred)^2)
    sst <- sum((df$logit_astensionism - mean(df$logit_astensionism))^2)
    r_squaredi <- 1 - (ssr / sst)
    r_squaredi
    ## Compute MSE
    #msei <- mean((df$target_logit - y_pred)^2)
    # Compute MAE
    maei <- mean(abs(df$logit_astensionism - y_pred))
    # Moran 
    set.seed(2023)
    simi <- moran.mc(x = res, listw=listw1, nsim=999 , alternative="two.sided")
    a <- rbind(a,c(r_squaredi,maei,AIC(modi),simi$statistic,simi$p.value))
    print(i)
  }
  a<-a[-1,]
  row.names(a)<-c("gam","gam-lag","gam-err")
  a
  # R2       MAE       AIC      MoranI Moran.p
  # gam     0.5574207 0.2048793  987.6933  0.23209271   0.000
  # gam-lag 0.5376101 0.2065503 1189.7641  0.09046222   0.000
  # gam-err 0.6179191 0.1822005  773.4535 -0.04268674   0.002
  
  
  
  
  ###########################  PREDICTION on 2020
  
  # gam
  y_pred <- predict(mod1,df20)
  # lag
  y_pred <- predict(mod2, newdata=df_bs20A[,-13], listw= listw1,
                    pred.type = "TC", zero.policy=T, legacy=F, all.data=T)
  # err
  y_pred <- predict(mod3, newdata=df_bs20A[,-13], listw= listw1,
                    pred.type = "TS", zero.policy=T)
  
  
  
  res <- df20$logit_astensionism - y_pred
  mae <- mean(abs(res))
  # Moran 
  set.seed(2023)
  simi <- moran.mc(x = res, listw=listw1, nsim=99 , alternative="two.sided")
  
  mae
  simi
  
  #  MODEL      MAE        MORAN
  #  gam    0.2867649      0.22068
  #  sar     0.340687      0.34909
  #  sem     0.352           0.402
  #
  
}



################################################################################
############ Pointwise vote result
################################################################################

{

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

moran.mc(mod3$fitted.values-df20$target_logit, listw = listw1, nsim=999)

sum(votes_matrix$GE_DEM) + 4
sum(votes_matrix$GE_GOP) + 3

}
