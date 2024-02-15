


################################################################################
################################################################################
########################          SOME PLOT          ###########################
################################################################################
################################################################################

############ PLOT of winning
values <- df$votes_dem 
sum(df$dem_win)
    
ccc <-  colorRamp(c("#FD1953", "#26AFFD"),space ="Lab")(values/max(values))
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
x11()
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")


############ PLOT of diff%
values <- df$target_diff

ccc <-  colorRamp(c("white", "black"),space ="Lab")(abs(values)/max(values))
ccc[which(values>0),] <- colorRamp(c("white", "#26AFFD"),space ="Lab")(values[which(values>0)]/1)
ccc[which(values<0),] <- colorRamp(c("white", "#FD1953"),space ="Lab")((-values[which(values<0)])/1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
x11()
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")
  scale_fill_viridis_c(limits = range(values)) +
    labs(title = "Original Local Moran's I (LISA) Results", fill = "LISA Value") +
    

    x11()
  ggplot(data = zones) +
    geom_sf(aes(fill = ccc), color = NA)
    
    
    
    # LISA (Local Moran) - NOT CLEAN
    lisa_original <- localmoran(values, listw1, alternative="two.sided")
  lisa_original_df <- data.frame(lisa_original)  # Convert to a data frame for easier handling
  zones$lisa_values_original <- lisa_original_df$Ii
  
  range_min <- min(lisa_original_df$Ii, na.rm = TRUE)
  range_max <- max(lisa_original_df$Ii, na.rm = TRUE)
  
  x11()
  ggplot(data = zones) +
    geom_sf(aes(fill = lisa_values_original), color = NA)+
   scale_fill_viridis_c(limits = c(range_min, range_max)) +
   #labs(title = "Original Local Moran's I (LISA) Results", fill = "LISA Value") +
    #theme_minimal()  
    
    
    
    
  
  


########## PLOT of population dnesity
values <- (df$E_TOTPOP/df$AREA_SQMI)
q95 <- quantile(values,0.95)
values[which(values >q95)] <- q95

x11()
ccc <-  colorRamp(c("#D4FFB6", "#00D758"),space ="Lab")(values/max(values))
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")

########## PLOT of astensionism

values <- df$astensionism

x11()
ccc <-  colorRamp(c("white", "blue"),space ="Lab")(values/1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")


########## PLOT of NO high school diploma
values <- df$EP_NOHSDP

x11()
ccc <-  colorRamp(c("#FFDEDE","#C80000"),space ="Lab")(values/75)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")

########## PLOT of Minorities

values <- df$EPL_MINRTY

x11()
ccc <-  colorRamp(c("#FFF5C1","#EC9A00"),space ="Lab")(values/1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")



#values[which(values==0)] <- mean(values[which(values>0)])

nb_class <- poly2nb(zones) 
colw <- nb2listw(nb_class, style="W", zero.policy = TRUE)
nsim <- 9999
#set.seed(1234)
sim1 <- moran.mc(x = df$target_diff, listw=colw, nsim=nsim , alternative="two.sided")
sim1


################################################################################
################################################################################
########################          MORAN TEST          ##########################
################################################################################
################################################################################


values <- df$target_diff

nb_class <- poly2nb(zones) 
colw <- nb2listw(nb_class, style="W", zero.policy = TRUE)
nsim <- 9999
#set.seed(1234)
sim1 <- moran.mc(x = values, listw=colw, nsim=nsim , alternative="two.sided")
sim1

#moran.mc(x = values, listw=colw, nsim=nsim , alternative="two.sided")

###### target_diff
# statistic = 0.59596, observed rank = 10000, p-value < 2.2e-16
# alternative hypothesis: two.sided

###### dem_win
# statistic = 0.35287, observed rank = 10000, p-value < 2.2e-16
# alternative hypothesis: two.sided

###### population density
# statistic = 0.53663, observed rank = 10000, p-value < 2.2e-16
# alternative hypothesis: two.sided

###### astensinism
# statistic = 0.4537, observed rank = 10000, p-value < 2.2e-16
# alternative hypothesis: two.sided

################################################################################
################################################################################
########################     GAM ON VOTE RESULT      ###########################
################################################################################
################################################################################


###########################################
#####  on diff%  ######################################################
###########################################
df$E_TOTPOP
df$target_diff


model_gam= gam(target_diff ~   s(lon,lat, bs="tp")
               + STATE
               + s(EP_NOHSDP, bs='cr')
               + s(EP_POV, bs='cr')
               + s(EP_MINRTY, bs='cr')
               + s(Homicide.Rate, bs='cr')
               + s(EP_CROWD, bs='cr')
               + s(EP_UNEMP, bs='cr')
               , data = df)
summary(model_gam)

#########  target_diff
# R-sq.(adj) =  0.831   Deviance explained = 83.7%
# GCV = 0.016612  Scale est. = 0.016009  n = 3135



model_gam= gam(target_diff ~ s(lon,lat, bs="tp")
               + STATE
               + s(EP_NOHSDP, bs='cr')
               + s(EP_MINRTY, bs='cr')
               , data = df)
summary(model_gam)
# R-sq.(adj) =  0.813   Deviance explained = 81.8%
# GCV = 0.018176  Scale est. = 0.017647  n = 3139




######### residuals
x11()
par(mfrow = c(1,3))
plot(model_gam)

plot(model_gam$fitted.values,model_gam$residuals)
abline(h = 0, col = "red")

### Normality
hist(model_gam$residuals, breaks = 100)

x11()
qqnorm(model_gam$residuals)
qqline(model_gam$residuals, col = "red")           # Add qqline to plot
shapiro.test(model_gam$residuals)

### Moran
values <- model_gam$residuals
nb_class <- poly2nb(zones) 
colw <- nb2listw(nb_class, style="W", zero.policy = TRUE)
nsim <- 9999
#set.seed(1234)
sim1 <- moran.mc(x = values, listw=colw, nsim=nsim , alternative="two.sided")
sim1








###########################################
#####  on astensionism  ######################################################
###########################################


df$E_TOTPOP
df$target_diff
df$astensionism

model_gam= gam(astensionism ~   s(lon,lat, bs="tp")
               + STATE
               + s(EP_NOHSDP, bs='cr')
               + s(EP_POV, bs='cr')
               + s(EP_MINRTY, bs='cr')
               + s(Homicide.Rate, bs='cr')
               + s(EP_CROWD, bs='cr')
               + s(EP_UNEMP, bs='cr')
               , data = df)
summary(model_gam)

#########  target_diff
# R-sq.(adj) =  0.644   Deviance explained = 65.7%
# GCV = 0.003485  Scale est. = 0.0033604  n = 31



model_gam= gam(astensionism ~ STATE
               + s(EP_NOHSDP, bs='cr')
               + s(EP_POV, bs='cr')
               , data = df)
summary(model_gam)

# R-sq.(adj) =  0.813   Deviance explained = 81.8%
# GCV = 0.018176  Scale est. = 0.017647  n = 3139




######### residuals
x11()
par(mfrow = c(1,3))
plot(model_gam)

plot(model_gam$fitted.values,model_gam$residuals)
abline(h = 0, col = "red")

### Normality
hist(model_gam$residuals, breaks = 100)

x11()
qqnorm(model_gam$residuals)
qqline(model_gam$residuals, col = "red")           # Add qqline to plot
shapiro.test(model_gam$residuals)

### Moran
values <- model_gam$residuals
nb_class <- poly2nb(zones) 
colw <- nb2listw(nb_class, style="W", zero.policy = TRUE)
nsim <- 9999
#set.seed(1234)
sim1 <- moran.mc(x = values, listw=colw, nsim=nsim , alternative="two.sided")
sim1






x11()
par(mfrow = c(1,3))
plot(model_gam)

plot(model_gam$fitted.values,model_gam$residuals)
abline(h = 0, col = "red")

vif(model_gam)[which(vif(model_gam)>10)]

qqnorm(model_gam$residuals)
qqline(model_gam$residuals, col = "red")           # Add qqline to plot
shapiro.test(model_gam$residuals)

#pred_tp = predict(model_gam,  newdata = data.frame(grid))


values <- model_gam$residuals

x11()
ccc <-  colorRamp(c("blue", "red"),space ="Lab")(abs(values)/max(values))
ccc[which(values>0),] <- colorRamp(c("white", "red"),space ="Lab")(values[which(values>0)]/1)
ccc[which(values<0),] <- colorRamp(c("white", "blue"),space ="Lab")((-values[which(values<0)])/1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 

ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")



nb_class <- poly2nb(zones) 
colw <- nb2listw(nb_class, style="W", zero.policy = TRUE)
nsim <- 9999
#set.seed(1234)
sim1 <- moran.mc(x = values, listw=colw, nsim=nsim , alternative="two.sided")
sim1


######### visual comparison

x11()
values <- model_gam$fitted.values
ccc <-  colorRamp(c("blue", "red"),space ="Lab")(abs(values)/max(values))
ccc[which(values>0),] <- colorRamp(c("white", "red"),space ="Lab")(values[which(values>0)]/1.1)
ccc[which(values<0),] <- colorRamp(c("white", "blue"),space ="Lab")((-values[which(values<0)])/1.1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")

x11()
values <- df$target_diff
ccc <-  colorRamp(c("blue", "red"),space ="Lab")(abs(values)/max(values))
ccc[which(values>0),] <- colorRamp(c("white", "red"),space ="Lab")(values[which(values>0)]/1.1)
ccc[which(values<0),] <- colorRamp(c("white", "blue"),space ="Lab")((-values[which(values<0)])/1.1)
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")








x11()
persp3d(lon.grid, lat.grid, pred_tp, col = 'grey30')
with(df,
     points3d(lon, lat, target_diff, col = 'black', size = 5))




model_gam=gam(prestige ~ s(education,bs="cr") + s(income,bs="cr"),data = Prestige)
summary(model_gam)
x11()
par(mfrow = c(1,2))
plot(model_gam)







################################################################################
################################################################################
########################     AREAL ANALYSIS      #########################
################################################################################
################################################################################

#df$target_logit

nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)



############ Base lm
mod1 <- gam(target_logit ~ 
             EP_MINRTY +
             EP_POV +
             EP_NOHSDP+
             log(E_TOTPOP),weights =log(E_TOTPOP),  data = df)
summary(mod1)






mod1$terms

x11()
plot(log(df$E_TOTPOP) ,mod1$residuals)

##########  Spatial lag model
mod2 <- lagsarlm(target_logit ~ 
                   EP_MINRTY +
                   EP_POV +
                   EP_NOHSDP+
                   log(E_TOTPOP), data = df,
                   listw = listw1, zero.policy = T)

mod2 <- lagsarlm(mod1$terms, data = df,
                 listw = listw1, zero.policy = T)

summary(mod2)

##########  Spatial error model
mod3 <- errorsarlm(target_logit ~ 
                     EP_MINRTY +
                     EP_POV +
                     EP_NOHSDP+
                     log(E_TOTPOP), data = df, weights =log(df$E_TOTPOP),
                   listw = listw1, zero.policy = T)
summary(mod3)

##########  SARAR
mod4 <- sacsarlm(target_logit ~ 
                   EP_MINRTY +
                   EP_POV +
                   EP_NOHSDP+
                   log(E_TOTPOP), data = df, 
                 listw = listw1, zero.policy = T)
summary(mod4)



#######################

a <- data.frame("R2"=0,"MAE"=0,"MSE"=0,"MoranI"=0,"Moran-p"=0)
for (i in 1:4) {
  modi <- get(paste("mod", i,sep = ""))
  y_pred <- modi$fitted.values
  res   <-  modi$residuals
  
  # Compute R-squared
  ssr <- sum((df$target_logit - y_pred)^2)
  sst <- sum((df$target_logit - mean(df$target_logit))^2)
  r_squaredi <- 1 - (ssr / sst)
  r_squaredi
  # Compute MSE
  msei <- mean((df$target_logit - y_pred)^2)
  # Compute MAE
  maei <- mean(abs(df$target_logit - y_pred))
  # Moran 
  set.seed(2023)
  simi <- moran.mc(x = res, listw=listw1, nsim=999 , alternative="two.sided")
  a <- rbind(a,c(r_squaredi,maei,msei,simi$statistic,simi$p.value))
  print(i)
}
a<-a[-1,]
row.names(a)<-c("lm","lag-lm","err-lm","SARAR")
a

#normality
shapiro.test(res)
x11()
par(mfrow=c(1,2))
hist(res, breaks = 100)
qqnorm(res)
qqline(res)

# test for heteroskedasticity => bad
bptest.Sarlm(mod3)

x11()
plot(mod2$fitted.values,mod2$residuals)
abline(h=0,col="red")


# spatial correlation
set.seed(2023)
res   <-  mod1$residuals
sim4 <- moran.mc(x = res, listw=listw1, nsim=9999 , alternative="two.sided")
sim4



#              R2       MAE        MSE      MoranI    Moran.p
# lm     0.4389877 0.4494183 0.34283751  0.68663054   0.000
# lag-lm 0.7249187 0.3079052 0.16810362  0.13687536   0.000
# err-lm 0.8332562 0.2301175 0.10189798 -0.07433043   0.002
# SARAR  0.8442816 0.2218759 0.09516032 -0.04813069   0.002





################################################################################
################################################################################
########################    FROM logit to vote result      #####################
################################################################################
################################################################################


# inverse tranform
# tttt <- exp(df$target_logit)/(1+exp(df$target_logit))
y_pred <- mod3$fitted.values
predicted_percent <- exp(y_pred)/(1+exp(y_pred))


US_states <- sort(unique(df$STATE))
votes_matrix <- data.frame(state="aaa",pop=0,votes=0,winner="aaa",GE=0)
for(i in 1:length(US_states)){
  index1 <- which(df$STATE==US_states[i])
  win <- NULL
  temp <-NULL
  state_pop = 0
  state_dem_votes = 0
  
  for (j in index1) {
    state_pop = state_pop + df$E_TOTPOP[j]
    state_dem_votes = state_dem_votes + df$E_TOTPOP[j]*predicted_percent[j]
  }
  
  if( state_dem_votes>(0.5*state_pop) ){
    win <- "DEM"
  }
  else{
    win <- "GOP"
  }
  temp <- data.frame(state = US_states[i],
                     pop = state_pop,
                     votes = state_dem_votes,
                     winner= win,
                     GE=0)
  
  votes_matrix <- rbind(votes_matrix, temp)
}

votes_matrix<- votes_matrix[-1,]



########## Counties-wise plot

### true
#values <- df$dem_win 
values<-df$per_dem
sum(values)

ccc <-  colorRamp(c("#FD1953", "#26AFFD"),space ="Lab")(values/max(values))
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
x11()
ggplot() +
  geom_sf(data = zones, fill = ccc, color = NA)

### predicted
#values2 <- ifelse(predicted_percent>0.5,1,0) 
values2 <- predicted_percent
sum(values2)

ccc <-  colorRamp(c("#FD1953", "#26AFFD"),space ="Lab")(values2/max(values2))
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
x11()
ggplot() +
  geom_sf(data = zones, fill = ccc, color = "black")





########## State-wise plot

# df$DEM_state <-0
# US_states <- sort(unique(df$STATE))
# 
# for(i in 1:length(US_states)){
#   if(votes_matrix$winner[which(votes_matrix$state==US_states[i])]=="DEM")
#     df$DEM_state[which(df$STATE==US_states[i])] = 1
# }
# 
# 
# values <- df$DEM_state
# sum(values)


zones2  <-  st_read("gz_2010_us_040_00_5m.json")
zones2 <- zones2[-52,]

values3 <- ifelse(votes_matrix$winner=="DEM",1,0)

ccc <-  colorRamp(c("#FD1953", "#26AFFD"),space ="Lab")(values3/max(values3))
ccc <- rgb(ccc[,1], ccc[,2], ccc[,3], maxColorValue=255) 
x11()
ggplot() +
  geom_sf(data = zones2, fill = ccc, color = 1)





############################################################################
####################    Anova ##############################################
############################################################################

nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)


# ONlY VALUE TO MODIFY
values <- df$EP_MINRTY



set.seed(2023)

sim4 <- moran.mc(x = values, listw=listw1, nsim=999 , alternative="two.sided")
sim4

a <- rep(1,3135)
##########  Spatial error model
mod3 <- errorsarlm(values ~ dem_win, data = df,
                   listw = listw1, zero.policy = T)
summary(mod3)


mod4 <- lagsarlm(values ~ 1, data = df,
                     listw = listw1, zero.policy = T)
summary(mod4)
  


# removing correlation
lambda1 <-  mod3$lambda #mod3$rho
W <- nb2mat(nb1, zero.policy = T)
W <- as.matrix(W)

values1 <- (diag(x=1,length(values),length(values)) -lambda1*W)%*%values


#values1 <- (diag(x=1,length(values),length(values)) -lambda1*W)%*%values


values1 <- as.vector(values1)
set.seed(2023)
sim41 <- moran.mc(x = values1, listw=listw1, nsim=999 , alternative="two.sided")
sim41


# anova  (NB replace with permutation version)
fit <- aov(values~ df$dem_win)
summary(fit)

fit <- aov(values1~ df$dem_win)
summary(fit)













######### boxplot


data.cont =  values1       #continuous variable column
var.names =   'data.cont'     #continuous variable name
factor1 =  as.factor(df$dem_win)               #groups columns
groups.names =  'feed'     #group name 

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
     main='Dataset Chicken data.conts')
abline(h=mean(data.cont))
#dev.off()







