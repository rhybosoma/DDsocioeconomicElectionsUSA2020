### ### ### ### ### ### ### ### ### ### ### 
### ### ### TARGET LOGIT MODEL  ### ### ###  
### ### ### ### ### ### ### ### ### ### ### 
# BEST ONE - VOTE
mod_vote = gam(target_logit ~ 
             #   EP_MINRTY
             s(EP_MINRTY,bs = "cr")
           + s(EP_NOHSDP,bs = "cr")
           #+ s(log(E_TOTPOP),bs = "cr")
           + STATE  #to reduce corr 0.3->0.2
           + s(lon,lat, bs="tp")
           
           , data = df)

summary(mod_vote)

# BEST ONE - ASTENSIONISM
logit_astensionism <- logit(df$astensionism)
mod_asten = gam(logit_astensionism ~ 
             # s(EP_MINRTY,bs = "cr")+
               s(EP_NOHSDP,bs = "cr")
            # + s(EP_POV,bs = "cr")
             #+ s(Homicide.Rate,bs = "cr")
             #+ s(EP_CROWD,bs = "cr")
             #+ s(EP_UNEMP,bs = "cr")
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


x11()
par(mfrow=c(1,3))
plot(mod_asten)
### ### ### ### ### ### ### ### ### ### ### 
### PLOTS - SPATIAL AUTOCORRELATION ### ### 
### ### ### ### ### ### ### ### ### ### ### 

# creo dataset (superfluo)
df_res <- NULL
df_res$original_res <- asten_votes$residuals
df_res$lon <- df$lon
df_res$lat <- df$lat
df_res$dem_win <- df$dem_win
# ONlY VALUE TO MODIFY
values <- df_res$original_res



nb1 <- poly2nb(zones)
listw1 <- nb2listw(nb1, style="W", zero.policy = T)
set.seed(2023)
mod_res <- lagsarlm(original_res ~ 1, data = df_res,
                    listw = listw1, zero.policy = T)

# computing inverse matrix (needed during Bootstrap)
W <- nb2mat(nb1, zero.policy = T)
W <- as.matrix(W)
lambda1 <- mod_res$rho
identity <- diag(length(values))
inverse <- solve(identity-lambda1*W)

# extracting clean residuals
beta_0 = mod_res$coefficients
X_beta <- beta_0
clean_res <- X_beta + mod_res$residuals
# (I - \rho W) Y = \beta X + \epsilon 
# A noi interessa solo [\beta X + \epsilon] -> ci siamo liberati dell'autocorrelazione spaziale
df_res$clean_residuals <- clean_res

# checking spatial autocorrelation
sim <- moran.mc(x = clean_res, listw=listw1, nsim=999 , alternative="two.sided")
sim


############################################################
#### ddplot for comparisong
##############################################

###### base
set.seed(2023)
normal <- as.matrix(cbind(df_res$original_res,df$lon,df$lat))  # unbiased estimate of residual variance
normal2 <- as.matrix(cbind(sample(df_res$original_res,replace=TRUE),df$lon,df$lat))

x11()
DepthProc::ddPlot(x = normal, y = normal2 ,depth_params = list(method='Tukey'))




######## with cleaning
set.seed(2023)
normal <- as.matrix(cbind(df_res$original_res,df$lon,df$lat))  # unbiased estimate of residual variance
normal2 <- as.matrix(cbind(Y.b,df$lon,df$lat))

x11()
DepthProc::ddPlot(x = normal, y = normal2 ,depth_params = list(method='Tukey'))

#graphics.off()

###### base
set.seed(2023)
a <- rnorm(1000)
normal <- as.matrix(cbind(a,df$lon,df$lat))  # unbiased estimate of residual variance
normal2 <- as.matrix(cbind(sample(a,replace=TRUE),df$lon,df$lat))

x11()
DepthProc::ddPlot(x = normal, y = normal2 ,depth_params = list(method='Tukey'))




mean(df_res$original_res)
sd(df_res$original_res)
mean(Y.b)
sd(Y.b)
### ### ### ### ### ### ### ### ###
### Reconstructing bootstrap samples
### ### ### ### ### ### ### ### ###

B = 1000
# Clean residuals to perform bootstrap with
clean_res

# Prediction
# Fitted values
fitted_values <- mod_vote$fitted.values

n <- length(clean_res)
pred.b <- vector("list", B) # è un vettore di liste (3135 valori * B volte)

pb = pb=progress::progress_bar$new(total=B, format = " Processing [:bar] :percent eta: :eta")

for (perm in 1:B) {
  
  # bootstrap on uncorrelated part
  clean_res.b <- as.matrix(sample(clean_res,replace=TRUE))
  
  # re-creating spatial correlation
  Y.b = inverse%*%(clean_res.b)
  
  # summing to obtain bootstrapped target_logit
  target_logit.b = fitted_values + Y.b
  
  mod_vote.b <- gam(target_logit.b ~ 
                   #   EP_MINRTY
                   s(EP_MINRTY,bs = "cr")
                 + s(EP_NOHSDP,bs = "cr")
                 #+ s(log(E_TOTPOP),bs = "cr")
                 + STATE  #to reduce corr 0.3->0.2
                 + s(lon,lat, bs="tp")
                 , data = df)
  
  pred.b[[perm]] <- predict(mod_vote.b, df20)
  
  pb$tick()
}


save(pred.b, file="bootstrap_predictions.RData")

############ ############ ####################### ############ ############ 
############ ############ ### WITH ASTENSIONISM ### ############ ############ 
############ ############ ####################### ############ ############ 
# Load the saved .RData file
load("bootstrap_predictions.RData")
print(pred.b)



##### Defining functions
reverse_logit <- function(y_pred) {
  exp(y_pred) / (1 + exp(y_pred))
}

determine_state_winners <- function(vote_percent, df) {
  
  winning_states <- data.frame(state = NULL, winner = NULL, percent = NULL, GE = NULL)
  
  for(j in 1:(length(US_states))) {
    index1 <- which(df$STATE == US_states[j])
    # state_votes= sum(
    #   (df$E_TOTPOP[index1]-df$E_AGE17[index1])# *(1-true_asten_pred[index1])
    #   *predicted_percent[index1]
    #   )
    # 
    # state_dem_votes <- sum(
    #   (df$E_TOTPOP[index1]-df$E_AGE17[index1])# *(1-true_asten_pred[index1])
    #   *predicted_percent[index1]
    #   )
    # 
    # state_dem_percent <- state_dem_votes/state_votes
    # 
    
    state_pop       <- sum(df$E_TOTPOP[index1])
    state_dem_votes <- sum(df$E_TOTPOP[index1]*vote_percent[index1])
    
    state_dem_percent <- state_dem_votes/state_pop
    win <- ifelse(state_dem_percent >0.5, "DEM", "GOP")
    
    temp <- data.frame(state = US_states[j], winner = win, percent = state_dem_percent, GE = 0)
    winning_states <- rbind(winning_states, temp)
  }
  
  return(winning_states)
}

determine_state_winners_absten <- function(vote_percent, absten_percent, df) {
  
  winning_states <- data.frame(state = NULL, winner = NULL, percent = NULL, GE = NULL)
  
  for(j in 1:(length(US_states))) {
    index1 <- which(df$STATE == US_states[j])
    state_votes= sum(
      (df$E_TOTPOP[index1]-df$E_AGE17[index1])*(1-absten_percent[index1])
    )
    
    state_dem_votes <- sum(
      (df$E_TOTPOP[index1]-df$E_AGE17[index1])*(1-absten_percent[index1])*vote_percent[index1]
    )
    
    state_dem_percent <- state_dem_votes/state_votes
    
    win <- ifelse(state_dem_percent >0.5, "DEM", "GOP")
    
    temp <- data.frame(state = US_states[j], winner = win, percent = state_dem_percent, GE = 0)
    winning_states <- rbind(winning_states, temp)
  }
  
  return(winning_states)
}


#### Counting bootstrap percentages

# Initializing Data Structures
# Count times in which parties have won in the bootstrap replicas
state_win_counts <- matrix(0, nrow = length(US_states), ncol = 2, 
                           dimnames = list(US_states, c("DEM", "GOP")))


# Save the dem and gop percentages in the bootstrap replicas
state_percentages <- setNames(vector("list", length(US_states)), US_states)
for (state in US_states) {
  state_percentages[[state]] <- data.frame(DEM_percent = numeric(), GOP_percent = numeric())
}


absten_pred <- predict(mod_asten, df20)
absten_perc <- reverse_logit(absten_pred)
  
# Iterate over each bootstrap sample
for (iter in 1:length(pred.b)) {
  
  # computing states results
  vote_pred <- pred.b[[iter]]
  vote_percent <- reverse_logit(vote_pred)
  
  #winning_states <- determine_state_winners_absten(vote_percent,absten_perc, df20)
  winning_states <- determine_state_winners(vote_percent = vote_percent, df20)
  
  
  for (i in 1:nrow(winning_states)) {
    # Increasing number of wins for each state
    state_win_counts[winning_states$state[i], winning_states$winner[i]] <- state_win_counts[winning_states$state[i], winning_states$winner[i]] + 1
    
    # Saving predicted state percentage in state_percentages
    state <- winning_states$state[i]
    percent_DEM <- winning_states$percent[i]
    percent_GOP <- 1 - percent_DEM
    
    # Append the percentages to the respective state's data frame
    state_percentages[[state]] <- rbind(state_percentages[[state]], 
                                        data.frame(DEM_percent = percent_DEM, GOP_percent = percent_GOP))
    
    }
}


state_win_counts
# Convert win counts to win percentages
state_win_percentages <- state_win_counts / length(pred.b)
# Display the win percentages
state_win_percentages


#########################################
### REVERSE PERCENTILE CONF INTERVALS ###
#########################################
confidence_intervals <- list()
alpha_level <- 0.05

# Function to calculate reverse quantile CI for a given state's percentages
calculate_reverse_quantile_ci <- function(percentages, true_pred, alpha_level) {
  right.quantile <- quantile(percentages, 1 - alpha_level / 2)
  left.quantile <- quantile(percentages, alpha_level / 2)
  lwr <- true_pred - (right.quantile - true_pred)
  upr <- true_pred - (left.quantile - true_pred)
  return(c(lwr, upr))
}

logit_vote_pred_county <- predict(mod_vote, df20)
vote_pred_county <- reverse_logit(logit_vote_pred_county)

# Add vote predictions to df20
df20$vote_pred_county <- vote_pred_county

# Calculate weighted vote prediction for each state
state_level_predictions <- df20 %>%
  group_by(STATE) %>%
  summarise(
    weighted_vote_pred = sum(vote_pred_county * E_TOTPOP) / sum(E_TOTPOP),
    total_population = sum(E_TOTPOP)
  )

state_level_predictions$STATE

# Convert STATE to a format that matches the state names in state_percentages, if necessary
state_level_predictions$STATE <- toupper(state_level_predictions$STATE)  # Adjust this as needed

# Iterate through each state in state_percentages
for (state in names(state_percentages)) {
  # Find the matching row in state_level_predictions
  matching_row <- state_level_predictions %>% filter(STATE == state)
  
  # Use weighted_vote_pred as true_pred for the current state
  true_pred <- matching_row$weighted_vote_pred
  
  # DEM Percentages
  dem_ci <- calculate_reverse_quantile_ci(state_percentages[[state]]$DEM_percent, true_pred, alpha_level)
  
  # GOP Percentages (if needed)
  gop_ci <- calculate_reverse_quantile_ci(state_percentages[[state]]$GOP_percent, (1-true_pred), alpha_level)
  
  # Determine if 50% falls within the DEM confidence interval, indicating a swing state
  swing_state <- (dem_ci[1] <= 0.5 && dem_ci[2] >= 0.5) || (gop_ci[1] <= 0.5 && gop_ci[2] >= 0.5)
  
  # Save the results
  confidence_intervals[[state]] <- list(
    DEM_CI = dem_ci,
    GOP_CI = gop_ci,
    SwingState = swing_state
  )
}

# Correctly iterate through each state in confidence_intervals
for (state in names(confidence_intervals)) {
  swing_state_info <- confidence_intervals[[state]]  # Access the list for the current state
  if (swing_state_info$SwingState) {  # If SwingState is TRUE
    print(state)
  }
}

confidence_intervals
confidence_intervals_absten

