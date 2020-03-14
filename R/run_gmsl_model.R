run_gmsl_model <- function(select_scenario = "Blue",
                          select_year = 2100,
                          survey = "H20")
{

  if(survey == "H20")
  {
    load("data/survey_H20.rda")
    prediction_dat <- survey_H20$prediction_dat 
    n_experts <- survey_H20$n_experts
    
  y_i <- prediction_dat %>% filter(Scenario == select_scenario,Year == paste0(select_year)) %>% 
          mutate(expert = rep(1:n_experts,5)) %>% 
    mutate(trans_y_i = value^(1/3)) %>% 
    filter(trans_y_i != -Inf) %>% 
    drop_na()
  }
  if(survey == "H14")
  {
    load("data/survey_H14.rda")
    prediction_dat <- survey_H14$prediction_dat 
    n_experts <- survey_H14$n_experts
    
    y_i <- prediction_dat %>% filter(Scenario == select_scenario,Year == paste0(select_year)) %>% 
      mutate(expert = rep(1:n_experts,4)) %>% 
      mutate(trans_y_i = value^(1/3)) %>% 
      filter(trans_y_i != -Inf) %>% 
      drop_na()
  }
  
  
  
  n_y <- nrow(y_i)
  expert_index <- y_i$expert

  gmslmodel = "
model{
  
  for(i in 1:n_y)
  {
  y_i[i] ~ dt(tmu[expert_index[i]],sigma_y^-2,4)
  }

  for(j in 1:n_experts)
  {
  mu[j] <- tmu[j]^3
  tmu[j] ~ dnorm(theta_mu,sigma_mu^-2)
  }
  
  
  for(i in 1:1000)
  {
  tpred[i] ~ dnorm(theta_mu,sigma_mu^-2)
  }
  
  sigma_y ~ dt(0, 2.5^-2, 1)T(0,)
  sigma_mu ~ dt(0, 2.5^-2, 1)T(0,)
  theta_mu ~ dnorm(0,0.001)
  
}
"  
  
jags.data <- list(y_i = y_i$trans_y_i, 
                  n_y = n_y, 
                  n_experts = n_experts,
                  expert_index = y_i$expert)
parnames <- c("theta_mu","mu","sigma_mu","sigma_y","tpred")

mod <- jags(data = jags.data, parameters.to.save=parnames, 
            model.file = textConnection(gmslmodel),
            n.iter=18000,
            n.burnin = 10000,
            n.thin=4)

mcmc.samps<-mod$BUGSoutput$sims.list


###Get predictions
pred_i<-array(NA,c(dim(mcmc.samps$tpred)[1],dim(mcmc.samps$tpred)[2]))
for(i in 1:dim(mcmc.samps$tpred)[1])
{
  pred_i[i,] <- sort(mcmc.samps$tpred[i,]^3)
}

suppressWarnings(dir.create("results"))
rsl_pred <- tibble::enframe(apply(pred_i,2,mean))
write_csv(rsl_pred,paste0("results/rsl_pred","_","Survey",survey,"_",select_scenario,"_",select_year,".csv"))
cat(paste0("PDF data for the"," ",select_scenario,"_",select_year, " scenario have been saved to the results folder"))

}



