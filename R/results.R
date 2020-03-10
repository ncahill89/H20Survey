plot_PDF <- function(select_year = 2100,
                     select_scenario = "Blue",
                     survey = "H20")
{
  if(select_year == 2100&&survey == "H20")
  {
  blue_hadj = 2.8
  red_hadj = 1.7
  vadj = 0.07
  xticks = 0.2
  bar_ends =0.05
  }
  
  if(select_year == 2100&&survey == "H14")
  {
    blue_hadj = 2.3
    red_hadj = 1.7
    vadj = 0.07
    xticks = 0.2
    bar_ends =0.05
  }
  
  if(select_year == 2300 && survey=="H20")
  {
  blue_hadj = 0.7
  red_hadj = 0.3
  vadj = 0.03
  xticks = 2
  bar_ends = 0.02
  }
  if(select_year == 2300 && survey=="H14")
  {
    blue_hadj = 0.7
    red_hadj = 0.5
    vadj = 0.03
    xticks = 2
    bar_ends = 0.02
  }

  rsl_pred <- read_csv(paste0("results/rsl_pred","_","Survey",survey,"_",select_scenario,"_",select_year,".csv"))
  rsl_pred <- rsl_pred %>% mutate(value = value/100)
 
  get_den <- internal_get_density(rsl_pred = rsl_pred)

  ##Plot a Hist of the data with density
  if(select_scenario == "Blue")
  {
  p<-ggplot(rsl_pred,aes(x=value))+
    geom_line(data=get_den$den_dat,aes(x=x,y=y),colour = "dodgerblue")+
    geom_errorbarh(aes(xmax = get_den$ile_95, xmin = get_den$ile_5, y = blue_hadj,height = bar_ends), colour = "dodgerblue")+
    geom_errorbarh(aes(xmax = get_den$ile_83, xmin = get_den$ile_17, y = blue_hadj,height = bar_ends), colour = "dodgerblue")+
    annotate("text",size = 2, x=c(get_den$ile_5,get_den$ile_17,get_den$ile_83, get_den$ile_95), y =c(blue_hadj-vadj,blue_hadj+vadj,blue_hadj-vadj,blue_hadj+vadj),label =round(c(get_den$ile_5,get_den$ile_17,get_den$ile_83, get_den$ile_95),2))
  }
  
  if(select_scenario == "Red")
  {
    p<-ggplot(rsl_pred,aes(x=value))+
    geom_line(data=get_den$den_dat,aes(x=x,y=y),colour = "firebrick")+
    geom_errorbarh(aes(xmax = get_den$ile_95, xmin = get_den$ile_5, y = red_hadj,height = bar_ends), colour = "firebrick")+
    geom_errorbarh(aes(xmax = get_den$ile_83, xmin = get_den$ile_17, y = red_hadj,height = bar_ends), colour = "firebrick")+
    annotate("text",size = 2, x=c(get_den$ile_5,get_den$ile_17,get_den$ile_83, get_den$ile_95), y = c(red_hadj-vadj,red_hadj+vadj,red_hadj-vadj,red_hadj+vadj),label =round(c(get_den$ile_5,get_den$ile_17,get_den$ile_83, get_den$ile_95),2))
  }
  p <- p + 
    theme_bw()+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title  =element_text(size=9))+
    scale_x_continuous(breaks = round(seq(0, max(get_den$den_dat$x), by = xticks),1)) +
    xlab("Anticipated Global Mean Sea-Level Rise (m)")+
    ylab("Probability Density")
  
   return(p)
}

get_exceedence_prob <- function(select_year = 2100,
                                select_scenario = "Blue",
                                survey = "H20",
                                gmsl = 0.98)
{
  rsl_pred <- read_csv(paste0("results/rsl_pred","_","Survey",survey,"_",select_scenario,"_",select_year,".csv"))
  rsl_pred <- rsl_pred %>% mutate(value = value/100)
  
  get_den <- internal_get_density(rsl_pred = rsl_pred)

  perc=ecdf(rsl_pred$value)
  
  cat(paste0("Under the ",select_scenario," scenario, the probability of gmsl being ",gmsl," or above in ",select_year, " is ", 1- perc(gmsl)))
  
}

internal_get_density <- function(rsl_pred)
{
  ##Get density for the DS samples in 2030 in AFG
  get_den<-density(rsl_pred$value,kernel=("gaussian"),
                   from = min(rsl_pred$value),
                   to = max(rsl_pred$value)
  )
den_dat<-data.frame(x=get_den$x,y=get_den$y)

##Get target based on 90th percentile of the density
y_dscr <- get_den$y/sum(get_den$y)
N<-length(y_dscr)

for(i in 1:(N-1)){
  cum_sum<-sum(y_dscr[1:(N-i)])
  index_95<-N-i
  if(cum_sum<=0.95) break;
}

for(i in 1:(N-1)){
  cum_sum<-sum(y_dscr[1:(N-i)])
  index_5<-N-i
  if(cum_sum<=0.05) break;
}

for(i in 1:(N-1)){
  cum_sum<-sum(y_dscr[1:(N-i)])
  index_17<-N-i
  if(cum_sum<=0.17) break;
}

for(i in 1:(N-1)){
  cum_sum<-sum(y_dscr[1:(N-i)])
  index_83<-N-i
  if(cum_sum<=0.83) break;
}

for(i in 1:(N-1)){
  cum_sum<-sum(y_dscr[1:(N-i)])
  index_50<-N-i
  if(cum_sum<=0.5) break;
}

ile_95<-get_den$x[index_95]
ile_5<-get_den$x[index_5]

ile_17<-get_den$x[index_17]
ile_83<-get_den$x[index_83]

ile_50<-get_den$x[index_50]

return(list(ile_5 = ile_5,
            ile_17 = ile_17,
            ile_50 = ile_50,
            ile_83 = ile_83,
            ile_95 = ile_95,
            den_dat = den_dat))
}