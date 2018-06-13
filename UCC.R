setwd("~/Documents/LvB/R/20180401 R2")

# --------------------------------------------------------------

libraries = c("xts","dplyr","RcppRoll","tidyverse", "tidyquant", "TTR")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list=ls())

dd = read.csv('data_cc_daily_log_returns.csv')
d = xts(x = dd[,-1],as.Date(dd[,1]))

# table overall correlations ----------------------------------------------
tmp = d[index(d) >= "2012-01-01"]
# daily 
cor(tmp, use = "pairwise.complete.obs" )
# monthly
cor(apply.monthly(tmp,mean, na.rm=TRUE), use = "pairwise.complete.obs")

# rolling window ----------------------------------------------------------
#install.packages("tidyverse")
#install.packages("tidyverse", dependencies=TRUE)
#install.packages("DBI", dependencies=TRUE)
#library(DBI)




tb = as.tibble(d) %>% mutate(date = date(d))
tab = combn(1:ncol(d),2)
tb = na.omit(tb)
# runCor = function(x,y,n){
#   tmp = cor(x,y,use        = "pairwise.complete.obs")
#   return(reclass(tmp,x))
# }
for (i in 1:ncol(tab)){
  x = colnames(d)[tab[1,i]]
  y = colnames(d)[tab[2,i]]
  tb = tb %>% 
    tq_mutate_xy_(
      x          = x,
      y          = y,
      mutate_fun = "runCor", 
      n          = 250,
      # use        = "complete.obs",
      # na.rm = T,
      # use        = "pairwise.complete.obs",
      # tq_mutate args
      col_rename = paste0("rolling_corr_",colnames(d)[tab[1,i]],"_",colnames(d)[tab[2,i]])
    )
}

tb2 = na.omit(tb)
tb2 = tb2[(ncol(d)+1):ncol(tb2)]
tmp = xts(x = tb2[,-1],as.POSIXct(as.matrix(tb2[,1])))
tmp = tmp[,grep("BTC",colnames(tmp))]



# Closing values ----------------------------------------------------------

dd = read.csv(paste0('data_cc_daily_prices.csv'))
tmp = xts(x = dd[,-1],as.Date(dd[,1]))
tmp = tmp[index(tmp) >= "2012-01-01"]
tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
colors = c("red3","blue3", "darkorchid3","goldenrod2", "chartreuse4", "steelblue", "pink")
names(colors) = c('GLD', "XRP", "LTC", "ETH", 'VIX',"OEX", "BTC")
tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]

tmp = apply.monthly(tmp,mean, na.rm=TRUE)

pdf('CL_monthly_BTC_GLD_SP100.pdf', family = 'Times')
  nam = names(tmp)
  nam = nam[nam %in% c('BTC','GLD','OEX')]
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "100 days Rolling Window Standard Deviation",
    ylab = "Standard Deviation",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(tmp[,j]), type="l",lwd = 3,col = colors[j])
  }
dev.off()

pdf('CL_monthly_CCs.pdf', family = 'Times')
  nam = names(tmp)
  nam = nam[!nam %in% c('GLD','OEX')]
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "100 days Rolling Window Standard Deviation",
    ylab = "Standard Deviation",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(tmp[,j]), type="l",lwd = 3,col = colors[j])
  }
dev.off()
pdf('CL_monthly_CCs_trad_wo_BTC.pdf', family = 'Times')
  nam = names(tmp)
  nam = nam[!nam %in% c('BTC')]
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "100 days Rolling Window Standard Deviation",
    ylab = "Standard Deviation",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(tmp[,j]), type="l",lwd = 3,col = colors[j])
  }
dev.off()




# correlation table -------------------------------------------------------
  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  ct_daily = cor(tmp, use = "pairwise.complete.obs" )
  # monthly
  ct_monthly = cor(apply.monthly(tmp,mean, na.rm=TRUE), use = "pairwise.complete.obs")
  
  library(xtable)
  xtable(ct_daily)
  xtable(ct_monthly)


# running sd ! REMOVE DIFF(LOG)--------------------------------------------------------------
for (dat_name in c('log_returns','prices')){
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  d = xts(x = dd[,-1],as.Date(dd[,1]))
  
  tb = as.tibble(d) %>% mutate(date = date(d))
  tb = na.omit(tb)
  for (i in colnames(d)){
    tb = tb %>% 
      tq_mutate_xy_(
        x          = i,
        mutate_fun = "runSD", 
        # na.rm      = TRUE,
        n          = 100,
        col_rename = paste0("rolling_sd_",i)
      )
  }
  tb2 = na.omit(tb)
  tb2 = tb2[(ncol(d)+1):ncol(tb2)]
  tmp = xts(x = tb2[,-1],as.POSIXct(as.matrix(tb2[,1])))
  colors = c("red3","blue3", "darkorchid3","goldenrod2", "chartreuse4", "steelblue", "pink")
  names(colors) = c('GLD', "XRP", "LTC", "ETH", 'VIX',"OEX", "BTC")
  
  colnames(tmp) = gsub('rolling_sd_','',colnames(tmp))
  tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
  
  
  
  # rolling SD plot with BTC -----------------------------------------------------------------
  pdf(paste0('rw_sd_w_btc','_',dat_name,'.pdf'), family = 'Times')
    plot(
      zoo(tmp[,1]), 
      type = "n",
      main = "100 days Rolling Window Standard Deviation",
      ylab = "Standard Deviation",
      xlab = "Time",
      ylim = range(tmp)#c(0,4700)
    )
    for (j in names(tmp)){
      lines(zoo(tmp[,j]), type="l",lwd = 3,col = colors[j])
    }
  dev.off()
  
  
  # rolling SD plot without BTC ---------------------------------------------
  
  nam = names(tmp)
  nam = nam[!nam %in% c('BTC')]
  
  pdf(paste0('rw_sd_wo_btc','_',dat_name,'.pdf'), family = 'Times')
    plot(
      zoo(tmp[,nam][,1]), 
      type = "n",
      main = "100 days Rolling Window Standard Deviation",
      ylab = "Standard Deviation",
      xlab = "Time",
      ylim = range(tmp[,nam])
    )
    
    for (j in nam){
      lines(zoo(tmp[,j]), type="l",lwd = 3,col = colors[j])
    }
  dev.off()
}


# log return statistics ---------------------------------------------------
  dd = read.csv(paste0('data_cc_daily_log_returns.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
  tmp = apply.monthly(tmp,mean, na.rm=TRUE)
  
  
  tbl = sapply(tmp,function(x){
    c('Mean' = mean(x, na.rm = T),
      'Std. Dev.' = sd(x, na.rm = T),
      'Skewness' = skewness(x, na.rm = T),
      'Kurtosis' = kurtosis(x, na.rm = T),
      'Min.' = min(x, na.rm = T),
      'Max.' = max(x, na.rm = T)
    )
  })
  tbl = t(tbl)
  xtable(tbl, digits = 4)
  
  


# QQ plots ----------------------------------------------------------------

  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
  
  for (j in names(tmp)){
    u = unlist((tmp[, j] - mean(tmp[, j], na.rm = T)) / sd(tmp[, j], na.rm = T))
    p = qqnorm(u, xlim = c(-3,3), ylim = c(-3,3))
    pdf(paste0('qqnorm_',j,'.pdf'), family = 'Times')
      plot(
        p,
        col = "steelblue", 
        main = paste0("Standard Normal QQ plot for ",j," returns"),
        xlim = c(-3,3),
        ylim = c(-3,3),
        xlab = 'Theoretical Quantiles',
        ylab = 'Sample Quantiles'
      )
      abline(0, 1)
    dev.off()
  }
  
  
  

# Cointtest ---------------------------------------------------------------

  library(urca)
  library(vars)
  
  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  tmp = apply.monthly(tmp,mean, na.rm=TRUE)
  
  dat = na.omit(tmp[,c('GLD','BTC')])
  #select lag
  VARselect(dat, lag.max = 10, type = "const") # produces lag suggestions
  VARselect(dat, lag.max = 10, type = "const")$selection
  
  #K=AIC lag// Eigen test
  cointest = ca.jo(dat, K=6, type = "eigen", ecdet="const", spec="transitory")
  summary(cointest)
  
  
  dat = na.omit(tmp[,c('GLD','BTC','OEX','LTC','XRP','ETH')])
  #select lag
  VARselect(dat, lag.max = 10, type = "const") # produces lag suggestions
  VARselect(dat, lag.max = 10, type = "const")$selection
  
  #K=AIC lag// Eigen test
  cointest = ca.jo(dat, K=6, type = "eigen", ecdet="const", spec="transitory")
  summary(cointest)
  