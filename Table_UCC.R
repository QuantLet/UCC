rm(list=ls())
# TABLE = OHNE VIX
# DATA = DAILY, sonst BTC over 9000
# SET MONTHLY = TRUE
# TABLE DATA FILE = statistics_log_return_monthly_latex.txt 
# TABLE STATISTICS = SET MONTHLY TRUE, USE statistics_log_return_monthly_latex.txt


# set parameters for plots ------------------------------------------------

colors = c("red3","blue3","goldenrod2", "chartreuse4", "steelblue", "black")
names(colors) = c('GLD', "XRP", "ETH", 'VIX',"SP500", "BTC")

print(colors)
# TRUE = monthly data, FALSE= daily
monthly = TRUE

# --------------------------------------------------------------

libraries = c("xts","dplyr","RcppRoll","tidyverse", "tidyquant", "TTR",'xtable')
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



dd = read.csv('data_cc_daily_log_returns.csv')
d0 = xts(x = dd[,-1],as.Date(dd[,1]))


colnames(d0)
d <- d0[, c(5, 3, 2, 1, 4)]
colnames(d)



# table overall correlations ----------------------------------------------
# ETH Genesis Block 2015 30. July 2015
# XRP liquid mid 2017
tmp = d[index(d) >= "2017-05-01"]

# Time series Closing Prices in USD of BTC, GOLD, SP 500 ----------------------------------------------------------


# correlation table -------------------------------------------------------
# correlation table -------------------------------------------------------
# correlation table -------------------------------------------------------
  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[, c(6, 3, 2, 1, 4, 5)]
  tmp = tmp[index(tmp) >= "2017-05-01"]
  ct_daily = cor(tmp, use = "pairwise.complete.obs" )
  # monthly
  ct_monthly = cor(apply.monthly(tmp,mean, na.rm=TRUE), use = "pairwise.complete.obs")

# Daily Correlation -------------------------------------------------------
# Daily Correlation -------------------------------------------------------
# Daily Correlation -------------------------------------------------------
  #ind = !colnames(ct_daily) %in% 'VIX'
  ind = colnames(ct_daily)
  s = print.xtable(xtable(ct_daily[ind,ind]),hline.after=c(-1,-1,0,nrow(ct_daily[ind,ind]),nrow(ct_daily[ind,ind])))
  write(s,'correlation_daily_latex.txt')
  
# Monthly Correlation -----------------------------------------------------
# Monthly Correlation -----------------------------------------------------
# Monthly Correlation -----------------------------------------------------
  #ind = !colnames(ct_daily) %in% 'VIX'
  ind = colnames(ct_daily)
  s = print.xtable(xtable(ct_monthly[ind,ind]),hline.after=c(-1,-1,0,nrow(ct_monthly[ind,ind]),nrow(ct_monthly[ind,ind])))
  write(s,'correlation_monthly_latex.txt')
  

# log return statistics ---------------------------------------------------
# set monthly to true
  dd = read.csv(paste0('data_cc_daily_log_returns.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2017-05-01"]
  tmp = tmp[, c(5, 3, 2, 1, 4)]
  tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
  if(monthly)
    tmp = apply.monthly(tmp,mean, na.rm=TRUE)
  tbl = sapply(tmp,function(x){
    c('Mean' = mean(x, na.rm = T),
      'Std. Dev.' = sd(x, na.rm = T),
      'Skewness' = skewness(x, na.rm = T),
      'Kurtosis' = kurtosis(x, na.rm = TRUE, excess = TRUE, unbiased = TRUE),
      'Min.' = min(x, na.rm = T),
      'Max.' = max(x, na.rm = T)
    )
  })
  tbl = t(tbl)
  s = print.xtable(xtable(tbl,digits=c(0,4,4,2,2,2,2)),hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl)))
  s = print.xtable(xtable(tbl,digits=4,hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl))))
  write(s,paste0('statistics_log_return_',ifelse(monthly,'monthly','daily'),'_latex.txt'))
  


# QQ plots ----------------------------------------------------------------

  #  for (dat_name in c('log_returns','prices')){

  for (dat_name in c('log_returns')){
    dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
    tmp = xts(x = dd[,-1],as.Date(dd[,1]))
    tmp = tmp[index(tmp) >= "2017-05-01"]
    if(monthly)
      tmp = apply.monthly(tmp,mean, na.rm=TRUE)
    for (j in names(tmp)){
      u = unlist((tmp[, j] - mean(tmp[, j], na.rm = T)) / sd(tmp[, j], na.rm = T))
      p = qqnorm(u, xlim = c(-3,3), ylim = c(-3,3))
      pdf(paste0('qqnorm_',dat_name,'_',j,'_',ifelse(monthly,'monthly','daily'),'.pdf'), family = 'Times')
        plot(
          p,
          col = "steelblue", 
          main = paste0("Standard Normal QQ plot for ",j," returns"),
          xlim = c(-3,3),
          ylim = c(-3,3),
          xlab = 'Theoretical Quantiles',
          ylab = 'Sample Quantiles',
          asp = 1
        )
        abline(0, 1)
      dev.off()
    }
  }
  
  
  

# Cointtest ---------------------------------------------------------------

  library(urca)
  library(vars)
  
  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2017-05-01"]
  tmp = apply.monthly(tmp,mean, na.rm=TRUE)
  
  dat = na.omit(tmp[,c('GLD','BTC')])
  #select lag
  VARselect(dat, lag.max = 5, type = "const") # produces lag suggestions
  VARselect(dat, lag.max = 5, type = "const")$selection
  
  #K=AIC lag// Eigen test
  cointest = ca.jo(dat, K=5, type = "eigen", ecdet="const", spec="transitory")
  summary(cointest)
  
  
  dat = na.omit(tmp[,c('GLD','BTC','SP500','LTC','XRP','ETH')])
  #select lag
  VARselect(dat, lag.max = 10, type = "const") # produces lag suggestions
  VARselect(dat, lag.max = 10, type = "const")$selection
  
  #K=AIC lag// Eigen test
  cointest = ca.jo(dat, K=6, type = "eigen", ecdet="const", spec="transitory")
  summary(cointest)
  