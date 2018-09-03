rm(list=ls())

# set parameters for plots ------------------------------------------------

colors = c("red3","blue3", "darkorchid3","goldenrod2", "chartreuse4", "steelblue", "pink")
names(colors) = c('GLD', "XRP", "LTC", "ETH", 'VIX',"OEX", "BTC")
print(colors)
# TRUE = monthly data, FALSE= daily
monthly = TRUE

# --------------------------------------------------------------

libraries = c("xts","dplyr","RcppRoll","tidyverse", "tidyquant", "TTR",'xtable')
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



dd = read.csv('data_cc_daily_log_returns.csv')
d = xts(x = dd[,-1],as.Date(dd[,1]))

# table overall correlations ----------------------------------------------
tmp = d[index(d) >= "2012-01-01"]

# Time series Closing Prices in USD of BTC, GOLD, SP 100 ----------------------------------------------------------

dd = read.csv(paste0('data_cc_daily_prices.csv'))
tmp = xts(x = dd[,-1],as.Date(dd[,1]))
tmp = tmp[index(tmp) >= "2012-01-01"]
# tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
# tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
if(monthly)
  tmp = apply.monthly(tmp, mean, na.rm=TRUE)
nam = names(tmp)
nam = sort(nam[nam %in% c('BTC','GLD','OEX')])
filename = paste('CL',ifelse(monthly,'monthly','daily'),paste(nam, collapse = '_'),'.pdf',sep = '_')
pdf(filename, family = 'Times')
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "Closing prices in USD",
    ylab = "Price",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(na.omit(tmp[,j])), type="l",lwd = 6,col = colors[j])
  }
dev.off()

# Time series Closing Prices in USD of BTC, XRP, LTC, ETH, GOLD, SP 100 ----------------------------------------------------------
nam = names(tmp)
nam = sort(nam[!nam %in% c('VIX')])
filename = paste('CL',ifelse(monthly,'monthly','daily'),paste(nam, collapse = '_'),'.pdf',sep = '_')
pdf(filename, family = 'Times')
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "Closing prices in USD",
    ylab = "Price",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(na.omit(tmp[,j])), type="l",lwd = 6,col = colors[j])
  }
dev.off()

# Time series Closing Prices in USD of XRP, LTC, ETH, GOLD, SP 100 --------
nam = names(tmp)
nam = sort(nam[!nam %in% c('VIX','BTC')])
filename = paste('CL',ifelse(monthly,'monthly','daily'),paste(nam, collapse = '_'),'.pdf',sep = '_')
pdf(filename, family = 'Times')
  plot(
    zoo(tmp[,nam][,1]), 
    type = "n",
    main = "Closing prices in USD",
    ylab = "Price",
    xlab = "Time",
    ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
  )
  
  for (j in names(tmp[,nam])){
    lines(zoo(na.omit(tmp[,j])), type="l",lwd = 6,col = colors[j])
  }
dev.off()



# rolling window ----------------------------------------------------------
for (window in c(100,250)){
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
        n          = window,
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
  
  nam = names(tmp)
  nam2 = gsub('rolling_corr_','',nam)
  nam2 = gsub('_BTC','',nam2)
  nam = sort(nam[!nam %in% c('BTC','VIX')])

  filename = paste('RunCor',window,ifelse(monthly,'monthly','daily'),paste(sort(nam2), collapse = '_'),'.pdf',sep = '_')
  pdf(filename, family = 'Times')
    
    plot(
      zoo(tmp[,nam][,1]), 
      type = "n",
      main = paste(window,"days Rolling Window Correlation to BTC"),
      ylab = "Correlation",
      xlab = "Time",
      ylim = range(tmp[,nam],na.rm = T)#c(0,4700)
    )
    
    for (j in names(tmp[,nam])){
      jc = nam2 = gsub('rolling_corr_','',j)
      jc = gsub('_BTC','',jc)
      lines(zoo(na.omit(tmp[,j])), type="l",lwd = 4,col = colors[jc])
    }
  dev.off()
}

# rolling window (100d) Standard Deviation of BTC, XRP, LTC, ETH,  --------
nam = names(d)
nam = sort(nam[!nam %in% c('VIX')])
for (window in c(100,250)){
  tb = as.tibble(d) %>% mutate(date = date(d)) %>% select(nam,'date')
  for (i in colnames(tb)){
    if (i == 'date')
      next
    x = i
    y = tb %>% 
      select(x,'date') %>% 
      na.omit() %>% 
      tq_mutate_(mutate_fun = 'runSD', n = window, col_rename = paste0("rolling_sd_",i))
    tb = tb %>% left_join(y)
  }
  tb = tb %>% select(tb %>% colnames() %>% grep('rolling_sd_',.),'date')
  colnames(tb) = tb %>% colnames() %>% gsub('rolling_sd_','',.)
  
  
  tmp = xts(x = tb %>% select(-'date'),as.POSIXct(as.matrix(tb %>% select('date'))))
  tmp = tmp[!apply(is.na(tmp),1,all),]

  filename = paste('RunSD',window,ifelse(monthly,'monthly','daily'),paste(sort(nam), collapse = '_'),'.pdf',sep = '_')
  pdf(filename, family = 'Times')
    plot(
      zoo(tmp[,nam[1]]),
      type = "n",
      main = paste(window,"days Rolling Window Standard Deviation"),
      ylab = "Standard Deviation",
      xlab = "Time",
      ylim = range(tmp[,nam],na.rm = T),
      xlim=c("2016-01-01", "2018-08-24") 
    )
    for (j in names(tmp[,nam])){
      lines(zoo(na.omit(tmp[,j])), type="l",lwd = 6,col = colors[j])
    }
  dev.off()
}


# correlation table -------------------------------------------------------
  dat_name = 'log_returns' # 'prices'
  dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  ct_daily = cor(tmp, use = "pairwise.complete.obs" )
  # monthly
  ct_monthly = cor(apply.monthly(tmp,mean, na.rm=TRUE), use = "pairwise.complete.obs")

# Daily Correlation -------------------------------------------------------
  s = print.xtable(xtable(ct_daily),hline.after=c(-1,-1,0,nrow(ct_daily),nrow(ct_daily)))
  write(s,'correlation_daily_latex.txt')
  
# Monthly Correlation -----------------------------------------------------
  s = print.xtable(xtable(ct_monthly),hline.after=c(-1,-1,0,nrow(ct_monthly),nrow(ct_monthly)))
  write(s,'correlation_monthly_latex.txt')
  

# log return statistics ---------------------------------------------------
  dd = read.csv(paste0('data_cc_daily_log_returns.csv'))
  tmp = xts(x = dd[,-1],as.Date(dd[,1]))
  tmp = tmp[index(tmp) >= "2012-01-01"]
  tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
  if(monthly)
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
  s = print.xtable(xtable(tbl,digits=c(0,4,4,2,2,2,2)),hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl)))
  s = print.xtable(xtable(tbl,digits=4,hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl))))
  write(s,paste0('statistics_log_return_',ifelse(monthly,'monthly','daily'),'_latex.txt'))
  


# QQ plots ----------------------------------------------------------------

  for (dat_name in c('log_returns','prices')){
    dd = read.csv(paste0('data_cc_daily_',dat_name,'.csv'))
    tmp = xts(x = dd[,-1],as.Date(dd[,1]))
    tmp = tmp[index(tmp) >= "2012-01-01"]
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
  