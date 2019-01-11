rm(list=ls())

###


libraries = c("xts","dplyr","RcppRoll","tidyverse", "tidyquant", "TTR",'xtable','tseries')
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


monthly = FALSE


# log return statistics ---------------------------------------------------
dd = read.csv(paste0('data_cc_daily_log_returns.csv', sep=';'))
tmp = xts(x = dd[,-1],as.Date(dd[,1]))
tmp = tmp[index(tmp) >= "2017-05-01"]
tmp = tmp[,!grepl(pattern = 'VIX',colnames(tmp))]
if(monthly)
  tmp = apply.monthly(tmp,mean, na.rm=TRUE)

#dplyr::select(tmp, "date")
tseries::jarque.bera.test(tmp)

jb = sapply(tmp,function(x){
  c('Jarque-Bera statistic' = as.numeric(jarque.bera.test(x)[1]),
    'P-value' = as.numeric(jarque.bera.test(x)[3])
  )
})

jb = t(jb)


table = print.xtable(xtable(jb,digits=c(0,4,4,2,2,2,2)),hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl)))
table = print.xtable(xtable(jb,digits=4,hline.after=c(-1,-1,0,nrow(tbl),nrow(tbl))))
write(table,paste0('jarque_bera_log_return_',ifelse(monthly,'monthly','daily'),'_latex.txt'))
