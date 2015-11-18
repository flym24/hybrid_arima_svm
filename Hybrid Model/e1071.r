svmFeatures = function(series)
{
   require(PerformanceAnalytics)

   close = Cl(series)

   rets = na.trim(ROC(close, type="discrete"))

   # 1-day, 2-day, 3-day, 5-day, 10-day, 20-day and 50-day returns
   res = merge(na.trim(lag(rets, 1)),
               na.trim(lag(ROC(close, type="discrete", n=2), 1)),
               na.trim(lag(ROC(close, type="discrete", n=3), 1)),
               na.trim(lag(ROC(close, type="discrete", n=5), 1)),
               na.trim(lag(ROC(close, type="discrete", n=10), 1)),
               na.trim(lag(ROC(close, type="discrete", n=20), 1)),
               na.trim(lag(ROC(close, type="discrete", n=50), 1)),
               all=FALSE)

   # Add mean, median, sd, mad, skew and kurtosis
   res = merge(res,
               xts(na.trim(lag(rollmean(rets, k=21, align="right"),1))),
               xts(na.trim(lag(rollmedian(rets, k=21, align="right"),1))),
               xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=sd),1))),
               xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=mad),1))),
               xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=skewness),1))),
               xts(na.trim(lag(rollapply(rets, width=21, align="right", FUN=kurtosis),1))),
               all=FALSE)

   # Add volume with a lag of two
   res = merge(res, xts(na.trim(lag(Vo(series),2))), all=FALSE)

   colnames(res) = c("ROC.1", "ROC.2", "ROC.3", "ROC.5", "ROC.10", "ROC.20", "ROC.50",
                     "MEAN", "MEDIAN", "SD", "MAD", "SKEW", "KURTOSIS",
                     "VOLUME")

   return(res)
}
