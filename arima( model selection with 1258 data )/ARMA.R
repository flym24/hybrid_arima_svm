################### Graph One #####################

require(zoo)    #na.trim
require(TTR)    #ROC


#read MSFT and get ROC
MSFT<-read.delim("clipboard")
MSFT.rets<-na.trim(ROC(MSFT$Close,type="discrete",n=1))

#read Citi and get ROC
Citi<-read.delim("clipboard")
Citi.rets<-na.trim(ROC(Citi$Close,type="discrete",n=1))

#read IBM and get ROC
IBM<-read.delim("clipboard")
IBM.rets<-na.trim(ROC(IBM$Close,type="discrete",n=1))

op<-par(mfrow=c(3,3),mar=c(2,4,3,2)+.1)

plot.ts(MSFT$Date,MSFT.rets,main="MSFT",xlab="time",ylab="MSFT ROC",type="l")
acf(MSFT.rets,main="")
pacf(MSFT.rets,main="")

plot.ts(Citi$Date,Citi.rets,main="Citi",xlab="time",ylab="Citi ROC",type="l")
acf(Citi.rets,main="")
pacf(Citi.rets,main="")

plot.ts(IBM$Date,IBM.rets,main="IBM",xlab="time",ylab="IBM ROC",type="l")
acf(IBM.rets,main="")
pacf(IBM.rets,main="")

par(op)




################# ADF Test #################

require(tseries)

#Augmented Dickey–Fuller Test for MSFT
adf.test(MSFT.rets)
#Phillips–Perron Unit Root Test
pp.test(MSFT.rets)

#Augmented Dickey–Fuller Test for Citi
adf.test(Citi.rets)
#Phillips–Perron Unit Root Test
pp.test(Citi.rets)

#Augmented Dickey–Fuller Test for IBM
adf.test(IBM.rets)
#Phillips–Perron Unit Root Test
pp.test(IBM.rets)



################# Model Selection (without seasonal part) ##################
###1###

best.aic <- 1e9
best.model <- NA
	for (p in 0:2) {
		for (q in 0:2) {
			r <- list(aic=NA)
			try(
				r <- arima( MSFT.rets, 
				order=c(p,0,q))
			)
				if (r$aic < best.aic)
				{
				best.aic <- r$aic
				best.model <- r
				}
		}
	}
#显示与AIC相近的ARIMA模型
close_best.models<-NA
	for (p in 0:5) {
		for(q in 0:5) {
			r.new <- arima( MSFT.rets,
			order=c(p,0,q))
			if ((abs(r.new$aic-best.aic)/best.aic)<0.1)
				{
				print(r.new$aic)
				print(r$call)
				}
		}
	}


	
################# Model Selection (with seasonal part) ##################
###2###

best.aic <- 1e9
best.model <- NA
for (p in 0:2) {
  for (d in 0:2) {
    for (q in 0:2) {
      for (P in 0:2) {
        for (D in 0:2) {
          for (Q in 0:2) {
            r <- list(aic=NA)
            try( 
              r <- arima( MSFT.rets, 
                          order=c(p,d,q),
                          list(order=c(P,D,Q), period=12) 
                        )
            )
		if (r$aic < best.aic)
		{
			best.aic <- r$aic
			best.model <- r
		}
          }
        }
      }
    }
  }
}


#################### Model Selection 2 ######################
###3###

a <- array(NA, dim=c(2,2,2,2,2,2))
for (p in 0:2) {
  for (d in 0:2) {
    for (q in 0:2) {
      for (P in 0:2) {
        for (D in 0:2) {
          for (Q in 0:2) {
            r <- list(aic=NA)
            try( 
              r <- arima( MSFT.rets, 
                          order=c(p,d,q),
                          list(order=c(P,D,Q), period=12) 
                        )
            )
            a[p,d,q,P,D,Q] <- r$aic
            cat(r$aic); cat("\n")
          }
        }
      }
    }
  }
}

# When I wrote this, I did not know the "which.min" function.
argmin.vector <- function (v) {
  (1:length(v)) [ v == min(v) ]
}
#x <- sample(1:10)
#x
#argmin.vector(x)
#x <- sample(1:5, 20, replace=T)
#x
#argmin.vector(x)
#x <- array(x, dim=c(5,2,2))

index.from.vector <- function (i,d) {
  res <- NULL
  n <- prod(d)
  i <- i-1
  for (k in length(d):1) {
    n <- n/d[k]
    res <- c( i %/% n, res )
    i <- i %% n
  }
  res+1
}
#index.from.vector(7, c(2,2,2))
#index.from.vector(29, c(5,3,2))

argmin <- function (a) {
  a <- as.array(a)
  d <- dim(a)
  a <- as.vector(a)
  res <- matrix(nr=0, nc=length(d))
  for (i in (1:length(a))[ a == min(a) ]) {
    j <- index.from.vector(i,d)
    res <- rbind(res, j)
  }
  res
}
#x <- array( sample(1:10,30, replace=T), dim=c(5,3,2) )
argmin(a)


################
##By looking at models whose AIC is close to this one
x <- as.vector(a)
d <- dim(a)
o <- order(x)
res <- matrix(nr=0, nc=6+2)
for (i in 1:30) {
  p <- index.from.vector(o[i],d)
  res <- rbind( res, c(p, sum(p), x[o[i]]))
}
colnames(res) <- c("p","d","q", "P","D","Q", "n", "AIC")
res
















