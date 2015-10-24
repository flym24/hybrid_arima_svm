#read table and get ROC
table <- read.table("/Users/Pyrena/Documents/Computer\ Science/Big\ Data/Advanced\ Big\ Data\ Analytics/HW/HW2/0\ final_arima_coding/table.txt", header = T)


to_zoo_format <- function(company.raw)
{
    z <- zoo( cbind(	company.Open=company.raw$Open,
    company.High=company.raw$High,
    company.Low=company.raw$Low,
    company.Close=company.raw$Close,
    company.Volume=company.raw$Volume,
    company.Adjusted=company.raw$Adj_C),
    as.Date(company.raw$Date) )
    
    ret <- as.xts(z)
    
    ret
}

table2 <- to_zoo_format(table)

table <- table2

table.close<-Cl(table)
table.rets <- na.trim(ROC(table.close,type = "discrete",n = 1))

