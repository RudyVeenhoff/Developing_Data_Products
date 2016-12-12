setwd("~/Rudy/Data Science/Projects/Developing Data Products/PA2")
rechtsSpeed <- read.csv("Rechts_Speed_2011.csv")
colnames <- gsub("^X","",colnames(rechtsSpeed))
order<-order(as.numeric(colnames))
SpeedOrdered <- rechtsSpeed[order]

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5){  
  abs(x - round(x)) < tol
}

dayOfYear <- function(n){
  if(n >=1 && n <= 365 && is.wholenumber(n)){
    beginofday <- (n-1)*60*24
    endofday <- n*60*24-1
    SpeedOrdered[beginofday:endofday,]
  }
  else
    stop("Please enter an integer between 1 and 365")
}

fullNameOfDay <- function(n){
  Sys.setlocale("LC_TIME", "English")
  daysof2011 <- seq(as.POSIXct("2011/1/1"),as.POSIXct("2011/12/31"),"day")
  format(daysof2011[n],"%A %d %B %Y")
}

plotHeatmap <- function(n){
  library(plotly)
  x <- list(title = "The 143 measurement points on the A13"  )
  y <- list(title = "Minutes past midnight")
  z <- dayOfYear(n)
  p <- plot_ly(z=as.matrix(dayOfYear(n)),
               type="heatmap") %>% 
       colorbar(title = "Speed in km/h") %>%
       layout(title = fullNameOfDay(n), xaxis = x, yaxis=y)
  p
}
