Sys.setenv(TZ="UTC")
setwd("D:/Recherche/SM2RAIN/")

format_pval <- function(pval){
  pval <- scales::pvalue(pval, accuracy= 0.001, add_p = TRUE)
  gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
}

import.rainfall <- function(products) {
  
  ldaily <- ldekad <- lmonthly <- lannual <- sapply(products,function(x) NULL)
  
  for (product in products) {
    
    print(paste0("Importing rainfall from: ",product))
    daily.df <- read.csv(paste0("EXP/",product,"_PP.csv"), header = TRUE, sep = ",", dec = ".")
    colnames(daily.df) <- c("Date", stations$Name)
    daily.df$Date <- as.POSIXct(daily.df$Date,format="%Y-%m-%d",tz="UTC")
    ldaily[[product]] <- daily.df
    
    dekad.df <- daily.df
    dekad.df$dekad <- ceiling((as.numeric(strftime(dekad.df$Date, format="%d", tz="UTC")))/10)
    dekad.df$dekad[dekad.df$dekad == 4] <- 3
    dekad.df$Date <- paste0(strftime(dekad.df$Date,format="%Y-%m",tz="UTC"),"-0",dekad.df$dekad)
    dekad.df$dekad <- NULL
    dekad.df <- aggregate(.~Date, dekad.df,FUN=sum, na.action = na.pass)
    ldekad[[product]] <- dekad.df
    
    monthly.df <- daily.df
    monthly.df$Date <- strftime(monthly.df$Date,format="%Y-%m",tz="UTC")
    monthly.df <- aggregate(.~Date, monthly.df,FUN=sum, na.action = na.pass)
    lmonthly[[product]] <- monthly.df
    
    annual.df <- daily.df
    annual.df$Date <- strftime(annual.df$Date,format="%Y",tz="UTC")
    annual.df <- aggregate(.~Date, annual.df,FUN=sum, na.action = na.pass)
    lannual[[product]] <- annual.df    
  }

  return (list(ldaily = ldaily,
               ldekad = ldekad,
               lmonthly = lmonthly,
               lannual = lannual))
}

flatten <- function(pr, timescale, products, stations) {
  pr <- pr
  ll <- pr[[timescale]]
  snames <- stations$Name
  ll.out <- sapply(products, function(x) NULL)
  for (p in products) {
    df <- ll[[p]][,2:ncol(ll[[p]])]
    ll.out[[p]] <- stack(df)$values
  }
  return (ll.out)
}

eval_ths <- function(pr, ths, lab.ths, fdiv, fun) {
  cc <- data.frame(matrix(nrow = length(pr), ncol = length(ths)+1))
  cc[,1] <- names(pr)
  colnames(cc) <-c("products", as.character(ths))
  
  for (p in names(pr)) {
    prv <- pr[[p]]
    prv[prv == 0] <- NA
    xs <- c()
    for (th in ths) xs <- append(xs, fun(which(prv<=th)))
    ffdiv <- ifelse(p == "CCI", 9*18,ifelse(p == "GPM", 12*18,fdiv))
    xs <-c(xs[1],diff(xs))/ffdiv
    cc[,2:ncol(cc)][which(cc$products== p),] <- xs
  }  
  
  df <- stack(cc[,2:ncol(cc)])
  df$ind <- rep(lab.ths, each = length(names(pr)))
  df$model <- rep(names(pr), n = length(ths))
  
  return (df)
  
}

average_product <- function(pr,i,j) {
  products <- names(pr)
  dates <- pr[["OBS"]]$Date
  df <- data.frame(matrix(nrow=length(dates), ncol=length(products)+1))
  colnames(df) <- c("Date",products)
  df$Date <- substr(dates,i,j) #sub("(.{8})(.*)", "\\1d\\2", dates)
  for (product in products) {
    #product <- "OBS"
    values = rowMeans(pr[[product]][,2:ncol(pr[[product]])], na.rm=T)
    df[,product] <- round(values,1)
  }
  return (df)
}

monthly_rainfall <- function(pr) {
  pps <- names(pr)
  df <- data.frame(matrix(nrow=0, ncol=3))
  colnames(df) <- c("model", "values", "ind")
  for (p in pps) {
    #p <- "OBS"
    ddf <- pr[[p]]
    ddf$Date <- rep(1:12,14)
    ddf <- round(aggregate(.~Date,ddf,FUN=mean),1)
    ddf <- ddf[,2:ncol(ddf)]
    ddf <- data.frame(model=p,stack(ddf))
    df <- rbind(df,ddf)
  }
  return (df)
}

annual_normals <- function(pr) {
  pps <- names(pr)
  df <- data.frame(matrix(nrow=ncol(pr$OBS)-1, ncol=length(pps)+1))
  colnames(df) <- c("Name",pps)
  df$Name <- colnames(pr$OBS)[-1]
  for (p in pps) {
    #p <- "OBS"
    ddf <- pr[[p]]
    df[,p] <- colMeans(ddf[,2:ncol(ddf)], na.rm=T)
  }
  return (df)
}

fmt <- function(x, n) {
  return (formatC(x = x, digits = n, format = "f"))
}
