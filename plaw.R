require(VGAM)

modccdfCal <- function(xmin, raw_dat){
  dif <- raw_dat$value - xmin
  u <- which(dif > 0)[1]
  l <- max(u - 1, 1)
  x_dif <- raw_dat$value[l] - raw_dat$value[u]
  y_dif <- raw_dat$ccdf[l] - raw_dat$ccdf[u]
  scale <- raw_dat$ccdf[l] - y_dif*(xmin - 
                                      raw_dat$value[l])/x_dif
  
  val <- ifelse(raw_dat$value >= xmin, raw_dat$value, NA)
  a <- alphaCal(xmin, raw_dat)
  mod <- scale*(val/xmin)^(1-a)
  
  return(mod)
}

modpdfCal <- function(xmin, raw_dat){
  dif <- raw_dat$value - xmin
  u <- which(dif > 0)[1]
  l <- max(u - 1, 1)
  x_dif <- raw_dat$value[l] - raw_dat$value[u]
  y_dif <- raw_dat$ccdf[l] - raw_dat$ccdf[u]
  scale <- raw_dat$ccdf[l] - y_dif*(xmin - 
                                      raw_dat$value[l])/x_dif
  
  val <- ifelse(raw_dat$value >= xmin, raw_dat$value, NA)
  a <- alphaCal(xmin, raw_dat)
  mod <- scale*(a-1)/xmin*(val/xmin)^-a
  
  return(mod)
}

modcdfCal <- function(xmin, raw_dat){
  dif <- raw_dat$value - xmin
  u <- which(dif > 0)[1]
  l <- max(u - 1, 1)
  x_dif <- raw_dat$value[l] - raw_dat$value[u]
  y_dif <- raw_dat$ccdf[l] - raw_dat$ccdf[u]
  scale <- raw_dat$ccdf[l] - y_dif*(xmin - 
                                      raw_dat$value[l])/x_dif
  
  val <- ifelse(raw_dat$value >= xmin, raw_dat$value, NA)
  a <- alphaCal(xmin, raw_dat)
  mod <- scale*(a-1)/xmin*(val/xmin)^-a
  
  return(mod)
}

alphaCal <- function(xmin, raw_dat){
  p_val <- raw_dat$value[raw_dat$value >= xmin]
  p_freq <- raw_dat$frequency[raw_dat$value >= xmin]
  xmin <- min(p_val)
  alpha <- 1 + (sum(p_freq)/(sum(p_freq*log(p_val)) - 
                               sum(p_freq)*log(xmin-0.5)))
  
  return(alpha)
}

ksCal <- function(xmin, raw_dat){
  alpha <- alphaCal(xmin, raw_dat)
  
  c <- zeta(alpha) - sum((1:(xmin-1))^-alpha)
  if (c == 0)
    dat_cdf <- rep(0, length(raw_dat$value))
  else
    dat_cdf <- cumsum((xmin:max(raw_dat$value))^-alpha)/c
  
  dat <- with(raw_dat, c(mapply(rep, value, frequency), 
                         recursive=TRUE))
  occur <- tabulate(dat)
  if (xmin > 1)
    occur <- occur[-(1:(xmin-1))]
  
  mod_cdf <- cumsum(occur/sum(occur))
  ks <- max(abs(dat_cdf- mod_cdf), na.rm=TRUE)
  
  return(ks)
}