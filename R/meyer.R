is.missing <- function(x = NULL){
  x <- ifelse(is.na(x) | is.null(x) | is.nan(x), TRUE, FALSE)
  return(x)
}
is.missing.or.infinite <- function(x = NULL){
  x <- ifelse(is.na(x) | is.null(x) | is.nan(x) | is.infinite(x), TRUE, FALSE)
  return(x)
}
meyerize <- function(x = NULL, na.rm = FALSE, inf.rm = FALSE){
  if(length(x)<2) stop('\'x\' must contain multiple values.')
  if(!is.numeric(x)) x <- as.numeric(x)

  if(length(na.rm)>1) na.rm <- na.rm[1]
  if(!is.logical(na.rm)) na.rm <- as.logical(na.rm)

  if(length(inf.rm)>1) inf.rm <- inf.rm[1]
  if(!is.logical(inf.rm)) inf.rm <- as.logical(inf.rm)

  if(na.rm == T & inf.rm == T){
    x <- x[!is.missing.or.infinite(x)]
  }else if(na.rm == T){
    x <- x[!is.missing(x)]
  }else if(inf.rm == T){
    x <- x[!is.infinite(x)]
  }

  x_min <- min(x)
  x_max <- max(x)
  x <- ((x-x_min)/(x_max-x_min))*2
  x_mean <- mean(x)
  x <- x-x_mean
  return(x)
}
meyersDF <- function(n = NULL, s = NULL, bias = 40){
  if(length(n)>1) n <- n[1]
  if(!is.numeric(n)) n <- as.numeric(n)
  if(n<1) stop('\'n\' cannot be smaller than 1.')

  if(length(s)>1) s <- s[1]
  if(!is.numeric(s)) s <- as.numeric(s)
  s <- round(s)
  if(s<1) stop('\'s\' cannot be smaller than 1.')

  if(length(bias)>1) s <- bias[1]
  if(!is.numeric(bias)) bias <- as.numeric(bias)

  df <- n + (log(2 + s^0.5))^sqrt(n) - 40
  df <- round(df)
  df <- ifelse(df<1, 1, df)
  return(df)
}
pooledSE <- function(se = NULL, q = NULL, m = NULL, na.rm = FALSE, inf.rm = FALSE){
  if(length(q)<2) stop('\'q\' must contain multiple values.')
  if(!is.numeric(q)) q <- as.numeric(q)

  if(length(se)<2) stop('\'se\' must contain multiple values.')
  if(length(se)!=length(q) | length(se)!=m) stop('\'se\' and \'q\' must both be of length \'m\'.')
  if(!is.numeric(se)) se <- as.numeric(se)

  if(length(m)>1) m <- m[1]
  if(!is.numeric(m)) m <- as.numeric(m)
  m <- round(m)
  if(m<2) stop('\'m\ cannot be smaller than 2.')

  if(length(na.rm)>1) na.rm <- na.rm[1]
  if(!is.logical(na.rm)) na.rm <- as.logical(na.rm)

  if(length(inf.rm)>1) inf.rm <- inf.rm[1]
  if(!is.logical(inf.rm)) inf.rm <- as.logical(inf.rm)

  if(na.rm == T & inf.rm == T){
    se <- se[!is.missing.or.infinite(se)]
    q <- q[!is.missing.or.infinite(q)]
  }else if(na.rm == T){
    se <- se[!is.missing(se)]
    q <- q[!is.missing(q)]
  }else if(inf.rm == T){
    se <- se[!is.infinite(se)]
    q <- q[!is.infinite(q)]
  }

  if(any(se<0)) stop('\'se\' cannot contain negative values.')

  se <- mean(se^2, na.rm = na.rm)
  q <- var(q, na.rm = na.rm)
  bias <- q/m
  pooled_se <- sqrt(se + q + bias)
  return(pooled_se)
}
statSigLevel <- function(p_values = NULL, rules = 'conventional'){
  if(length(rules)>1) rules <- rules[1]
  if(!is.character(rules)) rules <- as.character(rules)
  if(rules != 'conventional' & rules != 'pseudo') stop('Only supported rules are \'conventional\' and \'pseudo\'.')

  if(!is.numeric(p_values)) p_values <- as.numeric(p_values)
  if(any(p_values<0)) stop('\'p-values\' cannot be negative.')

  iterations <- length(p_values)
  sig_level <- rep(NA, iterations)
  if(rules == 'conventional'){
    for(i in 1:iterations){
      if(!is.missing(p_values[i])){
        if(p_values[i]>0.05){
          sig_level[i] <- ''
        }else if(p_values[i]>0.01){
          sig_level[i] <- '*'
        }else if(p_values[i]>0.001){
          sig_level[i] <- '**'
        }else{
          sig_level[i] <- '***'
        }
      }
    }
    return(sig_level)
  }
  if(rules == 'pseudo'){
    for(i in 1:iterations){
      if(!is.missing(p_values[i])){
        if(p_values[i]>0.1){
          sig_level[i] <- ''
        }else if(p_values[i]>0.05){
          sig_level[i] <- '#'
        }else if(p_values[i]>0.01){
          sig_level[i] <- '*'
        }else if(p_values[i]>0.001){
          sig_level[i] <- '**'
        }else{
          sig_level[i] <- '***'
        }
      }
    }
    return(sig_level)
  }
}
stargaze <- function(p_values = NULL, rules = 'conventional'){
  if(length(rules)>1) rules <- rules[1]
  if(!is.character(rules)) rules <- as.character(rules)
  if(rules != 'conventional' & rules != 'pseudo') stop('Only supported rules are \'conventional\' and \'pseudo\'.')

  if(!is.numeric(p_values)) p_values <- as.numeric(p_values)
  if(any(p_values<0)) stop('\'p-values\' cannot be negative.')

  iterations <- length(p_values)
  sig_level <- rep(NA, iterations)
  if(rules == 'conventional'){
    for(i in 1:iterations){
      if(!is.missing(p_values[i])){
        if(p_values[i]>0.05){
          sig_level[i] <- ''
        }else if(p_values[i]>0.01){
          sig_level[i] <- '*'
        }else if(p_values[i]>0.001){
          sig_level[i] <- '**'
        }else{
          sig_level[i] <- '***'
        }
      }
    }
    return(sig_level)
  }
  if(rules == 'pseudo'){
    for(i in 1:iterations){
      if(!is.missing(p_values[i])){
        if(p_values[i]>0.1){
          sig_level[i] <- ''
        }else if(p_values[i]>0.05){
          sig_level[i] <- '#'
        }else if(p_values[i]>0.01){
          sig_level[i] <- '*'
        }else if(p_values[i]>0.001){
          sig_level[i] <- '**'
        }else{
          sig_level[i] <- '***'
        }
      }
    }
    return(sig_level)
  }
}
pracSigLevel <- function(effect_sizes = NULL, type = 'd', rules = 'cohen1988'){
  if(length(type)>1) type <- type[1]
  if(!is.character(type)) type <- as.character(type)
  if(type != 'd' & type != 'g' & type != 'r' & type != 'eta' & type != 'epsilon' & type != 'omega') stop('only supported types are \'d\', \'g\', \'r\', \'eta\', \'epsilon\', or \'omega\'')

  if(length(rules)>1) rules <- rules[1]
  if(!is.character(rules)) rules <- as.character(rules)
  if(type=='d' | type=='g'){
    if(rules != 'cohen1988' & rules != 'sawilowsky2009' & rules != 'lovakov2021' & rules != 'gignac2016') stop('only supported rules for the selected type are \'cohen1988\', \'sawilowsky2009\', \'lovakov2021\', and \'gignac2016\'')
  }
  if(type=='r' | type=='eta' | type=='epsilon' | type=='omega'){
    if(rules != 'cohen1988' & rules != 'evans1996' & rules != 'lovakov2021' & rules != 'gignac2016' & rules != 'funder2019') stop('only supported rules for the selected type are \'cohen1988\', \'funder2019\', \'lovakov2021\', and \'gignac2016\'')
  }

  if(!is.numeric(effect_sizes)) effect_sizes <- as.numeric(effect_sizes)
  effect_sizes <- abs(effect_sizes)

  iterations <- length(effect_sizes)
  sig_level <- rep(NA, iterations)
  if(type == 'd' | type == 'g'){
    if(rules == 'cohen1988'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.5){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.8){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'sawilowsky2009'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.1){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.5){
            sig_level[i] <- 'III'
          }else if(effect_sizes[i]<0.8){
            sig_level[i] <- 'IV'
          }else if(effect_sizes[i]<1.2){
            sig_level[i] <- 'V'
          }else if(effect_sizes[i]<2){
            sig_level[i] <- 'VI'
          }else{
            sig_level[i] <- 'VII'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'lovakov2021'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.15){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.36){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.65){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'gignac2016'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.41){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.63){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }
  }
  if(type == 'r' | type == 'omega' | type == 'epsilon' | type == 'eta'){
    if(rules == 'cohen1988'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.1){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.3){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.5){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'funder2019'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.05){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.1){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'III'
          }else if(effect_sizes[i]<0.3){
            sig_level[i] <- 'IV'
          }else if(effect_sizes[i]<0.4){
            sig_level[i] <- 'V'
          }else{
            sig_level[i] <- 'VI'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'gignac2016'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.1){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.3){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'lovakov2021'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.12){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.24){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.41){
            sig_level[i] <- 'III'
          }else{
            sig_level[i] <- 'IV'
          }
        }else{
        }
      }
      return(sig_level)
    }else if(rules == 'evans1996'){
      for(i in 1:iterations){
        if(!is.missing(effect_sizes[i])){
          if(effect_sizes[i]==0){
            sig_level[i] <- ''
          }else if(effect_sizes[i]<0.2){
            sig_level[i] <- 'I'
          }else if(effect_sizes[i]<0.4){
            sig_level[i] <- 'II'
          }else if(effect_sizes[i]<0.6){
            sig_level[i] <- 'III'
          }else if(effect_sizes[i]<0.8){
            sig_level[i] <- 'IV'
          }else{
            sig_level[i] <- 'V'
          }
        }else{
        }
      }
      return(sig_level)
    }
  }
}
typeMlevel <- function(typeM = NULL, rules = 'meyer2023'){
  if(length(rules)>1) rules <- rules[1]
  if(!is.character(rules)) rules <- as.character(rules)
  if(rules != 'meyer2023') stop('Only supported rules are \'meyer2023\'.')

  if(!is.numeric(typeM)) typeM <- as.numeric(typeM)
  if(any(typeM<0)) stop('\'typeM\' cannot be negative.')

  typeM_level <- rep(NA, length(typeM))
  if(rules == 'meyer2023'){
    for(i in 1:length(typeM)){
      if(!is.missing(typeM[i])){
        if(typeM[i]>1.05 | typeM[i]<0.95){
          typeM_level[i] <- ''
        }else if(typeM[i]>1.01 | typeM[i]<0.99){
          typeM_level[i] <- '*'
        }else if(typeM[i]>1.001 | typeM[i]<0.999){
          typeM_level[i] <- '**'
        }else{
          typeM_level[i] <- '***'
        }
      }
    }
  }
  return(typeM_level)
}
assessPower <- function(r = NULL, min_detectable_r = NULL, p_value = NULL, alpha = 0.05){
  if(!is.numeric(r)) r <- as.numeric(r)
  if(!is.numeric(min_detectable_r)) r <- as.numeric(min_detectable_r)
  if(!is.numeric(p_value)) r <- as.numeric(p_value)
  if(any(p_value<0)) stop('\'p-value\' cannot be negative.')

  if(!is.numeric(alpha)) r <- as.numeric(alpha)
  if(any(alpha<0)) stop('\'alpha\' cannot be negative.')

  if(length(r)!=length(p_value)) stop('\'r\' and \'p_value\' must be of the same length.')
  if(length(p_value)!=length(alpha) & length(alpha)!=1) stop('The length of \'alpha\' must be one or equal to the length of \'p_value\'.')
  if(length(r)!=length(min_detectable_r) & length(min_detectable_r)!=1) stop('The length of \'min_detectable_r\' must be one or equal to the length of \'r\'.')

  r <- abs(r)
  min_detectable_r <- abs(min_detectable_r)

  r_length <- length(r)
  power_assessment <- rep(NA, r_length)
  for(i in 1:r_length){
    if(!is.missing(r[i])){
      if(r[i]<ifelse(length(min_detectable_r)>1, min_detectable_r[i], min_detectable_r) & p_value[i]<=ifelse(length(alpha)>1, alpha[i], alpha)){
        power_assessment[i] <- '#'
      }else if(r[i]>=ifelse(length(min_detectable_r)>1, min_detectable_r[i], min_detectable_r) & p_value[i]>ifelse(length(alpha)>1, alpha[i], alpha)){
        power_assessment[i] <- '#'
      }else{
        power_assessment[i] <- ''
      }
    }
  }
  return(power_assessment)
}
criticalValue <- function(alpha = NULL, df = Inf, alternative = 'two.tailed'){
  if(length(alternative)>1) alternative <- alternative[1]
  if(!is.character(alternative)) alternative <- as.character(alternative)
  if(alternative!='two.tailed') stop

  if(length(df)!=length(alpha) & length(df)!=1) stop('The length of \'df\' must be 1 or equal to the length of \'alpha\'.')
  if(!is.numeric(df)) df <- as.numeric(df)
  df <- round(df)
  if(any(df<1)) stop('\'df\' must be positive.')

  if(!is.numeric(alpha)) alpha <- as.numeric(alpha)
  if(any(alpha<0)) stop('\'alpha\' cannot be negative.')

  critical_value <- rep(NA, length(alpha))
  for(i in 1:length(alpha)){
    if(!is.missing.or.infinite(alpha[i])){
      critical_value[i] <- qt(1-alpha[i]/2, ifelse(length(df)==1, df[1], df[i]))
    }
  }
  return(critical_value)
}
pValue <- function(t_stat = NULL, df = Inf, alternative = 'two.tailed', correction = FALSE){
  if(length(alternative)>1) alternative <- alternative[1]
  if(!is.character(alternative)) alternative <- as.character(alternative)
  if(alternative!='two.tailed') stop('Only \'alternative\' supported is \'two.tailed\'.')

  if(length(df)!=length(t_stat) & length(df)!=1) stop('The length of \'df\' must be 1 or equal to the length of \'t_stat\'.')
  if(!is.numeric(df)) df <- as.numeric
  df <- round(df)
  if(any(df<1)) stop('\'df\' must be positive.')

  if(!is.numeric(t_stat)) t_stat <- as.numeric(t_stat)

  if(length(correction)>1) correction <- correction[1]
  if(!is.logical(correction)) correction <- as.logical(correction)

  p_value <- rep(NA, length(t_stat))
  for(i in 1:length(t_stat)){
    if(!is.missing(t_stat[i])){
      p_value[i] <- 2*pt(-abs(t_stat[i]),df=ifelse(length(df)==1, df[1], df[i]))
    }
  }
  if(correction==T){
    double.xmin <- .Machine$double.xmin
    ifelse(p_value<double.xmin & abs(t_stat)!=Inf, double.xmin, p_value)
  }
  return(p_value)
}
ci <- function(x = NULL, se = NULL, alpha = 0.05, df = Inf){
  # X
  if(length(x)!=length(se)) stop('\'x\' and \'se\' must be of the same length.')
  if(!is.numeric(x)) x <- as.numeric(x)

  # SE
  if(!is.numeric(se)) se <- as.numeric(se)
  if(any(se<0)) stop('\'se\' cannot contain negative values.')

  # Alpha
  if(length(alpha)!=1 & length(alpha)!=length(x)) stop('Length of \'alpha\' must be 1 or equal to x.')
  if(!is.numeric(alpha)) alpha <- as.numeric(alpha)
  if(any(alpha<0)) stop('\'alpha\' cannot contain negative values.')

  # DF
  if(length(df)!=1 & length(df)!=length(x)) stop('Length of \'df\' must be 1 or equal to x.')
  if(!is.numeric(df)) df <- as.numeric(df)
  if(any(df<1)) stop('\'df\' must be positive.')

  length_alpha <- length(alpha)
  critical_value <- rep(NA, length_alpha)
  for(i in 1:length_alpha){
    if(!is.missing(alpha[i])){
      critical_value[i] <- criticalValue(alpha[i], ifelse(length(df)>1, df[i], df))
    }
  }

  x_length <- length(x)
  ci <- matrix(nrow = x_length, ncol=2, dimnames = list(c(1:x_length), c('Lower', 'Upper')))
  for(i in 1:x_length){
    if(!is.missing(x[i]) & !is.missing(se[i]) & !is.missing(ifelse(length(critical_value)>1, critical_value[i], critical_value))){
      ci[i,1] <- x[i]-se[i]*ifelse(length(critical_value)>1, critical_value[i], critical_value)
      ci[i,2] <- x[i]+se[i]*ifelse(length(critical_value)>1, critical_value[i], critical_value)
    }

  }
  return(ci)
}
