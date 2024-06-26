#-----------------------------------------------------------------------------------------#
#                                                                                         #
#                                 data prep functions                                     #
#                                                                                         #
#-----------------------------------------------------------------------------------------#

require("ggplot2")


read.ices.from.disk <- function(dat_extension, stock_input_path){
  return(read.ices(file.path(stock_input_path, dat_extension)))
}

trim_age <- function(stock_data, min_age, plus_group){
  for(i in seq_along(stock_data)){
    unit <- "numbers"
    if(grepl("sw",names(stock_data)[i])){unit <- "weight"}
    if(grepl("dw",names(stock_data)[i])){unit <- "weight"}
    if(grepl("cw",names(stock_data)[i])){unit <- "weight"}
    if(grepl("lw",names(stock_data)[i])){unit <- "weight"}
    if(grepl("mo",names(stock_data)[i])){unit <- "rate"}
    if(grepl("nm",names(stock_data)[i])){unit <- "rate"}
    
    dat <- stock_data[[i]]
    
    if(class(dat) == "matrix"){
      idx_lw <- which(as.numeric(colnames(dat)) < min_age)
      if(length(idx_lw) > 0){dat <- dat[,-idx_lw]} #{dat[idx_lw,] <- NA}
      stock_data[[i]] <- dat
      dat <- stock_data[[i]] 
      if(any(as.numeric(colnames(dat)) > plus_group)){
        idx                 <- which(as.numeric(colnames(dat)) >= plus_group)
        add_to_plus_group   <- numeric(nrow(dat))
        if( unit == "numbers" ){ add_to_plus_group  <- rowSums(dat[,idx], na.rm = TRUE) }
        if( unit != "numbers" ){ add_to_plus_group  <- rowMeans(dat[,idx], na.rm = TRUE) }
        dat[,colnames(dat) == plus_group]       <- add_to_plus_group
        dat                                     <- dat[,!as.numeric(colnames(dat)) > plus_group]
        stock_data[[i]]                         <- dat# remove older species
      }
    }
  }
  return(stock_data)
}

get_survey_table <- function(tuning_list){
  survey_table <- do.call("rbind",lapply(seq_along(tuning_list),function(x,dat){
    attr   <- attributes(dat[[x]])
    name   <- names(dat)[x]
    type   <- unique(ifelse((attr$dimnames[[2]] == -1), "biomass", "age-based"))
    years  <- paste(unique(range(as.numeric(attr$dimnames[[1]]))),collapse = "-")
    age    <- paste(unique(range(as.numeric(attr$dimnames[[2]]))),collapse = "-")
    return(c(name, type, years, age))
  }, dat = tuning_list))
  survey_table            <- as.data.frame(survey_table)
  names(survey_table)     <- c("name","type","years","ages")
  return(survey_table)
}

min_max_year <- function(stock_list){
  for(i in seq_along(stock_list)){
    if(i == 1){
      ranges <- c("minYear" = NA, "maxYear" = NA)
    }
    stock_dat <- stock_list[[i]]
    if(class(stock_dat) == "matrix"){
      ranges[1] <- min(ranges[1],range(as.numeric(rownames(stock_dat)))[1], na.rm = TRUE)
      ranges[2] <- max(ranges[2],range(as.numeric(rownames(stock_dat)))[2], na.rm = TRUE)
    }
    if(class(stock_dat) == "list"){
      for(j in seq_along(stock_dat)){
        tun_dat <- stock_dat[[j]]
        if(class(tun_dat) == "matrix"){
          ranges[1] <- min(ranges[1],range(as.numeric(rownames(tun_dat)))[1], na.rm = TRUE)
          ranges[2] <- max(ranges[2],range(as.numeric(rownames(tun_dat)))[2], na.rm = TRUE)
        }
      }
    }
  }
  return(ranges)
}

min_max_age <- function(stock_list){
  for(i in seq_along(stock_list)){
    if(i == 1){
      ranges <- c("minYear" = NA, "maxYear" = NA)
    }
    stock_dat <- stock_list[[i]]
    if(class(stock_dat) == "matrix"){
      ranges[1] <- min(ranges[1],range(as.numeric(colnames(stock_dat)))[1], na.rm = TRUE)
      ranges[2] <- max(ranges[2],range(as.numeric(colnames(stock_dat)))[2], na.rm = TRUE)
    }
    if(class(stock_dat) == "list"){
      for(j in seq_along(stock_dat)){
        tun_dat <- stock_dat[[j]]
        if(class(tun_dat) == "matrix"){
          ranges[1] <- min(ranges[1],range(as.numeric(colnames(tun_dat)))[1], na.rm = TRUE)
          ranges[2] <- max(ranges[2],range(as.numeric(colnames(tun_dat)))[2], na.rm = TRUE)
        }
      }
    }
  }
  return(ranges)
}

#-----------------------------------------------------------------------------------------#
#                                                                                         #
#                                 STF                                                     #
#                                                                                         #
#-----------------------------------------------------------------------------------------#

getN <- function(x){
  idx <- fit$conf$keyLogFsta[1,]+1
  nsize <- length(idx)
  ret <- exp(x[1:nsize])
  ret
}

getF <- function(x, allowSelOverwrite=FALSE,customSel = NULL){
  idx <- fit$conf$keyLogFsta[1,]+1
  nsize <- length(idx)
  ret <- exp(x[nsize+idx])
  ret[idx==0] <- 0
  if(allowSelOverwrite){
    if(!is.null(overwriteSelYears)){
      thisfbar<-ret[age]
      ret<-fixedsel*thisfbar
    }
    if(!is.null(customSel)){
      thisfbar<-ret[age]
      ret<-customSel*thisfbar
    }
  }
  ret
}
