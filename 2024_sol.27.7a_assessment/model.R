## Run analysis, write model results


## Before: sol7a_data.RData (SAM data object)
## After: SAM_fit_sol_7a.RData (SAM fit object)


mkdir("model")

## Read SAM data object
load('data/sol7a_data.RData')


## Parametrisation

# Create a default parameter configuration object
conf <- defcon(dat.sol7a)

# //1//biomass or catch survey for the tuning series (catches = -1; survey = -1)
conf$keyBiomassTreat <- c(-1,-1,0)

# //2//define the fbar range
conf$fbarRange       <- c(3,7)

# //3//correlation between F-at-age
# Correlation of fishing mortality across ages (0 independent, 1 compound symmetry, or 2 AR(1)
conf$corFlag         <- 2          

# //4//number of parameters describing F-at-age
# conf$keyLogFsta[1,] <- c(0, 1, 2, 3, 4, 4, 4, 4)

# //5//number of parameters in the suryey processes
# conf$keyLogFpar[2,] <- c(0, 1, 2, 3, 3, 3, 3,-1)
# conf$keyLogFpar[3,] <- c(4, -1, -1, -1, -1, -1, -1, -1)


# //6//variance of parameters on F
# use a single parameter!!!
# coupling of process variance parameters for log(F)-process (nomally only first row is used)   
conf$keyVarF[1,]   

# //7//variance parameters on the observations
##Based on variance plots from full decoupling by age
conf$keyVarObs[1,1:10] <- c(0,1,2,3,3,4,4,4,4,4)
conf$keyVarObs[2,1:7]   <- max(conf$keyVarObs[1,]) + c(1,2,2,2,3,3,4)                # max(conf$keyVarObs[1,]) + 1 #c (5,6,6,8,8) #
conf$keyVarObs[3:3,1]   <- (max(conf$keyVarObs[2,]) +1)

# //8//correlation at age between observations
conf$obsCorStruct    <- factor(c("AR","ID","ID"), levels = c("ID","AR","US")) 

# Coupling of correlation parameters can only be specified if the AR(1) structure is chosen above.
# NA's indicate where correlation parameters can be specified (-1 where they cannot).
conf$keyCorObs[1,]   <- 0 #NA #c(0,0,0,1,1,1,1,1,1)
#conf$keyCorObs[1,]   <- 0:(length(conf$keyCorObs[1,])-1) 

## Fit the model
par                  <- defpar(dat.sol7a , conf)
SAM_fit_sol_7a      <- sam.fit(dat.sol7a ,conf , par)

# Convergence checks
SAM_fit_sol_7a$opt$convergence
SAM_fit_sol_7a$opt$message

# Save SAM fit object to the model directory
save(SAM_fit_sol_7a, file="model/SAM_fit_sol_7a.RData")

AIC(SAM_fit_sol_7a)


