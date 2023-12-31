# This is a short description of the experience that I have in R.

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# I have used R for:
# - Econometric time series: GARCH-type models, ARIMA models, realized volatility, Brownian motions, VAR models, State-Space models, network analysis
# - Monte Carlo Simulation: Generating random variables, numerical integration, Bayesian statistics, hidden Markov models, Markov Chain Monte Carlo,
# - Machine learning/Statistical leanrning: (Spectral) Cluster analysis, linear/quadratic discriminant analysis, kernel methods, kernel density estimation
# - Data processing & data analysis: Very broad, but many different types of data supplied by clients in my job as consultant.

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Books I have read

# "R for Data Science" (2016), by Garrett Grolemund and Hadley Wickham - https://r4ds.had.co.nz/
# "Advanced R, Second Edition" (2019), by Hadley Wickham - https://adv-r.hadley.nz/
# "The R Inferno" (2012), by Patrick Burns - https://www.burns-stat.com/pages/Tutor/R_inferno.pdf

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Short overview of some of the packages I have experience with

# - I have used the "Tidyverse" quite alot, particularly the "dplyr", "ggplot2", and "tibble" packages. --- https://www.tidyverse.org/
# - As some of work I have used R for and some of models I have implemented have been quite
# - Since I have used R for some heavy computations and models, I have sometimes sped up the calculations by coding parts in C++, 
# and then integrating the C++ using the "Rcpp" and "RcppArmadillo" packages --- https://www.rcpp.org/, https://cran.r-project.org/web/packages/RcppArmadillo/index.html
# - Another package I have used to sped up calculations is the "parallel" package --- https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf
# - R-Markdown --- https://rmarkdown.rstudio.com/
# - S3 --- https://adv-r.hadley.nz/s3.html

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# The code presented here is part of the code that I wrote for master's thesis.

CreateSpec <- function(Model, iDist = "norm", K, OptimMethod = "Optim", Scaling = 0.5, UseGASP = FALSE, dNP = 10, EMOnly = F) {
  
  lOut <- list()
  
  if(K == 1) {
    Model = c("sGARCH")
    iDist = c("norm")
    iType = c("sGARCH")
    do.mix = FALSE
    class(lOut) <- "DAMM"
  } else {
    do.mix = switch(Model, "DAMM" = TRUE, "MSGAS" = FALSE)
    chrDist = switch(iDist, "norm" = "G", "std" = "t")
    iType = "sGARCH"
    class(lOut) <- Model
    Model = paste0(chrDist, Model) 
  }
  lOut[["Scaling"]] <- Scaling
  lOut[["Model"]] <- Model
  lOut[["K"]] <- K
  lOut[["iDist"]] <- iDist
  lOut[["iType"]] <- iType
  lOut[["do.mix"]] <- do.mix
  lOut[["Optim_Method"]] <- OptimMethod
  lOut[["UseGASP"]] <- UseGASP
  lOut[["dNP"]] <- dNP
  lOut[["EMOnly"]] <- EMOnly
  return(lOut)
}

const List& Spec, const int& iT, const List& lP, const List& StartValues

MixSim <- function(spec, iT, lP, StartValues) {
  UseMethod(generic = "MixSim", spec)
  }

MixSim.DAMM <- function(spec, iT, lP, StartValues) {
  MixSim_DAMM(spec, iT, lP, StartValues)
}

MixSim.MS <- function(spec, iT, lP, StartValues) {
  MixSim_MS(spec, iT, lP, StartValues)
}

f_MLFit <- function(vY, Spec) {
  
  K <- Spec[["K"]]
  Model <- Spec[["Model"]]
  iType <- Spec[["iType"]]
  iDist <- Spec[["iDist"]]
  do.mix <- Spec[["do.mix"]]
  EMOnly <- Spec$EMOnly
  OptimMethod <- Spec$Optim_Method
  
  if(K > 1){
    #Find the starting values
    StartValues <- f_Starting_values(vY, Spec, EMOnly = EMOnly)
    Fit <- f_optimizer(vY, Spec, StartValues)
    
    if(do.mix) {
      while(abs(Fit$lPn$wKappa) > 1.5) {
        Fit <- f_optimizer(vY, Spec, StartValues)
      }
    }
    
  } else {
    return(0)
  }
  
  lPn <- Fit$lPn
  
  if(!do.mix) {
    lPn[["mGamma"]] <- f_Gamma(lPn$vGamma, K)
  }
  
  lOut <- list()
  if(OptimMethod == "DEoptim") {
    lOut[["llk"]] <- -Fit$Optimed$optim$bestval
    lOut[["Bestpop"]] <- Fit$Optimed$member$pop
  } else {
    lOut[["llk"]] <- -Fit$Optimed$value
  }
  
  lOut[["StartValues"]] <- Fit$StartValues
  lOut[["lPn"]] <- lPn
  lOut[["Spec"]] <- Spec
  lOut[["vY"]] <- vY
  
  return(lOut)
}


MixGas <- function(spec, vY, lPn, StartVal) {
  UseMethod(generic = "MixGAS", spec)
  }

MixGas.DAMM <- function(spec, vY, lPn, StartVal) {
  MixGas_DAMM(spec, vY, lPn, StartVal)
}

MixGas.MS <- function(spec, vY, lPn, StartVal) {
  MixGas_MS(spec, vY, lPn, StartVal)
}

fn_LLK <- function(vPw, vY, Spec, StartVal, vNames) {
  vPw <- setNames(vPw, vNames)
  do.mix = Spec$do.mix
  iT <- length(vY)
  K <- Spec$K
  iDist <- Spec$iDist
  Optim_method <- Spec$Optim_Method
  mLogU = matrix(NA, iT, K)
  lPw <- f_v2l(vPw, Spec)
  iType <- Spec$iType
  
  dLLK <- 0
  
  if(Spec$Optim_Method != "DEoptim") {
    lPn <- f_lPw2lPn(lPw, Spec)
  }
  
  if(Spec$iDist == "norm") {
    lPn[["vNu"]] <- rep(0,K)
  }
  

  if(do.mix) {
    lPn[["wA"]] <- diag(K-1)*c(lPn$wA)
    lPn[["wB"]] <- diag(K-1)*c(lPn$wB)
    
  } else {
    lPn <- f_lPw2lPn(lPw, Spec)
    lPn[["mGamma"]] <- f_Gamma(lPn$vGamma, K)
  }
  
  Filtered_data <- MixGAS(Spec, vY, lPn, StartVal)
  dLLK <- Filtered_data$llk
  
  if (!is.finite(dLLK)) {
    dLLK <- -1e+10
  }
  
  return(-dLLK)
}


f_optimizer <- function(vY, Spec, StartValues) {
  
  K <- Spec$K
  do.mix <- Spec$do.mix
  iDist <- Spec$iDist
  OptimMethod <- Spec$Optim_Method
  dNP <- Spec$dNP
  iType <- Spec$iType
  
  if(do.mix) {
    
    #Set StartValues
    lPn0 <- list()
    lPn0[["wKappa"]] <- setNames(rep(-0.04, (K-1)), c("wKappa_1"))
    lPn0[["wA"]] <- setNames(rep(2.25, (K-1)), c("wA_1"))
    lPn0[["wB"]] <- setNames(rep(0.95, (K-1)), c("wB_1"))
    
    lPn0[["vKappa"]] <- (StartValues$Start_Par_GARCH[,1])
    lPn0[["vA"]] <- StartValues$Start_Par_GARCH[,2]
    lPn0[["vB"]] <- StartValues$Start_Par_GARCH[,3]
    lPn0[["vMu"]] <- StartValues$vMu_start[1:(K-1)]
    
    lPn0[["vKappa"]] <- setNames(c(-0.0015, -0.0020), c("vKappa_1", "vKappa_2"))
    lPn0[["vA"]] <- setNames(c(0.004, 0.09), c("vA_1", "vA_2"))
    lPn0[["vB"]] <- setNames(c(0.97, 0.96), c("vB_1", "vB_2"))
    lPn0[["vMu"]] <- setNames(StartValues$vMu_start[1:(K-1)], c("vMu_1")) 
    
    if(iDist == "std") {
      lPn0[["vNu"]] <- StartValues$vNu
    }
    
    #Next unmap them
    lPw <- f_lPn2lPw(lPn0, Spec)
    vPw <- f_l2v(lPw, Spec)

    StartSigma2W <- list()
    StartSigma2W[["vSigma2_start"]] <- StartValues$vSigma2_start
    StartSigma2W[["vW_start"]] <- StartValues$vW_start
    
    vNames <- names(vPw)
    
    if(OptimMethod == "DEoptim") {
    
      #clusterExport(MyCluster, c("f_v2l", "f_lPw2lPn", "f_map", "wA_Upper", "vA_Lower", "vA_Upper", "vB_Lower", "MixGAS", "foo"))
      MyCluster <- makeCluster(parallel::detectCores() - 1)
      LowerLimit <- c(rep(-0.0999, (K-1)), rep(0, (K-1)), rep(0, (K-1)), rep(-0.999, K), rep(0, K), rep(0, K), rep(-2, (K-1)))
      UpperLimit <- c(rep(0.0999, (K-1)), rep(10, (K-1)), rep(0.9999, (K-1)), rep(0.999, K), rep(5, K), rep(0.9999, K), rep(2, (K-1)))
      
      if(iDist == "std") {
        LowerLimit <- c(LowerLimit, rep(2.1, K))
        UpperLimit <- c(UpperLimit, rep(vNu_Upper(), K))
      }
      
      clusterExport(MyCluster, c("f_v2l", "f_lPw2lPn", "f_map", "wA_Upper", "vA_Lower", "vA_Upper", "vB_Lower", "vB_Upper", "MixGAS", 
                                 "getLLK", "vNu_Upper" ,"lPw2lPn", "wKappa_Lower", "wKappa_Upper"))
      tmp_optim <- DEoptim::DEoptim(fn_LLK, lower = LowerLimit, upper = UpperLimit, 
                                    DEoptim.control(NP = dNP*length(vPw), F = 0.8, CR = 0.9, itermax = 200, cluster = MyCluster),
                                    vY = vY, Spec = Spec, vNames = vNames, StartVal = StartSigma2W)
      lPn <- f_v2lw2ln(setNames(tmp_optim$optim$bestmem, vNames), Spec)
      stopCluster(MyCluster)
      
    } else {
      tmp_optim <- solnp(vPw, fn_LLK,  vY = vY, Spec = Spec, StartVal = StartSigma2W, vNames = vNames)
      lPn <- f_v2lw2ln(tmp_optim$pars, Spec)
      
    }
    
    OutPut <- tmp_optim
  } else {
    
    lPn0 <- list()
    
    lPn0[["mGamma"]] <- StartValues$mGamma_start
    lPn0[["vGamma"]] <- setNames(c(t(lPn0[["mGamma"]][, -K])), f_GammaParNames(K))
    
    lPn0[["vKappa"]] <- (StartValues$Start_Par_GARCH[,1])
    lPn0[["vA"]] <- StartValues$Start_Par_GARCH[,2]
    lPn0[["vB"]] <- StartValues$Start_Par_GARCH[,3]
    
    lPn0[["vKappa"]] <- setNames(c(-0.0015, -0.0020), c("vKappa_1", "vKappa_2"))
    lPn0[["vA"]] <- setNames(c(0.004, 0.09), c("vA_1", "vA_2"))
    lPn0[["vB"]] <- setNames(c(0.97, 0.96), c("vB_1", "vB_2"))
    
    if(iDist == "std") {
      lPn0[["vNu"]] <- StartValues$vNu
    }
    
    #Next unmap them
    lPw <- f_lPn2lPw(lPn0, Spec)
    vPw <- f_l2v(lPw, Spec)
    
    #Time-varying parameter start values
    StartSigma2W <- list()
    StartSigma2W[["vSigma2_start"]] <- StartValues$vSigma2_start
    
    vNames <- names(vPw)
    
    if(OptimMethod == "DEoptim") {
      MyCluster <- makeCluster(parallel::detectCores() - 1)
      clusterExport(MyCluster, c("f_v2l", "f_lPw2lPn", "f_map", "wA_Upper", "vA_Lower", "vA_Upper", "vB_Lower", "vB_Upper", "MSGAS", "getLLK", 
                                 "f_GammaParNames", "f_mapGamma", "f_Gamma", "vNu_Upper"))

      LowerLimit <- c(rep(-0.0999, K), rep(0, K), rep(0, K), rep(-15, K*(K-1)))
      UpperLimit <- c(rep(0.0999, K), rep(5, K), rep(0.9999, K), rep(15, K*(K-1)))
      
      if(iDist == "std") {
        LowerLimit <- c(LowerLimit, rep(2.1, K))
        UpperLimit <- c(UpperLimit, rep(vNu_Upper(), K))
      }
      
      tmp_optim <- DEoptim::DEoptim(fn_LLK, lower = LowerLimit, upper = UpperLimit,
                                    DEoptim.control(NP = dNP*length(vPw), F = 0.8, CR = 0.9, itermax = 200, cluster = MyCluster),
                                    vY = vY, Spec = Spec, vNames = vNames, StartVal = StartSigma2W)
      stopCluster(MyCluster)
      lPn <- f_v2lw2ln(setNames(tmp_optim$optim$bestmem, vNames), Spec)
      
    } else {
      tmp_optim <- solnp(vPw, fn_LLK,  vY = vY, Spec = Spec, StartVal = StartSigma2W, vNames = vNames)
      lPn <- f_v2lw2ln(tmp_optim$pars, Spec)
    }
    
    OutPut <- tmp_optim
  }
  
  lOut <- list()
  lOut[["Optimed"]] <- tmp_optim
  lOut[["lPn"]] <- lPn
  lOut[["StartValues"]] <- StartSigma2W
  
  return(lOut)
}





