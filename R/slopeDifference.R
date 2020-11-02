#' @title Simple Slope Significance and Difference for 3-Way Interactions following Dawson & Richter (2006)
#'
#' @description This function passes an 'lm' object with predictor and moderators tests slope difference
#'
#' @param model
#'
#' @return dat.frame
#'
#' @examples slopeDifference(model, xvar, modvars)
#'
#' @export
#'

slopeDifference <- function(
  model, xvar, modvars){
  if(missing(model))
    stop("slopeTest requires a fitted lm model.")
  if(missing(xvar))
    stop('Please indicate your predictor variable.')
  else if(!is.character(xvar)) xvar <- as.character(substitute(xvar))
  if(missing(modvars))
    stop("Please indicate your moderator variables.")
  else if(!is.character(modvars[1])) modvars[1] <- as.character(substitute(modvars[1]))
  else if(!is.character(modvars[2])) modvars[2] <- as.character(substitute(modvars[2]))
  
  `%notin%` <- Negate(`%in%`)
  varNames <- colnames(model$model)
  mod1 <- modvars[1]
  if(xvar %notin% varNames)
    stop('Predictor variable is missing!')
  if(length(modvars) != 2)
    stop('Please indicate 2 moderator variables.')
  if(modvars[1] %notin% varNames)
    stop('Moderator variables are missing!')
  if(modvars[2] %notin% varNames)
    stop('Moderator variables are missing!')
  mod1 <- modvars[1]
  mod2 <- modvars[2]
  
  # getting unstandardized regression coefficients for var1, var2 and var3 as well as intercept
  coefs.model <- coef(model)
  coef.int <- coefs.model[1]
  coef.xvar <- coefs.model[xvar]
  coef.mod1 <- coefs.model[mod1]
  coef.mod2 <- coefs.model[mod2]
  coef.xvarmod1 <- coefs.model[paste(xvar, mod1, sep = ':')]
  coef.xvarmod2 <- coefs.model[paste(xvar, mod2, sep = ':')]
  coef.mod1mod2 <- coefs.model[paste(mod1, mod2, sep = ':')]
  coef.xvarmod1mod2 <- coefs.model[ifelse(paste(xvar,mod1, mod2, sep = ':') %in% names(coefs.model),paste(xvar,mod1, mod2, sep = ':'),paste(xvar,mod2, mod1, sep = ':') )]
  
  # create High and Low values [1 std above and below mean] for moderators
  HighMod1 <- mean(model$model[, mod1]) + sd(model$model[, mod1])
  LowMod1 <- mean(model$model[, mod1]) - sd(model$model[, mod1])
  HighMod2 <- mean(model$model[, mod2]) + sd(model$model[, mod2])
  LowMod2 <- mean(model$model[, mod2]) - sd(model$model[, mod2])
  
  # get variances of coefficients
  variances <- vcov(model)
  variance.xvar <- variances[xvar,xvar]
  variance.xvarmod1 <- variances[paste(xvar,mod1,sep = ':'),paste(xvar,mod1,sep = ':')]
  variance.xvarmod2 <- variances[paste(xvar,mod2,sep = ':'),paste(xvar,mod2,sep = ':')]
  variance.xvarmod1mod2 <- variances[ifelse(paste(xvar,mod1,mod2,sep = ':')%in% colnames(variances),paste(xvar,mod1,mod2,sep = ':'),paste(xvar,mod2,mod1,sep = ':')),
                                     ifelse(paste(xvar,mod1,mod2,sep = ':')%in% colnames(variances),paste(xvar,mod1,mod2,sep = ':'),paste(xvar,mod2,mod1,sep = ':'))]
  
  # get covariances of coefficients
  covariance.xvarWITHxvarmod1 <- variances[xvar, paste(xvar,mod1,sep = ':')]
  covariance.xvarWITHxvarmod2 <- variances[xvar, paste(xvar,mod2,sep = ':')]
  covariance.xvarWITHxvarmod1mod2 <- variances[xvar, ifelse(paste(xvar,mod1,mod2,sep = ':') %in% colnames(variances),paste(xvar,mod1,mod2,sep = ':') ,paste(xvar,mod2,mod1,sep = ':'))]
  covariance.xvarmod1WITHxvarmod2 <- variances[paste(xvar,mod1, sep = ':'), paste(xvar,mod2,sep = ':')]
  covariance.xvarmod1WITHxvarmod1mod2 <- variances[paste(xvar, mod1, sep = ':'),ifelse(paste(xvar,mod1,mod2,sep = ':')%in% colnames(variances),paste(xvar,mod1,mod2,sep = ':') ,paste(xvar,mod2,mod1,sep = ':'))]
  covariance.xvarmod2WITHxvarmod1mod2 <- variances[paste(xvar, mod2, sep = ':'), ifelse(paste(xvar,mod1,mod2,sep = ':')%in% colnames(variances),paste(xvar,mod1,mod2,sep = ':'),paste(xvar,mod2,mod1,sep = ':'))]
  # get degrees of freedom
  df.model <- model$df.residual
  
  
  # get the slope differences 
  sd1vs2 <- (coef.xvarmod2*(HighMod2 - LowMod2) + coef.xvarmod1mod2*HighMod1*(HighMod2 - LowMod2))
  sd1vs3 <- (coef.xvarmod1*(HighMod1 - LowMod1) + coef.xvarmod1mod2*HighMod2*(HighMod1 - LowMod1))
  sd1vs4 <- (coef.xvarmod1*(HighMod1 - LowMod1) + coef.xvarmod2*(HighMod2 - LowMod2) + coef.xvarmod1mod2*(HighMod1*HighMod2 - LowMod1*LowMod2))
  sd2vs3 <- (coef.xvarmod1*(HighMod1 - LowMod1) + coef.xvarmod2*(LowMod2 - HighMod2) + coef.xvarmod1mod2*(HighMod1*LowMod2 - LowMod1*HighMod2))
  sd2vs4 <- (coef.xvarmod1*(HighMod1 - LowMod1) + coef.xvarmod1mod2*LowMod2*(HighMod1 - LowMod1))
  sd3vs4 <- (coef.xvarmod2*(HighMod2 - LowMod2) + coef.xvarmod1mod2*LowMod1*(HighMod2 - LowMod2))
  
  
  # get the t-values for the slope differences
  # 1 vs 2
  tval.sd1vs2 <- sd1vs2/(sqrt(variance.xvarmod2*(HighMod2 - LowMod2)*(HighMod2 - LowMod2) + 
                                HighMod1*HighMod1*(HighMod2 - LowMod2)*(HighMod2 - LowMod2)*variance.xvarmod1mod2 +
                                2*HighMod1*(HighMod2 - LowMod2)*(HighMod2 - LowMod2)*covariance.xvarmod2WITHxvarmod1mod2))
  # 1 vs 3
  tval.sd1vs3 <- sd1vs3/(sqrt(variance.xvarmod1*(HighMod1 - LowMod1)*(HighMod1 - LowMod1) + 
                                HighMod2*HighMod2*(HighMod1 - LowMod1)*(HighMod1 - LowMod1)*variance.xvarmod1mod2 + 
                                2*HighMod2*(HighMod1 - LowMod1)*(HighMod1 - LowMod1)*covariance.xvarmod1WITHxvarmod1mod2))
  # 1 vs 4
  tval.sd1vs4 <- sd1vs4/(sqrt((HighMod1 - LowMod1)*(HighMod1 - LowMod1)*variance.xvarmod1 + 
                                (HighMod2 - LowMod2)*(HighMod2 - LowMod2)*variance.xvarmod2 + 
                                (HighMod1*HighMod2 - LowMod1*LowMod2)*(HighMod1*HighMod2 - LowMod1*LowMod2)*variance.xvarmod1mod2 + 
                                (2*(HighMod1 - LowMod1)*(HighMod2 - LowMod2)*covariance.xvarmod1WITHxvarmod2) + 
                                (2*(HighMod1 - LowMod1)*(HighMod1*HighMod2 - LowMod1*LowMod2)*covariance.xvarmod1WITHxvarmod1mod2) +
                                (2*(HighMod2 - LowMod2)*(HighMod1*HighMod2 - LowMod1*LowMod2)*covariance.xvarmod2WITHxvarmod1mod2)))
  # 2 vs 3
  tval.sd2vs3 <- sd2vs3/(sqrt((HighMod1 - LowMod1)*(HighMod1 - LowMod1)*variance.xvarmod1 + 
                                (LowMod2 - HighMod2)*(LowMod2 - HighMod2)*variance.xvarmod2 + 
                                (HighMod1*LowMod2 - LowMod1*HighMod2)*(HighMod1*LowMod2 - LowMod1*HighMod2)*variance.xvarmod1mod2 +
                                (2*(HighMod1 - LowMod1)*(LowMod2 - HighMod2)*covariance.xvarmod1WITHxvarmod2) +
                                (2*(HighMod1 - LowMod1)*(HighMod1*LowMod2 - LowMod1*HighMod2)*covariance.xvarmod1WITHxvarmod1mod2) + 
                                (2*(LowMod2 - HighMod2)*(HighMod1*LowMod2 - LowMod1*HighMod2)*covariance.xvarmod2WITHxvarmod1mod2)))
  # 2 vs 4
  tval.sd2vs4 <- sd2vs4/(sqrt(variance.xvarmod1*(HighMod1 - LowMod1)*(HighMod1 - LowMod1) + LowMod2*LowMod2*(HighMod1 - LowMod1)*(HighMod1 - LowMod1)*variance.xvarmod1mod2 + 
                                2*LowMod2*(HighMod1 - LowMod1)*(HighMod1 - LowMod1)*covariance.xvarmod1WITHxvarmod1mod2))
  
  # 3 vs 4
  tval.sd3vs4 <- sd3vs4/(sqrt(variance.xvarmod2*(HighMod2 - LowMod2)*(HighMod2 - LowMod2) + 
                                LowMod1*LowMod1*(HighMod2 - LowMod2)*(HighMod2 - LowMod2)*variance.xvarmod1mod2 + 
                                2*LowMod1*(HighMod2 - LowMod2)*(HighMod2 - LowMod2)*covariance.xvarmod2WITHxvarmod1mod2))
  
  # get p.values for each difference
  pval.sd1vs2 <- 2*pt(abs(tval.sd1vs2), df.model, lower.tail = FALSE)
  pval.sd1vs3 <- 2*pt(abs(tval.sd1vs3), df.model, lower.tail = FALSE)
  pval.sd1vs4 <- 2*pt(abs(tval.sd1vs4), df.model, lower.tail = FALSE)
  pval.sd2vs3 <- 2*pt(abs(tval.sd2vs3), df.model, lower.tail = FALSE)
  pval.sd2vs4 <- 2*pt(abs(tval.sd2vs4), df.model, lower.tail = FALSE)
  pval.sd3vs4 <- 2*pt(abs(tval.sd3vs4), df.model, lower.tail = FALSE)
  
  # get confidence intervals for each slope difference
  conf1 <- paste('(', paste(round((sd1vs2 - 1.96*(sd1vs2/tval.sd1vs2)), 3), round((sd1vs2 + 1.96*(sd1vs2/tval.sd1vs2)), 3), sep = ','), ')', sep = '')
  conf2 <- paste('(', paste(round((sd1vs3 - 1.96*(sd1vs3/tval.sd1vs3)), 3), round((sd1vs3 + 1.96*(sd1vs3/tval.sd1vs3)), 3), sep = ','), ')', sep = '')
  conf3 <- paste('(', paste(round((sd1vs4 - 1.96*(sd1vs4/tval.sd1vs4)), 3), round((sd1vs4 + 1.96*(sd1vs4/tval.sd1vs4)), 3), sep = ','), ')', sep = '')
  conf4 <- paste('(', paste(round((sd2vs3 - 1.96*(sd2vs3/tval.sd2vs3)), 3), round((sd2vs3 + 1.96*(sd2vs3/tval.sd2vs3)), 3), sep = ','), ')', sep = '')
  conf5 <- paste('(', paste(round((sd2vs4 - 1.96*(sd2vs4/tval.sd2vs4)), 3), round((sd2vs4 + 1.96*(sd2vs4/tval.sd2vs4)), 3), sep = ','), ')', sep = '')
  conf6 <- paste('(', paste(round((sd3vs4 - 1.96*(sd3vs4/tval.sd3vs4)), 3), round((sd3vs4 + 1.96*(sd3vs4/tval.sd3vs4)), 3), sep = ','), ')', sep = '')
  
  # getting all of these values in a table as dataframe
  dat.frame <- data.frame('Pairs of Differences' = c('1 versus 2', '1 versus 3',
                                                     '1 versus 4', '2 versus 3',
                                                     '2 versus 4', '3 versus 4'),
                          SlopeDiff = c(sd1vs2, sd1vs3, sd1vs4, sd2vs3, sd2vs4, sd3vs4),
                          t.value = c(tval.sd1vs2,tval.sd1vs3,tval.sd1vs4,tval.sd2vs3,tval.sd2vs4,tval.sd3vs4),
                          p.value = c(pval.sd1vs2,pval.sd1vs3,pval.sd1vs4,pval.sd2vs3, pval.sd2vs4,pval.sd3vs4),
                          conf.interval = c(conf1, conf2,conf3,conf4,conf5,conf6))
  # returning the dataframe
  return(dat.frame)
}
