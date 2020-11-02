#' @title Simple Slope Significance and Difference for 3-Way Interactions following Dawson & Richter (2006)
#'
#' @description This function passes an 'lm' object with predictor and moderators tests slope significance
#'
#' @param model
#'
#' @return dat.frame
#'
#' @examples slopeTest(model, xvar, modvars)
#'
#' @export
#'

slopeTest <- function(
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

  # get gradient of slopes
  grSlope1 <- coef.xvar + HighMod1*coef.xvarmod1 + HighMod2*coef.xvarmod2 + HighMod1*HighMod2*coef.xvarmod1mod2
  grSlope2 <- coef.xvar + HighMod1*coef.xvarmod1 + LowMod2*coef.xvarmod2 + HighMod1*LowMod2*coef.xvarmod1mod2
  grSlope3 <- coef.xvar + LowMod1*coef.xvarmod1 + HighMod2*coef.xvarmod2 + LowMod1*HighMod2*coef.xvarmod1mod2
  grSlope4 <- coef.xvar + LowMod1*coef.xvarmod1 + LowMod2*coef.xvarmod2 + LowMod1*LowMod2*coef.xvarmod1mod2

  # t-values for each slope
  # slope 1
  t.Slope1 <- grSlope1/sqrt(variance.xvar + HighMod1*HighMod1*variance.xvarmod1 + HighMod2*HighMod2*variance.xvarmod2 +
                              HighMod1*HighMod1*HighMod2*HighMod2*variance.xvarmod1mod2 + 2*(HighMod1*covariance.xvarWITHxvarmod1 +
                                                                                               HighMod2*covariance.xvarWITHxvarmod2 + HighMod1*HighMod2*covariance.xvarWITHxvarmod1mod2 +
                                                                                               HighMod1*HighMod2*covariance.xvarmod1WITHxvarmod2 +  HighMod1*HighMod1*HighMod2*covariance.xvarmod1WITHxvarmod1mod2 +
                                                                                               HighMod1*HighMod2*HighMod2*covariance.xvarmod2WITHxvarmod1mod2))
  # slope 2
  t.Slope2 <- grSlope2/sqrt(variance.xvar + HighMod1*HighMod1*variance.xvarmod1 + LowMod2*LowMod2*variance.xvarmod2 +
                              HighMod1*HighMod1*LowMod2*LowMod2*variance.xvarmod1mod2 + 2*(HighMod1*covariance.xvarWITHxvarmod1 +
                                                                                             LowMod2*covariance.xvarWITHxvarmod2 + HighMod1*LowMod2*covariance.xvarWITHxvarmod1mod2 +
                                                                                             HighMod1*LowMod2*covariance.xvarmod1WITHxvarmod2 +  HighMod1*HighMod1*LowMod2*covariance.xvarmod1WITHxvarmod1mod2 +
                                                                                             HighMod1*LowMod2*LowMod2*covariance.xvarmod2WITHxvarmod1mod2))
  # slope 3
  t.Slope3 <- grSlope3/sqrt(variance.xvar + LowMod1*LowMod1*variance.xvarmod1 + HighMod2*HighMod2*variance.xvarmod2 +
                              LowMod1*LowMod1*HighMod2*HighMod2*variance.xvarmod1mod2 + 2*(LowMod1*covariance.xvarWITHxvarmod1 +
                                                                                             HighMod2*covariance.xvarWITHxvarmod2 + LowMod1*HighMod2*covariance.xvarWITHxvarmod1mod2 +
                                                                                             LowMod1*HighMod2*covariance.xvarmod1WITHxvarmod2 +  LowMod1*LowMod1*HighMod2*covariance.xvarmod1WITHxvarmod1mod2 +
                                                                                             LowMod1*HighMod2*HighMod2*covariance.xvarmod2WITHxvarmod1mod2))

  # slope 4
  t.Slope4 <- grSlope4/sqrt(variance.xvar + LowMod1*LowMod1*variance.xvarmod1 + LowMod2*LowMod2*variance.xvarmod2 +
                              LowMod1*LowMod1*LowMod2*LowMod2*variance.xvarmod1mod2 + 2*(LowMod1*covariance.xvarWITHxvarmod1 +
                                                                                           LowMod2*covariance.xvarWITHxvarmod2 + LowMod1*LowMod2*covariance.xvarWITHxvarmod1mod2 +
                                                                                           LowMod1*LowMod2*covariance.xvarmod1WITHxvarmod2 +  LowMod1*LowMod1*LowMod2*covariance.xvarmod1WITHxvarmod1mod2 +
                                                                                           LowMod1*LowMod2*LowMod2*covariance.xvarmod2WITHxvarmod1mod2))

  # get the p.values for each slope gradient's t.value
  p.valSlope1 <- 2*pt(abs(t.Slope1), df.model, lower.tail = FALSE)
  p.valSlope2 <- 2*pt(abs(t.Slope2), df.model, lower.tail = FALSE)
  p.valSlope3 <- 2*pt(abs(t.Slope3), df.model, lower.tail = FALSE)
  p.valSlope4 <- 2*pt(abs(t.Slope4), df.model, lower.tail = FALSE)

  # getting all of these values in a table as dataframe
  dat.frame <- data.frame(Term = c(paste(paste('High', mod1, sep = ' '), paste('High', mod2, sep = ' '), sep = ','),
                                   paste(paste('High', mod1, sep = ' '), paste('Low', mod2, sep = ' '), sep = ','),
                                   paste(paste('Low', mod1, sep = ' '), paste('High', mod2, sep = ' '), sep = ','),
                                   paste(paste('Low', mod1, sep = ' '), paste('Low', mod2, sep = ' '), sep = ',')),
                          Slope = c(grSlope1, grSlope2, grSlope3, grSlope4),
                          t.value = c(t.Slope1,t.Slope2,t.Slope3,t.Slope4),
                          p.value = c(p.valSlope1,p.valSlope2,p.valSlope3,p.valSlope4))
  # returning the dataframe
  return(dat.frame)
}

