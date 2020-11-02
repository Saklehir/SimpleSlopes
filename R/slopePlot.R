#' @title Simple Slope Significance and Difference for 3-Way Interactions following Dawson & Richter (2006)
#'
#' @description This function passes an 'lm' object with predictor and moderators and creates an interaction plot
#'
#' @param model
#'
#' @return g
#'
#' @examples slopePlot(model, xvar, modvars)
#'
#' @export
#'

slopePlot <- function(
  model, xvar, modvars, modNames, xlab, ylab){
  if(missing(model))
    stop("slopeTest requires a fitted lm model.")
  if(missing(xvar))
    stop('Please indicate your predictor variable.')
  else if(!is.character(xvar)) xvar <- as.character(substitute(xvar))
  if(missing(xlab))
    stop('Please indicate your predictor variable name.')
  else if(!is.character(xlab)) xlab <- as.character(substitute(xlab))
  if(missing(ylab))
    stop('Please indicate your predictor variable name.')
  else if(!is.character(ylab)) ylab <- as.character(substitute(ylab))
  if(missing(modvars))
    stop("Please indicate your moderator variables.")
  else if(!is.character(modvars[1])) modvars[1] <- as.character(substitute(modvars[1]))
  else if(!is.character(modvars[2])) modvars[2] <- as.character(substitute(modvars[2]))
  if(missing(modNames))
    stop("Please indicate your moderator variable names.")
  else if(!is.character(modNames[1])) modNames[1] <- as.character(substitute(modNames[1]))
  else if(!is.character(modNames[2])) modNames[2] <- as.character(substitute(modNames[2]))
  
  `%notin%` <- Negate(`%in%`)
  varNames <- colnames(model$model)
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
  if(length(modNames) != 2)
    stop('Please indicate 2 moderator variable names.')
  modNames1 <- modNames[1]
  modNames2 <- modNames[2]
  
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
  
  # get gradient of slopes
  grSlope1 <- coef.xvar + HighMod1*coef.mod1 + HighMod2*coef.mod2 + HighMod1*HighMod2*coef.xvarmod1mod2
  grSlope2 <- coef.xvar + HighMod1*coef.mod1 + LowMod2*coef.mod2 + HighMod1*LowMod2*coef.xvarmod1mod2
  grSlope3 <- coef.xvar + LowMod1*coef.mod1 + HighMod2*coef.mod2 + LowMod1*HighMod2*coef.xvarmod1mod2
  grSlope4 <- coef.xvar + LowMod1*coef.mod1 + LowMod2*coef.mod2 + LowMod1*LowMod2*coef.xvarmod1mod2
  
  ## Create intercepts
  i.Slope1 <- coef.int + MindHigh*coef.mod1 + AcsfHigh*coef.mod2 + MindHigh*AcsfHigh*coef.xvarmod1mod2
  i.Slope2 <- coef.int + MindHigh*coef.mod1 + AcsfLow*coef.mod2 + MindHigh*AcsfLow*coef.xvarmod1mod2
  i.Slope3 <- coef.int + MindLow*coef.mod1 + AcsfHigh*coef.mod2 + MindLow*AcsfHigh*coef.xvarmod1mod2
  i.Slope4 <- coef.int + MindLow*coef.mod1 + AcsfLow*coef.mod2 + MindLow*AcsfLow*coef.xvarmod1mod2
  

  ## a set of values of x
  x.val <- seq(min(model$model[, xvar]), max(model$model[, xvar]), length.out = 2)
  dat.HighMod1HighMod2 <- data.frame(xval = x.val)
  dat.HighMod1HighMod2$yval <- i.Slope1 + dat.HighMod1HighMod2$xval*grSlope1
  dat.HighMod1HighMod2$Slope <- rep(paste(paste('High',mod1,sep = ''), paste('High', mod2, sep = ''), sep = ' '), nrow(dat.HighMod1HighMod2))
  
  # High Low
  dat.HighMod1LowMod2 <- data.frame(xval = x.val)
  dat.HighMod1LowMod2$yval <- i.Slope1 + dat.HighMod1LowMod2$xval*grSlope2
  dat.HighMod1LowMod2$Slope <- rep(paste(paste('High',mod1,sep = ''), paste('Low', mod2, sep = ''), sep = ' '), nrow(dat.HighMod1LowMod2))
  
  # Low High
  dat.LowMod1HighMod2 <- data.frame(xval = x.val)
  dat.LowMod1HighMod2$yval <- i.Slope1 + dat.LowMod1HighMod2$xval*grSlope3
  dat.LowMod1HighMod2$Slope <- rep(paste(paste('Low',mod1,sep = ''), paste('High', mod2, sep = ''), sep = ' '), nrow(dat.LowMod1HighMod2))
  
  # Low High
  dat.LowMod1LowMod2 <- data.frame(xval = x.val)
  dat.LowMod1LowMod2$yval <- i.Slope1 + dat.LowMod1LowMod2$xval*grSlope4
  dat.LowMod1LowMod2$Slope <- rep(paste(paste('Low',mod1,sep = ''), paste('Low', mod2, sep = ''), sep = ' '), nrow(dat.LowMod1LowMod2))
  
  ## Create final data frame
  dat.plot <- rbind(dat.HighMod1HighMod2, dat.HighMod1LowMod2, dat.LowMod1HighMod2, dat.LowMod1LowMod2)
  
  ## Remove unnecessary data frames
  rm(dat.HighMod1HighMod2, dat.HighMod1LowMod2, dat.LowMod1HighMod2, dat.LowMod1LowMod2)
  
  
  ## Call library
  library(ggplot2)
  
  ## Convert 'Slope' to factor
  dat.plot$Slope <- factor(dat.plot$Slope)
  
  
  ## Draw plot
  g <- ggplot(dat.plot, aes(x = xval,
                        y = yval)) +
    geom_line(aes(colour = Slope,
                  linetype = Slope)) +
    theme_bw() +
    theme(panel.background = element_rect(fill='white')) +
    theme(legend.key = element_blank()) +
    theme(text = element_text(size = 12)) +
    xlab(xlab) + ylab(ylab) +
    scale_colour_manual(name = "Partial effect",
                        labels = c(paste("High", modNames1, "High", modNames2), 
                                   paste("High", modNames1, "Low", modNames2), 
                                   paste("Low", modNames1, "High", modNames2), 
                                   paste("Low", modNames1, "Low", modNames2)),
                        values = c("#E41A1C", 
                                   "#E41A1C", 
                                   "#377EB8", 
                                   "#377EB8")) +   
    scale_linetype_manual(name = "Partial effect",
                          labels = c(paste("High", modNames1, "High", modNames2), 
                                     paste("High", modNames1, "Low", modNames2), 
                                     paste("Low", modNames1, "High", modNames2), 
                                     paste("Low", modNames1, "Low", modNames2)),
                          values = c("solid", 
                                     "longdash", 
                                     "solid", 
                                     "longdash"))
  
  # return the g object
  return(g)

}

