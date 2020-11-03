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
  model, xvar, modvars, modNames, x.lab, y.lab){
  if(missing(model))
    stop("slopeTest requires a fitted lm model.")
  if(missing(xvar))
    stop('Please indicate your predictor variable.')
  else if(!is.character(xvar)) xvar <- as.character(substitute(xvar))
  if(missing(x.lab))
    stop('Please indicate your predictor variable name.')
  else if(!is.character(x.lab)) x.lab <- as.character(substitute(x.lab))
  if(missing(y.lab))
    stop('Please indicate your predictor variable name.')
  else if(!is.character(y.lab)) y.lab <- as.character(substitute(y.lab))
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
  
  ## High / low for w and z
  HighMod1 <- mean(model$model[, mod1]) + sd(model$model[,mod1])
  LowMod1 <- mean(model$model[,mod1]) - sd(model$model[,mod1])
  HighMod2 <- mean(model$model[,mod2]) + sd(model$model[,mod2])
  LowMod2 <- mean(model$model[,mod2]) - sd(model$model[,mod2])
  
  ## Get coefficients from regression
  coefs.model <- coef(model)
  coef.int <- coefs.model["(Intercept)"]
  coef.xvar <- coefs.model[xvar]
  coef.mod1 <- coefs.model[mod1]
  coef.mod2 <- coefs.model[mod2]
  coef.xvarmod1 <- coefs.model[paste(xvar, mod1, sep = ':')]
  coef.xvarmod2 <- coefs.model[paste(xvar, mod2, sep = ':')]
  coef.mod1mod2 <- coefs.model[ifelse(paste(mod1,mod2, sep = ':') %in% names(coefs.model),paste(mod1,mod2, sep = ':'),paste(mod2,mod1, sep = ':') )]
  coef.xvarmod1mod2 <- coefs.model[ifelse(paste(xvar,mod1, mod2, sep = ':') %in% names(coefs.model),paste(xvar,mod1, mod2, sep = ':'),paste(xvar,mod2, mod1, sep = ':') )]
  
  ## Create gradient of slopesslopes
  grSlope1 <- coef.xvar + HighMod1*coef.xvarmod1 + HighMod2*coef.xvarmod2 + HighMod1*HighMod2*coef.xvarmod1mod2
  grSlope2 <- coef.xvar + HighMod1*coef.xvarmod1 + LowMod2*coef.xvarmod2 + HighMod1*LowMod2*coef.xvarmod1mod2
  grSlope3 <- coef.xvar + LowMod1*coef.xvarmod1 + HighMod2*coef.xvarmod2 + LowMod1*HighMod2*coef.xvarmod1mod2
  grSlope4 <- coef.xvar + LowMod1*coef.xvarmod1 + LowMod2*coef.xvarmod2 + LowMod1*LowMod2*coef.xvarmod1mod2
  
  ## Create intercepts
  i.HighMod1HighMod2 <- coef.int + HighMod1*coef.mod1 + HighMod2*coef.mod2 + HighMod1*HighMod2*coef.mod1mod2
  i.HighMod1LowMod2 <- coef.int + HighMod1*coef.mod1 + LowMod2*coef.mod2 + HighMod1*LowMod2*coef.mod1mod2
  i.LowMod1HighMod2 <- coef.int + LowMod1*coef.mod1 + HighMod2*coef.mod2 + LowMod1*HighMod2*coef.mod1mod2
  i.LowMod1LowMod2 <- coef.int + LowMod1*coef.mod1 + LowMod2*coef.mod2 + LowMod1*LowMod2*coef.mod1mod2
  
  # create a set of datasets to plot
  xVar <- seq(min(model$model[, xvar]), max(model$model[, xvar]), length.out = 2)
  
  # high mod1 & high mod2
  df.HighMod1HighMod2 <- data.frame(xVar = xVar)
  df.HighMod1HighMod2$yVar <- i.HighMod1HighMod2 + df.HighMod1HighMod2$xVar*grSlope1
  df.HighMod1HighMod2$Slope <- rep("HighMod1HighMod2", nrow(df.HighMod1HighMod2))
  
  # high mod1 & low mod2
  df.HighMod1LowMod2 <- data.frame(xVar = xVar)
  df.HighMod1LowMod2$yVar <- i.HighMod1LowMod2 + df.HighMod1LowMod2$xVar*grSlope2
  df.HighMod1LowMod2$Slope <- rep("HighMod1LowMod2", nrow(df.HighMod1LowMod2))
  
  # low mod1 & high mod2
  df.LowMod1HighMod2 <- data.frame(xVar = xVar)
  df.LowMod1HighMod2$yVar <- i.LowMod1HighMod2 + df.LowMod1HighMod2$xVar*grSlope3
  df.LowMod1HighMod2$Slope <- rep("LowMod1HighMod2", nrow(df.LowMod1HighMod2))
  
  # low mod1 & low mod2
  df.LowMod1LowMod2 <- data.frame(xVar = xVar)
  df.LowMod1LowMod2$yVar <- i.LowMod1LowMod2 + df.LowMod1LowMod2$xVar*grSlope4
  df.LowMod1LowMod2$Slope <- rep("LowMod1LowMod2", nrow(df.LowMod1LowMod2))
  
  # data.frame to plot
  data.plot <- rbind(df.HighMod1HighMod2, df.HighMod1LowMod2, df.LowMod1HighMod2, df.LowMod1LowMod2)
  
  ## Call library
  library(ggplot2)
  
  ## Convert 'Slope' to factor
  data.plot$Slope <- factor(data.plot$Slope)
  
  ## Draw plot
  g <- ggplot(data.plot, aes(x = xVar, y = yVar)) +
    geom_line(aes(colour = Slope,linetype = Slope), size = .8) +
    theme_bw() +
    theme(panel.background = element_rect(fill="white")) +
    theme(legend.key = element_blank()) + xlab(x.lab) + ylab(y.lab) +
    theme(text = element_text(size = 12)) +
    scale_colour_manual(name = "Partial effect",
                        labels = c(paste("High", modNames1, "High", modNames2),
                                   paste("High", modNames1, "Low", modNames2),
                                   paste("Low", modNames1, "High", modNames2),
                                   paste("Low", modNames1, "Low", modNames2)),
                        values = c("#000000", 
                                   "#000000", 
                                   "#A4A4A4", 
                                   "#A4A4A4")) +   
    scale_linetype_manual(name = "Partial effect",
                          labels = c(paste("High", modNames1, "High", modNames2),
                                     paste("High", modNames1, "Low", modNames2),
                                     paste("Low", modNames1, "High", modNames2),
                                     paste("Low", modNames1, "Low", modNames2)),
                          values = c("solid", 
                                     "longdash", 
                                     "solid", 
                                     "longdash"))
  return(g)
}
