# Function for GAM plots
# Model with a continuous outcome (ex: birthweight)
GAM_plot_function_continuous <- function(model, var_nr) {
  plot <- plot(model, select=var_nr)
  
  plot_x <- plot[[var_nr]]$x 
  plot_fit <- plot[[var_nr]]$fit+coef(model)[1]
  plot_CIl<- plot_fit-1.96*plot[[var_nr]]$se
  plot_CIu<- plot_fit+1.96*plot[[var_nr]]$se
  plot_data <-  data.frame(x=plot_x, fit=plot_fit,CIl=plot_CIl,CIu=plot_CIu)
}

####################
# Model with a binary outcome (ex: PTB)
GAM_plot_function_binary <- function(model, var_nr) {
  plot <- plot(model, select=var_nr)
  plot_x <- plot[[var_nr]]$x 
  plot_fit <- ((plot[[var_nr]]$fit+ coef(model)[1]))
  plot_CIl<- plot_fit-(1.96*(plot[[var_nr]]$se))
  plot_CIu<- plot_fit+(1.96*(plot[[var_nr]]$se))
  plot_data <-  data.frame(x=plot_x, fit=plogis(plot_fit),CIl=plogis(plot_CIl),CIu=plogis(plot_CIu))
}

GAM_plot_function_binarybis <- function(model, var_nr) {
  plot <- plot(model, select=var_nr, trans = plogis)
  plot_x <- plot[[var_nr]]$x 
  plot_fit <- ((plot[[var_nr]]$fit))
  plot_CIl<- plot_fit-(1.96*(plot[[var_nr]]$se))
  plot_CIu<- plot_fit+(1.96*(plot[[var_nr]]$se))
  plot_data <-  data.frame(x=plot_x, fit=exp(plot_fit),CIl=exp(plot_CIl),CIu=exp(plot_CIu))
}

# Table for estimates and conf interval and Cohen's d
  ## Continuous outcomes:
  estimates_cont <- function(model, n)
  {beta <- coef(model)
  Vb <- vcov(model, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  d <- (abs(beta)/(sqrt(n)*se))
  my.ci <- data.frame(cbind(beta, lci = beta-1.96*se, uci = beta + 1.96*se, d))
  }

  ## Binary outcomes: 
  estimates_binary <- function(model)
  {OR <- exp(coef(model))
  beta <- coef(model)
  Vb <- vcov(model, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  lci <- exp(beta-1.96*se)
  uci <- exp(beta+1.96*se)
  d <- abs(beta)/1.81
  my.ci <- data.frame(cbind(OR, lci, uci, d))
  }


# Function to calculate the mode of a variable
getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}