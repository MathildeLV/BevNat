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

# Model with a binary outcome (ex: PTB)
GAM_plot_function_binary <- function(model, var_nr) {
  plot <- plot(model, select=var_nr)
  plot_x <- plot[[var_nr]]$x 
  plot_fit <- ((plot[[var_nr]]$fit+ coef(model)[1]))
  plot_CIl<- plot_fit-(1.96*(plot[[var_nr]]$se))
  plot_CIu<- plot_fit+(1.96*(plot[[var_nr]]$se))
  plot_data <-  data.frame(x=plot_x, fit=plogis(plot_fit),CIl=plogis(plot_CIl),CIu=plogis(plot_CIu))
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
  
  
  estimates_cont_add_pval <- function(model, n)
  {beta <- coef(model)
  Vb <- vcov(model, unconditional = TRUE)
  se <- sqrt(diag(Vb))
  d <- abs(beta)/(sqrt(n)*se)
  pvalue <- summary(model)$p.table[, "Pr(>|t|)"]
  my.ci <- data.frame(cbind(beta, lci = beta-1.96*se, uci = beta + 1.96*se, d, pvalue))
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

# Normalizing the values in m to the range [0, 1] using min-max scaling.
normalit<-function(m){
  (m - min(m))/(max(m)-min(m))
}

# Count columns based on date condition for each row
# (I use it for counting the number of pregnancy months during which the mother
# was exposed to crisis - eco.crisis and the flu pandemic)
count_columns_by_date <- function(row, start_date, end_date) {
  sum(row >= start_date & row <= end_date, na.rm = TRUE)
}