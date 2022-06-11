#' Duration Curve Hydrological Model Indexes
#' @export
#' @param Q_obs Column with daily observed flows
#' @param Q_sim Column with daily simulated flows
#' @param c_opt Results, default 1 for indexes, 2 for duration curve values, 3 for CD plot and 4 for scatter plot
#' @import stats hydroGOF

hydroDC_Index <- function(Q_obs, Q_sim, c_opt) {
  n_obs = length(Q_obs)
  n_nas_obs = length(which(is.na(Q_obs)))
  n_sim = length(Q_sim)
  n_nas_sim = length(which(is.na(Q_sim)))
  n_nas = 0
  if (n_nas_obs != n_nas_sim) {
    n_nas = max(n_nas_obs,n_nas_sim)
  }
  if (n_obs != n_sim) {
    print("Q_obs y Q_sim does not have same amount of data")
  }
  # Duration Curve for Q_obs
  Registros = sort(Q_obs, decreasing = TRUE, na.last = T)
  Registros = Registros[1:(n_obs-n_nas)]
  n_Registros = length(Registros)
  Pexc = c()
  for (i in 1:n_Registros) {
    Pexc[i] = 100*(i/(n_Registros+1))
  }
  DC_Obs = data.frame("Obs" = Registros,"Pexc" = Pexc)
  # Duration Curve for Q_sim
  Registros = sort(Q_sim, decreasing = TRUE, na.last = T)
  Registros = Registros[1:(n_sim-n_nas)]
  n_Registros = length(Registros)
  Pexc = c()
  for (i in 1:n_Registros) {
    Pexc[i] = 100*(i/(n_Registros+1))
  }
  DC_Sim = data.frame("Sim" = Registros,"Pexc" = Pexc)
  Result_DC = data.frame("Pexc" = DC_Obs$Pexc,
                            "Amount_Obs" = DC_Obs$Obs,
                            "Amount_Sim" = DC_Sim$Sim)
  # Amounts middle section
  idx_Pexc_2  = which.min(abs(Result_DC$Pexc - 2))
  idx_Pexc_20 = which.min(abs(Result_DC$Pexc - 20))
  idx_Pexc_70 = which.min(abs(Result_DC$Pexc - 70))
  Pexc_2  = Result_DC$Pexc[idx_Pexc_2]
  Qobs_2  = Result_DC$Amount_Obs[idx_Pexc_2]
  Pexc_20 = Result_DC$Pexc[idx_Pexc_20]
  Qobs_20 = Result_DC$Amount_Obs[idx_Pexc_20]
  Qsim_20 = Result_DC$Amount_Sim[idx_Pexc_20]
  Pexc_70 = Result_DC$Pexc[idx_Pexc_70]
  Qobs_70 = Result_DC$Amount_Obs[idx_Pexc_70]
  Qsim_70 = Result_DC$Amount_Sim[idx_Pexc_70]
  Resultado_CD_tramo_medio =Result_DC[idx_Pexc_20:idx_Pexc_70,]
  Resultados_CD_tramo_alto = Result_DC[1:idx_Pexc_2,]
  Resultado_CD_tramo_bajo =Result_DC[idx_Pexc_70:(n_obs-n_nas),]
  # BiasFMS: Diagnosis of vertical redistribution in the midsection of the DC
  BiasFMS = ((log(Qsim_20)-log(Qsim_70))-(log(Qobs_20)-log(Qobs_70)))/(log(Qobs_20)-log(Qobs_70))
  # BiasFHV: Bias in peak flows
  BiasFHV = 100*(sum(Resultados_CD_tramo_alto$Amount_Sim-Resultados_CD_tramo_alto$Amount_Obs))/sum(Resultados_CD_tramo_alto$Amount_Obs)
  # BiasFLV: Bias at low flows
  BiasFLV = -100*(sum(log(Resultado_CD_tramo_bajo$Amount_Sim)-log(end(Resultado_CD_tramo_bajo$Amount_Sim))) - sum(log(Resultado_CD_tramo_bajo$Amount_Obs)-log(end(Resultado_CD_tramo_bajo$Amount_Obs))))/(sum(log(Resultado_CD_tramo_bajo$Amount_Obs)-log(end(Resultado_CD_tramo_bajo$Amount_Obs))))
  # BiasFMM_log: Log_Mean Flow Bias
  BiasFMM_log = 100*(log(median(Result_DC$Amount_Sim)) - log(median(Result_DC$Amount_Obs)))/log(median(Result_DC$Amount_Obs))
  # BiasFMM: Mean Flow Bias
  BiasFMM = 100*(median(Result_DC$Amount_Sim) - median(Result_DC$Amount_Obs))/median(Result_DC$Amount_Obs)
  # df of indexes
  names = c("BiasFMS","BiasFHV","BiasFLV","BiasFMM_log","BiasFMM")
  values = c(round(BiasFMS,2),round(BiasFHV,2),round(BiasFLV,2),round(BiasFMM_log,2),round(BiasFMM,2))
  Indexes = data.frame("Indice" = names, "Valor_Porcentual" = values)

  r_pearson <- stats::cor.test(Q_sim, Q_obs)
  MAE <- sum(abs(Q_obs - Q_sim))/length(Q_obs)
  rsq <- cor(Q_sim, Q_obs) ^ 2
  NSE <- hydroGOF::gof(Q_sim, Q_obs)[9]
  KGE <- hydroGOF::gof(Q_sim, Q_obs)[19]

  if (c_opt == 4) {

    aux_max <- ceiling(max(c(Q_obs, Q_sim), na.rm = T))
    plot(Q_obs, Q_sim, pch = 19, ylab = 'Qsim', xlab = 'Qobs',
         ylim = c(0,aux_max), xlim = c(0,aux_max), col = 'red')
    abline(a = 0, b = 1, lty = 2, col = 'blue')

    text_in1 <- paste0('r_pearson = ', round(r_pearson, digits = 2))
    text(5/30*aux_max,28/30*aux_max, text_in1, col="blue", font = 4, pos = 1)

    text_in2 <- paste0('MAE = ', round(MAE, digits = 2))
    text(5/30*aux_max,26/30*aux_max, text_in2, col="blue", font = 4, pos = 1)

    text_in3 <- paste0('R^2 = ', round(rsq, digits = 2))
    text(5/30*aux_max,24/30*aux_max, text_in3, col="blue", font = 4, pos = 1)

    text_in4 <- paste0('NSE = ', round(NSE, digits = 2))
    text(5/30*aux_max,22/30*aux_max, text_in2, col="blue", font = 4, pos = 1)

    text_in5 <- paste0('KGE = ', round(KGE, digits = 2))
    text(5/30*aux_max,20/30*aux_max, text_in3, col="blue", font = 4, pos = 1)

  } else if (c_opt == 3) {

    # Plot for DC + Indexes

    aux_max <- ceiling(max(c(max(Result_DC$Amount_Obs, na.rm = T), max(Result_DC$Amount_Sim,na.rm = T)),na.rm = T))

    plot(Result_DC$Pexc, Result_DC$Amount_Obs, type = 'l', lwd = 1, col = 'black',
         xlim = c(0,100), ylim = c(0.01, aux_max), log = 'y',
         ylab = 'Mean daily flow', xlab = 'Exceedance probability (%)', main = "Daily Flow Duration Curve")
    lines(Result_DC$Pexc, Result_DC$Amount_Sim, lwd = 1, col = 'red')
    legend("topright", col = c("black","red"),
           legend = c("Obs", "Sim"), lwd = 2, cex = 1,
           horiz = F)

    text_in1 <- paste0(Indexes$Indice, ' = ', round(Indexes$Valor_Porcentual,digits = 2), '%')
    text(30,25, text_in1[1], col="black", font = 3, pos = 1)
    text(30,12, text_in1[2], col="black", font = 3, pos = 1)
    text(30,6, text_in1[3], col="black", font = 3, pos = 1)

  } else if (c_opt == 2) {

    return(Result_DC)

  } else {

    return(Indexes)

  }
}
