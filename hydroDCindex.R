#' Duration Curve Hydrological Model Index
#' @export
#' @param Q_obs Column with daily observed flows
#' @param Q_sim Column with daily simulated flows

hydroDC_Index <- function(Q_obs, Q_sim) {
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
  return(Indexes)
  return(Result_DC)
}
