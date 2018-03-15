require(rjags)
require(HDInterval)
source("samples_to_hdis.R")
# if model is a map, include nPixels, else skip
run_muscle_model <- function(data_tall, model_path, is.map, varNames) {
    if (is.map) {
        jags_data <- list(y = data_tall$value, musc=data_tall$musc_id, 
                          cond=data_tall$cond_id, pix = data_tall$pix_id,
                          nMusc=length(levels(data_tall$musc_id)),
                          nPix=length(levels(data_tall$pix_id)) )
    } else {
        jags_data <- list(y = data_tall$value, musc=data_tall$musc_id, 
                          cond=data_tall$cond_id, 
                          nMusc=length(levels(data_tall$musc_id)))
    }
    muscle_mod <- jags.model(file=model_path, data=jags_data, n.chains=3) 
    update(muscle_mod, 1e3)
    muscle_mod_sim <- coda.samples(model = muscle_mod, variable.names = varNames, n.iter = 5e3)
    muscle_mod_csim <- as.mcmc(do.call(rbind, muscle_mod_sim))
    #DIC <- dic.samples(muscle_mod, 1e3)
    Gelman <- gelman.diag(muscle_mod_sim)
    #Raftery <- raftery.diag(muscle_mod_sim)
    #Autocorr <- autocorr.diag(muscle_mod_sim)
    means <- colMeans(muscle_mod_csim)
    hdis <- samples_to_hdis(muscle_mod_csim)
    model_results <- list(mod=muscle_mod, sim=muscle_mod_sim, 
                      csim=muscle_mod_csim, means=means, hdis=hdis,
                      DIC=DIC, Gelman=Gelman,
                      Raftery=Raftery, Autocorr=Autocorr)
    save(list=c("model_results"), file="models_temp.RData")
    return(model_results)
}

# get ranges in pascals
hdis_to_ranges <- function(hdis, csim_means) {
    nvars <- dim(hdis)[1] / 2
    range_lo <- vector(length = nvars)
    range_hi <- vector(length = nvars)
    for (n in 1:nvars) {
        range_lo[n] <- exp(csim_means[n] + hdis$hdi_lo[n+nvars])
        range_hi[n] <- exp(csim_means[n] + hdis$hdi_hi[n+nvars])
    }
    ranges <- data.frame(range_lo, range_hi)
}

# get difference between 0 and 1 conditions in pascals
hdis_to_beta_ranges <- function(hdis, csim_means) {
    nvars <- dim(hdis)[1] / 2
    range_lo <- vector(length = nvars)
    range_hi <- vector(length = nvars)
    for (n in 1:nvars) {
        range_lo[n] <- exp(csim_means[n] + hdis$hdi_lo[n+nvars]) - exp(csim_means[n])
        range_hi[n] <- exp(csim_means[n] + hdis$hdi_hi[n+nvars]) - exp(csim_means[n])
    }
    ranges <- data.frame(range_lo, range_hi)
}

mcmc_to_cond_means <- function(csim) {
    csim_means <- colMeans(csim)
    nvars <- dim(csim)[2] / 2
    cond_lo <- vector(length = nvars)
    cond_hi <- vector(length = nvars)
    for (n in 1:nvars) {
        cond_lo[n] <- exp(csim_means[n])
        cond_hi[n] <- exp(csim_means[n] + csim_means[n+nvars])
    }
    conds <- data.frame(cond_lo, cond_hi)
}

run_models <- function(data_tall) {
    model_folder <- file.path(proj_path, "models/")
    varNames_array <- list(c("beta0", "beta1", "delta0", "delta1"), 
                        c("beta0", "beta1"))
    model_paths <- dir(model_folder)
    #for (m in 1:length(model_paths)) {
    for (m in 2:length(model_paths)) {
        # if model is a map, add nPixels
        if (m == 2) {
            model_results[[m]] = run_muscle_model(data_tall, file.path(model_folder, model_paths[m]),
                             T, varNames_array[[m]])
        } else {
            model_results[[m]] = run_muscle_model(data_tall, file.path(model_folder, model_paths[m]), 
                             F, varNames_array[[m]])
        }
    }
    save(list=c("model_results"), file="models_complete.RData")
    return(model_results)
}

