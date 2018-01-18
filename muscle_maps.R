muscle_map <- function(data) {
    nMuscles <- 13
    nGroups <- 4
    nPixels <- 4070
    jags_data <- list(nMuscles=nMuscles, nGroups=nGroups, nPixels=nPixels,
                      y=data$Value, condition=data$cond_id, 
                      group=data$grp_id, muscle=data$musc_id, 
                      pixel = data$pix_id)
    muscle_map_model <- jags.model(file="muscle_map_model_string.txt", 
                                   data= jags_data,
                                   n.chains = 3)
    update(muscle_map_model, 1000)
    dic_an <- dic.samples(muscle_map_model, 1000)
    variable_names = c('beta0', 'beta1', 'beta2', 'beta3', 'beta4')
    muscle_map_sim <- coda.samples(model = muscle_map_model, n.iter = 1.5e4,
                                   thin = 5, variable.names = variable_names)
    muscle_map_csim <- do.call(rbind, muscle_map_sim)
    hdis <- samples_to_hdis(muscle_map_csim)
    results <- list(sim=muscle_map_sim, csim=muscle_map_csim, hdis=hdis, dic=dic_an)
}

muscle_gamma_results <- function(csim, data) {
    
}