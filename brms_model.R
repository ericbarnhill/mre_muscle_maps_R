value_scale <- median(data_tall$value)
data_tall$value <- data_tall$value / value_scale

mod <- brm(formula = value ~ 1 + musc_id + (1 + musc_id | cond_id), data = data_tall, family = "lognormal", prior = c( set_prior("normal(0, 100)", coef="musc_id"), set_prior("normal(0, 100)", coef="cond_id")))
