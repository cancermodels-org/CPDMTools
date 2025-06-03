library(dr4pl)

model <- dr4pl(ctg_value_norm ~ concentration,
  data = data_frame_2,
  method.init = "logistic",
  lowerl = c(-Inf, -Inf, -Inf, min(data_frame_2$ctg_value_norm)-0.2),
  upperl = c(max(data_frame_2$ctg_value_norm)+0.2, Inf, Inf, Inf)
)
summary(model)
summary(model$dr4pl.robust)
10^summary(model)$coefficients$Estimate[2]
IC(model, 50)
IC(model$dr4pl.robust, 50)

data_model_dr4pl <- data.frame(summary(model)$coefficients)
plot(model)

residuals(model)

plot(model$dr4pl.robust, indices.outlier = model$dr4pl.robust$idx.outlier)

# Test other packages

model_drc <- drc::drm(ctg_value_norm ~ concentration,
  data = data_frame_2,
  fct = drc::LL.4(names = c("slope", "lower_asy", "upper_asy", "ic50")),
  control = drc::drmc(maxIt = 10000)
)
summary(model_drc)$coefficients

plot(model_drc)

model_nplr <- nplr::nplr(data_frame_2$concentration, data_frame_2$ctg_value_norm,
                         npars = 4)
summary(model_nplr)
nplr::getPar(model_nplr)


