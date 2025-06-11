library(dr4pl)

data_frame_3 <- data_frame_2 %>%
  mutate(value_norm = 1 - value_norm)

# Note dr4pl and nplr both lead to negative slopes while drc leads to positive slops
model <- dr4pl(value_norm ~ concentration,
  data = data_frame_2,
  method.init = "logistic"
)
model2 <- dr4pl(value_norm ~ concentration,
               data = data_frame_3,
               method.init = "logistic"
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

model_drc <- drc::drm(value_norm ~ concentration,
  data = data_frame_2,
  fct = drc::LL.4(names = c("slope", "lower_asy", "upper_asy", "ic50")),
  control = drc::drmc(maxIt = 10000)
)
summary(model_drc)$coefficients

plot(model_drc)

model_nplr <- nplr::nplr(data_frame_2$concentration, data_frame_2$value_norm,
                         npars = 4)
summary(model_nplr)
nplr::getPar(model_nplr)


