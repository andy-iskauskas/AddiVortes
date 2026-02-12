### AddiVortes Model Testing

## Libraries
#library(AddiVortes)
library(ggplot2)
library(rgl)
library(purrr)
set.seed(69)

options(rgl.printRglwidget = TRUE)

## Helper for plotting the stuff.
plot_preds <- function(fit, X, Y, preds = NULL) {
  if (is.null(preds))
    preds <- predict(fit, X)
  print(plot(Y, preds,
             xlab = "True Values",
             ylab = "Predicted Values",
             main = "AddiVortes Predictions vs True Values",
             xlim = range(c(preds, Y)),
             ylim = range(c(preds, Y)),
             pch = 19, col = "darkblue"))
  abline(a = 0, b = 1, col = "darkred", lwd = 2)
  preds_quant <- predict(fit, X, "quantile", showProgress = FALSE)
  for (i in 1:nrow(preds_quant)) {
    segments(Y[i], preds_quant[i,1],
             Y[i], preds_quant[i,2],
             col = "darkblue", lwd = 1)
  }
  legend("topleft", legend = c("Prediction", "y=x line", "95% interval"),
         col = c("darkblue", "darkred", "darkblue"),
         lty = 1, pch = c(19, NA, NA), lwd = 2)
}

## Boston dataset: flat
## 506 observations, 13 covariates
Boston <- read.csv(paste0("https://raw.githubusercontent.com/anonymous2738/",
                          "AddiVortesAlgorithm/DataSets/BostonHousing_Data.csv"))
X_Boston <- as.matrix(Boston[,2:14])
Y_Boston <- as.numeric(Boston[,15])
n_Boston <- length(Y_Boston)
samp_B <- sort(sample.int(n_Boston, 5*n_Boston/6))

## Weather dataset: spherical
## 2000 observations, 2 covariates
weather <- read.csv("weather_dat.csv")
X_weather <- as.matrix(weather[,c(2,1)])
Y_weather <- as.numeric(weather[,3])
n_weather <- length(Y_weather)
samp_W <- sort(sample.int(n_weather, 5*n_weather/6))

## Fake dataset: cylindrical
## 400 observations, 2 covariates
cyl_f <- function(x) sin(x[1]) * cos(x[2]) + x[1] * sin(x[2])^2
X_cyl <- as.matrix(data.frame(z = runif(400, 0, 10), th = runif(400, -pi, pi)))
Y_cyl <- apply(X_cyl, 1, cyl_f)
n_cyl <- length(Y_cyl)
samp_C <- sort(sample.int(n_cyl, 5*n_cyl/6))

## Boston
# Training
results_Boston <- AddiVortes(y = Y_Boston[samp_B],
                             x = X_Boston[samp_B,],
                             m = 200,
                             totalMCMCIter = 2000,
                             mcmcBurnIn = 200,
                             nu = 3,
                             q = 0.9,
                             k = 3,
                             sd = 0.8,
                             Omega = 5,
                             LambdaRate = 25,
                             InitialSigma = "Naive"
)
results_Boston$inSampleRmse
# Prediction
preds_Boston <- predict(results_Boston, X_Boston[-samp_B,])
cat("Test Set RMSE:", sqrt(mean((preds_Boston-Y_Boston[-samp_B])^2)), "\n")
plot_preds(results_Boston, X_Boston[-samp_B,], Y_Boston[-samp_B], preds_Boston)


## Weather training
results_weather <- AddiVortes(y = Y_weather[samp_W],
                              x = X_weather[samp_W,],
                              m = 200,
                              totalMCMCIter = 2000,
                              mcmcBurnIn = 200,
                              nu = 3,
                              q = 0.9,
                              k = 3,
                              sd = NULL,
                              Omega = 1,
                              LambdaRate = 25,
                              InitialSigma = "Naive",
                              metric = "S",
)
results_weather$inSampleRmse
## Prediction
preds_weather <- predict(results_weather, X_weather[-samp_W,])
cat("Test Set RMSE:", sqrt(mean((preds_weather-Y_weather[-samp_W])^2)), "\n")
plot_preds(results_weather, X_weather[-samp_W,], Y_weather[-samp_W], preds_weather)

## Cylinder training
results_cylinder <- AddiVortes(y = Y_cyl[samp_C],
                               x = X_cyl[samp_C,],
                               m = 200,
                               totalMCMCIter = 2000,
                               mcmcBurnIn = 200,
                               nu = 3,
                               q = 0.9,
                               k = 3,
                               sd = NULL,
                               Omega = 1,
                               LambdaRate = 25,
                               InitialSigma = "Naive",
                               metric = c("E", "S")
)
results_cylinder$inSampleRmse
## Prediction
preds_cyl <- predict(results_cylinder, X_cyl[-samp_C,])
cat("Test Set RMSE:", sqrt(mean((preds_cyl-Y_cyl[-samp_C])^2)), "\n")
plot_preds(results_cylinder, X_cyl[-samp_C,], Y_cyl[-samp_C], preds_cyl)

## Plotting tessellations
## General idea is to look for a tesselation with a good number of centres,
# and 2 covariates
## Boston
ncB <- map(results_Boston$posteriorTess[[1800]], dim)
tess_choiceB <- which(map_dbl(ncB, ~.[2]) == 2)[which.max(map_dbl(ncB, ~.[1])[which(map_dbl(ncB, ~.[2])==2)])]
tess_B <- results_Boston$posteriorTess[[1800]][[tess_choiceB]]
## Since using cellIndices directly, scaling doesn't happen so have to scale directly
p_grid_B <- as.matrix(
  expand.grid(
    seq(-0.5, 0.5, length.out = 200),
    seq(-0.5, 0.5, length.out = 200)
  )
)
cell_member_B <- cellIndices(p_grid_B, tess_B, as.integer(c(1,2)), metric = as.integer(c(0,0)))
ggplot(data = setNames(cbind.data.frame(p_grid_B, as.factor(cell_member_B)), c('x', 'y', 'cell')),
       aes(x = x, y = y, fill = cell)) +
  geom_raster() +
  viridis::scale_fill_viridis(discrete = TRUE, labels = seq_len(length(unique(cell_member_B))), name = "Cell")

## Sphere
sph_to_xyz <- function(x, r = 1) c(r*cos(x[2])*sin(-x[1]+pi/2), r*sin(x[2])*sin(-x[1]+pi/2), r*cos(-x[1]+pi/2))
ncS <- map(results_weather$posteriorTess[[1800]], dim)
tess_choiceS <- which(map_dbl(ncS, ~.[2]) == 2)[which.max(map_dbl(ncS, ~.[1])[which(map_dbl(ncS, ~.[2])==2)])]
tess_S <- results_weather$posteriorTess[[1800]][[tess_choiceS]]

p_grid_S <- as.matrix(expand.grid(
  seq(-pi/2, pi/2, length.out = 200),
  seq(-pi, pi, length.out = 200)
))
p_grid_S_plot <- t(apply(p_grid_S, 1, sph_to_xyz))
cell_member_S <- cellIndices(p_grid_S, tess_S, as.integer(c(1,2)), metric = as.integer(c(1,1)))
plot_data_S <- cbind.data.frame(p_grid_S_plot, as.factor(cell_member_S)) |> setNames(c('x','y','z','cell'))
plot3d(x = plot_data_S$x, plot_data_S$y, plot_data_S$z,
       col = viridis::viridis(nrow(tess_S))[plot_data_S$cell],
       type = 'p', xlab = 'x', ylab = 'y', zlab = 'z')

## Cylinder
cyl_to_xyz <- function(x, r = 1) c(r*cos(x[2]), r*sin(x[2]), x[1])
ncC <- map(results_cylinder$posteriorTess[[1800]], dim)
tess_choiceC <- which(map_dbl(ncC, ~.[2]) == 2)[which.max(map_dbl(ncC, ~.[1])[which(map_dbl(ncC, ~.[2])==2)])]
tess_C <- results_cylinder$posteriorTess[[1800]][[tess_choiceC]]

p_grid_C <- as.matrix(expand.grid(
  seq(-0.5, 0.5, length.out = 200),
  seq(-pi, pi, length.out = 200)
))
p_grid_C_plot <- t(apply(p_grid_C, 1, cyl_to_xyz))
cell_member_C <- cellIndices(p_grid_C, tess_C, as.integer(c(1,2)), metric = as.integer(c(0,1)))
plot_data_C <- cbind.data.frame(p_grid_C_plot, as.factor(cell_member_C)) |> setNames(c('x','y','z','cell'))
plot3d(x = plot_data_C$x, plot_data_C$y, plot_data_C$z,
       col = viridis::viridis(nrow(tess_C))[plot_data_C$cell],
       type = 'p', xlab = 'x', ylab = 'y', zlab = 'z')
