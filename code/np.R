# read data
data_path = "../Data/return.txt"
data_return = read.table(data_path, header = FALSE)
y = data_return[, 6]*100
x = data_return[, 8]*100

# define functions
source("./functions.R")

### set initial values ###

# set points we estimate
cy = seq(from = -10, to = 10, by = 0.01)
cx = seq(from = -3 , to = 3 , by = 0.01)

# number of the data
n = nrow(data_return)

# set bandwidth
# bandwidths h variables defined as
#     h = c * (q[4] - q[2]) * n^(-1/5)
# where
#     q[4] is 75% quantile
#     q[2] is 25% quantile
# and
#     c takes three values 0.75, 7.5, 0.075.
q_y = quantile(y)
h0_y = as.numeric(0.75 *(q_y[4] - q_y[2])*(n^(-1/5)))
h1_y = as.numeric(7.5  *(q_y[4] - q_y[2])*(n^(-1/5)))
h2_y = as.numeric(0.075*(q_y[4] - q_y[2])*(n^(-1/5)))

density_y = kernel_density(y = y, h = h0_y, cy = cy)
# plot
pdf("../Figures/Figure01.pdf", width = 15, height = 10)
plot(x = cy, y = density_y, type = "l", lwd = 2,
    main = "Density of the return of IBM",
    ylab = "Density", xlab = "",
)
grid()
dev.off()

# define bandwidth of kernel regression
q_x = quantile(x)
h0_x = as.numeric(0.75  *(q_x[4] - q_x[2])*(n^(-1/5)))
h1_x = as.numeric(7.5   *(q_x[4] - q_x[2])*(n^(-1/5)))
h2_x = as.numeric(0.075 *(q_x[4] - q_x[2])*(n^(-1/5)))

# estimate by kernel regression using h0, h1, h2
out_h0 = kernel_regression(y = y, x = x, h = h0_x, cx = cx)
out_h1 = kernel_regression(y = y, x = x, h = h1_x, cx = cx)
out_h2 = kernel_regression(y = y, x = x, h = h2_x, cx = cx)

# plot
pdf("../Figures/Figure02.pdf", width = 15, height = 10)
    par(mfrow = c(2, 2))
        plot(x, y, pch = 20, col = "lightgray", ylim = c(-4, 4), xlim = c(-2.5, 2.5))
        points(cx, out_h0, type = "l", lwd = 2)
        grid()
        plot(x, y, pch = 20, col = "lightgray", ylim = c(-4, 4), xlim = c(-2.5, 2.5))
        points(cx, out_h1, type = "l", lwd = 2)
        grid()
        plot(x, y, pch = 20, col = "lightgray", ylim = c(-4, 4), xlim = c(-2.5, 2.5))
        points(cx, out_h2, type = "l", lwd = 2)
        grid()
        plot(x, y, pch = 20, col = "lightgray", ylim = c(-4, 4), xlim = c(-2.5, 2.5))
        X = cbind(1, x)
        beta.hat = matrix(solve(t(X)%*%X)%*%t(X)%*%y, ncol = 1)
        fit.X = cbind(1, cx)
        y.hat = fit.X%*%beta.hat
        points(cx, y.hat, type = "l", lwd = 2)
    par(mfrow = c(1, 1))
dev.off()
