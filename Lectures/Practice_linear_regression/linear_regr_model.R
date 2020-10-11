# Задание параметров эксперимента -----------------------------------------
x <- seq(0, 7, 0.01)
len_var <- length(x)
n <- 3
a <- c(-51, 17, 6, -0.9)
sd_coeff <- 160
mean_coeff <- 0
iterations <- 1000

# Задание матрицы входов и выходов ----------------------------------------
y_func <- function(n, x, a, noise) {
    stopifnot(length(a) == (n + 1))
    X <- matrix(ncol = (n + 1), nrow = length(x))
    for (i in 0:n) {
        X[, i + 1] <- x^i
    }
    y <- X[, 1:(n + 1)] %*% a + noise
    return(list(matrix_input = X, vector_output = y))
}

regression_model <- function(X, y) {
    return(as.vector(solve(t(X) %*% X) %*% t(X) %*% y))
}

get_output <- function(x, a) {
    y <- numeric(length = length(x))
    for (i in 1:length(a)) {
        y <- y + a[i] * (x ^ (i - 1))
    }
    return(y)
}

Monte_Carlo_method <- function(n, x, a, mean_coeff, sd_coeff, iterations) {
    len_var <- length(x)
    A <- matrix(nrow = iterations, ncol = length(a))
    for (iter in 1:iterations) {
        X_monte <- y_func(n = n, x = x, a = a, noise = rnorm(len_var, mean_coeff, sd_coeff))
        a_monte <- regression_model(X_monte[[1]], X_monte[[2]])
        A[iter, ] <- a_monte
    }
    return(apply(A, 2, mean))
}

data <- y_func(n = n, x = x, a = a, noise = rnorm(len_var, mean_coeff, sd_coeff))
y <- get_output(x, a)

a_regr <- regression_model(data[[1]], data[[2]])
y_regr <- get_output(x, a_regr)

a_monte <- Monte_Carlo_method(n, x, a, mean_coeff, sd_coeff, iterations)
y_monte <- get_output(x, a_monte)


# отрисовка графика, полученного в результате моделирования с шумом -------
plot(x = x, 
     y = y, 
     type = "l", 
     lwd = 2,  
     col = "red",
     main = "График заданной функции с шумом",
     ylab = "y(x)",
     xlab = "x")
points(x = x, 
       y = data[[2]], 
       type = "p", 
       cex = 1, 
       pch = 20, 
       col = "black")
lines(y = y_regr, x = x, col = "blue", lwd = 2)
lines(y = y_monte, x = x, col = "green", lwd = 2)

plot(x = x, 
     y = y, 
     type = "l", 
     lwd = 2,  
     col = "red",
     main = "График заданной функции с шумом",
     ylab = "y(x)",
     xlab = "x")
points(x = x, 
       y = data[[2]], 
       type = "p", 
       cex = 1, 
       pch = 20, 
       col = "black")
lines(y = y_regr, 
      x = x, 
      col = "blue", 
      lwd = 2)
lines(y = y_monte, 
      x = x, 
      col = "forestgreen", 
      lwd = 2)


print(a - a_regr)
print(a - a_monte)
