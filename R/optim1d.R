# Создание директории для картинок ----------------------------------------

directory_out <- "./figures/optim1d/"


if (!dir.exists("./figures")) {
    dir.create("./figures")
}

if (!dir.exists(directory_out)) {
    dir.create(directory_out)
}


# Функция которую мы оптимизируем -----------------------------------------

f_function <- function(x) return(x^2 + x + x*sin(x))
grad_f_function <- function(x) return(x*2 + 1 + sin(x) + x * cos(x))
x_min = -20
x_max = 20
x_step = 0.0001
x <- seq(x_min, x_max, x_step)


# Начальные данные --------------------------------------------------------

init_x = 12
alpha = 0.15
steps <- 15

# Начальный график --------------------------------------------------------

filename <- paste0("./figures/optim1d/x2_x_xsinx__initx_", 
                   init_x, 
                   "_alpha_", 
                   floor(alpha),
                   "dot", round((alpha - floor(alpha)) * 100), "_steps_", steps)

pdf(paste0(filename, ".pdf"))
matplot(x, 
        f_function(x), 
        type = "l", 
        col = "red3",
        lwd = I(2),
        main = paste0("x**2 + x + x * sin(x), init_x = ", init_x, ", alpha = ", alpha),
        xlim = c(-12, 12),
        ylim = c(0, f_function(12)))
abline(h = 0)
abline(v = 0)
abline(v = seq(x_min, x_max, round((x_max - x_min) / 21, 2)), 
       col = "grey3",
       lty = 3,
       lwd = I(0.5))
abline(h = seq(0, f_function(x_max), length.out = 21), 
       col = "grey3",
       lty = 3,
       lwd = I(0.5))
arrows(x0 = min(f_function(x)), x1 = min(f_function(x)), y0 = 40, y1 = 1, col = "brown", lwd = I(2))
segments(x0 = init_x,
         x1 = init_x,
         y0 = 0,
         y1 = f_function(init_x),
         col = "blue",
         lwd = I(2))


# Вычисление оптимизации --------------------------------------------------
x_vec <- numeric(steps + 1)
grads <- numeric(steps)
x_vec[1] <- init_x
for (i in 2:(steps + 1)){
    grads[i - 1] <- grad_f_function(x_vec[i - 1])
    x_vec[i] <- x_vec[i - 1] - alpha * grad_f_function(x_vec[i - 1])
} 

x_vec


# Отрисовка шагов ---------------------------------------------------------

for (i in 1:steps) {
    arrows(x0 = x_vec[i], x1 = x_vec[i + 1], 
           y0 = f_function(x_vec[i]), y1 = f_function(x_vec[i]), 
           col = "blue", 
           angle = 10, lwd = I(0.5), length = 0.3 * (1/i))
    arrows(x0 = x_vec[i + 1], x1 = x_vec[i + 1], 
           y0 = f_function(x_vec[i]), y1 = f_function(x_vec[i + 1]), 
           col = "blue", 
           angle = 10, lwd = I(0.5), length = 0.3 * (1/i))
    segments(x0 = x_vec[i + 1],
             x1 = x_vec[i + 1],
             y0 = 0,
             y1 = f_function(x_vec[i + 1]),
             col = "blue",
             lwd = I(1),
             lty = 3)
}

dev.off()

# Вывод вычислений в tex --------------------------------------------------
write("\\begin{array}{c}", file = paste0(filename,".txt"), append = F)
cat("\n\\begin{array}{c}")
cat("\n")
write(paste0("x_0 = ", init_x, ";\\ \\alpha = ", alpha, ";\\ i = ", 1, ",\\dots,",  steps,  "\\\\"), file = paste0(filename,".txt"), append = T)
write(paste0("x_{i} = x_{i - 1} - \\alpha \\cdot \\frac{\\partial f( x_{i - 1})}{\\partial x} \\\\"), file = paste0(filename,".txt"), append = T)
write("\\\\",  file = paste0(filename,".txt"), append = T)
for (i in 1:steps) {
    write(paste0("x_{", i, "} = ", round(x_vec[i], 3), " - ", round(alpha, 2), " \\cdot ", round(grads[i], 3), " = ", round(x_vec[i + 1], 3)), 
          file = paste0(filename,".txt"), append = T)
    cat(paste0("x_{", i, "} = x_{", i - 1, "} - \\alpha \\cdot \\frac{\\partial f(x)}{\\partial x} \\right|_{x = x_{", i - 1, "}}"))
    if (!(i == steps)) {
        write("\\\\", file = paste0(filename,".txt"), append = T)
        cat("\\\\")
    }
}
write("\n\\end{array}\n", file = paste0(filename,".txt"), append = T)
cat("\n\\end{array}\n")
