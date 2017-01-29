library(rgl)

x = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
y = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
z = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)

rgl::plot3d(x = x, y = y, z = z, size = 10, col = "steelblue", expand=1.2)
