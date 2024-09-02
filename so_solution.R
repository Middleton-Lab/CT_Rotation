library(tidyverse)
library(plotly)

dot <- function(u, v) {
  return(sum(u * v))
}

normalize <- function(u) {
  return(sqrt(sum(u ^ 2)))
}

ssc <- function(v) {
  return(matrix(c(    0,   -v[3],  v[2],
                   v[3],       0, -v[1],
                  -v[2],    v[1],    0),
                nrow = 3, byrow = TRUE))
}

RU <- function(A, B) {
  A_norm <- A / norm(as.matrix(A))
  B_norm <- B / norm(as.matrix(B))
  
  # Calculate the rotation matrix. Note that the order is x, y, x.
  R <- diag(3) +
    ssc(xprod(A_norm, B_norm)) +
    ((ssc(xprod(A_norm, B_norm)) %*%
        ssc(xprod(A_norm, B_norm))) *
       (1 - dot(A_norm, B_norm)) /
       (dot(norm(xprod(A_norm, B_norm)),
            norm(xprod(A_norm, B_norm))))
    )
  return(R)
}

euler <- function(R) {
  xrot <- atan2(R[3, 2], R[3, 3]) * 180 / pi
  yrot <- atan2(-R[3, 1], sqrt(R[3, 2] ^ 2 + R[3, 3] ^ 2)) * 180 / pi
  zrot <- atan2(R[2, 1], R[1, 1]) * 180 / pi
  
  return(c(xrot, yrot, zrot))
}

xprod <- function(a, b) {
  if (length(a) & length(b) != 3) {
    stop("a and b must be of length 3.")
  }
  cp <- c(a[2] * b[3] - a[3] * b[2],
          a[3] * b[1] - a[1] * b[3],
          a[1] * b[2] - a[2] * b[1])
  return(as.matrix(cp))
}

# Source
v0 <- c(0, 2, 0)
v1 <- matrix(c(0, 2, 0, 0, 3, 0), ncol = 3, byrow = TRUE)

# Target
v2 <- c(1, 0, 0)

R <- RU(v0, v2)
euler(R)

# Rotations
v1 %*% t(R)

##############################################

R <- c(3.9203848838806152, -0.6056719422340393, -0.5852092504501343)
L <- c(3.8098082542419434, -6.479684352874756, 0.8229515552520752)
Ant <- c(-11.43181324005127, -1.661534309387207, 7.838191509246826)
names(L) <- c("x", "y", "z")
names(R) <- c("x", "y", "z")
names(Ant) <- c("x", "y", "z")

origin <- colMeans(matrix(c(L, R), ncol = 3, byrow = TRUE))

Lc <- L - origin
Rc <- R - origin
Antc <- Ant - origin

O <- c(0, 0, 0)
names(O) <- c("x", "y", "z")

D <- matrix(c(Lc, Rc, Antc, O), ncol = 3, byrow = TRUE)

plot_ly() |> add_markers(data = as.data.frame(D), x = ~V1, y = ~V2, z = ~V3)

# Define a plane containing (0, 1, 0), (0, -1, 0), and (1, 0, 0).

plane3d <- function(p1, p2, p3){
  names(p1) <- NULL
  names(p2) <- NULL
  names(p3) <- NULL
  a1 <- p2[1] - p1[1]
  b1 <- p2[2] - p1[2]
  c1 <- p2[3] - p1[3]
  a2 <- p3[1] - p1[1]
  b2 <- p3[2] - p1[2]
  c2 <- p3[3] - p1[3]
  a <- b1 * c2 - b2 * c1
  b <- a2 * c1 - a1 * c2
  c <- a1 * b2 - b1 * a2
  d <- (- a * p1[1] - b * p1[2] - c * p1[3])
  
  c("a" = a, "b" = b, "c" = c, "d" = d)
}

p1 <- c(0, 1, 0)
p2 <- c(0, -1, 0)
p3 <- c(1, 0, 0)

XY_plane <- plane3d(p1, p2, p3)

# Find the plane defined by L, R, and Ant
LRA_plane <- plane3d(L, R, Ant)

# Cross product of the xyz coefficients = direction vector
cp <- xprod(XY_plane[1:3], LRA_plane[1:3])
cp_norm <- cp / norm(cp)

# Find a point on the line

# y = (-123 - 48x) / 20.6
y <- function(x, LRA_plane) {
  return((LRA_plane['d'] - LRA_plane['a'] * x) / LRA_plane['d'])
}

LRA_pts <- expand_grid(x = seq(-15, 5, by = 0.5),
                       y = seq(-3, 3, by = 0.5)) |> 
  mutate(z = (LRA_plane['d'] + LRA_plane['a'] * x + LRA_plane['b'] * y) / 
             -LRA_plane['c'])

plane_line <- tibble(x = seq(-5, 5, length.out = 50),
                     y = y(x, LRA_plane),
                     z = 0)

plot_ly() |> 
  add_markers(data = as.data.frame(D), x = ~V1, y = ~V2, z = ~V3,
              marker = list(color = "red")) |> 
  add_markers(data = plane_line, x = ~x, y = ~y, z = ~z,
              marker = list(size = 2, color = "blue")) |> 
  add_trace(data = LRA_pts,
            x = ~x, y = ~y, z = ~z,
            type = "surface")
