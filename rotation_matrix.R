library(MuscleTernary)

cross_product <- function(v1, v2) {
  c(v1[2]*v2[3] - v1[3]*v2[2],
    v1[3]*v2[1] - v1[1]*v2[3],
    v1[1]*v2[2] - v1[2]*v2[1])
}

rotation_matrix <- function(axis, angle) {
  # Make sure the axis is a unit vector
  axis <- axis / sqrt(sum(axis^2))

  # Calculate the rotation matrix
  x <- axis[1]
  y <- axis[2]
  z <- axis[3]
  c <- cos(angle)
  s <- sin(angle)
  t <- 1 - c
  matrix(c(t*x^2+c, t*x*y-s*z, t*x*z+s*y,
           t*x*y+s*z, t*y^2+c, t*y*z-s*x,
           t*x*z-s*y, t*y*z+s*x, t*z^2+c),
         nrow = 3, ncol = 3, byrow = TRUE)
}

#' Convert radians to degrees
#'
#' This function converts an angle from radians to degrees.
#'
#' @param x A numeric value representing an angle in radians.
#' @return A numeric value representing the angle in degrees.
#' @examples
#' rad2deg(pi/2)
#' # [1] 90
#' @export
rad2deg <- function(x) {
  return(x / pi * 180)
}



v1 <- matrix(c(1, 0, 0))
v2 <- matrix(c(0, 1, 0))

(axis_of_rotation <- cross_product(v1, v2) |> as.matrix())

(unit_axis <- axis_of_rotation / norm(axis_of_rotation))

(angle_of_rotation <- acos(dot(v1, v2) / (norm(v1) * norm(v2))))

(R <- rotation_matrix(unit_axis, angle_of_rotation))

(rotated_vector <- R %*% v1)

###

v1 <- matrix(c(1, 0, 0))

Left <- matrix(c(3.418143, -0.719809, -0.39438))
Right <- matrix(c(3.42447, -6.569044, 0.957253))
Ant <- matrix(c(-11.445497, -1.679053, 7.836357))

(Mid_LR <- rowMeans(cbind(Left, Right)))

(Ant_zero <- Ant - Mid_LR)

s <- read_stl("Mouse.stl")
head(s)

# Translate to origin
s_zero <- apply(s, MARGIN = 1, FUN = function(x) {x - Mid_LR}) |> t()
head(s_zero)

(axis_of_rotation <- cross_product(v1, Ant_zero) |> as.matrix())

(unit_axis <- axis_of_rotation / norm(axis_of_rotation))

(angle_of_rotation <- acos(dot(v1, Ant_zero) / (norm(v1) * norm(Ant_zero))))

(R <- rotation_matrix(unit_axis, angle_of_rotation))

R %*% Ant_zero

(xrot <- atan2(-R[3, 1], sqrt(R[3, 2]^2 + R[3, 3]^2)) |> rad2deg())
(yrot <- atan2(R[3, 2], R[3, 3]) |> rad2deg())
(zrot <- atan2(R[2, 1], R[1, 1]) |> rad2deg())

atan2(R[2, 1], R[2, 2])

s_rot <- apply(s_zero, 1, function(x) {R %*% x}) |> t()

# library(rgl)
# open3d()
# shade3d(as.mesh3d(s_rot))
# writeSTL("outfile.stl")
