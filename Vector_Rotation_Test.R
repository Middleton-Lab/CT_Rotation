angle_between_3d_vectors <- function(v1, v2) {
  # Check if the input vectors are of the same length
  if (length(v1) != length(v2)) {
    stop("The input vectors must be of the same length")
  }
  
  # Check that the vectors are different
  if (identical(v1, v2)) {
    stop("v1 and v2 can't be identical.")
  }
  
  # Calculate the dot product
  dot_product <- sum(v1 * v2)
  
  # Calculate the magnitudes of the vectors
  v1_magnitude <- sqrt(sum(v1^2))
  v2_magnitude <- sqrt(sum(v2^2))
  
  # Calculate the angle
  angle <- acos(dot_product / (v1_magnitude * v2_magnitude))
  
  # Return the angle in radians
  return(angle)
}

rotation_matrix <- function(angle, v) {
  # Check if the input vector is not of length 3
  if (length(v) != 3) {
    stop("The input vector must be of length 3")
  }
  
  # Calculate the rotation matrix
  # angle is in radians
  R <- matrix(c(
    cos(angle), -sin(angle), 0,
    sin(angle), cos(angle), 0,
    0, 0, 1
  ), nrow = 3)
  
  # Return the rotation matrix
  return(R)
}

rotate_vector <- function(v1, v2, angle) {
  # Rotate v1 onto v2
  
  # Check if the input vectors are of the same length
  if (length(v1) != length(v2)) {
    stop("The input vectors must be of the same length")
  }
  
  # Calculate the rotation matrix
  # angle is in radians
  R <- rotation_matrix(angle, v2)
  
  # Rotate the vector
  v3 <- R %*% v1
  
  # Return the rotated vector
  return(v3)
}

rad2deg <- function(x) {
  return(x / pi * 180)
}

v1 <- c(1, 0, 0)
v2 <- c(0, 1, 0)
(angle <- angle_between_3d_vectors(v1, v2))

rotate_vector(v1, v2, angle)
rotate_vector(v2, v1, angle)
# [1] 0 0 1

###

v1 <- c(1, 0, 0)
v2 <- c(0, 1, 0)
(angle <- angle_between_3d_vectors(v1, v2))

rad2deg(angle)

v2 <- Ant_zero

rotate_vector(v1, v2, angle)
rotate_vector(v2, v1, angle)

###

library(MuscleTernary)
library(rgl)

s <- read_stl("~/Documents/GitHub/MuscleTernary/inst/extdata/L_mPTd_Or.stl")

s_rot <- apply(s, 1, function(x) {R %*% x}) |> t()
class(s_rot) <- "mesh3d"

head(s)
head(s_rot)

open3d()
shade3d(as.mesh3d(s))

open3d()
shade3d(as.mesh3d(s_rot))
writeSTL("outfile.stl")
