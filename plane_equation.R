
# Function to find the plane given three coordinates in 3d space
# p1, p2, and p3 are vectors of x, y, z coordinates
plane3d <- function(p1, p2, p3) {
  # Find the vector between p1 and p2
  v1 <- p2 - p1
  
  # Find the vector between p1 and p3
  v2 <- p3 - p1
  
  # Find the cross product of v1 and v2
  cross <- MuscleTernary::CrossProduct(v1, v2)
  
  # Find the equation of the plane
  a <- cross[1]
  b <- cross[2]
  c <- cross[3]
  d <- -(a * p1[1] + b * p1[2] + c * p1[3])
  
  # Return the equation of the plane
  return(c(a, b, c, d))
}

p1 <- c(0, 0, 0)
p2 <- c(0, 1, 0)
p3 <- c(1, 0, 0)

plane3d(p1, p2, p3)
