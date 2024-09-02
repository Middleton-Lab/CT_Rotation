rodrigues_to_euler <- function(R) {
    # Extract the rotation angles from the rotation matrix
    theta_x <- atan2(R[3, 2], R[3, 3])
    theta_y <- atan2(-R[3, 1], sqrt(R[3, 2]^2 + R[3, 3]^2))
    theta_z <- atan2(R[2, 1], R[1, 1])
    
    # Return the rotation angles as a vector
    return(c(theta_x, theta_y, theta_z))
}

rodrigues_to_euler(R)
