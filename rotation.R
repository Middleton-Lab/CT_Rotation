cross_product <- function(a, b) {
    if (length(a) != 3 || length(b) != 3) {
        stop("Both vectors must have length 3")
    }
    c(a[2]*b[3] - a[3]*b[2], a[3]*b[1] - a[1]*b[3], a[1]*b[2] - a[2]*b[1])
}

get_rodriguez <- function(v1, v2) {
    # Normalize the input vectors
    v1 <- v1 / sqrt(sum(v1^2))
    v2 <- v2 / sqrt(sum(v2^2))
    
    # Calculate the cross product of the input vectors
    cp <- cross_product(v1, v2)
    
    # Calculate the skew-symmetric matrix of the cross product
    cp_skew <- matrix(c(0, -cp[3], cp[2], cp[3], 0, -cp[1], -cp[2], cp[1], 0), nrow = 3)
    
    # Calculate the rotation matrix using the Rodrigues' formula
    R <- diag(3) + cp_skew + cp_skew %*% cp_skew * (1 - sum(v1 * v2))
    
    return(R)
}

# Define the vector to be rotated
v1 <- c(1, 2, 3)

# Define the target vector
v2 <- c(1, 0, 0)

# Rotate v1 to lie on the x-axis
R <- get_rodriguez(v1, v2)
v1_rotated <- t(matrix(v1)) %*% R

# Print the rotated vector
print(v1_rotated)


