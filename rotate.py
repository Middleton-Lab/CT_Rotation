import math

def angle_between_vectors(v1, v2):
    dot_product = sum((a*b) for a, b in zip(v1, v2))
    magnitude_v1 = math.sqrt(sum(a**2 for a in v1))
    magnitude_v2 = math.sqrt(sum(b**2 for b in v2))
    cos_theta = dot_product / (magnitude_v1 * magnitude_v2)
    return math.acos(cos_theta)

# Example usage:
v1 = [1, 2, 3]
v2 = [4, 5, 6]
angle = angle_between_vectors(v1, v2)
print(angle)

# Test case 1
v1 = [1, 0, 0]
v2 = [0, 1, 0]
angle = angle_between_vectors(v1, v2)
assert math.isclose(angle, math.pi/2)

# Test case 2
v1 = [1, 0, 0]
v2 = [1, 0, 0]
angle = angle_between_vectors(v1, v2)
assert math.isclose(angle, 0)

# Test case 3
v1 = [1, 0, 0]
v2 = [-1, 0, 0]
angle = angle_between_vectors(v1, v2)
assert math.isclose(angle, math.pi)

# Test case 4
v1 = [1, 2, 3]
v2 = [4, 5, 6]
angle = angle_between_vectors(v1, v2)
assert math.isclose(angle, 0.2257261285527342)