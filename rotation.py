"""Rotate a 3D mesh into standard anatomical orientation using landmark points.

Usage:
    python rotation.py <stl_file> <left.fcsv> <right.fcsv> <anterior.fcsv>

The output STL is written to the same directory as the input with '_zero' appended
to the filename (e.g. Mouse.stl -> Mouse_zero.stl).
"""

import argparse
from pathlib import Path

import numpy as np
from scipy.spatial.transform import Rotation
from stl import mesh


def read_fcsv(filepath):
    """Read a 3DSlicer .fcsv file and return the (x, y, z) coordinates."""
    with open(filepath) as f:
        for line in f:
            if line.startswith("#"):
                continue
            parts = line.strip().split(",")
            return np.array([float(parts[1]), float(parts[2]), float(parts[3])])


def compute_rotation(L, R, Ant):
    """Compute the translation and two-step rotation from landmark points.

    Two-step rotation:
      1. Align the landmark plane with the XY plane (normal -> Z)
      2. Rotate around Z to place Ant on the positive X-axis

    Returns:
        translation: 3-vector to apply before rotation
        rot_matrix: 3x3 combined rotation matrix
        quaternion: combined quaternion (x, y, z, w)
        q1: first rotation (plane -> XY)
        q2: second rotation (Ant -> X-axis)
    """
    # Translation: midpoint of L-R to origin
    midpoint = (L + R) / 2
    translation = -midpoint

    L_t = L + translation
    R_t = R + translation
    Ant_t = Ant + translation

    # Rotation 1: align plane normal with Z-axis
    lr_vec = R_t - L_t
    ant_vec = Ant_t
    normal = np.cross(ant_vec, lr_vec)
    normal = normal / np.linalg.norm(normal)

    axis = np.cross(normal, [0, 0, 1])
    axis_norm = np.linalg.norm(axis)
    angle = np.arccos(np.clip(np.dot(normal, [0, 0, 1]), -1, 1))

    if axis_norm < 1e-10:
        q1 = Rotation.identity()
    else:
        rotvec = (axis / axis_norm) * angle
        q1 = Rotation.from_rotvec(rotvec)

    R1 = q1.as_matrix()
    Ant_r1 = R1 @ Ant_t

    # Rotation 2: rotate around Z to align Ant with X-axis
    theta = np.arctan2(Ant_r1[1], Ant_r1[0])
    q2 = Rotation.from_rotvec([0, 0, -theta])

    # Combined rotation
    q_total = q2 * q1
    rot_matrix = q_total.as_matrix()
    quaternion = q_total.as_quat()

    return translation, rot_matrix, quaternion, q1, q2


def transform_point(p, translation, rot_matrix):
    """Apply translation then rotation to a point."""
    return rot_matrix @ (p + translation)


def transform_mesh(stl_path, translation, rot_matrix, output_path):
    """Load an STL, apply the transform, and save the result."""
    mouse_mesh = mesh.Mesh.from_file(str(stl_path))

    for i in range(len(mouse_mesh.vectors)):
        for j in range(3):
            mouse_mesh.vectors[i][j] = transform_point(
                mouse_mesh.vectors[i][j], translation, rot_matrix
            )

    mouse_mesh.update_normals()
    mouse_mesh.save(str(output_path))
    return len(mouse_mesh.vectors)


def main():
    parser = argparse.ArgumentParser(
        description="Rotate a 3D mesh into standard orientation using anatomical landmarks."
    )
    parser.add_argument("stl", type=Path, help="Input STL file")
    parser.add_argument("left", type=Path, help="Left landmark .fcsv file")
    parser.add_argument("right", type=Path, help="Right landmark .fcsv file")
    parser.add_argument("anterior", type=Path, help="Anterior landmark .fcsv file")
    args = parser.parse_args()

    # Read landmarks
    L = read_fcsv(args.left)
    R = read_fcsv(args.right)
    Ant = read_fcsv(args.anterior)

    print(f"L   = {L}")
    print(f"R   = {R}")
    print(f"Ant = {Ant}")

    # Compute rotation
    translation, rot_matrix, quaternion, q1, q2 = compute_rotation(L, R, Ant)

    # Verify with landmarks
    L_new = transform_point(L, translation, rot_matrix)
    R_new = transform_point(R, translation, rot_matrix)
    Ant_new = transform_point(Ant, translation, rot_matrix)

    print(f"\nTransformed landmarks:")
    print(f"  L   = {L_new}")
    print(f"  R   = {R_new}")
    print(f"  Ant = {Ant_new}")

    print(f"\nVerification:")
    print(f"  Ant on X-axis? y≈0: {abs(Ant_new[1]) < 1e-10}, z≈0: {abs(Ant_new[2]) < 1e-10}")
    print(f"  L in XY plane? z≈0: {abs(L_new[2]) < 1e-10}")
    print(f"  R in XY plane? z≈0: {abs(R_new[2]) < 1e-10}")

    # Transform and save STL
    output_path = args.stl.parent / f"{args.stl.stem}_zero{args.stl.suffix}"
    n_triangles = transform_mesh(args.stl, translation, rot_matrix, output_path)
    print(f"\nTransformed {n_triangles} triangles -> {output_path}")

    # Record transformations
    print(f"\n=== Transformation Record ===")
    print(f"Translation vector: {translation}")
    print(f"Rotation matrix:\n{rot_matrix}")
    print(f"Quaternion (x, y, z, w): {quaternion}")
    print(f"Quaternion q1 (plane→XY): {q1.as_quat()}")
    print(f"Quaternion q2 (Ant→X):    {q2.as_quat()}")


if __name__ == "__main__":
    main()
