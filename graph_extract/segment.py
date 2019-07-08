import numpy as np 
import quickshift

def make_clusters(infname, width, height):

    mat_dtype = np.dtype([('ks', np.int64), ('vs', np.float64)])

    struct_mat = np.fromfile(infname, dtype=mat_dtype).reshape(
        (width, height, -1))

    image_ks = struct_mat['ks']
    image_vs = struct_mat['vs']

    labels = quickshift._quickshift_cython(
        image_ks, image_vs, 5, 2, False, 42)

    window_size_x = 10
    window_size_y = 10
    for r in range(width-window_size_x):
        for c in range(height-window_size_y):
            window = labels[r:(r + window_size_x), c:(c + window_size_y)]
            vals, counts = np.unique(window, return_counts=True)
            target_val = vals[np.argmax(counts)]
            labels[r, c] = target_val

    labels_clean = labels.copy()
    window_size_x = 10
    window_size_y = 10
    for r in range(width-window_size_x):
        for c in range(height-window_size_y):
            window = labels_clean[r:(
                r + window_size_x), c:(c + window_size_y)]
            vals, counts = np.unique(window, return_counts=True)
            target_val = vals[np.argmax(counts)]
            labels_clean[r, c] = target_val

    return labels

if __name__ == '__main__':
    import sys

    infname = sys.argv[1]
    height = int(sys.argv[2])
    width = int(sys.argv[3])
    outfname = sys.argv[4]


    make_clusters(infname, width, height).tofile(outfname)
