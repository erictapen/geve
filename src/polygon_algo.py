import numpy as np
from matplotlib import pyplot as plt


def pts():
    """ generate 4 random points in [0,2] x [0,2] """
    ret = 2*np.random.random(size=(4, 2))
    return ret


def poly(P):
    """ Return a closed polygon from 4 points in P according to the algorithym
        described here:
        https://stackoverflow.com/questions/14263284/create-non-intersecting-\
        polygon-passing-through-all-given-points
    """
    # the left- and rightmost point:
    lft = np.argmin(P[:, 0])
    rgt = np.argmax(P[:, 0])

    p = P[lft]
    q = P[rgt]

    rest = np.delete(P, (lft, rgt), axis=0)

    A = []
    B = []
    for it in rest:
        is_abo = is_above(p, q, it)
        if is_abo < 0:
            B.append(it)
        else:
            A.append(it)

    ret = np.array([p])
    if A:
        A = np.array(A)
        A = A[np.argsort(A[:, 0])]
        ret = np.concatenate((ret, A))
    ret = np.concatenate((ret, [q]))
    if B:
        B = np.array(B)
        B = B[np.argsort(-B[:, 0])]
        ret = np.concatenate((ret, B))

    return np.concatenate((ret, [p]))


def is_above(p, q, point):
    """ check wether 'point' is above the line q-p """
    return (p[0] - point[0]) * (q[1] - point[1]) \
        - (p[1] - point[1]) * (q[0] - point[0])


if __name__ == '__main__':
    for it in range(10):
        P = poly(pts())
        plt.plot(2*it + P[:, 0], P[:, 1])
    plt.show()
