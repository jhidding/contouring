#include "pre-scan.hh"

#include <utility>
#include <experimental/tuple>

using std::experimental::apply;

inline int sign(double x)
{
    if (x < 0) return -1;
    return +1;
}

std::vector<int> pre_scan(
        std::function<double (double, double)> f,
        double s,
        Grid const &grid)
{
    unsigned n = grid.n, m = grid.m, dy = 2*n + 1;
    std::vector<int> result((n*2 + 1) * (m*2 + 1));

    // get the signs of the function on the grid
    size_t o = 0;
    for (unsigned j = 0; j <= m; ++j, o += 2*n)
    {
        for (unsigned i = 0; i <= n; ++i, o += 2)
        {
            result[o] = sign(apply(f, grid(i, j)) - s);
        }
    }

    // see where the function crosses zero on the
    // horizontal edges
    o = 1;
    for (unsigned j = 0; j <= m; ++j, o += n*2+2)
    {
        for (unsigned i = 0; i < n; ++i, o += 2)
        {
            result[o] = (result[o-1] * result[o+1] < 0 ? TODO : CLEAR);
        }
    }

    // see where the function crosses zero on the
    // vertical edges
    o = 2*n+1;
    for (unsigned j = 0; j < m; ++j, o += n*2)
    {
        for (unsigned i = 0; i <= n; ++i, o += 2)
        {
            result[o] = (result[o-dy] * result[o+dy] < 0 ? TODO : CLEAR);
        }
    }

    return std::move(result);
}

