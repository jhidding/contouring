#pragma once
#include <functional>
#include <vector>

#include "grid.hh"
#include "common.hh"

/*!
 * `pre_scan` is used to sample the given function at given
 * grid locations.
 *
 * @param f     Function to contour.
 * @param s     Level at which to draw the contour.
 * @param grid  Grid specification.
 *
 * Returns a `std::vector<int>` that can be passed to `draw_path`
 * function.
 */
extern std::vector<int> pre_scan(
                std::function<double (double, double)> f,
                double s,
                Grid const &grid);
