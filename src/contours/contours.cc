/* Computes the level-set of a 2D function for plotting using Cairo compatible
 * drawing operations: move_to, line_to, curve_to and close_path.
 *
 * The algorithm computes the level-set in two stages. First it scans a (n x m)
 * grid for positive and negative function values, marking edges on the grid
 * for which the points have different signs. This information is kept in an
 * array called `tally`. This array has dimensions (2n+1 x 2m+1), where
 * elements with both indices being even represent the points on the grid,
 * while elements with just one even index are the edges. Two odd indices means
 * the cell spanned by four points, but these cells are not used in this
 * algorithm.
 *
 * In the second stage, we start 'walking' over edges that were marked in the
 * first stage.  We traverse the marked edges anti-clockwise around positive
 * areas, each time scanning edges adjacent to the current cell. In most cases
 * the cell will have two edges marked, in which case we will search these
 * edges for the correct root of the function along that edge. The direction of
 * the level-set is determined by a second root-finding on a small circle
 * around the point on the edge. These directions are used to create support
 * points for the Bezier curves.
 *
 * If this traversal reaches the boundary of the scanned area, we traverse the
 * boundary, always anti-clockwise as to enclose positive areas, until we find
 * the next boundary edge that was marked in the first stage, after which we
 * continue walking the level-set as described.
 *
 * The edges that were visited in this manner are marked `DONE` in the tally.
 * If the walk encounters a cell where the next edge is already marked done,
 * the curve is complete.  We repeat until all marked edges were visited.
 */

#include "grid.hh"
#include "pre-scan.hh"
#include "draw-path.hh"


extern "C" void contour(
    double (*f)(double, double),
    double s,
    double x0, double y0,
    double x1, double y1,
    unsigned n, unsigned m,

    void (*move_to)(double, double),
    void (*line_to)(double, double),
    void (*curve_to)(double, double, double, double, double, double),
    void (*close_path)())
{
    Grid grid(x0, y0, x1, y1, n, m);
    auto tally = pre_scan(f, s, grid);
    draw_path(f, s, grid, tally, move_to, line_to, curve_to, close_path);
}


#include <cairo/cairo.h>

extern "C" void cairo_contour(cairo_t *cr,
    double (*f)(double, double), double s,
    double x0, double y0,
    double x1, double y1,
    unsigned n, unsigned m)
{
    Grid grid(x0, y0, x1, y1, n, m);
    auto tally = pre_scan(f, s, grid);
    draw_path(f, s, grid, tally,
        std::bind(cairo_move_to, cr, _1, _2),
        std::bind(cairo_line_to, cr, _1, _2),
        std::bind(cairo_curve_to, cr, _1, _2, _3, _4, _5, _6),
        std::bind(cairo_close_path, cr));
}

// vim:ts=4:sw=4
