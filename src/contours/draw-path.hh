#pragma once
#include <vector>
#include <tuple>
#include <functional>
#include <numeric>
#include <experimental/tuple>

#include "grid.hh"
#include "common.hh"

#include "../numeric/roots.hh"

using namespace std::placeholders;

/* We use Ridders' algorithm to find roots */
using numeric::find_root_ridders;

/* Using tuples, arrays, bind and the likes */
using std::experimental::apply;

/* Every time we set a step on the grid, we have to decide
 * wether to draw a curve, line or close the path, depending
 * on the state of the tally.
 */
enum Continuation { CURVE, LINE, CLOSE_PATH };

/* Traveling the grid, there are four posible directions. Each
 * direction has a corresponding angle
 */
enum Direction { RIGHT, UP, LEFT, DOWN };
extern double angle[4];

inline bool even(unsigned i)
{
    return i % 2 == 0;
}

/*!
 * `draw_path` is the core function of the contouring library.
 *
 * @param f         Function `f(double x, double y) -> double`. The function
 *                  that needs contouring.
 * @param s         Level at which the contour should be drawn.
 * @param grid      Grid at which the function should be sampled.
 * @param tally     Result of the pre-scan.
 *
 * The other parameters should be call-backs for the drawing functions.
 * These commands are modelled after the Cairo API and work well with
 * SVG paths for instance.
 *
 * @param move_to    Function of (x, y), command to move pen.
 * @param line_to    Function of (x, y), draw line.
 * @param curve_to   Draw Bezier curve (x1, y1, x2, y2, x3, y3).
 * @param close_path Close the path (no arguments).
 */
template <typename F, typename F2, typename F6, typename F0>
void draw_path(
        F f, double s,
        Grid const &grid,
        std::vector<int> &tally,

        // these are supposed to be the cairo drawing routines.
        F2 move_to, F2 line_to, F6 curve_to, F0 close_path)
{
    unsigned n = grid.n, m = grid.m, dy = 2*n+1;

    auto find_intersection = [&] (unsigned i, unsigned j, size_t o)
    {
        double x, y;
        Direction d;

        if (even(i))                     // vertical grid line
        {
            x = grid.x(double(i)/2.);
            double y0 = grid.y(double(j-1)/2.),
                   y1 = grid.y(double(j+1)/2.);

            y = find_root_ridders(
                std::bind(f, x, _1),
                y0, y1, s, epsilon);

            d = (tally[o - dy] < 0 ? RIGHT : LEFT);
        } else {                         // horizontal grid line
            y = grid.y(double(j)/2.);
            double x0 = grid.x(double(i-1)/2.),
                   x1 = grid.x(double(i+1)/2.);

            x = find_root_ridders(
                std::bind(f, _1, y),
                x0, x1, s, epsilon);
            d = (tally[o - 1] < 0 ? DOWN : UP);
        }

        return std::make_tuple(x, y, d);
    };

    auto walk_edge = [&] (unsigned i, unsigned j, size_t o)
    {
        do {
            if (i == 0) // go down
            {
                if (j == 1)
                {
                    line_to(grid.x0, grid.y0);
                    i = 1; j = 0; o -= dy-1; continue;
                }
                j -= 2; o -= 2*dy; continue;
            }

            if (j == 0) // go right
            {
                if (i == 2*n-1)
                {
                    line_to(grid.x1, grid.y0);
                    i = 2*n; j = 1; o += dy+1; continue;
                }
                i += 2; o += 2; continue;
            }

            if (i == 2*n) // go up
            {
                if (j == 2*m-1)
                {
                    line_to(grid.x1, grid.y1);
                    i = 2*n-1; j = 2*m; o += dy-1; continue;
                }
                j += 2; o += 2*dy; continue;
            }

            if (j == 2*m) // go left
            {
                if (i == 1)
                {
                    line_to(grid.x0, grid.y1);
                    i = 0; j = 2*m-1; o -= dy+1; continue;
                }
                i -= 2; o -= 2; continue;
            }
        } while (tally[o] == CLEAR);

        return std::make_tuple(i, j, o, LINE);
    };

    auto single_step = [&] (unsigned i, unsigned j, size_t o, Direction d0)
    {
        switch (d0)
        {
            case RIGHT:
                if (i == 2*n)   // on the left edge
                    return walk_edge(i, j, o);

                if (tally[o+dy+1] != CLEAR)
                    { ++i; ++j; o += dy+1; }
                else if (tally[o+2] != CLEAR)
                    { i += 2; o += 2; }
                else if (tally[o-dy+1] != CLEAR)
                    { ++i; --j; o -= dy-1; }
                else
                    { return std::make_tuple(0u, 0u, 0UL, CLOSE_PATH); }
                break;

            case UP:
                if (j == 2*m)   // on the top edge
                    return walk_edge(i, j, o);

                if (tally[o+dy-1] != CLEAR)
                    { --i; ++j; o += dy-1; }
                else if (tally[o+2*dy] != CLEAR)
                    { j += 2; o += 2*dy; }
                else if (tally[o+dy+1] != CLEAR)
                    { ++i; ++j; o += dy+1; }
                else
                    { return std::make_tuple(0u, 0u, 0UL, CLOSE_PATH); }
                break;

            case LEFT:
                if (i == 0)   // on the right edge
                    return walk_edge(i, j, o);

                if (tally[o-dy-1] != CLEAR)
                    { --i; --j; o -= dy+1; }
                else if (tally[o-2] != CLEAR)
                    { i -= 2; o -= 2; }
                else if (tally[o+dy-1] != CLEAR)
                    { --i; ++j; o += dy-1; }
                else
                    { return std::make_tuple(0u, 0u, 0UL, CLOSE_PATH); }
                break;

            case DOWN:
                if (j == 0)   // on the top edge
                    return walk_edge(i, j, o);

                if (tally[o-dy+1] != CLEAR)
                    { ++i; --j; o -= dy-1; }
                else if (tally[o-2*dy] != CLEAR)
                    { j -= 2; o -= 2*dy; }
                else if (tally[o-dy-1] != CLEAR)
                    { --i; --j; o -= dy+1; }
                else
                    { return std::make_tuple(0u, 0u, 0UL, CLOSE_PATH); }
                break;
        }

        return std::make_tuple(i, j, o, CURVE);
    };

    auto draw_curve = [&] (double x0, double y0, Direction d0,
                           double x1, double y1, Direction d1)
    {
        double dr = 0.01 * std::min(grid.px, grid.py);
        // define the function in a circle around a point
        auto h = [&] (double x, double y, double phi)
            { return f(x + dr * cos(phi), y + dr * sin(phi)); };

        double phi0 = find_root_ridders(std::bind(h, x0, y0, _1), angle[d0] - M_PI/2,
                                        angle[d0] + M_PI/2, s, epsilon),
               phi1 = find_root_ridders(std::bind(h, x1, y1, _1), angle[d1] + M_PI/2,
                                        angle[d1] + 3*M_PI/2, s, epsilon);
        double scale = sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)) / 3.;

        curve_to(x0 + cos(phi0) * scale, y0 + sin(phi0) * scale,
                 x1 + cos(phi1) * scale, y1 + sin(phi1) * scale,
                 x1,                     y1);
    };

    auto start_path = [&] (unsigned i, unsigned j, size_t o)
    {
        double x0, y0, x1, y1;
        Direction d0, d1;

        std::tie(x0, y0, d0) = find_intersection(i, j, o);
        move_to(x0, y0);

        while (true)
        {
            tally[o] = DONE;
            Continuation c;
            std::tie(i, j, o, c) = single_step(i, j, o, d0);

            switch (c)
            {
                case CLOSE_PATH:
                    close_path();
                    return;

                case LINE:
                    std::tie(x0, y0, d0) = find_intersection(i, j, o);
                    line_to(x0, y0);
                    if (tally[o] == DONE)
                    {
                        close_path(); return;
                    }
                    break;

                case CURVE:
                    std::tie(x1, y1, d1) = find_intersection(i, j, o);
                    draw_curve(x0, y0, d0, x1, y1, d1);
                    if (tally[o] == DONE)
                    {
                        close_path(); return;
                    }
                    x0 = x1; y0 = y1; d0 = d1;
                    break;

                default:
                    exit(-1);
            }
        }
    };

    size_t o = 0;
    for (unsigned j = 0; j < m*2 + 1; ++j)
    {
        for (unsigned i = 0; i < n*2 + 1; ++i, ++o)
        {
            if (even(i + j)) continue;
            if (tally[o] != TODO) continue;

            start_path(i, j, o);
        }
    }
}
