#pragma once
#include <array>
#include <experimental/array>

using std::experimental::make_array;

class Grid
{
    public:
        double x0, y0, x1, y1;
        unsigned n, m;
        double px, py;

        Grid(double x0_, double y0_, double x1_, double y1_,
             unsigned n_, unsigned m_):
            x0(x0_), y0(y0_), x1(x1_), y1(y1_), n(n_), m(m_),
            px((x1_ - x0_)/n_), py((y1_ - y0_)/m_)
        {}

        double x(double i) const { return px * i + x0; }
        double y(double j) const { return py * j + y0; }

        std::array<double, 2> operator()(double i, double j) const
        {
            return make_array<double>(px * i + x0, py * j + y0);
        }
};

