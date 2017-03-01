#pragma once
#include <cmath>

namespace numeric {
    /*! \brief Root finding by Brent's method.
     *
     * Brent's method employs Newton-Raphson method to converge to a root,
     * but falls back to a simpler but more robust bi-section method when
     * the guess by Newton method falls outside the known bounds.
     *
     * \param f The function to solve for.
     * \param df Derivative of f.
     * \param x1 Left bound of interval to search.
     * \param x2 Right bound of interval to search.
     * \param y0 Value function f should atain.
     * \param epsilon Absolute error.
     *
     * Newton's method uses the derivative of f to guess the next value:
     *      \f[ x_{i+1} = x_{i} - \Delta y / f'(x_{i}) \f]
     * If the function is not so nice to us -- it may be rapidly changing,
     * but more importantly, we could be mucking about in the exponential tail
     * of a distribution -- this method will behave bad, so we back it up
     * with a bisection method, which always converges.
     */
    template <typename real_t, typename Fn1, typename Fn2>
    real_t find_root_brent(Fn1 f, Fn2 df, real_t x1, real_t x2, real_t y0, real_t epsilon)
    {
        real_t b = (x1 + x2) / 2,           // initial guess
               w = f(x1) - y0,
               y = f(b) - y0,
               a = (w * y < 0 ? x1 : x2);   // set other bound

        while (epsilon < fabs(y))
        {
            // try Newton method
            real_t c = b - y / df(b);

            // keep if within bounds otherwise bisection
            c = ((b - c) * (a - c) < 0 ? c : (a + b) / 2);

            // update bounds
            w = f(c) - y0;
            a = (w * y < 0 ? b : a);
            b = c;
            y = w;
        }

        return b;
    }

    /*! \brief Root finding by Ridders' algorithm.
     *
     * Ridders' algorithm converges faster than the bi-section method
     * but doesn't need an explicit derivative, like Brent's method.
     * DOI:10.1109/TCS.1979.1084580
     *
     * \param f The function to solve for.
     * \param a Left bound of interval to search.
     * \param b Right bound of interval to search.
     * \param y0 Value function f should atain.
     * \param epsilon Absolute error.
     */
    template <typename real_t, typename Fn>
    real_t find_root_ridders(Fn f, real_t a, real_t b, real_t y0, real_t epsilon)
    {
        real_t d, m, x, f_x, f_a, f_b, f_m, p, q;

        f_a = f(a) - y0;
        f_b = f(b) - y0;

        if (f_a * f_b >= 0)
            return (fabs(f_a) < fabs(f_b) ? a : b);

        while (true) {
            d = (b - a) / 2;
            m = a + d;
            f_m = f(m) - y0;
            p = f_b / f_a;
            q = f_m / f_a;
            x = m + d*q / sqrt(q*q - p);
            f_x = f(x) - y0;

            if (fabs(f_x) < epsilon)
                return x;

            if (f_x * f_m < 0)
            {
                if (m > x)
                {
                    a = x; b = m;
                    f_a = f_x; f_b = f_m;
                    continue;
                } else {
                    a = m; b = x;
                    f_a = f_m; f_b = f_x;
                    continue;
                }
            }

            if (f_x * f_a < 0)
            {
                b = x;
                f_b = f_x;
                continue;
            } else {
                a = x;
                f_a = f_x;
                continue;
            }
        }
    }
}

