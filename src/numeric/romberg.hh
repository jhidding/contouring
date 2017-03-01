#pragma once
#include <vector>
#include <algorithm>
#include <cmath>

namespace numeric
{
    /*! \brief Function integration by Romberg method.
     *
     * \param f Function to integrate.
     * \param a Left bound.
     * \param b Right bound.
     * \param epsilon Absolute precision.
     * \param max_it Maximum number of iterations.
     * \return Integral of f, from a to b.
     *
     * Romberg integration uses the trapezoidal rule to estimate the area
     * under a curve. We double the sampling in each step to approach the
     * correct value. Each next guess is improved upon by Richardson
     * extrapolation, using the convergence rate to make a better guess.
     */
    template <typename real_t, typename Fn>
    real_t integrate_romberg(Fn f, real_t a, real_t b, real_t epsilon, unsigned max_it)
    {
        std::vector<real_t> R(max_it);
        real_t result;

        real_t h = b - a;
        real_t s;

        R[0] = h/2 * (f(a) + f(b));
        result = R[0];

        unsigned long n = 1;
        unsigned i = 1;
        while (true)
        {
            real_t x = a + h / 2;
            s = 0.0;

            for (unsigned j = 0; j < n; ++j)
            {
                s += f(x);
                x += h;
            }

            s *= h / 2;
            s += R[0] / 2;

            unsigned k = 4;
            for (unsigned j = 1; j <= i; ++j)
            {
                std::swap(s, R[j - 1]);
                s = (k * R[j - 1] - s) / (k - 1);
                k *= 4;
            }

            if (++i == max_it or fabs(s - result) < epsilon)
                return s;

            R[i] = s;
            result = s;

            h /= 2;
            n *= 2;
        }
    }
}

