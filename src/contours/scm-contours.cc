#include <libguile.h>
#include "../guile-tools/load-guile-cairo.hh"

#include "grid.hh"
#include "pre-scan.hh"
#include "draw-path.hh"

SCM scm_cairo_contour(
    SCM cr_,
    SCM fn_, SCM s_, SCM x0_, SCM y0_,
    SCM x1_, SCM y1_, SCM n_, SCM m_)
{
    auto fn = [fn_] (double x, double y)
    {
        SCM x_ = scm_from_double(x);
        SCM y_ = scm_from_double(y);
        return scm_to_double(scm_call_2(fn_, x_, y_));
    };

    double x0 = scm_to_double(x0_),
           y0 = scm_to_double(y0_),
           x1 = scm_to_double(x1_),
           y1 = scm_to_double(y1_),
           s  = scm_to_double(s_);

    unsigned n = scm_to_uint(n_),
             m = scm_to_uint(m_);

    cairo_t *cr = my_scm_to_cairo(cr_);

    Grid grid(x0, y0, x1, y1, n, m);
    auto tally = pre_scan(fn, s, grid);
    draw_path(fn, s, grid, tally,
        std::bind(cairo_move_to, cr, _1, _2),
        std::bind(cairo_line_to, cr, _1, _2),
        std::bind(cairo_curve_to, cr, _1, _2, _3, _4, _5, _6),
        std::bind(cairo_close_path, cr));

    return SCM_UNDEFINED;
}

extern "C" void init_contour()
{
    load_guile_cairo();
    scm_c_define_gsubr("cairo-contour", 9, 0, 0, (void *)scm_cairo_contour);
}

// vim:ts=4:sw=4
