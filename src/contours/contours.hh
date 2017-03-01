#pragma once
#include <cairo/cairo.h>

extern "C" void contour(
    double (*f)(double, double), double s,
    double x0, double y0,
    double x1, double y1,
    unsigned n, unsigned m,

    void (*move_to)(double, double),
    void (*line_to)(double, double),
    void (*curve_to)(double, double, double, double, double, double),
    void (*close_path)());

extern "C" void cairo_contour(cairo_t *cr,
    double (*f)(double, double), double s,
    double x0, double y0,
    double x1, double y1,
    unsigned n, unsigned m);

