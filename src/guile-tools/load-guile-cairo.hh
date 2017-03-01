#pragma once
#include <cairo.h>
#include <libguile.h>

extern SCM (*my_scm_from_cairo)(cairo_t *cr);
extern cairo_t *(*my_scm_to_cairo)(SCM cr_);

extern void load_guile_cairo();

