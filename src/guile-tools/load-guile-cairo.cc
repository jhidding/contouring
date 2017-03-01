#include <iostream>
#include <dlfcn.h>

#include "load-guile-cairo.hh"

/* We have to load this function dynamically from guile-cairo.so,
 * it converts a cairo context into the correct SMOB.
 */
SCM (*my_scm_from_cairo)(cairo_t *cr);
cairo_t *(*my_scm_to_cairo)(SCM cr_);


void load_guile_cairo()
{
    char *error;
    void *handle = dlopen("libguile-cairo.so", RTLD_LAZY);
    error = dlerror();
    if (error != NULL)
    {
        std::cout << "Could not load 'libguile-cairo.so'.\n";
        exit(EXIT_FAILURE);
    }

    my_scm_from_cairo = (SCM (*)(cairo_t *))dlsym(handle, "scm_from_cairo");
    error = dlerror();
    if (error != NULL)
    {
        std::cout << "Could not load shared library function.\n";
        exit(EXIT_FAILURE);
    }

    my_scm_to_cairo = (cairo_t *(*)(SCM))dlsym(handle, "scm_to_cairo");
    error = dlerror();
    if (error != NULL)
    {
        std::cout << "Could not load shared library function.\n";
        exit(EXIT_FAILURE);
    }
}

