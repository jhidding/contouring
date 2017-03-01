#pragma once
#include <libguile.h>

/* Helper function, see if a name is defined in the current top-level
 * scheme context.
 */
inline bool scm_c_defined_p(std::string const &name)
{
    return scm_is_true(
        scm_defined_p(
            scm_from_utf8_symbol(name.c_str()), SCM_UNDEFINED));
}

/* Helper function, retrieve the value referenced to by the given name.
 */
inline SCM scm_c_ref(std::string const &name)
{
    return scm_variable_ref(scm_c_lookup(name.c_str()));
}

template <typename T>
inline T from_scm(SCM v);

template <>
inline double from_scm<double>(SCM v)
{
    return scm_to_double(v);
}

template <>
inline std::string from_scm<std::string>(SCM v)
{
    char *cs = scm_to_utf8_stringn(v, NULL);
    std::string s(cs);
    free(cs);
    return s;
}

template <>
inline int from_scm<int>(SCM v)
{
    return scm_to_int(v);
}

template <unsigned ...n>
struct seq {};

template <unsigned n, unsigned ...i>
struct make_seq : make_seq<n-1, n-1, i...> {};

template <unsigned ...i>
struct make_seq<0, i...> : seq<i...> {};

template <typename ...T, unsigned ...N>
inline std::tuple<T...> from_scm_values_helper(SCM v, seq<N...>)
{
    return std::make_tuple(from_scm<T>(scm_c_value_ref(v, N))...);
}

template <typename ...T>
inline std::tuple<T...> from_scm_values(SCM v)
{
    return from_scm_values_helper<T...>(v, make_seq<sizeof...(T)>());
}

class Dict
{
    SCM fn_;

    public:
        Dict(SCM fn):
            fn_(fn)
        {
            scm_gc_protect_object(fn_);
        }

        ~Dict()
        {
            scm_gc_unprotect_object(fn_);
        }

        template <typename T>
        T get(std::string const &name)
        {
            return from_scm<T>(scm_call_1(fn_, scm_from_utf8_symbol(name.c_str())));
        }

        template <typename T, typename ...Args>
        T get(std::string const &name, Args &&...args)
        {
            return Dict(scm_call_1(fn_, scm_from_utf8_symbol(name.c_str())))
                .get<T>(std::forward<Args>(args)...);
        }

        template <typename ...T>
        std::tuple<T...> get_values(std::string const &name)
        {
            return from_scm_values<T...>(scm_call_1(fn_, scm_from_utf8_symbol(name.c_str())));
        }

        template <typename ...T, typename ...Args>
        std::tuple<T...> get_values(std::string const &name, Args &&...args)
        {
            return Dict(scm_call_1(fn_, scm_from_utf8_symbol(name.c_str())))
                .get_values<T...>(std::forward<Args>(args)...);
        }
};

// vim: sw=4 ts=4
