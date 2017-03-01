#pragma once
#include <functional>
#include <vector>

#include "grid.hh"
#include "common.hh"

extern std::vector<int> pre_scan(
                std::function<double (double, double)> f,
                double s,
                Grid const &grid);

