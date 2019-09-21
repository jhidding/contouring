.PHONY: clean all

build_dir := build
libraries := guile-2.2 guile-cairo

cflags := -g -std=c++14 -O2 -fdiagnostics-color -Wpedantic -Wall -fPIC
cflags += $(foreach lib, $(libraries), $(shell pkg-config --cflags $(lib)))

ldflags := -lm -ldl -shared
ldflags += $(foreach lib, $(libraries), $(shell pkg-config --libs $(lib)))

sources := $(shell find src -name *.cc)
obj_files := $(sources:src/%.cc=$(build_dir)/%.o)
dep_files := $(obj_files:%.o=%.d)

compile := g++
link := g++

all: $(build_dir)/libguile-contouring.so

clean:
	rm -rf build

-include $(dep_files)

$(build_dir)/libguile-contouring.so: $(obj_files)
	$(link) $^ $(ldflags) -shared -o $@


$(build_dir)/%.o: src/%.cc
	@mkdir -p $(@D)
	$(compile) $(cflags) -MMD -c $< -o $@

