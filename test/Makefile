SRCDIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
JULIAHOME := $(abspath $(SRCDIR)/..)
BUILDDIR := .
include $(JULIAHOME)/Make.inc
# TODO: this Makefile ignores BUILDDIR, except for computing JULIA_EXECUTABLE

TESTS = all linalg sparse unicode strings dates $(filter-out TestHelpers runtests testdefs,$(patsubst $(SRCDIR)/%.jl,%,$(wildcard $(SRCDIR)/*.jl $(SRCDIR)/linalg/*.jl)))

default: all

# set hard and soft limits for spawn fd-exhaustion test
ifeq ($(OS),WINNT)
ULIMIT_TEST=
else
ULIMIT_TEST=ulimit -n 128 &&
endif

$(TESTS):
	@cd $(SRCDIR) && \
	$(ULIMIT_TEST) \
	$(call PRINT_JULIA, $(call spawn,$(JULIA_EXECUTABLE)) --check-bounds=yes --startup-file=no ./runtests.jl $@)

perf:
	@$(MAKE) -C $(SRCDIR)/perf all

clean:
	@$(MAKE) -C perf $@

.PHONY: $(TESTS) perf clean
