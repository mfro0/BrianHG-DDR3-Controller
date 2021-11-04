GHDL = ghdl

VPATH=.. . 
ELLIPSE_MODULES = \
	ellipse_generator.o \
	eg_tb.o

GHDLFLAGS=--std=08 -Pintel --ieee=synopsys -Wno-binding
GHDL_RUNFLAGS=--ieee-asserts=disable-at-0

.PHONY: all
all:    ellipse

ellipse: $(ELLIPSE_MODULES) eg_tb

run_ellipse: $(ELLIPSE_MODULES)
	$(GHDL) -r $(GHDLFLAGS) eg_tb --disp-tree $(GHDL_RUNFLAGS) --wave=ellipse.ghw

%: %.o
	$(GHDL) -e $(GHDLFLAGS) $@

%.o: %.vhd
	$(GHDL) -a $(GHDLFLAGS) $<

COMPILE_INTEL=/usr/local/lib/ghdl/vendors/compile-intel.sh
SIM_LIB=/opt/intelFPGA_lite/20.1/quartus/eda/sim_lib

.PHONY: intel
intel:
	$(COMPILE_INTEL) --vhdl2008 --output intel --source $(SIM_LIB) -a

.PHONY: view
view_ellipse:
	gtkwave ellipse.ghw ellipse.gtkw
.PHONY: clean
clean:
	rm -f *.o *.cf e*.lst *.ghw
