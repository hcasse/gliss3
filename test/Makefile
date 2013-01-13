TESTS = \
	image_param

all: irg log $(TESTS)

irg:
	mkdir $@

log:
	mkdir $@

define test_failed
TEST=coucou

$(1): log
	@echo -n "$(1) ... "; \
	if $(2) > log/image_param.log 2>&1 ; then \
		echo "[FAILED]"; \
	else \
		echo "[OK]"; \
	fi
endef

$(eval $(call test_failed,image_param,../irg/mkirg image_param.nml irg/image_param.irg))
