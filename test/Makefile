TESTS = \
	image_param \
	double_param \
	bad_named_param

all: test

test: irg log $(TESTS)

out/irg:
	mkdir $@

log:
	mkdir $@

out:
	mkdir $@

clean:
	rm -rf log out include src

define test_failed
TEST=coucou

$(1): log
	@echo -n "$(1) ... "; \
	if $(2) > log/$(1).log 2>&1 ; then \
		echo "[FAILED]"; \
	else \
		echo "[OK]"; \
	fi
endef

define test_success
$(1): log out
	for x in `ls`; do echo "$${x}"; done; \
	echo -n "$(1) ... "; \
	if [ -n "$(3)" ]; then \
		rm -rf "out/$(1)"; \
		mkdir -p "out/$(1)"; \
		cd "out/$(1)"; \
		echo "cd to out/$(1)"; \
		l="$(1).log"; \
		echo "A l=$$l"; \
	else \
		l="log/$(1).log"; \
	fi; \
	l="ok"; \
	echo "l=$$l"; \
	echo $(2); \
	if $(2) > "$$l" 2>&1 ; then \
		echo "[OK]"; \
	else \
		echo "[FAILED]"; \
	fi
endef

$(eval $(call test_failed,image_param,../irg/mkirg image_param.nml irg/image_param.irg))
$(eval $(call test_failed,double_param,../irg/mkirg double_param.nml irg/double_param.irg))
$(eval $(call test_failed,bad_named_param,../irg/mkirg bad_named_param.nml irg/bad_named_param.irg))
$(eval $(call test_success,for,../../../irg/mkirg ../for.nmp for.irg,for))

