REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
# ToDo: remove unused TEMPLATES_PATH here, when the bug
# with handling of the varriable in build_utils is fixed
TEMPLATES_PATH := .
SERVICE_NAME := scoper

BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := aaa79c2d6b597f93f5f8b724eecfc31ec2e2a23b

CALL_W_CONTAINER := all submodules rebar-update compile xref lint \
					dialyze test clean distclean check_format format

.PHONY: $(CALL_W_CONTAINER)

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk

$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

compile: submodules rebar-update
	$(REBAR) compile

rebar-update:
	$(REBAR) update

test: submodules
	$(REBAR) ct

xref: submodules
	$(REBAR) as test xref

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean -a
	rm -rfv _build _builds _cache _steps _temp

dialyze:
	$(REBAR) as test dialyzer

lint:
	elvis rock -V

check_format:
	$(REBAR) as test fmt -c

format:
	$(REBAR) fmt -w
