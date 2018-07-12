PROJECT = supervisor3
PROJECT_VERSION = 1.1.7

otp_release_prefix = $(shell erl -noshell -eval 'io:put_chars([hd(erlang:system_info(otp_release))]), init:stop()')

## R16 or earlier
ifeq ($(otp_release_prefix), R)
  ERLC_OPTS += -Dprior_17_otp
endif

include erlang.mk

vsn-check:
	$(verbose) ./vsn-check.sh $(PROJECT_VERSION)

hex-publish: distclean
	$(verbose) rebar3 hex publish

