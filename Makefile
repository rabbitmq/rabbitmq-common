PROJECT = supervisor3
PROJECT_VERSION = 1.1.1

otp_release_prefix = $(shell erl -noshell -eval 'io:put_chars([hd(erlang:system_info(otp_release))]), init:stop()')

## R16 or earlier
ifeq ($(otp_release_prefix), R)
  ERLC_OPTS += -Dprior_17_otp
endif

include erlang.mk

