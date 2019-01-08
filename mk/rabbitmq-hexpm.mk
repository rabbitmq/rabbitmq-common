# --------------------------------------------------------------------
# Hex.pm.
# --------------------------------------------------------------------

.PHONY: hex-publish hex-publish-docs

HEXPM_URL = https://github.com/rabbitmq/hexpm-cli/releases/download/v0.2.0/hexpm
HEXPM_CLI = $(ERLANG_MK_TMP)/hexpm

$(HEXPM_CLI):
	$(verbose) mkdir -p $(ERLANG_MK_TMP)
	$(gen_verbose) $(call core_http_get,$@,$(HEXPM_URL))
	$(verbose) chmod +x $@

RABBIT_COMMON_HEXPM_VERSION = $(PROJECT_VERSION)
AMQP_CLIENT_HEXPM_VERSION = $(PROJECT_VERSION)

rebar.config: dep_rabbit_common = hex $(RABBIT_COMMON_HEXPM_VERSION)
rebar.config: dep_amqp_client = hex $(AMQP_CLIENT_HEXPM_VERSION)

define RABBITMQ_HEXPM_DEFAULT_FILES
	    "erlang.mk",
	    "git-revisions.txt",
	    "include",
	    "LICENSE*",
	    "Makefile",
	    "rabbitmq-components.mk",
	    "README",
	    "README.md",
	    "src"
endef

ifeq ($(PROJECT),rabbit_common)
RMQ_COMPONENTS_PREFIX = mk
RMQ_COMPONENTS_HEXPM = mk/rabbitmq-components.hexpm.mk
else
RMQ_COMPONENTS_PREFIX = .
RMQ_COMPONENTS_HEXPM = $(DEPS_DIR)/rabbit_common/mk/rabbitmq-components.hexpm.mk
endif

hex-publish: $(HEXPM_CLI) app rebar.config
	$(gen_verbose) echo "$(PROJECT_DESCRIPTION) $(PROJECT_VERSION)" \
		> git-revisions.txt
	$(verbose) mv \
		$(RMQ_COMPONENTS_PREFIX)/rabbitmq-components.mk \
		rabbitmq-components.mk.not-hexpm
	$(verbose) cp \
		$(RMQ_COMPONENTS_HEXPM) \
		$(RMQ_COMPONENTS_PREFIX)/rabbitmq-components.mk
	$(verbose) grep -E '^dep.* = hex' \
		rabbitmq-components.mk.not-hexpm \
		>> $(RMQ_COMPONENTS_PREFIX)/rabbitmq-components.mk
	$(verbose) touch -r \
		rabbitmq-components.mk.not-hexpm \
		$(RMQ_COMPONENTS_PREFIX)/rabbitmq-components.mk
	$(verbose) trap '\
		rm -f git-revisions.txt rebar.lock; \
		if test -f rabbitmq-components.mk.not-hexpm; then \
		  mv \
		    rabbitmq-components.mk.not-hexpm \
		    $(RMQ_COMPONENTS_PREFIX)/rabbitmq-components.mk; \
		fi' EXIT INT; \
		$(HEXPM_CLI) publish

hex-publish-docs: $(HEXPM_CLI) app docs
	$(gen_verbose) trap 'rm -f rebar.lock' EXIT INT; \
		$(HEXPM_CLI) docs
