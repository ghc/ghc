
docs/users_guide_DOCBOOK_SOURCES := \
    $(wildcard docs/users_guide/*.xml) \
    $(basename $(wildcard docs/users_guide/*.xml.in))

$(eval $(call docbook,docs/users_guide,users_guide))

