#  `(file-name-nondirectory  (buffer-file-name))` ---
#
#  Author: `(concat user-full-name)` <`(concat user-mail-address)`>
#  Copyright © `(format-time-string "%Y")`, `(concat user-full-name)`, all rights reserved.
#  Created: `(format-time-string "%e %B %Y")`
#
# $@       the file name of the target
# $<       the name of the first prerequisite (i.e., dependency)
# $^       the names of all prerequisites (i.e., dependencies)
# $(@D)    the directory part of the target
# $(@F)    the file part of the target
# $(<D)    the directory part of the first prerequisite (i.e., dependency)
# $(<F)    the file part of the first prerequisite (i.e., dependency)

#include config.mk  # include if have GLOBAL variables

# ENV variables
MAKE_SHELL := bash
RM         := rm -f
MKDIR      := mkdir -p

# find which makefile you are in
where-am-i = $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
THIS_MAKEFILE := $(call where-am-i)
# rsync options
RSYNC_OPTS=-avPz -e "ssh -i ~/.ssh/id_rsa" --delete-after --exclude="node_modules" --exclude=".git"
REMOTE_DIR="atearoot@repo:"

TXT_FILES=$(wildcard $(TXT_DIR)/*.txt)
DAT_FILES=$(patsubst $(TXT_DIR)/%.txt, %.dat, $(TXT_FILES))
ARCHIVE_DIR=app
ARCHIVE=$(ARCHIVE_DIR).tar.gz

.PHONY : help
help : `(file-name-nondirectory (buffer-file-name))`
	@sed -n 's/^##//p' $<

## all         : Generate archive of code, data, plots and Zipf summary table.
.PHONY : all
all : $(ARCHIVE)

$(ARCHIVE) : $(ARCHIVE_DIR)
	tar -czf $@ $<

$(ARCHIVE_DIR): Makefile config.mk $(RESULTS_FILE) \
             $(DAT_FILES) $(TXT_DIR) \
             $(ARCHIVE_SRC)
	$(MKDIR) $@
	cp -r $^ $@
	touch $@

## backup       : Backup files.
.PHONY : backup
backup: $(CURDIR)/`(file-name-nondirectory (buffer-file-name))`
	@echo Backing UP!
	rsync ${RSYNC_OPTS} $< ${REMOTE_DIR}

## install    : Build $2
.PHONY : install
install:
	$0

.PHONY : clean
clean :
    ${RM} $(DAT_FILES)

#  `(file-name-nondirectory (buffer-file-name))` ends here
