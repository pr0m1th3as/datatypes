# Makefile for compiling required oct files

TARGET_OS ?= $(OS)
ifeq ($(TARGET_OS),Windows_NT)
	LIBS = -lole32 -lcurl
else
	LIBS =
endif

LDFLAGS += $(LIBS)

TZFLAGS := $(shell octave --no-gui --no-init-file --no-history --eval \
             "q = pkg ('prefix'); q = fullfile (q, 'datatypes-1.1.5'); \
             fprintf ('-DINSTALL=%s -DHAS_REMOTE_API=0', q)")

all:
	$(MKOCTFILE)       '-I ./date'  __datetime__.cc tz.cpp $(TZFLAGS) $(LDFLAGS)
	$(MKOCTFILE)       __table2csv__.cc $(LDFLAGS)
	$(MKOCTFILE)       __csv2table__.cc $(LDFLAGS)
	$(MKOCTFILE)       __ckeyHash__.cc $(LDFLAGS)
	$(MKOCTFILE)       __nkeyHash__.cc $(LDFLAGS)
