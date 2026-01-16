# Makefile for compiling required oct files

TARGET_OS ?= $(OS)
ifeq ($(TARGET_OS),Windows_NT)
	LIBS = -lole32
else
	LIBS =
endif

LDFLAGS += $(LIBS)

TZFLAGS = -DHAS_REMOTE_API=0

all:
	$(MKOCTFILE)       '-I ./date'  __datetime__.cc tz.cpp $(TZFLAGS) $(LDFLAGS)
	$(MKOCTFILE)       __table2csv__.cc $(LDFLAGS)
	$(MKOCTFILE)       __csv2table__.cc $(LDFLAGS)
	$(MKOCTFILE)       __ckeyHash__.cc $(LDFLAGS)
	$(MKOCTFILE)       __nkeyHash__.cc $(LDFLAGS)
