# Makefile for compiling required oct files

TARGET_OS ?= $(OS)
ifeq ($(TARGET_OS),Windows_NT)
	LIBS = -lole32
else
	LIBS =
endif

MKOCTFILE ?= mkoctfile

LDFLAGS += $(LIBS)

TZFLAGS = -DHAS_REMOTE_API=0

OCTFILES = __datetime__.oct	\
           __table2csv__.oct	\
           __csv2table__.oct	\
           __ckeyHash__.oct	\
           __nkeyHash__.oct

.PHONY: all
all: $(OCTFILES)

__datetime__.oct: __datetime__.cc tz.cpp
	$(MKOCTFILE) '-I ./date'  __datetime__.cc tz.cpp $(TZFLAGS) $(LDFLAGS)

%.oct: %.cc
	$(MKOCTFILE) $< $(LDFLAGS)
