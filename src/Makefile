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

# Make glibc define __USE_LARGEFILE64 so miniz selects its fopen64/ftello64/
# fseeko64 branch instead of the plain fopen/ftello path.  This silences miniz's
# "may not support large files" #pragma message and enables 64-bit file offsets.
MINIZFLAGS = -D_LARGEFILE64_SOURCE

OCTFILES = __datetime__.oct	\
           __table2csv__.oct	\
           __csv2table__.oct	\
           __table2ods__.oct	\
           __ods2table__.oct	\
           __table2xlsx__.oct	\
           __xlsx2table__.oct	\
           __ckeyHash__.oct	\
           __nkeyHash__.oct

.PHONY: all
all: $(OCTFILES)

__datetime__.oct: __datetime__.cc tz.cpp
	$(MKOCTFILE) '-I ./date'  __datetime__.cc tz.cpp $(TZFLAGS) $(LDFLAGS)

__table2ods__.oct: __table2ods__.cc ods/pugixml.cpp ods/miniz.c
	$(MKOCTFILE) -I./ods $(MINIZFLAGS) __table2ods__.cc ods/pugixml.cpp ods/miniz.c $(LDFLAGS)

__ods2table__.oct: __ods2table__.cc ods/pugixml.cpp ods/miniz.c
	$(MKOCTFILE) -I./ods $(MINIZFLAGS) __ods2table__.cc ods/pugixml.cpp ods/miniz.c $(LDFLAGS)

__table2xlsx__.oct: __table2xlsx__.cc ods/miniz.c
	$(MKOCTFILE) -I./ods $(MINIZFLAGS) __table2xlsx__.cc ods/miniz.c $(LDFLAGS)

__xlsx2table__.oct: __xlsx2table__.cc ods/pugixml.cpp ods/miniz.c
	$(MKOCTFILE) -I./ods $(MINIZFLAGS) __xlsx2table__.cc ods/pugixml.cpp ods/miniz.c $(LDFLAGS)

%.oct: %.cc
	$(MKOCTFILE) $< $(LDFLAGS)

.PHONY: clean
clean:
	rm -f $(OCTFILES)
