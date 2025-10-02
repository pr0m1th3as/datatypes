# Makefile for compiling required oct files

ifeq ($(OS),Windows_NT)
	LIBS = -lOle32 -lcurl
else
	LIBS =
endif

LDFLAGS += $(LIBS)

all:
	$(MKOCTFILE)       '-I ./date'  __datetime__.cc tz.cpp $(LDFLAGS)
	$(MKOCTFILE)       __table2csv__.cc $(LDFLAGS)
	$(MKOCTFILE)       __csv2table__.cc $(LDFLAGS)
	$(MKOCTFILE)       __ckeyHash__.cc $(LDFLAGS)
