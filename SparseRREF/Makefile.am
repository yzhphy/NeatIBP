AUTOMAKE_OPTIONS = gnu
AM_CPPFLAGS = -I$(top_srcdir)/src
AM_CFLAGS = -I$(top_srcdir)/src
LDADD = -lm $(top_builddir)/src/libspasm.la $(FFLAS_FFPACK_LIBS) $(GIVARO_LIBS)

bin_PROGRAMS = rank_gplu dm bitmap pluq kernel transpose stack \
               schur_complement rank_hybrid vertical_swap maximum_matching \
               rank_dense find_pivots uetree mtx_reader rref echelonize u_checker \
               rref_gplu chains modules complement_cc

if SPASM_HAVE_LEMON
bin_PROGRAMS += lemon
lemon_SOURCES = lemon.cpp
lemon_LDFLAGS = $(LEMON_LIBS)
lemon_CFLAGS = $(LEMON_CFLAGS)
endif

if SPASM_HAVE_METIS
bin_PROGRAMS += metis_symmetric metis_asymmetric
metis_symmetric_SOURCES = metis_symmetric.c
metis_asymmetric_SOURCES = metis_asymmetric.c
metis_symmetric_LDFLAGS = $(METIS_LIBS)
metis_symmetric_CFLAGS = $(METIS_CFLAGS)
metis_asymmetric_LDFLAGS = $(METIS_LIBS)
metis_asymmetric_CFLAGS = $(METIS_CFLAGS)
endif

if SPASM_HAVE_LINBOX
bin_PROGRAMS += linbox_rank_gauss linbox_rank_wm
linbox_rank_gauss_SOURCES = linbox_rank_gauss.cpp
linbox_rank_gauss_LDFLAGS = $(LINBOX_LIBS) $(FFLAS_FFPACK_LIBS) $(GIVARO_LIBS)
linbox_rank_gauss_CXXFLAGS = $(LINBOX_CXXFLAGS) $(FFLAS_FFPACK_CXXFLAGS) $(GIVARO_CXXFLAGS)

linbox_rank_wm_SOURCES = linbox_rank_wm.cpp
linbox_rank_wm_LDFLAGS = $(LINBOX_LIBS) $(FFLAS_FFPACK_LIBS) $(GIVARO_LIBS)
linbox_rank_wm_CXXFLAGS = $(LINBOX_CXXFLAGS) $(FFLAS_FFPACK_CXXFLAGS) $(GIVARO_CXXFLAGS)
endif

%.png : %.pbm
	convert $< $@

%.png : %.pgm
	convert $< $@

%.png : %.pnm
	convert $< $@

# %.c : %.nw
# 	notangle -filter disambiguate -R$@ -L $< > $@ 
#Date:Dec10_2022:Set the last 2 lines as remarks 
#Changed by Hefeng Xu rwinters@mail.ustc.edu.cn
