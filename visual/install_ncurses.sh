#!/bin/bash

DIR=$1
MERCURY_SRC_DIR=$2
MERCURY_BIN_DIR=$3
OLD_DIR=$(pwd)

if [ "${DIR}" == "" ]
then
        echo "Dir is empty."
        exit 1
fi

if [ "$(echo ${DIR} | grep ncurses)" == "" ]
then
        echo "Dir ${DIR} seems not to be ncurses."
        exit 1
fi

if [ ! -f "${DIR}.tar.gz" ]
then
        wget http://ftp.gnu.org/gnu/ncurses/${DIR}.tar.gz
fi

if [ -d "${DIR}" ]
then
        echo "NCurses source directory $DIR already exists. Will not install."
        exit
fi

echo ncurses_dir=${DIR}
echo mercury_src=${MERCURY_SRC_DIR}
echo mercury_bin=${MERCURY_BIN_DIR}

rm -rf "${DIR}"
tar xvfz "${DIR}.tar.gz"

cd "$DIR" || exit 1

# Here he will find gc.h and libpar_gc.so:
export LDFLAGS="-L${MERCURY_BIN_DIR}/lib/mercury/lib"
export CLAGS="-fPIC"
export CPPLAGS="-I${MERCURY_BIN_DIR}/lib/mercury/inc"
export CXXFLAGS="${CFLAGS}"
# Add the new mygc.h header which accounts for new->GC_MALLOC and replace malloc->GC_MALLOC
for f in $(find . -name \*.h -or -name \*.c); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
for f in $(find . -name \*.hpp -or -name \*.cpp); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<malloc\>/GC_MALLOC_UNCOLLECTABLE/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<realloc\>/GC_REALLOC/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<free\>/GC_FREE/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<strdup\>/GC_STRDUP/g'
./configure --prefix=$(pwd)/dist --without-cxx-binding --without-ada --without-manpages --without-tests --without-progs --with-shared || exit 1
# Tell him to link GC and some other libraries (do we need them?)
find . -name Makefile | xargs sed --in-place -e "s/^BUILD_LIBS[\\t ]\\+=/BUILD_LIBS = -lpar_gc -ldl -lpthread/g"
find . -name Makefile | xargs sed --in-place -e "s/^BUILD_LDFLAGS[\\t ]\\+=/BUILD_LDFLAGS = -L$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/lib/g"
find . -name Makefile | xargs sed --in-place -e "s/^BUILD_LDFLAGS[\\t ]\\+=/BUILD_LDFLAGS = -Wl,-rpath=$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/lib/g"
find . -name Makefile | xargs sed --in-place -e "s/^BUILD_CPPFLAGS[\\t ]\\+=/BUILD_CPPFLAGS = -I$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/inc/g"
find . -name Makefile | xargs sed --in-place -e "s/^BUILD_CFLAGS[\\t ]\\+=/BUILD_CFLAGS = -fPIC/g"
find . -name Makefile | xargs sed --in-place -e "s/^LIBS[\\t ]\\+=/LIBS = -ldl -lpar_gc/g"
find . -name Makefile | xargs sed --in-place -e "s/^LDFLAGS[\\t ]\\+=/LDFLAGS = -L$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/lib/g"
find . -name Makefile | xargs sed --in-place -e "s/^LDFLAGS[\\t ]\\+=/LDFLAGS = -Wl,-rpath=$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/lib/g"
find . -name Makefile | xargs sed --in-place -e "s/^CPPFLAGS[\\t ]\\+=/CPPFLAGS = -I$(echo ${MERCURY_BIN_DIR} | sed -e 's/\//\\\//g')\\/lib\\/mercury\\/inc/g"
find . -name Makefile | xargs sed --in-place -e "s/^CFLAGS[\\t ]\\+=/CFLAGS = -fPIC/g"

# Here comes the header:
cat >include/mygc.h <<\EOF
//#define GC_DEBUG
#define GC_THREADS
#include <gc.h>

#include <stdlib.h>
#include <string.h>

#ifndef malloc
#define malloc(x) GC_MALLOC_UNCOLLECTABLE(x)
#endif

#ifndef realloc
#define realloc(x, y) GC_REALLOC(x, y)
#endif

#ifndef calloc
#define calloc(x, y) GC_MALLOC_UNCOLLECTABLE((x) * (y))
#endif

#ifndef free
#define free(x) GC_FREE(x)
#endif

#ifndef strdup
#define strdup(x) GC_STRDUP(x)
#endif

#ifndef dlopen
#define dlopen(fn, flags) GC_dlopen(fn, flags)
#endif


#ifdef __cplusplus
#ifndef MYGC
#define MYGC

#include <new>

inline void* operator new(std::size_t size) throw (std::bad_alloc)
{
  void* ptr = GC_MALLOC_UNCOLLECTABLE(size);
  if (!ptr) throw std::bad_alloc();
  return ptr;
}

inline void* operator new[](std::size_t size) throw (std::bad_alloc)
{
  void* ptr = GC_MALLOC_UNCOLLECTABLE(size);
  if (!ptr) throw std::bad_alloc();
  return ptr;
}

inline void operator delete(void* ptr) throw()
{
  if (ptr) GC_FREE(ptr);
}

inline void operator delete[](void* ptr) throw()
{
  if (ptr) GC_FREE(ptr);
}

inline void* operator new(std::size_t size, const std::nothrow_t&) throw()
{
  void* ptr = GC_MALLOC_UNCOLLECTABLE(size);
  return ptr;
}

inline void* operator new[](std::size_t size, const std::nothrow_t&) throw()
{
  void* ptr = GC_MALLOC_UNCOLLECTABLE(size);
  return ptr;
}

inline void operator delete(void* ptr, const std::nothrow_t&) throw()
{
  if (ptr) GC_FREE(ptr);
}

inline void operator delete[](void* ptr, const std::nothrow_t&) throw()
{
  if (ptr) GC_FREE(ptr);
}

#endif
#endif
EOF

# And now go for it:
make || exit 1
make install || exit 1
# The header must be includable:
cp include/mygc.h dist/include/ncurses/ || exit 1


cd "${OLD_DIR}"

