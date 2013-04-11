#!/bin/bash

COIN_DIR=$1
MERCURY_SRC_DIR=$2
MERCURY_BIN_DIR=$3
OLD_DIR=$(pwd)

if [ -d "/tmp/boehm_gc" ]
then
        echo "Boehm GC seems to be installed already. Will not install."
fi

if [ -d "$COIN_DIR/dist" ]
then
        echo "COIN source directory $COIN_DIR already exists. Will not install."
fi

if [ -d "/tmp/boehm_gc" -a -d "$COIN_DIR/dist" ]
then
        exit 0
fi

if [ ! -d "$MERCURY_SRC_DIR" ]
then
        echo "Mercury source directory $MERCURY_SRC_DIR does not exists. Aborting."
        exit 1
fi

if [ ! -d "$MERCURY_BIN_DIR" ]
then
        echo "Mercury installation directory $MERCURY_BIN_DIR does not exists. Aborting."
        exit 1
fi



# BOEHM GC:
if [ -d "/tmp/boehm_gc" ]
then
        rm -rf "/tmp/boehm_gc" 
fi
cp -r "${MERCURY_SRC_DIR}/boehm_gc" "/tmp/boehm_gc" || exit 1
cd "/tmp/boehm_gc/" || exit 1
find -type f | xargs sed --in-place -e 's/libgc/libpar_gc/g' || exit 1
./configure "--prefix=$(pwd)/dist" "--enable-cplusplus" || exit 1
make || exit 1
make install || exit 1
if [ ! -f "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so.bak" ]
then
        cp "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so" "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so.bak"
else
        echo "Not backing up original libpar_gc.so because there already was a backup."
fi
cp $(find "dist/lib/" -name "libpar_gc.so*") "${MERCURY_BIN_DIR}/lib/mercury/lib/" || exit 1


cd "${OLD_DIR}"


# COIN-OR OSI/CLP:
if [ ! -d "$COIN_DIR" ]
then
        svn co https://projects.coin-or.org/svn/Osi/stable/0.102 "$COIN_DIR" || exit 1
fi
cd "$COIN_DIR" || exit 1

# Here he will find gc.h and libpar_gc.so:
export LDFLAGS="-L${MERCURY_BIN_DIR}/lib/mercury/lib"
export CXXFLAGS="-I${MERCURY_BIN_DIR}/lib/mercury/inc -fPIC"
# Add the new mygc.h header which accounts for new->GC_MALLOC and replace malloc->GC_MALLOC
for f in $(find . -name \*.h -or -name \*.c); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
for f in $(find . -name \*.hpp -or -name \*.cpp); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<malloc\>/MY_MALLOC/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<realloc\>/MY_REALLOC/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<free\>/MY_FREE/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<strdup\>/MY_STRDUP/g'
# We only need CLP, CoinUtils, and OSI.
export COIN_SKIP_PROJECTS="DyLP Vol"
./configure "--prefix=$(pwd)/dist" "--enable-static" || exit 1
# Tell him to link GC and some other libraries (do we need them?)
find . -name Makefile | xargs sed --in-place -e 's/ADDLIBS =/ADDLIBS = -ldl -lpthread -lpar_gc/g'

# Here comes the header:
cat >CoinUtils/src/mygc.h <<\EOF
//#define GC_DEBUG
#define GC_THREADS
#include <gc.h>

#include <stdlib.h>
#include <string.h>

#ifndef malloc
#define malloc(x) MY_MALLOC(x)
#endif

#ifndef realloc
#define realloc(x, y) MY_REALLOC(x, y)
#endif

#ifndef calloc
#define calloc(x, y) MY_MALLOC((x) * (y))
#endif

#ifndef free
#define free(x) MY_FREE(x)
#endif

#ifndef strdup
#define strdup(x) MY_STRDUP(x)
#endif


#ifndef MY_MALLOC
#define MY_MALLOC(x) GC_MALLOC(x)
#endif

#ifndef MY_REALLOC
#define MY_REALLOC(x, y) GC_REALLOC(x, y)
#endif

#ifndef MY_FREE
#define MY_FREE(x) GC_FREE(x)
#endif

#ifndef MY_STRDUP
#define MY_STRDUP(x) GC_STRDUP(x)
#endif


#ifdef __cplusplus
#ifndef MYGC
#define MYGC

#include <new>

inline void* operator new(std::size_t size) throw (std::bad_alloc)
{
  void* ptr = MY_MALLOC(size);
  if (!ptr) throw std::bad_alloc();
  return ptr;
}

inline void* operator new[](std::size_t size) throw (std::bad_alloc)
{
  void* ptr = MY_MALLOC(size);
  if (!ptr) throw std::bad_alloc();
  return ptr;
}

inline void operator delete(void* ptr) throw()
{
  if (ptr) MY_FREE(ptr);
}

inline void operator delete[](void* ptr) throw()
{
  if (ptr) MY_FREE(ptr);
}

inline void* operator new(std::size_t size, const std::nothrow_t&) throw()
{
  void* ptr = MY_MALLOC(size);
  return ptr;
}

inline void* operator new[](std::size_t size, const std::nothrow_t&) throw()
{
  void* ptr = MY_MALLOC(size);
  return ptr;
}

inline void operator delete(void* ptr, const std::nothrow_t&) throw()
{
  if (ptr) MY_FREE(ptr);
}

inline void operator delete[](void* ptr, const std::nothrow_t&) throw()
{
  if (ptr) MY_FREE(ptr);
}

#endif
#endif
EOF

# And now go for it:
make || exit 1
make install || exit 1
# The header must be includable:
cp CoinUtils/src/mygc.h dist/include/coin/ || exit 1


cd "${OLD_DIR}"

