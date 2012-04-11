COIN_DIR=$1
MERCURY_SRC_DIR=$2
MERCURY_BIN_DIR=$3
OLD_DIR=$(pwd)

if [ -d "$COIN_DIR/dist" ]
then
        echo "COIN source directory $COIN_DIR already exists. Will not install."
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
find -type f | xargs sed --in-place -e 's/libgc/libpar_gc/g'
./configure "--prefix=$(pwd)/dist" "--enable-cplusplus"
make
make install
if [ ! -f "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so.bak" ]
then
        cp "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so" "${MERCURY_BIN_DIR}/lib/mercury/lib/libpar_gc.so.bak"
else
        echo "Not backing up original libpar_gc.so because there already was a backup."
fi
cp $(find "dist/lib/" -name "libpar_gc.so*") "${MERCURY_BIN_DIR}/lib/mercury/lib/"



# COIN-OR OSI/CLP:
if [ ! -d "$COIN_DIR" ]
then
        svn co https://projects.coin-or.org/svn/Osi/stable/0.102 "$COIN_DIR" || exit 1
        cd "$COIN_DIR" || exit 1
fi

# Here he will find gc.h and libpar_gc.so:
export LDFLAGS="-L${MERCURY_BIN_DIR}/lib/mercury/lib/"
export CPPFLAGS="-I${MERCURY_BIN_DIR}/lib/mercury/inc/"
# Add the new mygc.h header which accounts for new->GC_MALLOC and replace malloc->GC_MALLOC
for f in $(find . -name \*.h -or -name \*.c); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
for f in $(find . -name \*.hpp -or -name \*.cpp); do cp "$f" "$f.tmp"; echo '#include "mygc.h"' >"$f"; grep -v '#include "mygc.h"' "$f.tmp" >>"$f"; rm "$f.tmp"; done
# We do this with macros in mygc.h now:
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<malloc\>/GC_MALLOC_UNCOLLECTABLE/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<realloc\>/GC_REALLOC/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<free\>/GC_FREE/g'
find . -name \*.h -or -name \*.c -or -name \*.hpp -or -name \*.cpp | xargs sed --in-place -e 's/\<strdup\>/GC_STRDUP/g'
# We only need CLP, CoinUtils, and OSI.
export COIN_SKIP_PROJECTS="DyLP Vol"
./configure "--prefix=$(pwd)/dist"
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
make
make install
# The header must be includable:
cp CoinUtils/src/mygc.h dist/include/coin/


cd "${OLD_DIR}"

