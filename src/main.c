#include <stdio.h>
#include "HsFFI.h"

#include <SDL/SDL.h>

#ifdef __GLASGOW_HASKELL__
#include "HSMain_stub.h"
extern void __stginit_HSMain ( void );
#endif

int main(int argc, char *argv[])
{
    int i;

    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_HSMain);
#endif

    my_hs_main();

    hs_exit();

    return 0;
}
