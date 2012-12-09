#include "font-desc.h"

#include <stdio.h>
#if 0
#  if !defined(_WIN32) && !defined(_WIN64)
#    include <fontconfig/fontconfig.h>
#  endif
#endif

char *
font_desc_find_filename( const font_desc_t * );
{
// Use of fontconfig is disabled by default.
#if 1
    return NULL;
#else
#  if defined _WIN32 || defined _WIN64
    fprintf( stderr, "\"font_manager_match_description\" not implemented for windows.\n" );
    return 0;
#  endif
    char *filename = 0;
    int weight = FC_WEIGHT_REGULAR;
    int slant = FC_SLANT_ROMAN;
    if ( bold )
    {
        weight = FC_WEIGHT_BOLD;
    }
    if( italic )
    {
        slant = FC_SLANT_ITALIC;
    }
    FcInit();
    FcPattern *pattern = FcPatternCreate();
    FcPatternAddDouble( pattern, FC_SIZE, size );
    FcPatternAddInteger( pattern, FC_WEIGHT, weight );
    FcPatternAddInteger( pattern, FC_SLANT, slant );
    FcPatternAddString( pattern, FC_FAMILY, (FcChar8*) family );
    FcConfigSubstitute( 0, pattern, FcMatchPattern );
    FcDefaultSubstitute( pattern );
    FcResult result;
    FcPattern *match = FcFontMatch( 0, pattern, &result );
    FcPatternDestroy( pattern );

    if ( !match ) {
        fprintf( stderr, "fontconfig error: could not match family '%s'", family );
        return 0;
    }
    FcValue value;
    FcResult result = FcPatternGet( match, FC_FILE, 0, &value );
    FcPatternDestroy( match );
    if ( result ) {
        fprintf( stderr, "fontconfig error: could not match family '%s'", family );
        return 0;
    }
    return strdup( (char *)(value.u.s) );
#endif
}
