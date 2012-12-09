#ifndef __FONT_DESC_H__
#define __FONT_DESC_H__

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    char * family;
    float size;
    int bold;
    int italic;
} font_desc_t;

/**
 *  Search for a font filename that match description.
 *
 *  @param family   font family
 *  @param size     font size
 *  @param bold     whether font is bold
 *  @param italic   whether font is italic
 *
 *  @return Requested font filename (or NULL if not found).
 *          need to free() the resulting non-NULL string.
 */
char *
font_desc_find_filename( const font_desc_t * );


#ifdef __cplusplus
}
#endif

#endif
