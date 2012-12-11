/*
 * <copyright-info>
 * IBM Confidential
 * OCO Source Materials
 * 2810
 * (C) Copyright IBM Corp. 2004, 2011
 * The source code for this program is not published or otherwise
 * divested of its trade secrets, irrespective of what has
 * been deposited with the U.S. Copyright Office.
 * </copyright-info>
 */
#include "init.h"

#if defined(_WIN32) || defined(_WIN64)
#include "opengl.h"

int freetypegl_init() {
    GLenum err = glewInit();
    if (GLEW_OK != err) {
        fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
        return -1;
    }
    return 0;
}

#else

int freetypegl_init() { return 0; }

#endif
