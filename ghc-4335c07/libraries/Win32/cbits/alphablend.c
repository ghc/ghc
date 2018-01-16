#include <alphablend.h>

BOOL c_AlphaBlend ( HDC hdcDest, int nXOriginDest, int nYOriginDest, int nWidthDest, int hHeightDest
                  , HDC hdcSrc, int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc
                  , PBLENDFUNCTION pblendFunction)
{
    BLENDFUNCTION blendFunction;
    blendFunction.BlendOp             = pblendFunction->BlendOp;
    blendFunction.BlendFlags          = pblendFunction->BlendFlags;
    blendFunction.SourceConstantAlpha = pblendFunction->SourceConstantAlpha;
    blendFunction.AlphaFormat         = pblendFunction->AlphaFormat;
    return AlphaBlend ( hdcDest, nXOriginDest, nYOriginDest, nWidthDest, hHeightDest
                      , hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc
                      , blendFunction);
}
