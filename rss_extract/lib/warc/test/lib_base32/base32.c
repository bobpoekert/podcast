/*
 * CyoDecode.c - part of the CyoEncode library
 *
 * Copyright (c) 2009-2015, Graham Bull.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdlib.h>
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#define xstr(a) str(a)
#define str(a) #a
#define assert(v) if (!(v)) caml_failwith("assert failed: " xstr(v))

static const size_t BASE32_INPUT = 5;
static const size_t BASE32_OUTPUT = 8;
static const char* const BASE32_TABLE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567=";

static size_t cyoBaseXXEncodeGetLength(size_t srcBytes, size_t inputBytes, size_t outputBytes)
{
    return (((srcBytes + inputBytes - 1) / inputBytes) * outputBytes) + 1; /*plus terminator*/
}

size_t cyoBase32EncodeGetLength(size_t srcBytes)
{
    return cyoBaseXXEncodeGetLength(srcBytes, BASE32_INPUT, BASE32_OUTPUT);
}

size_t cyoBase32EncodeA(char* dest, const void* src, size_t srcBytes)
{
    if (dest && src)
    {
        unsigned char* pSrc = (unsigned char*)src;
        char* pDest = dest;
        size_t dwSrcSize = srcBytes;
        size_t dwDestSize = 0;
        size_t dwBlockSize;
        unsigned char n1, n2, n3, n4, n5, n6, n7, n8;

        while (dwSrcSize >= 1)
        {
            /* Encode inputs */
            dwBlockSize = (dwSrcSize < BASE32_INPUT ? dwSrcSize : BASE32_INPUT);
            n1 = n2 = n3 = n4 = n5 = n6 = n7 = n8 = 0;
            switch (dwBlockSize)
            {
            case 5:
                n8 = (pSrc[4] & 0x1f);
                n7 = ((pSrc[4] & 0xe0) >> 5);
            case 4:
                n7 |= ((pSrc[3] & 0x03) << 3);
                n6 = ((pSrc[3] & 0x7c) >> 2);
                n5 = ((pSrc[3] & 0x80) >> 7);
            case 3:
                n5 |= ((pSrc[2] & 0x0f) << 1);
                n4 = ((pSrc[2] & 0xf0) >> 4);
            case 2:
                n4 |= ((pSrc[1] & 0x01) << 4);
                n3 = ((pSrc[1] & 0x3e) >> 1);
                n2 = ((pSrc[1] & 0xc0) >> 6);
            case 1:
                n2 |= ((pSrc[0] & 0x07) << 2);
                n1 = ((pSrc[0] & 0xf8) >> 3);
                break;

            default:
                assert(0);
            }
            pSrc += dwBlockSize;
            dwSrcSize -= dwBlockSize;

            /* Validate */
            assert(n1 <= 31);
            assert(n2 <= 31);
            assert(n3 <= 31);
            assert(n4 <= 31);
            assert(n5 <= 31);
            assert(n6 <= 31);
            assert(n7 <= 31);
            assert(n8 <= 31);

            /* Padding */
            switch (dwBlockSize)
            {
            case 1: n3 = n4 = 32;
            case 2: n5 = 32;
            case 3: n6 = n7 = 32;
            case 4: n8 = 32;
            case 5:
                break;

            default:
                assert(0);
            }

            /* 8 outputs */
            *pDest++ = BASE32_TABLE[n1];
            *pDest++ = BASE32_TABLE[n2];
            *pDest++ = BASE32_TABLE[n3];
            *pDest++ = BASE32_TABLE[n4];
            *pDest++ = BASE32_TABLE[n5];
            *pDest++ = BASE32_TABLE[n6];
            *pDest++ = BASE32_TABLE[n7];
            *pDest++ = BASE32_TABLE[n8];
            dwDestSize += BASE32_OUTPUT;
        }
        *pDest++ = '\x0'; /*append terminator*/

        return dwDestSize;
    }
    else
        return 0; /*ERROR - null pointer*/
}

value call_base32_encode(value inp) {
    CAMLparam1(inp);
    char *instring = String_val(inp);
    size_t insize = caml_string_length(inp);
    size_t res_size = cyoBase32EncodeGetLength(insize);
    value res = caml_alloc_string(res_size);
    char *res_string = String_val(res);
    cyoBase32EncodeA(res_string, instring, insize);
    CAMLreturn(res);
}