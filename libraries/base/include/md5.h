/* MD5 message digest */
#pragma once

#include <stdint.h>

struct MD5Context {
	uint32_t buf[4];
	uint32_t bytes[2];
	uint32_t in[16];
};

void __hsbase_MD5Init(struct MD5Context *context);
void __hsbase_MD5Update(struct MD5Context *context, uint8_t const *buf, int len);
void __hsbase_MD5Final(uint8_t digest[16], struct MD5Context *context);
void __hsbase_MD5Transform(uint32_t buf[4], uint32_t const in[16]);
