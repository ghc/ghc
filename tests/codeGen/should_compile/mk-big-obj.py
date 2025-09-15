#!/usr/bin/env python3

for i in range(70000):
    print(f'''
        int __attribute__((section("text.test{i}")))
        test{i}(void)
        {{ return {i}; }}
        ''')

