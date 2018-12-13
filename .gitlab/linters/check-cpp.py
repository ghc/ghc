#!/usr/bin/env python3

# A linter to warn for ASSERT macros which are separated from their argument
# list by a space, which Clang's CPP barfs on

from linter import run_linters, RegexpLinter

linters = [
    RegexpLinter(r'ASSERT\s+\(',
                 message='CPP macros should not have a space between the macro name and their argument list'),
    RegexpLinter(r'ASSERT2\s+\(',
                 message='CPP macros should not have a space between the macro name and their argument list'),
    RegexpLinter(r'#ifdef\s+',
                 message='`#if defined(x)` is preferred to `#ifdef x`'),
    RegexpLinter(r'#if\s+defined\s+',
                 message='`#if defined(x)` is preferred to `#if defined x`'),
    RegexpLinter(r'#ifndef\s+',
                 message='`#if !defined(x)` is preferred to `#ifndef x`'),
]

if __name__ == '__main__':
    run_linters(linters)
