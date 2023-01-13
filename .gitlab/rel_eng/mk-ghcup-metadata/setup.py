#!/usr/bin/env python

from distutils.core import setup

setup(name='ghcup-metadata',
      author='Matthew Pickering',
      author_email='matthew@well-typed.com',
      py_modules=['mk_ghcup_metadata'],
      entry_points={
          'console_scripts': [
              'ghcup-metadata=mk_ghcup_metadata:main',
          ]
      }
     )
