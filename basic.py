#!/usr/bin/env python3
# pylint: disable=C0111
import sys
import bparser


def main():
    code = sys.stdin.read()
    bparser.BParser().execute(code)


if __name__ == '__main__':
    main()
