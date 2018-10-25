#!/usr/bin/env python

import sys

# Mapping of CP437 control characters as graphical characters
# http://www.unicode.org/Public/MAPPINGS/VENDORS/MISC/IBMGRAPH.TXT
char_map = {ord(c): ord(t) for c, t in
            zip(u'\x15\x14\x07\x13\x1b\x18\x1a\x19\x1d\x12\x17\x1c'
                u'\x7f\x16\x1e\x10\x1f\x11\x09\x08\x0a\x01\x02\x0f'
                u'\x0c\x0b\x06\x05\x03\x04\x0d\x0e',
                u'\u00a7\u00b6\u2022\u203c\u2190\u2191\u2192\u2193'
                u'\u2194\u2195\u21a8\u221f\u2302\u25ac\u25b2\u25ba'
                u'\u25bc\u25c4\u25cb\u25d8\u25d9\u263a\u263b\u263c'
                u'\u2640\u2642\u2660\u2663\u2665\u2666\u266a\u266b')}

text = sys.stdin.read()

# Each line is 160 characters
line_size = 160
offset = 0

while offset < len(text):
    # Read line and trim string
    line = text[offset : offset + line_size].strip()
    offset += line_size
    # Convert to UTF-8 and reverse string
    print(line.decode('cp437').translate(char_map)[::-1].encode('utf-8'))
