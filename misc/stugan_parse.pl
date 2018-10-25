#!/usr/bin/env perl

# This reads the original Stugan code and prints it out with newlines.
# There are no newlines in the original code, instead each block of 160 
# characters corresponds to a line.

use warnings;
use strict;

# Read complete Stugan data (there are no UNIX newlines)
my $Text = <STDIN>;

# Each line is 160 characters
my $Len = 160;
my $Offset = 0;

# Read lines and print out reversed
while ($Offset < length($Text)) {
    my $str = substr($Text, $Offset, $Len);

    for ($str) {
	# trim string
	s/^\s+|\s+$//g;
    }
    
    print(reverse($str) . "\n");
    $Offset += $Len;
}
