#! /usr/bin/env perl

local $, = ' ';
local $\ = "\n";

print "'(";
while (<>) {
    chomp;
    @fields = split /,/;
    print "(@fields)";
}
print ")";
