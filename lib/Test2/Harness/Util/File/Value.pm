package Test2::Harness::Util::File::Value;
use strict;
use warnings;

use parent 'Test2::Harness::Util::File';
use Test2::Harness::Util::HashBase;

sub read {
    my $self = shift;
    my $out = $self->SUPER::read(@_);
    chomp($out);
    return $out;
}

sub read_line {
    my $self = shift;
    my $out = $self->SUPER::read_line(@_);
    chomp($out);
    return $out;
}

1;
