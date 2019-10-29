package App::Yath::Command::which;
use strict;
use warnings;

our $VERSION = '0.001100';

use App::Yath::Util qw/find_pfile/;

use Test2::Harness::Util::File::JSON;

use parent 'App::Yath::Command';
use Test2::Harness::Util::HashBase;

sub group { 'persist' }

sub summary  { "Locate the persistent test runner" }
sub cli_args { "" }

sub description {
    return <<"    EOT";
This will tell you about any persistent runners it can find.
    EOT
}

sub run {
    my $self = shift;

    my $pfile = find_pfile();

    unless ($pfile) {
        print "\nNo persistent harness was found for the current path.\n\n";
        return 0;
    }

    print "\nFound: $pfile\n";
    my $data = Test2::Harness::Util::File::JSON->new(name => $pfile)->read();
    print "  PID: $data->{pid}\n";
    print "  Dir: $data->{dir}\n";
    print "\n";

    return 0;
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

App::Yath::Command::which - Command to check if there is a persistent runner
running.

=head1 DESCRIPTION

B<PLEASE NOTE:> Test2::Harness is still experimental, it can all change at any
time. Documentation and tests have not been written yet!

=head1 SOURCE

The source code repository for Test2-Harness can be found at
F<http://github.com/Test-More/Test2-Harness/>.

=head1 MAINTAINERS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 AUTHORS

=over 4

=item Chad Granum E<lt>exodist@cpan.orgE<gt>

=back

=head1 COPYRIGHT

Copyright 2019 Chad Granum E<lt>exodist7@gmail.comE<gt>.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

See F<http://dev.perl.org/licenses/>

=cut
