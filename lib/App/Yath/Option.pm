package App::Yath::Option;
use strict;
use warnings;

our $VERSION = '1.000000';

use Carp qw/confess/;

use Test2::Harness::Util::HashBase qw{
    <title
    <field <name <type <trace

    <prefix <short <alt

    <pre_command <from_plugin <from_command

    <no_build
    <pre_process
    <adds_options

    <default <normalize <action <negate
    <env_vars <clear_env_vars

    +applicable

    <builds
    <category
    <description
    <short_examples <long_examples
};

my %TYPES = (
    b => 1,
    c => 1,
    s => 1,
    m => 1,
    d => 1,
    D => 1,
    h => 1,
    H => 1,
);
sub valid_type { $TYPES{$_[-1]} }

my %LONG_TO_SHORT_TYPES = (
    bool    => 'b',
    boolean => 'b',

    count    => 'c',
    counter  => 'c',
    counting => 'c',

    scalar => 's',
    string => 's',
    number => 's',

    multi    => 'm',
    multiple => 'm',
    list     => 'm',
    array    => 'm',

    default => 'd',
    def     => 'd',

    'multi-def'        => 'D',
    'multiple-default' => 'D',
    'list-default'     => 'D',
    'array-default'    => 'D',

    'hash' => 'h',
    'hash-list' => 'H',
);
sub canon_type { $LONG_TO_SHORT_TYPES{$_[-1]} }

my %REQUIRES_ARG = (s => 1, m => 1, h => 1, H => 1);
sub requires_arg { $REQUIRES_ARG{$_[0]->{+TYPE}} }

my %ALLOWS_ARG = (d => 1, D => 1);
sub allows_arg { $ALLOWS_ARG{$_[0]->{+TYPE}} || $REQUIRES_ARG{$_[0]->{+TYPE} } }

sub init {
    my $self = shift;

    confess "You must specify 'title' or both 'field' and 'name'"
        unless $self->{+TITLE} || ($self->{+FIELD} && $self->{+NAME});

    confess "The 'prefix' attribute is required"
        unless $self->{+PREFIX};

    confess "The 'alt' attribute must be an array-ref"
        if $self->{+ALT} && ref($self->{+ALT}) ne 'ARRAY';

    if (my $title = $self->{+TITLE}) {
        $self->{+FIELD} //= $title;
        $self->{+NAME} //= ($self->{+FROM_PLUGIN} && $self->{+PREFIX}) ? "$self->{+PREFIX}-$title" : $title;
    }

    $self->{+FIELD} =~ s/-/_/g;
    $self->{+NAME}  =~ s/_/-/g;

    if (my $class = $self->{+BUILDS}) {
        confess "class '$class' does not have a '$self->{+FIELD}' method"
            unless $self->{+NO_BUILD} || $class->can($self->{+FIELD});
    }

    $self->{+TYPE} //= 'b';
    $self->{+TYPE} = $self->canon_type($self->{+TYPE}) // $self->{+TYPE} if length($self->{+TYPE}) > 1;
    confess "Invalid type '$self->{+TYPE}'" unless $self->valid_type($self->{+TYPE});

    if (my $def = $self->{+DEFAULT}) {
        my $ref = ref($def);
        confess "'default' must be a simple scalar, or a coderef, got a '$ref'" if $ref && $ref ne 'CODE';
    }

    for my $key (NORMALIZE(), ACTION()) {
        my $val = $self->{$key} or next;
        my $ref = ref($val) || 'not a ref';
        next if $ref eq 'CODE';
        confess "'$key' must be undef, or a coderef, got '$ref'";
    }

    $self->{+TRACE}       //= [caller(1)];
    $self->{+CATEGORY}    //= 'NO CATEGORY - FIX ME';
    $self->{+DESCRIPTION} //= 'NO DESCRIPTION - FIX ME';

    for my $key (sort keys %$self) {
        confess "'$key' is not a valid option attribute"
            unless $self->can(uc($key));
    }

    return $self;
}

sub applicable {
    my $self = shift;
    my ($options) = @_;
    my $cb = $self->{+APPLICABLE} or return 1;
    return $self->$cb($options);
}

sub long_args {
    my $self = shift;

    return ($self->{+NAME}, @{$self->{+ALT} || []});
}

sub option_slot {
    my $self = shift;
    my ($settings) = @_;

    confess "A settings instance is required" unless $settings;
    return $settings->define_prefix($self->{+PREFIX})->vivify_field($self->{+FIELD});
}

sub get_default {
    my $self = shift;

    for my $var (@{$self->{+ENV_VARS} // []}) {
        my ($neg) = $var =~ s/^(!)//;
        next unless exists $ENV{$var};
        return !$ENV{$var} if $neg;
        return $ENV{$var};
    }

    if (defined $self->{+DEFAULT}) {
        my $def = $self->{+DEFAULT};

        return $self->$def() if ref($def);

        return $def;
    }

    return 0
        if $self->{+TYPE} eq 'c'
        || $self->{+TYPE} eq 'b';

    return []
        if $self->{+TYPE} eq 'm'
        || $self->{+TYPE} eq 'D';

    return {}
        if $self->{+TYPE} eq 'h'
        || $self->{+TYPE} eq 'H';

    return undef;
}

sub get_normalized {
    my $self = shift;
    my ($raw) = @_;

    return $self->{+NORMALIZE}->($raw)
        if $self->{+NORMALIZE};

    return $raw ? 1 : 0
        if $self->{+TYPE} eq 'b';

    if (lc($self->{+TYPE}) eq 'h') {
        my ($key, $val) = split /=/, $raw, 2;

        if ($self->{+TYPE} eq 'H') {
            $val //= '';
            $val = [split /,/, $val];
            return [$key, $val];
        }

        return [$key, $val // 1];
    }

    return $raw;
}

my %HANDLERS = (
    c => sub { ${$_[0]}++ },
    m => sub { push @{${$_[0]} //= []} => $_[1] },
    D => sub { push @{${$_[0]} //= []} => $_[1] },
    h => sub {
        my $hash = ${$_[0]} //= {};
        my $key = $_[1]->[0];
        my $val = $_[1]->[1];

        push @{$hash->{'@'} //= []} => $key unless $hash->{$key};
        $hash->{$key} = $val;
    },
    H => sub {
        my $hash = ${$_[0]} //= {};
        my $key = $_[1]->[0];
        my $vals = $_[1]->[1];

        push @{$hash->{'@'} //= []} => $key unless $hash->{$key};
        push @{$hash->{$key} //= []} => @$vals;
    },
);

sub handle {
    my $self = shift;
    my ($raw, $settings, $options, $list) = @_;

    confess "A settings instance is required" unless $settings;
    confess "An options instance is required" unless $options;

    my $slot = $self->option_slot($settings);
    my $norm = $self->get_normalized($raw);

    my $handler = $HANDLERS{$self->{+TYPE}} //= sub { ${$_[0]} = $_[1] };

    return $self->{+ACTION}->($self->{+PREFIX}, $self->{+FIELD}, $raw, $norm, $slot, $settings, $handler, $options)
        if $self->{+ACTION};

    return $handler->($slot, $norm);
}

sub handle_negation {
    my $self = shift;
    my ($settings, $options) = @_;

    confess "A settings instance is required" unless $settings;
    confess "An options instance is required" unless $options;

    my $slot = $self->option_slot($settings);

    return $self->{+NEGATE}->($self->{+PREFIX}, $self->{+FIELD}, $slot, $settings, $options)
        if $self->{+NEGATE};

    return $$slot = 0
        if $self->{+TYPE} eq 'b'
        || $self->{+TYPE} eq 'c';

    return @{$$slot //= []} = ()
        if $self->{+TYPE} eq 'm'
        || $self->{+TYPE} eq 'D';

    return %{$$slot //= {}} = ()
        if $self->{+TYPE} eq 'h'
        || $self->{+TYPE} eq 'H';

    return $$slot = undef;
}

sub trace_string {
    my $self  = shift;
    my $trace = $self->{+TRACE} or return "[UNKNOWN]";
    return "$trace->[1] line $trace->[2]";
}

my %TYPE_LONG_ARGS = (
    b => [''],
    c => [''],
    s => [' ARG', '=ARG'],
    m => [' ARG', '=ARG'],
    d => ['[=ARG]'],
    D => ['[=ARG]'],
    h => [' KEY=VAL', '=KEY=VAL'],
    H => [' KEY=VAL1,VAL2,...', '=KEY=VAL1,VAL2,...'],
);

my %TYPE_SHORT_ARGS = (
    b => [''],
    c => [''],
    s => [' ARG', '=ARG'],
    m => [' ARG', '=ARG'],
    d => ['[=ARG]', '[ARG]'],
    D => ['[=ARG]', '[ARG]'],
    h => [' KEY=VAL', '=KEY=VAL'],
    H => [' KEY=VAL1,VAL2,...', '=KEY=VAL1,VAL2,...'],
);

my %TYPE_NOTES = (
    'c' => "Can be specified multiple times",
    'm' => "Can be specified multiple times",
    'D' => "Can be specified multiple times",
    'h' => "Can be specified multiple times",
    'H' => "Can be specified multiple times. If the same key is listed multiple times the value lists will be appended together.",
);

sub cli_docs {
    my $self = shift;

    my @forms = (map { "--$self->{+NAME}$_" } @{$self->{+LONG_EXAMPLES}  || $TYPE_LONG_ARGS{$self->{+TYPE}}});

    for my $alt (@{$self->{+ALT} || []}) {
        push @forms => (map { "--$alt$_" } @{$self->{+LONG_EXAMPLES}  || $TYPE_LONG_ARGS{$self->{+TYPE}}});
    }

    push @forms => map { "-$self->{+SHORT}$_" } @{$self->{+SHORT_EXAMPLES} || $TYPE_SHORT_ARGS{$self->{+TYPE}}}
        if $self->{+SHORT};

    push @forms => "--no-$self->{+NAME}";

    my @out;

    require App::Yath::Util;
    require Test2::Util::Term;

    my $width = Test2::Util::Term::term_size() - 20;
    $width = 80 unless $width && $width >= 80;

    push @out => App::Yath::Util::fit_to_width($width, ",  ", \@forms);

    my $desc = App::Yath::Util::fit_to_width($width, " ", $self->{+DESCRIPTION});
    $desc =~ s/^/  /gm;
    push @out => $desc;

    push @out => "\n  Can also be set with the following environment variables: " . join(", ", @{$self->{+ENV_VARS}}) if $self->{+ENV_VARS};

    push @out => "\n  Note: " . $TYPE_NOTES{$self->{+TYPE}} if $TYPE_NOTES{$self->{+TYPE}};

    return join "\n" => @out;
}

sub pod_docs {
    my $self = shift;

    my @forms = (map { "--$self->{+NAME}$_" } @{$self->{+LONG_EXAMPLES}  || $TYPE_LONG_ARGS{$self->{+TYPE}}});
    for my $alt (@{$self->{+ALT} || []}) {
        push @forms => (map { "--$alt$_" } @{$self->{+LONG_EXAMPLES}  || $TYPE_LONG_ARGS{$self->{+TYPE}}});
    }
    push @forms => map { "-$self->{+SHORT}$_" } @{$self->{+SHORT_EXAMPLES} || $TYPE_SHORT_ARGS{$self->{+TYPE}}}
        if $self->{+SHORT};
    push @forms => "--no-$self->{+NAME}";

    my @out = map { "=item $_" } @forms;

    push @out => $self->{+DESCRIPTION};

    push @out => "Can also be set with the following environment variables: " . join(", ", map { "C<$_>" } @{$self->{+ENV_VARS}}) if $self->{+ENV_VARS};

    push @out => $TYPE_NOTES{$self->{+TYPE}} if $TYPE_NOTES{$self->{+TYPE}};

    return join("\n\n" => @out) . "\n";
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

App::Yath::Option - Representation of a yath option.

=head1 DESCRIPTION

This class represents a single command line option for yath.

=head1 SYNOPSIS

You usually will not be creating option instances directly. Usually you will
use App::Yath::Options which provides sugar, and helps make sure options get to
the right place.

    use App::Yath::Options;

    # You can specify a single option:
    option color => (
        prefix      => 'display',
        category    => "Display Options",
        description => "Turn color on, default is true if STDOUT is a TTY.",
        default     => sub { -t STDOUT ? 1 : 0 },
    );

    # If you are specifying multiple options you can use an option_group to
    # define common parameters.
    option_group {prefix => 'display', category => "Display Options"} => sub {
        option color => (
            description => "Turn color on, default is true if STDOUT is a TTY.",
            default     => sub { -t STDOUT ? 1 : 0 },
        );

        option verbose => (
            short       => 'v',
            type        => 'c',
            description => "Be more verbose",
            default     => 0,
        );
    };

=head1 ATTRIBUTES

These can be provided at object construction, or are generated internally.

=head2 CONSTRUCTION ONLY

=over 4

=item applicable => sub { ($opt, $options) = @_; ...; return $bool }

This is callback is used by the C<applicable()> method.

=back

=head2 READ-ONLY

=over 4

=item $class->new(action => sub ...)

=item $coderef = $opt->action()

    option foo => (
        ...,
        action => sub {
            my ($prefix, $field_name, $raw_value, $normalized_value, $slot_ref, $settings, $handler, $options) = @_;

            # If no action is specified the following is all that is normally
            # done. Having an action means this is not done, so if you want the
            # value stored you must call this or similar.
            $handler->($slot, $normalized_value);
        },
    );

=item $class->new(adds_options => $bool)

=item $bool = $opt->adds_options()

If this is true then it means using this option could result in more options
being available (example: Loading a plugin).

=item $class->new(alt => ['alt1', 'alt2', ...])

=item $arrayref = $opt->alt()

Provide alternative names for the option. These are aliases that can be used to
achieve the same thing on the command line. This is mainly useful for
backcompat if an option is renamed.

=item $class->new(builds => ...)

=item $val = $opt->builds()



=item $class->new(category => ...)

=item $val = $opt->category()

=item $class->new(clear_env_vars => ...)

=item $val = $opt->clear_env_vars()

=item $class->new(default => ...)

=item $val = $opt->default()

=item $class->new(description => ...)

=item $val = $opt->description()

=item $class->new(env_vars => ...)

=item $val = $opt->env_vars()

=item $class->new(field => ...)

=item $val = $opt->field()

=item $class->new(from_command => ...)

=item $val = $opt->from_command()

=item $class->new(from_plugin => ...)

=item $val = $opt->from_plugin()

=item $class->new(long_examples => ...)

=item $val = $opt->long_examples()

=item $class->new(name => ...)

=item $val = $opt->name()

=item $class->new(negate => ...)

=item $val = $opt->negate()

=item $class->new(no_build => ...)

=item $val = $opt->no_build()

=item $class->new(normalize => ...)

=item $val = $opt->normalize()

=item $class->new(pre_command => ...)

=item $val = $opt->pre_command()

=item $class->new(pre_process => ...)

=item $val = $opt->pre_process()

=item $class->new(prefix => ...)

=item $val = $opt->prefix()

=item $class->new(short => ...)

=item $val = $opt->short()

=item $class->new(short_examples => ...)

=item $val = $opt->short_examples()

=item $class->new(title => ...)

=item $val = $opt->title()

=item $class->new(trace => ...)

=item $val = $opt->trace()

=item $class->new(type => ...)

=item $val = $opt->type()

=back

=head1 METHODS

=over 4

=item $bool = $opt->allows_arg()

True if arguments can be provided to the option (based on type).

=item $bool = $opt->applicable($options)

If an option provides an applicability callback this will use it to determine
if the option is applicable given the L<App::Yath::Options> instance.

If no callback was provided then this returns true.

=item $opt->canon_type()

=item $opt->get_default()

=item $opt->get_normalized()

=item $opt->handle()

=item $opt->handle_negation()

=item $opt->init()

=item $opt->long_args()

=item $opt->option_slot()

=item $opt->requires_arg()

=item $opt->trace_string()

=item $opt->valid_type()

=back

=head2 DOCUMENTATION GENERATION

=over 4

=item $string = $opt->cli_docs()

Get the option documentation in a format that works for the C<yath help
COMMAND> command.

=item $string = $opt->pod_docs()

Get the option documentation in POD format.

    =item ....

    .. option details ...

=back

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

Copyright 2020 Chad Granum E<lt>exodist7@gmail.comE<gt>.

This program is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.

See F<http://dev.perl.org/licenses/>

=cut
