requires "Carp" => "0";
requires "Config" => "0";
requires "Cwd" => "0";
requires "Data::Dumper" => "0";
requires "Data::UUID" => "1.148";
requires "Exporter" => "0";
requires "Fcntl" => "0";
requires "File::Path" => "2.11";
requires "File::Spec" => "0";
requires "File::Temp" => "0";
requires "Filter::Util::Call" => "0";
requires "Getopt::Long" => "2.36";
requires "HTTP::Tiny" => "0.070";
requires "IO::Compress::Bzip2" => "0";
requires "IO::Compress::Gzip" => "0";
requires "IO::Handle" => "1.27";
requires "IO::Uncompress::Bunzip2" => "0";
requires "IO::Uncompress::Gunzip" => "0";
requires "IPC::Cmd" => "0";
requires "IPC::Open3" => "0";
requires "Importer" => "0.024";
requires "JSON::PP" => "0";
requires "List::Util" => "1.44";
requires "Long::Jump" => "0.000001";
requires "Module::Pluggable" => "0";
requires "POSIX" => "0";
requires "Scalar::Util" => "0";
requires "Scope::Guard" => "0";
requires "Symbol" => "0";
requires "Sys::Hostname" => "0";
requires "Term::Table" => "0.011";
requires "Test2" => "1.302165";
requires "Test2::API" => "1.302165";
requires "Test2::Bundle::Extended" => "0.000126";
requires "Test2::Event" => "1.302165";
requires "Test2::Event::V2" => "1.302165";
requires "Test2::Formatter" => "1.302165";
requires "Test2::Plugin::MemUsage" => "0.002002";
requires "Test2::Plugin::UUID" => "0.002001";
requires "Test2::Require::Module" => "0.000126";
requires "Test2::Tools::AsyncSubtest" => "0.000126";
requires "Test2::Tools::Subtest" => "0.000126";
requires "Test2::Util" => "1.302165";
requires "Test2::Util::Term" => "0.000126";
requires "Test2::V0" => "0.000126";
requires "Test::Builder" => "1.302165";
requires "Test::Builder::Formatter" => "1.302165";
requires "Test::More" => "1.302165";
requires "Time::HiRes" => "0";
requires "base" => "0";
requires "constant" => "0";
requires "goto::file" => "0.005";
requires "parent" => "0";
requires "perl" => "5.010000";
suggests "Cpanel::JSON::XS" => "0";
suggests "Email::Stuffer" => "0.016";
suggests "JSON::MaybeXS" => "0";
suggests "Term::ANSIColor" => "4.03";

on 'test' => sub {
  requires "File::Find" => "0";
};

on 'configure' => sub {
  requires "ExtUtils::MakeMaker" => "0";
};

on 'develop' => sub {
  requires "Test::Pod" => "1.41";
  requires "Test::Spelling" => "0.12";
};
