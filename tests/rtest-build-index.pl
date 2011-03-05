#!/usr/bin/perl -s
use warnings;
use strict;
use locale;
use POSIX qw(locale_h);
use File::Find;
sub find_info_subdirs(@);
sub set_encoding($);
sub vprint(@);
sub msystem(@);

my $pid=$$;
our $verbose||=undef;
our $info_dir||="$ENV{PWD}/../doc/info/";
our $info_subdirs||=undef;
our @info_subdirs=find_info_subdirs($info_dir, 'maxima.info', $info_subdirs); #$(find $info_dir -name 'maxima.info' -execdir pwd \;);
our $xterm||='xterm';
our $xterm_opts||='-exec';
our $xterm_cmd||="$xterm $xterm_opts";
our $lisps||="sbcl:clisp:cmucl";
our @lisps=split(/:/,$lisps);
our $maxima_opts||='--init=/dev/null --very-quiet';
our $maxima_cmd||="$ENV{PWD}/../maxima-local";
our $maxima_batch_string||='describe("expand");system("printenv;locale;");read("quit?");';
our $run_rtest_build_index||=1;
our $rtest_build_index_bs||='load("rtest-run.lisp");';
our @locale=grep {/^LC_|LOCALE|^LANG$/ } keys %ENV;
our $default_locale=setlocale(LC_CTYPE);

if ($run_rtest_build_index =~ /^(1|true)$/oi) {
   for my $lisp (@lisps) {
      msystem("$maxima_cmd $maxima_opts -l $lisp --batch-string='$rtest_build_index_bs'");
   }
}
for my $dir (@info_subdirs) {
   my %locale_settings=set_encoding($dir);
   vprint "Testing in $dir";
   for my $lisp (@lisps) {
      open my $shell, "|/bin/bash";
      print $shell "cd $dir;";
      vprint "Setting LC_ALL=$locale_settings{'LC_ALL'}.\n";
      foreach my $e (keys %locale_settings) {
	 my $v=$locale_settings{$e};
	 print $shell "export $e=$v;";
      }
      vprint "$xterm_cmd $maxima_cmd $maxima_opts -l $lisp --batch-string='$maxima_batch_string';";
      print $shell "$xterm_cmd $maxima_cmd $maxima_opts -l $lisp --batch-string='$maxima_batch_string'";
   }
}

sub find_info_subdirs(@)
{
   my ($infodir,$maxima_info,$info_subdirs)=@_;
   if ($info_subdirs) {
      return split(/:/,$info_subdirs);
   }
   my %infodirs;
   my $wanted=sub {
      /^$maxima_info/s && $infodirs{$File::Find::dir}++;
   };
   &File::Find::find({wanted => $wanted}, $infodir);
   return keys %infodirs;
}
sub set_encoding($)
{
   my $dir=shift;
   my @path=split(m{/},"$dir");
   my ($lang,$code)=split(/\./,$path[-1]);
   my %encoding;
   my $locale;
   if($lang eq "info"){
      $locale=$default_locale;
   } else {
      $code=($code ? "UTF-8" : "ISO-8859-1");
      if(length($lang)==2){$lang=~s/(.+)/$1_\U$1/;}
      $locale="${lang}.${code}";
   }
   foreach (@locale) { $encoding{$_}=$locale; }
   $encoding{'LC_ALL'}=$locale;
   $encoding{'LANG'}=$locale;
   return %encoding;
}
sub vprint(@)
{
   local $,=" ";
   $verbose and print @_,"\n";
}
sub msystem(@)
{
   vprint @_;
   system(@_);
}
